use kola_builtins::{BuiltinId, BuiltinLabel};
use kola_ir::{
    id::Id,
    instr::{
        Atom, BinaryExpr, BinaryOp, CallExpr, DoExpr, Expr, Func, HandleExpr, Identity, IfExpr,
        IsBool, IsChar, IsList, IsNum, IsRecord, IsStr, IsUnit, IsVariant, LetExpr, ListExpr,
        ListGetAt, ListIsAtLeast, ListIsExact, ListSplitAt, ListSplitHead, ListSplitTail,
        PatternMatchExpr, PatternMatcher, PatternSuccess, RecordAccessExpr, RecordExpr,
        RecordExtendExpr, RecordField, RecordGetAt, RecordHasField, RecordRestrictExpr,
        RecordUpdateExpr, RecordUpdateOp, RetExpr, Symbol, UnaryExpr, UnaryOp, VariantGet,
    },
    ir::{Ir, IrView},
};
use kola_runtime::{
    closure::Closure,
    env::{EnvArena, EnvIdx},
    heap::Heap,
    list::ListIdx,
    record::RecordIdx,
    string::StringIdx,
    value::Value,
};

use crate::{
    builtins,
    config::{MachineState, OperationConfig, PatternConfig, StandardConfig},
    cont::{Cont, ContFrame},
    handler::{Handler, ReturnClause},
    machine::Ctx,
};

#[inline]
pub fn eval_symbol(symbol: Symbol, env: EnvIdx, envs: &EnvArena) -> Result<Value, String> {
    // Look up the symbol in the environment
    envs.get(env, symbol)
        .ok_or_else(|| format!("Unbound variable: {}", symbol))
}

pub fn eval_atom(atom: Atom, env: EnvIdx, envs: &EnvArena) -> Result<Value, String> {
    match atom {
        Atom::Noop => Ok(Value::None),
        Atom::Bool(b) => Ok(Value::Bool(b)),
        Atom::Char(c) => Ok(Value::Char(c)),
        Atom::Num(n) => Ok(Value::Num(n)),
        Atom::Str(s) => Ok(Value::Str(Some(StringIdx::Static(s)))),
        Atom::Func(f) => {
            // Create a closure by capturing the current environment
            Ok(Value::Closure(Closure::new(env, f)))
        }
        Atom::Symbol(s) => eval_symbol(s, env, envs),
        Atom::Builtin(b) => Ok(Value::Builtin(b)),
        Atom::Tag(t) => Ok(Value::Tag(t)),
        Atom::Witness(w) => Ok(Value::Witness(w)),
        Atom::BuiltinWitness(bw) => Ok(Value::BuiltinWitness(bw)),
        Atom::Label(l) => Ok(Value::Label(l)),
    }
}

pub trait Eval {
    fn eval(&self, env: EnvIdx, cont: Cont, ctx: &mut Ctx, heap: &mut Heap) -> MachineState;
}

impl Eval for Expr {
    fn eval(&self, env: EnvIdx, cont: Cont, ctx: &mut Ctx, heap: &mut Heap) -> MachineState {
        match self {
            Expr::Ret(ret_expr) => ret_expr.eval(env, cont, ctx, heap),
            Expr::Call(call) => call.eval(env, cont, ctx, heap),
            Expr::Handle(handle) => handle.eval(env, cont, ctx, heap),
            Expr::Do(do_expr) => do_expr.eval(env, cont, ctx, heap),
            Expr::Let(let_expr) => let_expr.eval(env, cont, ctx, heap),
            Expr::If(if_expr) => if_expr.eval(env, cont, ctx, heap),
            Expr::Unary(unary_expr) => unary_expr.eval(env, cont, ctx, heap),
            Expr::Binary(binary_expr) => binary_expr.eval(env, cont, ctx, heap),
            Expr::List(list_expr) => list_expr.eval(env, cont, ctx, heap),
            Expr::Record(record_expr) => record_expr.eval(env, cont, ctx, heap),
            Expr::RecordExtend(record_extend_expr) => record_extend_expr.eval(env, cont, ctx, heap),
            Expr::RecordRestrict(record_restrict_expr) => {
                record_restrict_expr.eval(env, cont, ctx, heap)
            }
            Expr::RecordUpdate(record_update_expr) => record_update_expr.eval(env, cont, ctx, heap),
            Expr::RecordAccess(record_access_expr) => record_access_expr.eval(env, cont, ctx, heap),
            Expr::PatternMatch(pattern_match_expr) => pattern_match_expr.eval(env, cont, ctx, heap),
        }
    }
}

// M-RET : Return with a value
impl Eval for RetExpr {
    fn eval(&self, env: EnvIdx, mut cont: Cont, ctx: &mut Ctx, heap: &mut Heap) -> MachineState {
        // Evaluate the return machine state of value
        let value = match eval_atom(self.arg.get(&ctx.ir), env, &heap.envs) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Remove the top continuation frame
        let Some(cont_frame) = cont.pop() else {
            // If the continuation is empty, return the value
            return MachineState::Value(value);
        };

        match cont_frame {
            ContFrame::Pure { var, body, env } => {
                // M-RETCONT: <return V | γ | ((γ', x, N) :: σ, χ) :: κ> --> <N | γ'[x ↦ V] | (σ, χ) :: κ>
                //
                // When a return expression with value V reaches a pure continuation frame:
                // 1. The value V is evaluated in the current environment γ
                // 2. We extract the pure continuation frame containing variable x, body N, and environment γ'
                // 3. We bind the value V to variable x in the environment γ'
                // 4. We continue by evaluating N in this updated environment
                // 5. The continuation stack is updated by removing the top pure frame

                // Create new environment with the bound variable
                let env = heap.envs.insert(env, var, value);

                // Continue with the next expression
                MachineState::Standard(StandardConfig {
                    control: body.get(&ctx.ir),
                    env,
                    cont,
                })
            }
            ContFrame::Handler { handler, env } => {
                eval_handler_frame(handler, env, value, cont, ctx, heap, *self)
            }
            ContFrame::NumRec { data, step } => eval_num_rec(data, step, value, cont, ctx, heap),
            ContFrame::ListRec { data, step } => eval_list_rec(data, step, value, cont, ctx, heap),
            ContFrame::RecordRec { data, step } => {
                eval_record_rec(data, step, value, cont, ctx, heap)
            }
            ContFrame::StrRec { data, step } => eval_str_rec(data, step, value, cont, ctx, heap),
        }
    }
}

// M-RETHANDLER: <return V | γ | ([], (γ', H)) :: κ> --> <M | γ'[x ↦ V] | κ>,
// if H(return) = {return x ↦ M}
//
// When a return expression with value V reaches a handler frame with
// an empty pure continuation, we:
// 1. Extract the return clause from the handler H
// 2. Bind the returned value V to parameter x in the handler's environment γ'
// 3. Continue by evaluating the return handler's body M with the updated environment
// 4. Use the continuation κ below the current handler frame
#[inline]
fn eval_handler_frame(
    handler: Handler,
    env: EnvIdx,
    value: Value,
    cont: Cont,
    ctx: &Ctx,
    heap: &mut Heap,
    expr: RetExpr,
) -> MachineState {
    match handler.return_clause {
        ReturnClause::Identity => {
            // Identity function just returns the value
            if cont.is_empty() {
                // No more frames, return the final value
                MachineState::Value(value)
            } else {
                // Continue with the next frame
                MachineState::Standard(StandardConfig {
                    control: Expr::from(expr),
                    env,
                    cont,
                })
            }
        }
        ReturnClause::Function(Func { param, body }) => {
            // Create a new environment with the value bound to param
            let env = heap.envs.insert(env, param, value);

            // Evaluate the return handler body
            MachineState::Standard(StandardConfig {
                control: body.get(&ctx.ir),
                env,
                cont,
            })
        }
    }
}

#[inline]
fn eval_num_rec(
    data: f64,
    step: Closure,
    value: Value,
    mut cont: Cont,
    ctx: &Ctx,
    heap: &mut Heap,
) -> MachineState {
    if data >= 1.0 {
        cont.push(ContFrame::NumRec {
            data: data - 1.0,
            step,
        });
    }

    let Closure {
        env,
        func: Func { param, body },
    } = step;

    // Create argument for the step function
    let acc = (ctx.lexicon.label(BuiltinLabel::Acc), value); // value = result from previous step
    let head = (ctx.lexicon.label(BuiltinLabel::Head), Value::Num(data));
    let record = heap.records.alloc_fixed([acc, head]);

    let env = heap.envs.insert(env, param, Value::Record(record));

    MachineState::Standard(StandardConfig {
        control: body.get(&ctx.ir),
        env,
        cont,
    })
}

#[inline]
fn eval_list_rec(
    data: ListIdx,
    step: Closure,
    value: Value,
    mut cont: Cont,
    ctx: &Ctx,
    heap: &mut Heap,
) -> MachineState {
    let (head, tail) = heap.lists.split_front(data);

    // Continue with the next frame
    if let Some(tail) = tail {
        cont.push(ContFrame::ListRec { data: tail, step });
    }

    let Closure {
        env,
        func: Func { param, body },
    } = step;

    // Create argument for the step function
    let acc = (ctx.lexicon.label(BuiltinLabel::Acc), value); // value = result from previous step
    let head = (ctx.lexicon.label(BuiltinLabel::Head), head);
    let record = heap.records.alloc_fixed([acc, head]);

    let env = heap.envs.insert(env, param, Value::Record(record));

    MachineState::Standard(StandardConfig {
        control: body.get(&ctx.ir),
        env,
        cont,
    })
}

#[inline]
fn eval_record_rec(
    data: RecordIdx,
    step: Closure,
    value: Value,
    mut cont: Cont,
    ctx: &Ctx,
    heap: &mut Heap,
) -> MachineState {
    let (first, tail) = heap.records.split_front(data);

    let key = (
        ctx.lexicon.label(BuiltinLabel::Key),
        Value::Str(Some(StringIdx::Static(first.0))),
    );
    let val = (ctx.lexicon.label(BuiltinLabel::Value), first.1);
    let head = heap.records.alloc_fixed([key, val]);

    // Continue with the next frame
    if let Some(tail) = tail {
        cont.push(ContFrame::RecordRec { data: tail, step });
    }

    let Closure {
        env,
        func: Func { param, body },
    } = step;

    // Create argument for the step function
    let acc = (ctx.lexicon.label(BuiltinLabel::Acc), value); // value = result from previous step
    let head = (ctx.lexicon.label(BuiltinLabel::Head), Value::Record(head));
    let record = heap.records.alloc_fixed([acc, head]);

    let env = heap.envs.insert(env, param, Value::Record(record));

    MachineState::Standard(StandardConfig {
        control: body.get(&ctx.ir),
        env,
        cont,
    })
}

#[inline]
fn eval_str_rec(
    data: StringIdx,
    step: Closure,
    value: Value,
    mut cont: Cont,
    ctx: &Ctx,
    heap: &mut Heap,
) -> MachineState {
    let (head, tail) = heap.strings.split_front(data);

    // Continue with the next frame
    if let Some(tail) = tail {
        cont.push(ContFrame::StrRec { data: tail, step });
    }

    let Closure {
        env,
        func: Func { param, body },
    } = step;

    // Create argument for the step function
    let acc = (ctx.lexicon.label(BuiltinLabel::Acc), value); // value = result from previous step
    let head = (
        ctx.lexicon.label(BuiltinLabel::Head),
        Value::Str(Some(head)),
    );
    let record = heap.records.alloc_fixed([acc, head]);

    let env = heap.envs.insert(env, param, Value::Record(record));

    MachineState::Standard(StandardConfig {
        control: body.get(&ctx.ir),
        env,
        cont,
    })
}

// M-APP: <V W | γ | κ> --> <M | γ'[x ↦ W] | κ> if V = (γ', λx.M)
//
// This rule describes function application in the CEK machine:
// 1. V is a function value evaluating to a closure with environment γ' and function body M with parameter x
// 2. W is the argument value being passed to the function
// 3. The machine transitions to evaluating the function body M
// 4. Using an extended environment that adds the binding [x ↦ W] to the function's captured environment γ'
// 5. The continuation κ remains unchanged during function application
impl Eval for CallExpr {
    fn eval(&self, env: EnvIdx, mut cont: Cont, ctx: &mut Ctx, heap: &mut Heap) -> MachineState {
        let Self {
            bind,
            func,
            arg,
            next,
        } = *self;

        // Get the function and argument
        let func_val = match eval_atom(func.get(&ctx.ir), env, &heap.envs) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        let arg_val = match eval_atom(arg.get(&ctx.ir), env, &heap.envs) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        match func_val {
            // Apply the function to the argument
            Value::Closure(Closure {
                env: func_env,
                func: Func { param, body },
            }) => {
                // Create a pure continuation frame for the next expression
                let pure_frame = ContFrame::pure(bind, next, env);
                cont.push(pure_frame);

                // Create a new environment with the bound parameter
                let func_env = heap.envs.insert(func_env, param, arg_val);

                // Evaluate the function body
                MachineState::Standard(StandardConfig {
                    control: body.get(&ctx.ir),
                    env: func_env,
                    cont,
                })
            }
            // // If the function is a continuation, we apply it to the argument
            // Value::Cont(mut captured) => {
            //     // M-APPCONT: <V W | γ | κ> --> <return W | γ | κ' ++ κ>  if V = κ'
            //     //
            //     // This rule handles continuation application in the CEK machine:
            //     // 1. When we apply a captured continuation value (V = κ') to an argument W
            //     // 2. We transition to returning the argument value W
            //     // 3. The environment γ remains unchanged
            //     // 4. We prepend the captured continuation κ' to the current continuation κ,
            //     //    effectively composing the continuations
            //     // 5. This allows control to transfer to the point where the continuation was captured,
            //     //    with the argument W becoming the returned value at that point

            //     // Create a pure continuation frame for the next expression
            //     let pure_frame = ContFrame::pure(bind, next, env.alloc(heap));
            //     cont.push(pure_frame);

            //     captured.append(&mut cont);

            //     // Apply the captured continuation to the argument
            //     MachineState::Standard(StandardConfig {
            //         control: Expr::from(RetExpr { arg }),
            //         env,
            //         cont: captured,
            //     })
            // }
            Value::Builtin(builtin) => {
                eval_builtin(builtin, bind, arg_val, env, cont, next, ctx, heap)
            }
            Value::Tag(tag) => {
                let value = Value::Variant(heap.variants.alloc(tag, arg_val));

                // Create a new environment with the result bound to the variable
                let env = heap.envs.insert(env, bind, value);

                // Continue with the next expression
                MachineState::Standard(StandardConfig {
                    control: next.get(&ctx.ir),
                    env,
                    cont,
                })
            }
            // If it's neither, return an error
            _ => MachineState::Error(format!("Cannot apply non-function value: {:?}", func_val)),
        }
    }
}

fn eval_builtin(
    builtin: BuiltinId,
    bind: Symbol,
    arg: Value,
    env: EnvIdx,
    cont: Cont,
    next: Id<Expr>,
    ctx: &mut Ctx,
    heap: &mut Heap,
) -> MachineState {
    // Some builtins (list_rec, num_rec, record_rec, str_rec) interact
    // directly with the continuation stack and environment, returning
    // a MachineState. The rest (simple builtins) produce a Value.
    let result = match builtin {
        BuiltinId::ListRec => {
            return builtins::list_rec(bind, arg, env, cont, next, ctx, heap);
        }
        BuiltinId::NumRec => {
            return builtins::num_rec(bind, arg, env, cont, next, ctx, heap);
        }
        BuiltinId::RecordRec => {
            return builtins::record_rec(bind, arg, env, cont, next, ctx, heap);
        }
        BuiltinId::StrRec => {
            return builtins::str_rec(bind, arg, env, cont, next, ctx, heap);
        }

        BuiltinId::IoDebug => builtins::io_debug(arg, heap, ctx),
        BuiltinId::IoReadFile => builtins::io_read_file(arg, heap, ctx),
        BuiltinId::IoWriteFile => builtins::io_write_file(arg, heap, ctx),
        BuiltinId::ListLength => builtins::list_length(arg, heap, ctx),
        BuiltinId::ListIsEmpty => builtins::list_is_empty(arg, heap, ctx),
        BuiltinId::ListReverse => builtins::list_reverse(arg, heap, ctx),
        BuiltinId::ListSum => builtins::list_sum(arg, heap, ctx),
        BuiltinId::ListContains => builtins::list_contains(arg, heap, ctx),
        BuiltinId::ListAt => builtins::list_at(arg, heap, ctx),
        BuiltinId::ListFirst => builtins::list_first(arg, heap, ctx),
        BuiltinId::ListLast => builtins::list_last(arg, heap, ctx),
        BuiltinId::ListPrepend => builtins::list_prepend(arg, heap, ctx),
        BuiltinId::ListAppend => builtins::list_append(arg, heap, ctx),
        BuiltinId::ListConcat => builtins::list_concat(arg, heap, ctx),
        BuiltinId::NumAbs => builtins::num_abs(arg, heap, ctx),
        BuiltinId::NumSqrt => builtins::num_sqrt(arg, heap, ctx),
        BuiltinId::NumFloor => builtins::num_floor(arg, heap, ctx),
        BuiltinId::NumCeil => builtins::num_ceil(arg, heap, ctx),
        BuiltinId::NumRound => builtins::num_round(arg, heap, ctx),
        BuiltinId::NumSin => builtins::num_sin(arg, heap, ctx),
        BuiltinId::NumCos => builtins::num_cos(arg, heap, ctx),
        BuiltinId::NumTan => builtins::num_tan(arg, heap, ctx),
        BuiltinId::NumLn => builtins::num_ln(arg, heap, ctx),
        BuiltinId::NumLog10 => builtins::num_log10(arg, heap, ctx),
        BuiltinId::NumExp => builtins::num_exp(arg, heap, ctx),
        BuiltinId::NumPow => builtins::num_pow(arg, heap, ctx),
        BuiltinId::RecordSelect => builtins::record_select(arg, heap, ctx),
        BuiltinId::RecordInsert => builtins::record_insert(arg, heap, ctx),
        BuiltinId::RecordRemove => builtins::record_remove(arg, heap, ctx),
        BuiltinId::RecordRename => builtins::record_rename(arg, heap, ctx),
        BuiltinId::RecordContains => builtins::record_contains(arg, heap, ctx),
        BuiltinId::RecordKeys => builtins::record_keys(arg, heap, ctx),
        BuiltinId::RecordSize => builtins::record_size(arg, heap, ctx),
        BuiltinId::RecordMergeLeft => builtins::record_merge_left(arg, heap, ctx),
        BuiltinId::RecordMergeRight => builtins::record_merge_right(arg, heap, ctx),
        BuiltinId::SerdeFromJson => builtins::serde_from_json(arg, heap, ctx),
        BuiltinId::SerdeToJson => builtins::serde_to_json(arg, heap, ctx),
        BuiltinId::StrLength => builtins::str_length(arg, heap, ctx),
        BuiltinId::StrIsEmpty => builtins::str_is_empty(arg, heap, ctx),
        BuiltinId::StrReverse => builtins::str_reverse(arg, heap, ctx),
        BuiltinId::StrFirst => builtins::str_first(arg, heap, ctx),
        BuiltinId::StrLast => builtins::str_last(arg, heap, ctx),
        BuiltinId::StrContains => builtins::str_contains(arg, heap, ctx),
        BuiltinId::StrAt => builtins::str_at(arg, heap, ctx),
        BuiltinId::StrPrepend => builtins::str_prepend(arg, heap, ctx),
        BuiltinId::StrAppend => builtins::str_append(arg, heap, ctx),
        BuiltinId::StrConcat => builtins::str_concat(arg, heap, ctx),
    };

    match result {
        Ok(value) => {
            // Create a new environment with the result bound to the variable
            let env = heap.envs.insert(env, bind, value);

            // Continue with the next expression
            MachineState::Standard(StandardConfig {
                control: next.get(&ctx.ir),
                env,
                cont,
            })
        }
        Err(err) => MachineState::Error(err),
    }
}
//
// M-HANDLE: ⟨handle M with H | γ | κ⟩ → ⟨M | γ | ([], (γ, H)) :: κ⟩
//
// This rule pushes a handler frame onto the continuation stack and
// continues evaluating the source computation M with the handler available.
impl Eval for HandleExpr {
    fn eval(&self, env: EnvIdx, mut cont: Cont, _ctx: &mut Ctx, _heap: &mut Heap) -> MachineState {
        let Self {
            bind,
            source,
            clause,
            next,
        } = *self;

        // Create a pure continuation frame for the ANF binding
        let pure_frame = ContFrame::pure(bind, next, env);

        // Create handler from clauses (H in the rule)
        let handler = Handler::from_clauses(clause);
        let handler_frame = ContFrame::handler(handler, env);

        // Push handler frame first ([], (γ, H)) :: κ
        // This ensures the handler is deeper in the stack
        cont.push(handler_frame);

        // Push the frame with our ANF continuation on top
        cont.push(pure_frame);

        // Continue evaluating the source computation with the handler active
        MachineState::Standard(StandardConfig {
            control: Expr::Ret(RetExpr { arg: source }),
            env,
            cont,
        })
    }
}

// M-OP: ⟨(do ℓ V)^E | γ | κ⟩ → ⟨(do ℓ V)^E | γ | κ | []⟩^op
//
// This rule transitions to an operation configuration where the machine
// searches for a handler that can handle operation ℓ with argument V.
impl Eval for DoExpr {
    fn eval(&self, env: EnvIdx, mut cont: Cont, _ctx: &mut Ctx, _heap: &mut Heap) -> MachineState {
        let Self {
            bind,
            op,
            arg,
            next,
        } = *self;

        // Create a pure continuation frame
        let pure_frame = ContFrame::pure(bind, next, env);
        cont.push(pure_frame);

        // Transition to operation configuration
        // The forwarding continuation starts empty ([])
        MachineState::Operation(OperationConfig {
            op,                   // ℓ - operation name
            arg,                  // V - operation argument (as atom ID)
            env,                  // γ - current environment
            cont,                 // κ - current continuation
            forward: Cont::new(), // [] - empty forwarding continuation
        })
    }
}

// M-LET : <let x ← M in N | γ | (σ, χ) :: κ> --> <M | γ | ((γ, x, N) :: σ, χ) :: κ>
//
// This rule handles let-bindings in the CEK machine. When evaluating a let-expression:
// 1. We push a pure continuation frame onto the current continuation stack
// 2. The pure frame captures: the current environment (γ), the variable to be bound (x),
//    and the body expression (N) to evaluate after binding
// 3. We then proceed to evaluate the right-hand-side expression (M) with the current environment
// 4. When M evaluates to a value, it will be bound to x via the continuation mechanism,
//    which will then evaluate N with the extended environment
impl Eval for LetExpr {
    fn eval(&self, env: EnvIdx, cont: Cont, ctx: &mut Ctx, heap: &mut Heap) -> MachineState {
        let value = match eval_atom(self.value.get(&ctx.ir), env, &heap.envs) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        let env = heap.envs.insert(env, self.bind, value);

        // Evaluate the next expression
        MachineState::Standard(StandardConfig {
            control: self.next.get(&ctx.ir),
            env,
            cont,
        })
    }
}

// M-IF: <if V then M else N | γ | κ> --> <M | γ | κ>, if V evaluates to true
//                                     --> <N | γ | κ>, if V evaluates to false
//
// This rule describes conditional branching in the CEK machine:
// 1. We evaluate the condition V in the current environment γ
// 2. If the condition evaluates to true, we continue by evaluating the "then" branch M
// 3. If the condition evaluates to false, we continue by evaluating the "else" branch N
// 4. In both cases, we maintain the same environment γ and continuation κ
// 5. The machine state transitions directly to evaluating the selected branch
impl Eval for IfExpr {
    fn eval(&self, env: EnvIdx, mut cont: Cont, ctx: &mut Ctx, heap: &mut Heap) -> MachineState {
        let Self {
            bind,
            predicate,
            then,
            or,
            next,
        } = *self;

        // Evaluate the predicate
        let pred_val = match eval_atom(predicate.get(&ctx.ir), env, &heap.envs) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Create a pure continuation frame for the next expression
        let pure_frame = ContFrame::pure(bind, next, env);
        cont.push(pure_frame);

        // Determine which branch to evaluate based on the predicate
        let branch = match pred_val {
            Value::Bool(true) => then,
            Value::Bool(false) => or,
            _ => {
                return MachineState::Error(format!(
                    "Predicate must be a boolean, got: {:?}",
                    pred_val
                ));
            }
        };

        // Evaluate the selected branch
        MachineState::Standard(StandardConfig {
            control: branch.get(&ctx.ir),
            env,
            cont,
        })
    }
}

pub fn eval_unary_op(op: UnaryOp, value: Value) -> Result<Value, String> {
    match op {
        UnaryOp::Neg => match value {
            Value::Num(n) => Ok(Value::Num(-n)),
            _ => Err(format!("Cannot apply negate to: {:?}", value)),
        },
        UnaryOp::Not => match value {
            Value::Bool(b) => Ok(Value::Bool(!b)),
            _ => Err(format!("Cannot apply not to: {:?}", value)),
        },
    }
}
//
// M-UNARY: <x = op V; N | γ | κ> --> <N | γ[x ↦ op(V)] | κ>
//
// This rule describes unary operations in the CEK machine:
// 1. We evaluate the operand V in the current environment γ
// 2. The unary operation op is applied to the evaluated value V
// 3. The result of the operation is bound to variable x in the environment
// 4. The machine transitions directly to evaluating the next expression N
// 5. The continuation κ remains unchanged during this transition
impl Eval for UnaryExpr {
    fn eval(&self, env: EnvIdx, cont: Cont, ctx: &mut Ctx, heap: &mut Heap) -> MachineState {
        let Self {
            bind,
            op,
            arg,
            next,
        } = *self;

        // Evaluate the operand
        let arg_val = match eval_atom(arg.get(&ctx.ir), env, &heap.envs) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Apply the unary operation
        let result = match eval_unary_op(op, arg_val) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Create a new environment with the result bound to the variable
        let env = heap.envs.insert(env, bind, result);

        // Continue directly with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(&ctx.ir),
            env,
            cont,
        })
    }
}

#[inline]
pub fn eval_binary_op(
    op: BinaryOp,
    left: Value,
    right: Value,
    heap: &mut Heap,
) -> Result<Value, String> {
    match op {
        BinaryOp::Add => eval_add(left, right),
        BinaryOp::Sub => eval_sub(left, right),
        BinaryOp::Mul => eval_mul(left, right),
        BinaryOp::Div => eval_div(left, right),
        BinaryOp::Rem => eval_rem(left, right),

        BinaryOp::Less => eval_less(left, right),
        BinaryOp::LessEq => eval_less_eq(left, right),
        BinaryOp::Greater => eval_greater(left, right),
        BinaryOp::GreaterEq => eval_greater_eq(left, right),

        BinaryOp::And => eval_and(left, right),
        BinaryOp::Or => eval_or(left, right),

        BinaryOp::Eq => eval_eq(left, right),
        BinaryOp::NotEq => eval_not_eq(left, right),

        BinaryOp::Merge => eval_merge(left, right, heap),
        BinaryOp::Concat => eval_concat(left, right, heap),
    }
}

#[inline]
fn eval_add(left: Value, right: Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Num(l), Value::Num(r)) => Ok(Value::Num(l + r)),
        (left, right) => Err(type_error(BinaryOp::Add, left, right)),
    }
}

#[inline]
fn eval_sub(left: Value, right: Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Num(l), Value::Num(r)) => Ok(Value::Num(l - r)),
        (left, right) => Err(type_error(BinaryOp::Sub, left, right)),
    }
}

#[inline]
fn eval_mul(left: Value, right: Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Num(l), Value::Num(r)) => Ok(Value::Num(l * r)),
        (left, right) => Err(type_error(BinaryOp::Mul, left, right)),
    }
}

#[inline]
fn eval_div(left: Value, right: Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Num(_), Value::Num(0.0)) => Err("Division by zero".to_owned()),
        (Value::Num(l), Value::Num(r)) => Ok(Value::Num(l / r)),
        (left, right) => Err(type_error(BinaryOp::Div, left, right)),
    }
}

#[inline]
fn eval_rem(left: Value, right: Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Num(_), Value::Num(0.0)) => Err("Remainder by zero".to_owned()),
        (Value::Num(l), Value::Num(r)) => Ok(Value::Num(l % r)),
        (left, right) => Err(type_error(BinaryOp::Rem, left, right)),
    }
}

#[inline]
fn eval_less(left: Value, right: Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Num(l), Value::Num(r)) => Ok(Value::Bool(l < r)),
        (left, right) => Err(type_error(BinaryOp::Less, left, right)),
    }
}

#[inline]
fn eval_less_eq(left: Value, right: Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Num(l), Value::Num(r)) => Ok(Value::Bool(l <= r)),
        (left, right) => Err(type_error(BinaryOp::LessEq, left, right)),
    }
}

#[inline]
fn eval_greater(left: Value, right: Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Num(l), Value::Num(r)) => Ok(Value::Bool(l > r)),
        (left, right) => Err(type_error(BinaryOp::Greater, left, right)),
    }
}

#[inline]
fn eval_greater_eq(left: Value, right: Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Num(l), Value::Num(r)) => Ok(Value::Bool(l >= r)),
        (left, right) => Err(type_error(BinaryOp::GreaterEq, left, right)),
    }
}

#[inline]
fn eval_and(left: Value, right: Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l && r)),
        (left, right) => Err(type_error(BinaryOp::And, left, right)),
    }
}

#[inline]
fn eval_or(left: Value, right: Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l || r)),
        (left, right) => Err(type_error(BinaryOp::Or, left, right)),
    }
}

#[inline]
fn eval_eq(left: Value, right: Value) -> Result<Value, String> {
    Ok(Value::Bool(left == right))
}

#[inline]
fn eval_not_eq(left: Value, right: Value) -> Result<Value, String> {
    Ok(Value::Bool(left != right))
}

#[inline]
fn eval_merge(left: Value, right: Value, heap: &mut Heap) -> Result<Value, String> {
    match (left, right) {
        (Value::Record(l), Value::Record(r)) => heap
            .records
            .merge(l, r)
            .map(Value::Record)
            .ok_or_else(|| "Cannot merge records with conflicting fields".to_owned()),
        (left, right) => Err(type_error(BinaryOp::Merge, left, right)),
    }
}

#[inline]
fn eval_concat(left: Value, right: Value, heap: &mut Heap) -> Result<Value, String> {
    match (left, right) {
        (Value::List(l), Value::List(r)) => Ok(Value::List(heap.lists.concat(l, r))),
        (left, right) => Err(type_error(BinaryOp::Concat, left, right)),
    }
}

#[cold]
#[inline(never)]
fn type_error(op: BinaryOp, left: Value, right: Value) -> String {
    format!(
        "Cannot apply binary operation {:?} to: {:?} and {:?}",
        op, left, right
    )
}

// M-BINARY: <x = V1 op V2; N | γ | κ> --> <N | γ[x ↦ op(V1, V2)] | κ>
//
// This rule describes binary operations in the CEK machine:
// 1. We evaluate both operands V1 and V2 in the current environment γ
// 2. The binary operation op is applied to the evaluated values
// 3. The result is bound to variable x in the environment
// 4. The machine transitions directly to evaluating the next expression N
impl Eval for BinaryExpr {
    fn eval(&self, env: EnvIdx, cont: Cont, ctx: &mut Ctx, heap: &mut Heap) -> MachineState {
        let Self {
            bind,
            op,
            lhs,
            rhs,
            next,
        } = *self;

        // Evaluate the left operand
        let left_val = match eval_atom(lhs.get(&ctx.ir), env, &heap.envs) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Evaluate the right operand
        let right_val = match eval_atom(rhs.get(&ctx.ir), env, &heap.envs) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Apply the binary operation
        let result = match eval_binary_op(op, left_val, right_val, heap) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Create a new environment with the result bound to the variable
        let env = heap.envs.insert(env, bind, result);

        // Continue directly with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(&ctx.ir),
            env,
            cont,
        })
    }
}

impl Eval for ListExpr {
    fn eval(&self, env: EnvIdx, cont: Cont, ctx: &mut Ctx, heap: &mut Heap) -> MachineState {
        let ListExpr {
            bind,
            head,
            tail: _,
            next,
        } = *self;

        let item_iter = ctx
            .ir
            .iter_items(head)
            .map(|item| eval_atom(item.value.get(&ctx.ir), env, &heap.envs));

        let list = match heap.lists.try_alloc_from_iter(item_iter) {
            Ok(list_idx) => list_idx,
            Err(err) => return MachineState::Error(err),
        };

        let env = heap.envs.insert(env, bind, Value::List(list));

        MachineState::Standard(StandardConfig {
            control: next.get(&ctx.ir),
            env,
            cont,
        })
    }
}

// M-RECORD: <x = {l1=V1, ..., ln=Vn}; N | γ | κ> --> <N | γ[x ↦ {l1=V1, ..., ln=Vn}] | κ>
//
// This rule describes record creation in the CEK machine:
// 1. When creating a record, we evaluate all field values in the current environment γ
// 2. We then construct a new record value with the evaluated fields
// 3. The record value is bound to the variable specified in the RecordExpr
// 4. We continue with the next expression
impl Eval for RecordExpr {
    fn eval(&self, env: EnvIdx, cont: Cont, ctx: &mut Ctx, heap: &mut Heap) -> MachineState {
        let RecordExpr { bind, head, next } = *self;

        let entry_iter = ctx
            .ir
            .iter_fields(head)
            .map(|RecordField { label, value, .. }| {
                eval_atom(value.get(&ctx.ir), env, &heap.envs)
                    .map(|val| (label, val))
                    .map_err(|err| format!("Error evaluating field '{}': {}", label, err))
            });

        // Create a record value from the evaluated fields
        let record = match heap.records.try_alloc_from_iter(entry_iter) {
            Ok(record_idx) => record_idx,
            Err(err) => return MachineState::Error(err),
        };

        // Bind the record to the variable in the environment
        let env = heap.envs.insert(env, bind, Value::Record(record));

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(&ctx.ir),
            env,
            cont,
        })
    }
}
//
// M-RECORDEXTEND: <x = R.{l=V}; N | γ | κ> --> <N | γ[x ↦ (γ(R) ⊕ {l=V})] | κ>
//
// This rule describes record extension in the CEK machine:
// 1. We evaluate the base record R and the value V in the current environment γ
// 2. We extend the record with the new field path
// 3. The resulting extended record is bound to variable x in the environment
// 4. The machine transitions directly to evaluating the next expression N
impl Eval for RecordExtendExpr {
    fn eval(&self, env: EnvIdx, cont: Cont, ctx: &mut Ctx, heap: &mut Heap) -> MachineState {
        let Self {
            bind,
            base,
            path,
            value,
            next,
        } = *self;

        // Evaluate the base record
        let record_val = match eval_atom(base.get(&ctx.ir), env, &heap.envs) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Evaluate the value to extend with
        let extend_value = match eval_atom(value.get(&ctx.ir), env, &heap.envs) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Extract the record from the value
        let Value::Record(record) = record_val else {
            return MachineState::Error(format!(
                "Cannot extend non-record value: {:?}",
                record_val
            ));
        };

        // Get field path as StrKeys
        let field_iter = ctx.ir.iter_path(Some(path)).map(|fp| fp.label);

        // Use Record's extend_at_path method
        let record = match heap
            .records
            .extend_at_path(record, field_iter, extend_value)
        {
            Ok(record) => record,
            Err(err) => return MachineState::Error(err.to_string()),
        };

        // Bind the extended record to the variable in the environment
        let env = heap.envs.insert(env, bind, Value::Record(record));

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(&ctx.ir),
            env,
            cont,
        })
    }
}

// M-RECORDRESTRICT: <x = R\l; N | γ | κ> --> <N | γ[x ↦ (γ(R) - l)] | κ>
//
// This rule describes record restriction in the CEK machine:
// 1. We evaluate the base record R in the current environment γ
// 2. We remove the field at the specified path from the record
// 3. The resulting restricted record is bound to variable x in the environment
// 4. The machine transitions directly to evaluating the next expression N
impl Eval for RecordRestrictExpr {
    fn eval(&self, env: EnvIdx, cont: Cont, ctx: &mut Ctx, heap: &mut Heap) -> MachineState {
        let Self {
            bind,
            base,
            path,
            next,
        } = *self;

        // Evaluate the base record
        let record_val = match eval_atom(base.get(&ctx.ir), env, &heap.envs) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Extract the record from the value
        let Value::Record(record) = record_val else {
            return MachineState::Error(format!(
                "Cannot restrict non-record value: {:?}",
                record_val
            ));
        };

        // Get field path as StrKeys
        let field_iter = ctx.ir.iter_path(Some(path)).map(|fp| fp.label);

        // Use Record's restrict_at_path method
        let record = match heap.records.restrict_at_path(record, field_iter) {
            Ok(record) => record,
            Err(err) => return MachineState::Error(err.to_string()),
        };

        // Bind the restricted record to the variable in the environment
        let env = heap.envs.insert(env, bind, Value::Record(record));

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(&ctx.ir),
            env,
            cont,
        })
    }
}

pub fn eval_record_op(op: RecordUpdateOp, left: Value, right: Value) -> Result<Value, String> {
    match (op, left, right) {
        (RecordUpdateOp::Assign, _, r) => Ok(r),
        // TODO str concatenation ?
        (RecordUpdateOp::AddAssign, Value::Num(l), Value::Num(r)) => Ok(Value::Num(l + r)),
        (RecordUpdateOp::SubAssign, Value::Num(l), Value::Num(r)) => Ok(Value::Num(l - r)),
        (RecordUpdateOp::MulAssign, Value::Num(l), Value::Num(r)) => Ok(Value::Num(l * r)),
        (RecordUpdateOp::DivAssign, Value::Num(l), Value::Num(r)) => Ok(Value::Num(l / r)), // TODO check for division by zero?
        (RecordUpdateOp::RemAssign, Value::Num(l), Value::Num(r)) => Ok(Value::Num(l % r)),
        (op, left, right) => Err(format!(
            "Cannot apply record update operation {:?} to: {:?} and {:?}",
            op, left, right
        )),
    }
}
// M-RECORDUPDATE: <x = R.{l←V}; N | γ | κ> --> <N | γ[x ↦ (γ(R)[l←V])] | κ>
//
// This rule describes record update in the CEK machine:
// 1. We evaluate the base record R and the value V in the current environment γ
// 2. We update the field at the specified path to have the new value V
// 3. The resulting updated record is bound to variable x in the environment
// 4. The machine transitions directly to evaluating the next expression N
impl Eval for RecordUpdateExpr {
    fn eval(&self, env: EnvIdx, cont: Cont, ctx: &mut Ctx, heap: &mut Heap) -> MachineState {
        let Self {
            bind,
            base,
            path,
            op,
            value,
            next,
        } = *self;

        // Evaluate the base record
        let record_val = match eval_atom(base.get(&ctx.ir), env, &heap.envs) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Evaluate the value to update with
        let update_val = match eval_atom(value.get(&ctx.ir), env, &heap.envs) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Extract the record from the value
        let Value::Record(record) = record_val else {
            return MachineState::Error(format!(
                "Cannot update non-record value: {:?}",
                record_val
            ));
        };

        // Get field path as StrKeys
        let field_iter = ctx.ir.iter_path(Some(path)).map(|fp| fp.label);

        // Use Record's update_at_path method
        let update_fn = |cur: Value| eval_record_op(op, cur, update_val).ok();
        let record = match heap.records.update_at_path(record, field_iter, update_fn) {
            Ok(record) => record,
            Err(err) => return MachineState::Error(err.to_string()),
        };

        // Bind the updated record to the variable in the environment
        let env = heap.envs.insert(env, bind, Value::Record(record));

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(&ctx.ir),
            env,
            cont,
        })
    }
}
//
// M-RECORDACCESS: <x = R.l; N | γ | κ> --> <N | γ[x ↦ (γ(R).l)] | κ>
//
// This rule describes record field access in the CEK machine:
// 1. We evaluate the record R in the current environment γ
// 2. We access the field with label l from the record
// 3. The resulting field value is bound to variable x in the environment
// 4. The machine transitions directly to evaluating the next expression N
impl Eval for RecordAccessExpr {
    fn eval(&self, env: EnvIdx, cont: Cont, ctx: &mut Ctx, heap: &mut Heap) -> MachineState {
        let Self {
            bind,
            base,
            label,
            next,
        } = *self;

        // Evaluate the record
        let record_val = match eval_atom(base.get(&ctx.ir), env, &heap.envs) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Extract the record from the value
        let Value::Record(record) = record_val else {
            return MachineState::Error(format!(
                "Cannot access field from non-record value: {:?}",
                record_val
            ));
        };

        // Access the specified field from the record
        let Some(value) = heap.records.get_value(record, label) else {
            return MachineState::Error(format!("Field {:?} not found in record", label));
        };

        // Bind the field value to the variable in the environment
        let env = heap.envs.insert(env, bind, value);

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(&ctx.ir),
            env,
            cont,
        })
    }
}
impl Eval for PatternMatchExpr {
    fn eval(&self, env: EnvIdx, mut cont: Cont, _ctx: &mut Ctx, _heap: &mut Heap) -> MachineState {
        let Self {
            bind,
            matcher,
            next,
        } = *self;

        // Create a pure continuation frame for the next expression
        let pure_frame = ContFrame::pure(bind, next, env);
        cont.push(pure_frame);

        // Transition to pattern matching state
        MachineState::Pattern(PatternConfig { matcher, env, cont })
    }
}

/// Evaluates a pattern matching configuration
impl Eval for PatternMatcher {
    fn eval(&self, env: EnvIdx, cont: Cont, ctx: &mut Ctx, heap: &mut Heap) -> MachineState {
        match self {
            PatternMatcher::IsUnit(is_unit) => eval_is_unit(is_unit, env, cont, heap),
            PatternMatcher::IsBool(is_bool) => eval_is_bool(is_bool, env, cont, heap),
            PatternMatcher::IsNum(is_num) => eval_is_num(is_num, env, cont, heap),
            PatternMatcher::IsChar(is_char) => eval_is_char(is_char, env, cont, heap),
            PatternMatcher::IsStr(is_str) => eval_is_str(is_str, env, cont, heap),
            PatternMatcher::IsVariant(is_variant) => eval_is_variant(is_variant, env, cont, heap),
            PatternMatcher::IsRecord(is_record) => eval_is_record(is_record, env, cont, heap),
            PatternMatcher::RecordHasField(record_has_field) => {
                eval_record_has_field(record_has_field, env, cont, heap)
            }
            PatternMatcher::IsList(is_list) => eval_is_list(is_list, env, cont, heap),
            PatternMatcher::ListIsExact(list_is_exact) => {
                eval_list_is_exact(list_is_exact, env, cont, heap)
            }
            PatternMatcher::ListIsAtLeast(list_is_at_least) => {
                eval_list_is_at_least(list_is_at_least, env, cont, heap)
            }
            PatternMatcher::Identity(extract) => eval_identity(extract, env, cont, heap),
            PatternMatcher::VariantGet(extract) => eval_variant_get(extract, env, cont, heap),
            PatternMatcher::RecordGetAt(extract) => eval_record_get_at(extract, env, cont, heap),
            PatternMatcher::ListSplitHead(extract) => {
                eval_list_split_head(extract, env, cont, heap)
            }
            PatternMatcher::ListSplitTail(extract) => {
                eval_list_split_tail(extract, env, cont, heap)
            }
            PatternMatcher::ListGetAt(extract) => eval_list_get_at(extract, env, cont, heap),
            PatternMatcher::ListSplitAt(extract) => eval_list_split_at(extract, env, cont, heap),

            PatternMatcher::Success(success) => eval_pattern_success(success, env, cont, &ctx.ir),
            PatternMatcher::Failure(_) => {
                MachineState::Error("Pattern match failure - no matching case found".to_string())
            }
        }
    }
}

/// Evaluates IsUnit pattern matcher - tests if the source value is Unit
fn eval_is_unit(is_unit: &IsUnit, env: EnvIdx, cont: Cont, heap: &Heap) -> MachineState {
    let IsUnit {
        source,
        on_success,
        on_failure,
    } = *is_unit;

    // Get the actual source value from the environment
    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    let next_matcher = match source_val {
        Value::None => on_success, // Unit matches, go to success path
        _ => on_failure,           // Not unit, go to failure path
    };

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: next_matcher,
        env,
        cont,
    })
}

/// Evaluates IsBool pattern matcher
fn eval_is_bool(is_bool: &IsBool, env: EnvIdx, cont: Cont, heap: &Heap) -> MachineState {
    let IsBool {
        source,
        payload,
        on_success,
        on_failure,
    } = *is_bool;

    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    let next_matcher = match source_val {
        Value::Bool(b) if b == payload => on_success,
        _ => on_failure,
    };

    MachineState::Pattern(PatternConfig {
        matcher: next_matcher,
        env,
        cont,
    })
}

/// Evaluates IsNum pattern matcher
fn eval_is_num(is_num: &IsNum, env: EnvIdx, cont: Cont, heap: &Heap) -> MachineState {
    let IsNum {
        source,
        payload,
        on_success,
        on_failure,
    } = *is_num;

    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    let next_matcher = match source_val {
        Value::Num(n) if (n - payload).abs() < f64::EPSILON => on_success,
        _ => on_failure,
    };

    MachineState::Pattern(PatternConfig {
        matcher: next_matcher,
        env,
        cont,
    })
}

/// Evaluates IsChar pattern matcher
fn eval_is_char(is_char: &IsChar, env: EnvIdx, cont: Cont, heap: &Heap) -> MachineState {
    let IsChar {
        source,
        payload,
        on_success,
        on_failure,
    } = *is_char;

    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    let next_matcher = match source_val {
        Value::Char(c) if c == payload => on_success,
        _ => on_failure,
    };

    MachineState::Pattern(PatternConfig {
        matcher: next_matcher,
        env,
        cont,
    })
}

/// Evaluates IsStr pattern matcher
fn eval_is_str(is_str: &IsStr, env: EnvIdx, cont: Cont, heap: &mut Heap) -> MachineState {
    let IsStr {
        source,
        payload,
        on_success,
        on_failure,
    } = *is_str;

    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    let next_matcher = match source_val {
        Value::Str(Some(s)) => {
            if heap.strings.eq(s, StringIdx::Static(payload)) {
                on_success
            } else {
                on_failure
            }
        }
        _ => on_failure,
    };

    MachineState::Pattern(PatternConfig {
        matcher: next_matcher,
        env,
        cont,
    })
}

fn eval_is_variant(is_tag: &IsVariant, env: EnvIdx, cont: Cont, heap: &Heap) -> MachineState {
    let IsVariant {
        source,
        tag,
        on_success,
        on_failure,
    } = *is_tag;

    // Get the source value from the environment
    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    let next_matcher = match source_val {
        Value::Variant(t) if t.tag().0 == tag => on_success,
        _ => on_failure,
    };

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: next_matcher,
        env,
        cont,
    })
}

fn eval_is_record(is_record: &IsRecord, env: EnvIdx, cont: Cont, heap: &Heap) -> MachineState {
    let IsRecord {
        source,
        on_success,
        on_failure,
    } = *is_record;

    // Get the source value from the environment
    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    let next_matcher = match source_val {
        Value::Record(_) => on_success,
        _ => on_failure,
    };

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: next_matcher,
        env,
        cont,
    })
}

fn eval_record_has_field(
    record_has_field: &RecordHasField,
    env: EnvIdx,
    cont: Cont,
    heap: &Heap,
) -> MachineState {
    let RecordHasField {
        source,
        field,
        on_success,
        on_failure,
    } = *record_has_field;

    // Get the source value from the environment
    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    let next_matcher = match source_val {
        Value::Record(record) if heap.records.contains_key(record, field) => on_success,
        _ => on_failure,
    };

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: next_matcher,
        env,
        cont,
    })
}

fn eval_is_list(is_list: &IsList, env: EnvIdx, cont: Cont, heap: &Heap) -> MachineState {
    let IsList {
        source,
        on_success,
        on_failure,
    } = *is_list;

    // Get the source value from the environment
    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    let next_matcher = match source_val {
        Value::List(_) => on_success,
        _ => on_failure,
    };

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: next_matcher,
        env,
        cont,
    })
}

fn eval_list_is_exact(
    list_is_exact: &ListIsExact,
    env: EnvIdx,
    cont: Cont,
    heap: &Heap,
) -> MachineState {
    let ListIsExact {
        source,
        length,
        on_success,
        on_failure,
    } = *list_is_exact;

    // Get the source value from the environment
    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    let next_matcher = match source_val {
        Value::List(list) if heap.lists.len(list) as u32 == length => on_success,
        _ => on_failure,
    };

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: next_matcher,
        env,
        cont,
    })
}

fn eval_list_is_at_least(
    list_is_atleast: &ListIsAtLeast,
    env: EnvIdx,
    cont: Cont,
    heap: &Heap,
) -> MachineState {
    let ListIsAtLeast {
        source,
        min_length,
        on_success,
        on_failure,
    } = *list_is_atleast;

    // Get the source value from the environment
    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    let next_matcher = match source_val {
        Value::List(list) if heap.lists.len(list) as u32 >= min_length => on_success,
        _ => on_failure,
    };

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: next_matcher,
        env,
        cont,
    })
}

/// Evaluates Identity - binds the source value to a variable (for bind patterns and wildcards)
fn eval_identity(extract: &Identity, env: EnvIdx, cont: Cont, heap: &mut Heap) -> MachineState {
    let Identity {
        bind: extract_bind,
        source,
        next: pattern_next,
    } = *extract;

    // Get the source value from the environment
    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    // Bind the source value to the target variable

    let env = heap.envs.insert(env, extract_bind, source_val);

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: pattern_next,
        env,
        cont,
    })
}
fn eval_variant_get(
    extract: &VariantGet,
    env: EnvIdx,
    cont: Cont,
    heap: &mut Heap,
) -> MachineState {
    let VariantGet {
        bind: extract_bind,
        source,
        next: pattern_next,
    } = *extract;

    // Get the source value from the environment
    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    // Ensure the source value is a variant
    let Value::Variant(variant) = source_val else {
        return MachineState::Error(format!("Expected a variant, got: {:?}", source_val));
    };

    // Bind the payload to the target variable
    let env = heap
        .envs
        .insert(env, extract_bind, heap.variants.get_value(variant));

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: pattern_next,
        env,
        cont,
    })
}

fn eval_record_get_at(
    extract: &RecordGetAt,
    mut env: EnvIdx,
    cont: Cont,
    heap: &mut Heap,
) -> MachineState {
    let RecordGetAt {
        bind: extract_bind,
        source,
        field,
        next: pattern_next,
    } = *extract;

    // Get the source value from the environment
    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    // Ensure the source value is a record
    let Value::Record(record) = source_val else {
        return MachineState::Error(format!("Expected a record, got: {:?}", source_val));
    };

    // Get the field value from the record
    if let Some(value) = heap.records.get_value(record, field) {
        // Bind the field value to the target variable
        env = heap.envs.insert(env, extract_bind, value);
    } else {
        return MachineState::Error(format!("Field {:?} not found in record", field));
    }

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: pattern_next,
        env,
        cont,
    })
}

fn eval_list_split_head(
    extract: &ListSplitHead,
    mut env: EnvIdx,
    cont: Cont,
    heap: &mut Heap,
) -> MachineState {
    let ListSplitHead {
        head,
        tail_list,
        source,
        next: pattern_next,
    } = *extract;

    // Get the source value from the environment
    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    // Ensure the source value is a list
    let Value::List(Some(list)) = source_val else {
        return MachineState::Error(format!("Expected a non-empty list, got: {:?}", source_val));
    };

    // Get the head of the list
    let (h, t) = heap.lists.split_front(list);
    // Bind the head to the target variable
    env = heap.envs.insert(env, head, h);
    env = heap.envs.insert(env, tail_list, t);

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: pattern_next,
        env,
        cont,
    })
}

fn eval_list_split_tail(
    extract: &ListSplitTail,
    mut env: EnvIdx,
    cont: Cont,
    heap: &mut Heap,
) -> MachineState {
    let ListSplitTail {
        tail,
        head_list,
        source,
        next: pattern_next,
    } = *extract;

    // Get the source value from the environment
    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    // Ensure the source value is a list
    let Value::List(Some(list)) = source_val else {
        return MachineState::Error(format!("Expected a non-empty list, got: {:?}", source_val));
    };

    // Get the tail of the list
    let (h, t) = heap.lists.split_back(list);
    // Bind the tail to the target variable
    env = heap.envs.insert(env, tail, t);
    env = heap.envs.insert(env, head_list, h);

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: pattern_next,
        env,
        cont,
    })
}

fn eval_list_get_at(
    extract: &ListGetAt,
    mut env: EnvIdx,
    cont: Cont,
    heap: &mut Heap,
) -> MachineState {
    let ListGetAt {
        bind: extract_bind,
        source,
        index,
        next: pattern_next,
    } = *extract;

    // Get the source value from the environment
    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    // Ensure the source value is a list
    let Value::List(list) = source_val else {
        return MachineState::Error(format!("Expected a list, got: {:?}", source_val));
    };

    // Get the item at the specified index
    if let Some(item) = heap.lists.get_element(list, index as usize) {
        // Bind the item to the target variable
        env = heap.envs.insert(env, extract_bind, item);
    } else {
        return MachineState::Error(format!("Index out of bounds for list"));
    }

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: pattern_next,
        env,
        cont,
    })
}

fn eval_list_split_at(
    extract: &ListSplitAt,
    mut env: EnvIdx,
    cont: Cont,
    heap: &mut Heap,
) -> MachineState {
    let ListSplitAt {
        head,
        tail,
        source,
        index,
        next: pattern_next,
    } = *extract;

    // Get the source value from the environment
    let source_val = match heap.envs.get(env, source) {
        Some(val) => val,
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    // Ensure the source value is a list
    let Value::List(Some(list)) = source_val else {
        return MachineState::Error(format!("Expected a non-empty list, got: {:?}", source_val));
    };

    // Get the slices from the specified range
    let (h, t) = heap.lists.split_at(list, index as usize);
    env = heap.envs.insert(env, head, h);
    env = heap.envs.insert(env, tail, t);

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: pattern_next,
        env,
        cont,
    })
}

/// Evaluates PatternSuccess - pattern matching succeeded, continue to the matched expression
fn eval_pattern_success(
    success: &PatternSuccess,
    env: EnvIdx,
    cont: Cont,
    ir: &Ir,
) -> MachineState {
    // TODO either use PatternMatcherExpr to setup continuation and use it here,
    // or use the next id of PatternSuccess directly but do not do both
    let PatternSuccess { next } = *success;

    // Bind the original source value to the result binding
    // env.insert(bind, source_value); // TODO this would only be necessary if I would actually mutate variables I guess

    // Continue with the matched branch expression
    MachineState::Standard(StandardConfig {
        control: next.get(ir),
        env,
        cont,
    })
}
