use std::fs;

use crate::{
    config::{MachineState, OperationConfig, PatternConfig, PrimitiveRecConfig, StandardConfig},
    cont::{Cont, ContFrame, Handler, HandlerClosure, PureCont, PureContFrame, ReturnClause},
    env::Env,
    value::{Closure, List, Record, Value, Variant, to_usize_exact},
};
use kola_builtins::BuiltinId;
use kola_collections::ImShadowMap;
use kola_ir::{
    id::Id,
    instr::{
        Atom, BinaryExpr, BinaryOp, CallExpr, DoExpr, Expr, Func, HandleExpr, Identity, IfExpr,
        IsBool, IsChar, IsList, IsNum, IsRecord, IsStr, IsUnit, IsVariant, LetExpr, ListExpr,
        ListGetAt, ListIsAtLeast, ListIsExact, ListItem, ListSplitAt, ListSplitHead, ListSplitTail,
        PatternMatchExpr, PatternMatcher, PatternSuccess, RecordAccessExpr, RecordExpr,
        RecordExtendExpr, RecordField, RecordGetAt, RecordHasField, RecordRestrictExpr,
        RecordUpdateExpr, RecordUpdateOp, RetExpr, Symbol, Tag, UnaryExpr, UnaryOp, VariantGet,
    },
    ir::{Ir, IrView},
};
use kola_utils::interner_ext::InternerExt;

pub fn eval_symbol(symbol: Symbol, env: &Env) -> Result<Value, String> {
    // Look up the symbol in the environment
    env.get(&symbol)
        .cloned()
        .ok_or(format!("Unbound variable: {}", symbol))
}

pub fn eval_atom(atom: Atom, env: &Env) -> Result<Value, String> {
    match atom {
        Atom::Noop => Ok(Value::None),
        Atom::Bool(b) => Ok(Value::Bool(b)),
        Atom::Char(c) => Ok(Value::Char(c)),
        Atom::Num(n) => Ok(Value::Num(n)),
        Atom::Str(s) => Ok(Value::str(&env[s])),
        Atom::Func(f) => {
            // Create a closure by capturing the current environment
            Ok(Value::Closure(Closure::new(env.clone(), f)))
        }
        Atom::Symbol(s) => eval_symbol(s, env),
        Atom::Builtin(b) => Ok(Value::Builtin(b)),
        Atom::Tag(t) => Ok(Value::Tag(t)),
    }
}

pub trait Eval {
    fn eval(&self, env: Env, cont: Cont, ir: &Ir) -> MachineState;
}

impl Eval for Expr {
    fn eval(&self, env: Env, cont: Cont, ir: &Ir) -> MachineState {
        match self {
            Expr::Ret(ret_expr) => ret_expr.eval(env, cont, ir),
            Expr::Call(call) => call.eval(env, cont, ir),
            Expr::Handle(handle) => handle.eval(env, cont, ir),
            Expr::Do(do_expr) => do_expr.eval(env, cont, ir),
            Expr::Let(let_expr) => let_expr.eval(env, cont, ir),
            Expr::If(if_expr) => if_expr.eval(env, cont, ir),
            Expr::Unary(unary_expr) => unary_expr.eval(env, cont, ir),
            Expr::Binary(binary_expr) => binary_expr.eval(env, cont, ir),
            Expr::List(list_expr) => list_expr.eval(env, cont, ir),
            Expr::Record(record_expr) => record_expr.eval(env, cont, ir),
            Expr::RecordExtend(record_extend_expr) => record_extend_expr.eval(env, cont, ir),
            Expr::RecordRestrict(record_restrict_expr) => record_restrict_expr.eval(env, cont, ir),
            Expr::RecordUpdate(record_update_expr) => record_update_expr.eval(env, cont, ir),
            Expr::RecordAccess(record_access_expr) => record_access_expr.eval(env, cont, ir),
            Expr::PatternMatch(pattern_match_expr) => pattern_match_expr.eval(env, cont, ir),
        }
    }
}

// M-RET : Return with a value
impl Eval for RetExpr {
    fn eval(&self, env: Env, mut cont: Cont, ir: &Ir) -> MachineState {
        // Evaluate the return machine state of value
        let value = match eval_atom(self.arg.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Remove the top continuation frame
        let Some(ContFrame {
            mut pure,
            handler_closure,
        }) = cont.pop()
        else {
            // If the continuation is empty, return the value
            return MachineState::Value(value);
        };

        // If there's a pure continuation, apply it
        let Some(PureContFrame { var, body, mut env }) = pure.pop() else {
            // M-RETHANDLER: <return V | γ | ([], (γ', H)) :: κ> --> <M | γ'[x ↦ V] | κ>,
            // if H(return) = {return x ↦ M}
            //
            // When a return expression with value V reaches a handler frame with
            // an empty pure continuation, we:
            // 1. Extract the return clause from the handler H
            // 2. Bind the returned value V to parameter x in the handler's environment γ'
            // 3. Continue by evaluating the return handler's body M with the updated environment
            // 4. Use the continuation κ below the current handler frame

            let HandlerClosure { handler, mut env } = handler_closure;

            return match handler.return_clause {
                ReturnClause::Identity => {
                    // Identity function just returns the value
                    if cont.is_empty() {
                        // No more frames, return the final value
                        MachineState::Value(value)
                    } else {
                        // Continue with the next frame
                        MachineState::Standard(StandardConfig {
                            control: Expr::from(*self),
                            env,
                            cont,
                        })
                    }
                }
                ReturnClause::Function(Func { param, body }) => {
                    // Create a new environment with the value bound to param
                    env.insert(param, value);

                    // Evaluate the return handler body
                    MachineState::Standard(StandardConfig {
                        control: body.get(ir),
                        env,
                        cont,
                    })
                }
                ReturnClause::PrimitiveRec {
                    head,
                    step: Func { param, body },
                } => {
                    // Create argument for the step function
                    let mut record = Record::new();
                    record.insert(env.interner["acc"], value); // value = result from previous step
                    record.insert(env.interner["head"], head);

                    env.insert(param, Value::Record(record));

                    MachineState::Standard(StandardConfig {
                        control: body.get(ir),
                        env,
                        cont, // Remaining handlers continue the chain
                    })
                }
            };
        };

        // M-RETCONT: <return V | γ | ((γ', x, N) :: σ, χ) :: κ> --> <N | γ'[x ↦ V] | (σ, χ) :: κ>
        //
        // When a return expression with value V reaches a pure continuation frame:
        // 1. The value V is evaluated in the current environment γ
        // 2. We extract the pure continuation frame containing variable x, body N, and environment γ'
        // 3. We bind the value V to variable x in the environment γ'
        // 4. We continue by evaluating N in this updated environment
        // 5. The continuation stack is updated by removing the top pure frame

        // Create new environment with the bound variable
        env.insert(var, value);

        // Push back the modified ContFrame to preserve the handler context.
        // Even if the pure continuation is now empty, the handler_closure must
        // remain active to handle any effects raised during execution of the
        // next expression and to process the final return value when the
        // computation within this handler scope completes.
        cont.push(ContFrame {
            pure,
            handler_closure,
        });

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control: body.get(ir),
            env,
            cont,
        })
    }
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
    fn eval(&self, mut env: Env, mut cont: Cont, ir: &Ir) -> MachineState {
        let Self {
            bind,
            func,
            arg,
            next,
        } = *self;

        // Get the function and argument
        let func_val = match eval_atom(func.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        let arg_val = match eval_atom(arg.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        match func_val {
            // Apply the function to the argument
            Value::Closure(Closure {
                env: mut func_env,
                func: Func { param, body },
            }) => {
                // Create a pure continuation frame for the next expression
                let pure_frame = PureContFrame {
                    var: bind,
                    body: next,
                    env: env.clone(),
                };

                // Add the frame to the continuation
                let mut frame = cont.pop_or_identity(|| Env::from_env(&env));
                frame.pure.push(pure_frame);
                cont.push(frame);

                // Create a new environment with the bound parameter
                func_env.insert(param, arg_val);

                // Evaluate the function body
                MachineState::Standard(StandardConfig {
                    control: body.get(ir),
                    env: func_env,
                    cont,
                })
            }
            // If the function is a continuation, we apply it to the argument
            Value::Cont(mut captured) => {
                // M-APPCONT: <V W | γ | κ> --> <return W | γ | κ' ++ κ>  if V = κ'
                //
                // This rule handles continuation application in the CEK machine:
                // 1. When we apply a captured continuation value (V = κ') to an argument W
                // 2. We transition to returning the argument value W
                // 3. The environment γ remains unchanged
                // 4. We prepend the captured continuation κ' to the current continuation κ,
                //    effectively composing the continuations
                // 5. This allows control to transfer to the point where the continuation was captured,
                //    with the argument W becoming the returned value at that point

                // Create a pure continuation frame for the next expression
                let pure_frame = PureContFrame {
                    var: bind,
                    body: next,
                    env: env.clone(),
                };

                // Add the frame to the continuation
                let mut frame = cont.pop_or_identity(|| Env::from_env(&env));
                frame.pure.push(pure_frame);
                cont.push(frame);

                captured.append(&mut cont);

                // Apply the captured continuation to the argument
                MachineState::Standard(StandardConfig {
                    control: Expr::from(RetExpr { arg }),
                    env,
                    cont: captured,
                })
            }
            Value::Builtin(builtin) => eval_builtin(builtin, bind, arg_val, env, cont, next, ir),
            Value::Tag(tag) => {
                let value = Value::variant(tag, arg_val);

                // Create a new environment with the result bound to the variable
                env.insert(bind, value);

                // Continue with the next expression
                MachineState::Standard(StandardConfig {
                    control: next.get(ir),
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
    mut env: Env,
    mut cont: Cont,
    next: Id<Expr>,
    ir: &Ir,
) -> MachineState {
    // TODO use mutable functions to avoid copying.
    // arg is moved by value anyway.
    // On the other hand the underlying data structure of the record is a persistent data structure,
    // where removal is O(log n), while lookup is only O(1).
    // So bottom line I think get + clone might be a tad faster than get_mut + remove.
    // Before changing anything definitely profile this code.

    let value = match (builtin, arg) {
        (BuiltinId::IoDebug, value) => {
            println!("Debug: {:?}", value);
            value
        }
        (BuiltinId::IoReadFile, Value::Str(path)) => {
            match fs::read_to_string(env.working_dir.join(path)) {
                Err(err) => Value::variant(Tag(env.interner["Err"]), Value::Str(err.to_string())),
                Ok(contents) => Value::variant(Tag(env.interner["Ok"]), Value::Str(contents)),
            }
        }
        (BuiltinId::IoWriteFile, Value::Record(record)) => {
            let Some(Value::Str(path)) = record.get(env.interner["path"]) else {
                return MachineState::Error(
                    "io_write_file requires 'path' field with a string".to_owned(),
                );
            };

            let Some(Value::Str(contents)) = record.get(env.interner["contents"]) else {
                return MachineState::Error(
                    "io_write_file requires 'contents' field with a string".to_owned(),
                );
            };

            match fs::write(env.working_dir.join(path), contents) {
                Err(err) => Value::variant(Tag(env.interner["Err"]), Value::Str(err.to_string())),
                Ok(_) => Value::variant(Tag(env.interner["Ok"]), Value::None),
            }
        }
        (BuiltinId::ListLength, Value::List(list)) => Value::Num(list.len() as f64),
        (BuiltinId::ListIsEmpty, Value::List(list)) => Value::Bool(list.is_empty()),
        (BuiltinId::ListReverse, Value::List(list)) => Value::List(list.reverse()),
        (BuiltinId::ListSum, Value::List(list)) => {
            match list.into_iter().try_fold(0.0, |acc, v| {
                if let Value::Num(n) = v {
                    Ok(acc + n)
                } else {
                    Err(format!("Cannot sum non-numeric value: {:?}", v))
                }
            }) {
                Ok(sum) => Value::Num(sum),
                Err(err) => {
                    return MachineState::Error(err);
                }
            }
        }
        (BuiltinId::ListContains, Value::Record(record)) => {
            let Some(Value::List(list)) = record.get(env.interner["list"]) else {
                return MachineState::Error(
                    "list_contains requires 'list' field with a list".to_owned(),
                );
            };

            let Some(value) = record.get(env.interner["value"]) else {
                return MachineState::Error("list_contains requires 'value' field".to_owned());
            };

            Value::Bool(list.contains(value))
        }
        (BuiltinId::ListAt, Value::Record(record)) => {
            let Some(Value::List(list)) = record.get(env.interner["list"]) else {
                return MachineState::Error(
                    "list_get requires 'list' field with a list".to_owned(),
                );
            };

            let Some(Value::Num(index)) = record.get(env.interner["index"]) else {
                return MachineState::Error("list_get requires 'index' field".to_owned());
            };

            match to_usize_exact(*index).and_then(|idx| list.get(idx)) {
                Some(value) => {
                    Value::Variant(Variant::new(Tag(env.interner["Some"]), value.clone()))
                }
                None => Value::Variant(Variant::new(Tag(env.interner["None"]), Value::None)),
            }
        }
        (BuiltinId::ListFirst, Value::List(list)) => match list.first() {
            Some(head) => Value::Variant(Variant::new(Tag(env.interner["Some"]), head)),
            None => Value::Variant(Variant::new(Tag(env.interner["None"]), Value::None)),
        },
        (BuiltinId::ListLast, Value::List(list)) => match list.last() {
            Some(tail) => Value::Variant(Variant::new(Tag(env.interner["Some"]), tail)),
            None => Value::Variant(Variant::new(Tag(env.interner["None"]), Value::None)),
        },
        (BuiltinId::ListPrepend, Value::Record(record)) => {
            let Some(head_value) = record.get(env.interner["head"]) else {
                return MachineState::Error("list_prepend requires 'head' field".to_owned());
            };

            let Some(Value::List(tail_list)) = record.get(env.interner["tail"]) else {
                return MachineState::Error(
                    "list_prepend requires 'tail' field with a list".to_owned(),
                );
            };

            Value::List(tail_list.prepend(head_value))
        }
        (BuiltinId::ListAppend, Value::Record(record)) => {
            let Some(Value::List(head_list)) = record.get(env.interner["head"]) else {
                return MachineState::Error(
                    "list_append requires 'head' field with a list".to_owned(),
                );
            };

            let Some(tail_value) = record.get(env.interner["tail"]) else {
                return MachineState::Error("list_append requires 'tail' field".to_owned());
            };

            Value::List(head_list.append(tail_value))
        }
        (BuiltinId::ListConcat, Value::Record(record)) => {
            let Some(Value::List(head_list)) = record.get(env.interner["head"]) else {
                return MachineState::Error(
                    "list_concat requires 'head' field with a list".to_owned(),
                );
            };

            let Some(Value::List(tail_list)) = record.get(env.interner["tail"]) else {
                return MachineState::Error(
                    "list_concat requires 'tail' field with a list".to_owned(),
                );
            };

            Value::List(head_list.concat(tail_list))
        }
        // list_rec([], base, step) = base
        // list_rec([head, ...tail], base, step) = step(head, list_rec(tail, base step))
        (BuiltinId::ListRec, Value::Record(record)) => {
            let Some(Value::List(list)) = record.get(env.interner["list"]).cloned() else {
                return MachineState::Error(
                    "list_rec requires 'list' field with a list".to_owned(),
                );
            };

            let Some(base) = record.get(env.interner["base"]).cloned() else {
                return MachineState::Error("list_rec requires 'base' field".to_owned());
            };

            let Some(Value::Closure(step)) = record.get(env.interner["step"]).cloned() else {
                return MachineState::Error(
                    "list_rec requires 'step' field with a func".to_owned(),
                );
            };

            // Create a pure continuation frame for the next expression
            let mut frame = cont.pop_or_identity(|| Env::from_env(&env));
            let pure_frame = PureContFrame {
                var: bind,
                body: next,
                env,
            };
            frame.pure.push(pure_frame);
            cont.push(frame);

            return MachineState::PrimitiveRec(PrimitiveRecConfig {
                data: Value::List(list),
                base,
                step,
                cont,
            });
        }
        (BuiltinId::NumAbs, Value::Num(n)) => Value::Num(n.abs()),
        (BuiltinId::NumSqrt, Value::Num(n)) => Value::Num(n.sqrt()),
        (BuiltinId::NumFloor, Value::Num(n)) => Value::Num(n.floor()),
        (BuiltinId::NumCeil, Value::Num(n)) => Value::Num(n.ceil()),
        (BuiltinId::NumRound, Value::Num(n)) => Value::Num(n.round()),
        (BuiltinId::NumSin, Value::Num(n)) => Value::Num(n.sin()),
        (BuiltinId::NumCos, Value::Num(n)) => Value::Num(n.cos()),
        (BuiltinId::NumTan, Value::Num(n)) => Value::Num(n.tan()),
        (BuiltinId::NumLn, Value::Num(n)) => Value::Num(n.ln()),
        (BuiltinId::NumLog10, Value::Num(n)) => Value::Num(n.log10()),
        (BuiltinId::NumExp, Value::Num(n)) => Value::Num(n.exp()),
        (BuiltinId::NumPow, Value::Record(record)) => {
            let Some(Value::Num(base)) = record.get(env.interner["base"]) else {
                return MachineState::Error("num_pow requires 'base' field".to_owned());
            };
            let Some(Value::Num(exp)) = record.get(env.interner["exp"]) else {
                return MachineState::Error("num_pow requires 'exp' field".to_owned());
            };

            Value::Num(base.powf(*exp))
        }
        (BuiltinId::NumRec, Value::Record(record)) => {
            let Some(Value::Num(n)) = record.get(env.interner["n"]) else {
                return MachineState::Error("num_rec requires 'n' field with a number".to_owned());
            };

            let Some(base) = record.get(env.interner["base"]).cloned() else {
                return MachineState::Error("num_rec requires 'base' field".to_owned());
            };

            let Some(Value::Closure(step)) = record.get(env.interner["step"]).cloned() else {
                return MachineState::Error("num_rec requires 'step' field with a func".to_owned());
            };

            // Create a pure continuation frame for the next expression
            let mut frame = cont.pop_or_identity(|| Env::from_env(&env));
            let pure_frame = PureContFrame {
                var: bind,
                body: next,
                env,
            };
            frame.pure.push(pure_frame);
            cont.push(frame);

            return MachineState::PrimitiveRec(PrimitiveRecConfig {
                data: Value::Num(*n),
                base,
                step,
                cont,
            });
        }
        (BuiltinId::RecordRec, Value::Record(record)) => {
            let Some(Value::Record(data)) = record.get(env.interner["data"]).cloned() else {
                return MachineState::Error(
                    "record_rec requires 'data' field with a record".to_owned(),
                );
            };

            let Some(base) = record.get(env.interner["base"]).cloned() else {
                return MachineState::Error("record_rec requires 'base' field".to_owned());
            };

            let Some(Value::Closure(step)) = record.get(env.interner["step"]).cloned() else {
                return MachineState::Error(
                    "record_rec requires 'step' field with a func".to_owned(),
                );
            };

            // Create a pure continuation frame for the next expression
            let mut frame = cont.pop_or_identity(|| Env::from_env(&env));
            let pure_frame = PureContFrame {
                var: bind,
                body: next,
                env,
            };
            frame.pure.push(pure_frame);
            cont.push(frame);

            return MachineState::PrimitiveRec(PrimitiveRecConfig {
                data: Value::Record(data),
                base,
                step,
                cont,
            });
        }
        // TODO these serde methods do not enforce type annotations
        // TODO from_json would mean that the interner is mutable
        // (BuiltinId::SerdeFromJson, Value::Str(json_str)) => match Value::from_json(&json_str) {
        //     Ok(value) => Value::variant(Tag(env.interner["Ok"]), value),
        //     Err(err) => Value::variant(Tag(env.interner["Err"]), Value::Str(err.to_string())),
        // },
        (BuiltinId::SerdeToJson, value) => match env.interner.to_json(&value) {
            Ok(json_str) => Value::variant(Tag(env.interner["Ok"]), Value::Str(json_str)),
            Err(err) => Value::variant(Tag(env.interner["Err"]), Value::Str(err.to_string())),
        },
        (BuiltinId::StrLength, Value::Str(s)) => Value::Num(s.len() as f64),
        (BuiltinId::StrIsEmpty, Value::Str(s)) => Value::Bool(s.is_empty()),
        (BuiltinId::StrReverse, Value::Str(s)) => Value::Str(s.chars().rev().collect()),
        (BuiltinId::StrFirst, Value::Str(s)) => {
            if let Some(first) = s.chars().next() {
                Value::Variant(Variant::new(Tag(env.interner["Some"]), Value::Char(first)))
            } else {
                Value::Variant(Variant::new(Tag(env.interner["None"]), Value::None))
            }
        }
        (BuiltinId::StrLast, Value::Str(mut s)) => {
            if let Some(last) = s.pop() {
                Value::Variant(Variant::new(Tag(env.interner["Some"]), Value::Char(last)))
            } else {
                Value::Variant(Variant::new(Tag(env.interner["None"]), Value::None))
            }
        }
        (BuiltinId::StrContains, Value::Record(record)) => {
            let Some(Value::Str(s)) = record.get(env.interner["str"]) else {
                return MachineState::Error(
                    "str_contains requires 'str' field with a string".to_owned(),
                );
            };

            let Some(Value::Char(c)) = record.get(env.interner["value"]) else {
                return MachineState::Error(
                    "str_contains requires 'value' field with a char".to_owned(),
                );
            };

            Value::Bool(s.contains(*c))
        }
        (BuiltinId::StrAt, Value::Record(record)) => {
            let Some(Value::Str(s)) = record.get(env.interner["str"]) else {
                return MachineState::Error("str_at requires 'str' field with a string".to_owned());
            };

            let Some(Value::Num(index)) = record.get(env.interner["index"]) else {
                return MachineState::Error(
                    "str_at requires 'index' field with a number".to_owned(),
                );
            };

            match to_usize_exact(*index).and_then(|idx| s.chars().nth(idx)) {
                Some(c) => Value::Variant(Variant::new(Tag(env.interner["Some"]), Value::Char(c))),
                None => Value::Variant(Variant::new(Tag(env.interner["None"]), Value::None)),
            }
        }
        (BuiltinId::StrPrepend, Value::Record(record)) => {
            let Some(Value::Char(c)) = record.get(env.interner["head"]) else {
                return MachineState::Error(
                    "str_prepend requires 'head' field with a char".to_owned(),
                );
            };

            let Some(Value::Str(s)) = record.get(env.interner["tail"]) else {
                return MachineState::Error(
                    "str_prepend requires 'tail' field with a string".to_owned(),
                );
            };

            Value::Str(format!("{}{}", c, s))
        }
        (BuiltinId::StrAppend, Value::Record(record)) => {
            let Some(Value::Str(s)) = record.get(env.interner["head"]) else {
                return MachineState::Error(
                    "str_append requires 'head' field with a string".to_owned(),
                );
            };

            let Some(Value::Char(c)) = record.get(env.interner["tail"]) else {
                return MachineState::Error(
                    "str_append requires 'tail' field with a char".to_owned(),
                );
            };

            Value::Str(format!("{}{}", s, c))
        }
        (BuiltinId::StrConcat, Value::Record(record)) => {
            let Some(Value::Str(s1)) = record.get(env.interner["head"]) else {
                return MachineState::Error(
                    "str_concat requires 'head' field with a string".to_owned(),
                );
            };

            let Some(Value::Str(s2)) = record.get(env.interner["tail"]) else {
                return MachineState::Error(
                    "str_concat requires 'tail' field with a string".to_owned(),
                );
            };

            Value::Str(format!("{}{}", s1, s2))
        }
        (BuiltinId::StrRec, Value::Record(record)) => {
            let Some(Value::Str(s)) = record.get(env.interner["str"]).cloned() else {
                return MachineState::Error("str_rec requires 's' field with a string".to_owned());
            };

            let Some(base) = record.get(env.interner["base"]).cloned() else {
                return MachineState::Error("str_rec requires 'base' field".to_owned());
            };

            let Some(Value::Closure(step)) = record.get(env.interner["step"]).cloned() else {
                return MachineState::Error("str_rec requires 'step' field with a func".to_owned());
            };

            // Create a pure continuation frame for the next expression
            let mut frame = cont.pop_or_identity(|| Env::from_env(&env));
            let pure_frame = PureContFrame {
                var: bind,
                body: next,
                env,
            };
            frame.pure.push(pure_frame);
            cont.push(frame);

            return MachineState::PrimitiveRec(PrimitiveRecConfig {
                data: Value::Str(s),
                base,
                step,
                cont,
            });
        }
        (_, value) => {
            return MachineState::Error(format!("Cannot apply {builtin} to: {:?}", value));
        }
    };

    // Create a new environment with the result bound to the variable
    env.insert(bind, value);

    // Continue with the next expression
    MachineState::Standard(StandardConfig {
        control: next.get(ir),
        env,
        cont,
    })
}

// M-HANDLE: ⟨handle M with H | γ | κ⟩ → ⟨M | γ | ([], (γ, H)) :: κ⟩
//
// This rule pushes a handler frame onto the continuation stack and
// continues evaluating the source computation M with the handler available.
impl Eval for HandleExpr {
    fn eval(&self, env: Env, mut cont: Cont, ir: &Ir) -> MachineState {
        let Self {
            bind,
            source,
            clause,
            next,
        } = *self;

        // Create a pure continuation frame for the ANF binding
        let pure_frame = PureContFrame {
            var: bind,
            body: next,
            env: env.clone(),
        };

        // Get the current frame and add our pure continuation to it
        let mut current_frame = cont.pop_or_identity(|| Env::from_env(&env));
        current_frame.pure.push(pure_frame);

        // Create handler from clauses (H in the rule)
        let handler = Handler::from_clauses(clause, ir);
        let handler_closure = HandlerClosure {
            handler,
            env: env.clone(),
        };

        // Push handler frame first ([], (γ, H)) :: κ
        // This ensures the handler is deeper in the stack
        cont.push(ContFrame {
            pure: PureCont::empty(), // Empty pure continuation as per M-HANDLE rule
            handler_closure,
        });

        // Push the frame with our ANF continuation on top
        cont.push(current_frame);

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
    fn eval(&self, env: Env, mut cont: Cont, _ir: &Ir) -> MachineState {
        let Self {
            bind,
            op,
            arg,
            next,
        } = *self;

        // Create a pure continuation frame
        let pure_frame = PureContFrame {
            var: bind,
            body: next,
            env: env.clone(),
        };

        let mut frame = cont.pop_or_identity(|| Env::from_env(&env));
        frame.pure.push(pure_frame);
        cont.push(frame);

        // Transition to operation configuration
        // The forwarding continuation starts empty ([])
        MachineState::Operation(OperationConfig {
            op,                     // ℓ - operation name
            arg,                    // V - operation argument (as atom ID)
            env,                    // γ - current environment
            cont,                   // κ - current continuation
            forward: Cont::empty(), // [] - empty forwarding continuation
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
    fn eval(&self, env: Env, mut cont: Cont, _ir: &Ir) -> MachineState {
        // Create a pure continuation frame
        let pure_frame = PureContFrame {
            var: self.bind,
            body: self.next,
            env: env.clone(),
        };

        let mut frame = cont.pop_or_identity(|| Env::from_env(&env));
        frame.pure.push(pure_frame);
        cont.push(frame);

        // Evaluate the bound value
        MachineState::Standard(StandardConfig {
            control: Expr::from(RetExpr { arg: self.value }),
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
    fn eval(&self, env: Env, mut cont: Cont, ir: &Ir) -> MachineState {
        let Self {
            bind,
            predicate,
            then,
            or,
            next,
        } = *self;

        // Evaluate the predicate
        let pred_val = match eval_atom(predicate.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Create a pure continuation frame for the next expression
        let pure_frame = PureContFrame {
            var: bind,
            body: next,
            env: env.clone(),
        };

        // Add the frame to the continuation
        let mut frame = cont.pop_or_identity(|| Env::from_env(&env));
        frame.pure.push(pure_frame);
        cont.push(frame);

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
            control: branch.get(ir),
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

// M-UNARY: <x = op V; N | γ | κ> --> <N | γ[x ↦ op(V)] | κ>
//
// This rule describes unary operations in the CEK machine:
// 1. We evaluate the operand V in the current environment γ
// 2. The unary operation op is applied to the evaluated value V
// 3. The result of the operation is bound to variable x in the environment
// 4. The machine transitions directly to evaluating the next expression N
// 5. The continuation κ remains unchanged during this transition
impl Eval for UnaryExpr {
    fn eval(&self, mut env: Env, cont: Cont, ir: &Ir) -> MachineState {
        let Self {
            bind,
            op,
            arg,
            next,
        } = *self;

        // Evaluate the operand
        let arg_val = match eval_atom(arg.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Apply the unary operation
        let result = match eval_unary_op(op, arg_val) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Create a new environment with the result bound to the variable
        env.insert(bind, result);

        // Continue directly with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(ir),
            env,
            cont,
        })
    }
}

pub fn eval_binary_op(op: BinaryOp, left: Value, right: Value) -> Result<Value, String> {
    match (op, left, right) {
        // TODO:
        // - String concatenation (Add?)
        (BinaryOp::Add, Value::Num(l), Value::Num(r)) => Ok(Value::Num(l + r)),
        (BinaryOp::Sub, Value::Num(l), Value::Num(r)) => Ok(Value::Num(l - r)),
        (BinaryOp::Mul, Value::Num(l), Value::Num(r)) => Ok(Value::Num(l * r)),
        (BinaryOp::Div, Value::Num(l), Value::Num(r)) => Ok(Value::Num(l / r)), // TODO check for division by zero?
        (BinaryOp::Rem, Value::Num(l), Value::Num(r)) => Ok(Value::Num(l % r)),
        (BinaryOp::Less, Value::Num(l), Value::Num(r)) => Ok(Value::Bool(l < r)),
        (BinaryOp::LessEq, Value::Num(l), Value::Num(r)) => Ok(Value::Bool(l <= r)),
        (BinaryOp::Greater, Value::Num(l), Value::Num(r)) => Ok(Value::Bool(l > r)),
        (BinaryOp::GreaterEq, Value::Num(l), Value::Num(r)) => Ok(Value::Bool(l >= r)),
        (BinaryOp::And, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l && r)),
        (BinaryOp::Or, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l || r)),
        (BinaryOp::Eq, l, r) => Ok(Value::Bool(l == r)), // TODO: might need special handling
        (BinaryOp::NotEq, l, r) => Ok(Value::Bool(l != r)), // TODO: might need special handling
        (BinaryOp::Merge, l, r) => todo!(),              // TODO: Merge records
        (op, left, right) => Err(format!(
            "Cannot apply binary operation {:?} to: {:?} and {:?}",
            op, left, right
        )),
    }
}

// M-BINARY: <x = V1 op V2; N | γ | κ> --> <N | γ[x ↦ op(V1, V2)] | κ>
//
// This rule describes binary operations in the CEK machine:
// 1. We evaluate both operands V1 and V2 in the current environment γ
// 2. The binary operation op is applied to the evaluated values
// 3. The result is bound to variable x in the environment
// 4. The machine transitions directly to evaluating the next expression N
impl Eval for BinaryExpr {
    fn eval(&self, mut env: Env, cont: Cont, ir: &Ir) -> MachineState {
        let Self {
            bind,
            op,
            lhs,
            rhs,
            next,
        } = *self;

        // Evaluate the left operand
        let left_val = match eval_atom(lhs.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Evaluate the right operand
        let right_val = match eval_atom(rhs.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Apply the binary operation
        let result = match eval_binary_op(op, left_val, right_val) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Create a new environment with the result bound to the variable
        env.insert(bind, result);

        // Continue directly with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(ir),
            env,
            cont,
        })
    }
}

impl Eval for ListExpr {
    fn eval(&self, mut env: Env, cont: Cont, ir: &Ir) -> MachineState {
        let ListExpr {
            bind,
            head,
            tail: _,
            next,
        } = *self;

        let mut list = List::new();

        for ListItem { value, .. } in ir.iter_items(head) {
            match eval_atom(value.get(ir), &env) {
                Ok(value) => list.push_back(value),
                Err(err) => return MachineState::Error(err),
            }
        }

        let list_value = Value::List(list);
        env.insert(bind, list_value);

        MachineState::Standard(StandardConfig {
            control: next.get(ir),
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
    fn eval(&self, mut env: Env, cont: Cont, ir: &Ir) -> MachineState {
        let RecordExpr { bind, head, next } = *self;

        let mut record = ImShadowMap::new();

        // Evaluate each field value
        for RecordField { label, value, .. } in ir.iter_fields(head) {
            match eval_atom(value.get(ir), &env) {
                Ok(value) => record.insert(label, value),
                Err(err) => return MachineState::Error(err),
            }
        }

        // Create a record value from the evaluated fields
        let record_value = Value::Record(Record::from(record));

        // Bind the record to the variable in the environment
        env.insert(bind, record_value);

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(ir),
            env,
            cont,
        })
    }
}

// M-RECORDEXTEND: <x = R.{l=V}; N | γ | κ> --> <N | γ[x ↦ (γ(R) ⊕ {l=V})] | κ>
//
// This rule describes record extension in the CEK machine:
// 1. We evaluate the base record R and the value V in the current environment γ
// 2. We extend the record with the new field path
// 3. The resulting extended record is bound to variable x in the environment
// 4. The machine transitions directly to evaluating the next expression N
impl Eval for RecordExtendExpr {
    fn eval(&self, mut env: Env, cont: Cont, ir: &Ir) -> MachineState {
        let Self {
            bind,
            base,
            path,
            value,
            next,
        } = *self;

        // Evaluate the base record
        let record_val = match eval_atom(base.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Evaluate the value to extend with
        let extend_value = match eval_atom(value.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Extract the record from the value
        let Value::Record(mut record) = record_val else {
            return MachineState::Error(format!(
                "Cannot extend non-record value: {:?}",
                record_val
            ));
        };

        // Get field path as StrKeys
        let field_path: Vec<_> = ir.iter_path(Some(path)).map(|fp| fp.label).collect();

        // Use Record's extend_at_path method
        if let Err(err) = record.extend_at_path(&field_path, extend_value) {
            return MachineState::Error(err);
        }

        // Bind the extended record to the variable in the environment
        env.insert(bind, Value::Record(record));

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(ir),
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
    fn eval(&self, mut env: Env, cont: Cont, ir: &Ir) -> MachineState {
        let Self {
            bind,
            base,
            path,
            next,
        } = *self;

        // Evaluate the base record
        let record_val = match eval_atom(base.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Extract the record from the value
        let Value::Record(mut record) = record_val else {
            return MachineState::Error(format!(
                "Cannot restrict non-record value: {:?}",
                record_val
            ));
        };

        // Get field path as StrKeys
        let field_path: Vec<_> = ir.iter_path(Some(path)).map(|fp| fp.label).collect();

        // Use Record's restrict_at_path method
        if let Err(err) = record.restrict_at_path(&field_path) {
            return MachineState::Error(err);
        }

        // Bind the restricted record to the variable in the environment
        env.insert(bind, Value::Record(record));

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(ir),
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
    fn eval(&self, mut env: Env, cont: Cont, ir: &Ir) -> MachineState {
        let Self {
            bind,
            base,
            path,
            op,
            value,
            next,
        } = *self;

        // Evaluate the base record
        let record_val = match eval_atom(base.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Evaluate the value to update with
        let update_val = match eval_atom(value.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Extract the record from the value
        let Value::Record(mut record) = record_val else {
            return MachineState::Error(format!(
                "Cannot update non-record value: {:?}",
                record_val
            ));
        };

        // Get field path as StrKeys
        let field_path: Vec<_> = ir.iter_path(Some(path)).map(|fp| fp.label).collect();

        // Use Record's update_at_path method
        let update_fn = |current_val: Value| eval_record_op(op, current_val, update_val.clone());
        if let Err(err) = record.update_at_path(&field_path, update_fn) {
            return MachineState::Error(err);
        }

        // Bind the updated record to the variable in the environment
        env.insert(bind, Value::Record(record));

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(ir),
            env,
            cont,
        })
    }
}

// M-RECORDACCESS: <x = R.l; N | γ | κ> --> <N | γ[x ↦ (γ(R).l)] | κ>
//
// This rule describes record field access in the CEK machine:
// 1. We evaluate the record R in the current environment γ
// 2. We access the field with label l from the record
// 3. The resulting field value is bound to variable x in the environment
// 4. The machine transitions directly to evaluating the next expression N
impl Eval for RecordAccessExpr {
    fn eval(&self, mut env: Env, cont: Cont, ir: &Ir) -> MachineState {
        let Self {
            bind,
            base,
            label,
            next,
        } = *self;

        // Evaluate the record
        let record_val = match eval_atom(base.get(ir), &env) {
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
        let Some(value) = record.get(label).cloned() else {
            return MachineState::Error(format!("Field {:?} not found in record", label));
        };

        // Bind the field value to the variable in the environment
        env.insert(bind, value);

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(ir),
            env,
            cont,
        })
    }
}
impl Eval for PatternMatchExpr {
    fn eval(&self, env: Env, mut cont: Cont, _ir: &Ir) -> MachineState {
        let Self {
            bind,
            matcher,
            next,
        } = *self;

        // Create a pure continuation frame for the next expression
        let pure_frame = PureContFrame {
            var: bind,
            body: next,
            env: env.clone(),
        };

        // Add the frame to the continuation
        let mut frame = cont.pop_or_identity(|| Env::from_env(&env));
        frame.pure.push(pure_frame);
        cont.push(frame);

        // Transition to pattern matching state
        MachineState::Pattern(PatternConfig { matcher, env, cont })
    }
}

/// Evaluates a pattern matching configuration
impl Eval for PatternConfig {
    fn eval(&self, _env: Env, _cont: Cont, ir: &Ir) -> MachineState {
        let PatternConfig { matcher, env, cont } = self;

        eval_pattern_matcher(&matcher.get(ir), env.clone(), cont.clone(), ir)
    }
}
/// Evaluates a pattern matcher instruction, returning the next machine state
fn eval_pattern_matcher(matcher: &PatternMatcher, env: Env, cont: Cont, ir: &Ir) -> MachineState {
    match matcher {
        PatternMatcher::IsUnit(is_unit) => eval_is_unit(is_unit, env, cont, ir),
        PatternMatcher::IsBool(is_bool) => eval_is_bool(is_bool, env, cont, ir),
        PatternMatcher::IsNum(is_num) => eval_is_num(is_num, env, cont, ir),
        PatternMatcher::IsChar(is_char) => eval_is_char(is_char, env, cont, ir),
        PatternMatcher::IsStr(is_str) => eval_is_str(is_str, env, cont, ir),
        PatternMatcher::IsVariant(is_variant) => eval_is_variant(is_variant, env, cont, ir),
        PatternMatcher::IsRecord(is_record) => eval_is_record(is_record, env, cont, ir),
        PatternMatcher::RecordHasField(record_has_field) => {
            eval_record_has_field(record_has_field, env, cont, ir)
        }
        PatternMatcher::IsList(is_list) => eval_is_list(is_list, env, cont, ir),
        PatternMatcher::ListIsExact(list_is_exact) => {
            eval_list_is_exact(list_is_exact, env, cont, ir)
        }
        PatternMatcher::ListIsAtLeast(list_is_at_least) => {
            eval_list_is_at_least(list_is_at_least, env, cont, ir)
        }
        PatternMatcher::Identity(extract) => eval_identity(extract, env, cont, ir),
        PatternMatcher::VariantGet(extract) => eval_variant_get(extract, env, cont, ir),
        PatternMatcher::RecordGetAt(extract) => eval_record_get_at(extract, env, cont, ir),
        PatternMatcher::ListSplitHead(extract) => eval_list_split_head(extract, env, cont, ir),
        PatternMatcher::ListSplitTail(extract) => eval_list_split_tail(extract, env, cont, ir),
        PatternMatcher::ListGetAt(extract) => eval_list_get_at(extract, env, cont, ir),
        PatternMatcher::ListSplitAt(extract) => eval_list_split_at(extract, env, cont, ir),

        PatternMatcher::Success(success) => eval_pattern_success(success, env, cont, ir),
        PatternMatcher::Failure(_) => {
            MachineState::Error("Pattern match failure - no matching case found".to_string())
        }
    }
}

/// Evaluates IsUnit pattern matcher - tests if the source value is Unit
fn eval_is_unit(is_unit: &IsUnit, env: Env, cont: Cont, _ir: &Ir) -> MachineState {
    let IsUnit {
        source,
        on_success,
        on_failure,
    } = *is_unit;

    // Get the actual source value from the environment
    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
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
fn eval_is_bool(is_bool: &IsBool, env: Env, cont: Cont, _ir: &Ir) -> MachineState {
    let IsBool {
        source,
        payload,
        on_success,
        on_failure,
    } = *is_bool;

    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
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
fn eval_is_num(is_num: &IsNum, env: Env, cont: Cont, _ir: &Ir) -> MachineState {
    let IsNum {
        source,
        payload,
        on_success,
        on_failure,
    } = *is_num;

    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
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
fn eval_is_char(is_char: &IsChar, env: Env, cont: Cont, _ir: &Ir) -> MachineState {
    let IsChar {
        source,
        payload,
        on_success,
        on_failure,
    } = *is_char;

    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
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
fn eval_is_str(is_str: &IsStr, env: Env, cont: Cont, _ir: &Ir) -> MachineState {
    let IsStr {
        source,
        payload,
        on_success,
        on_failure,
    } = *is_str;

    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    let next_matcher = match source_val {
        Value::Str(ref s) => {
            // Get the string from the interner to compare
            let expected_str = &env[payload];
            if s == expected_str {
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

fn eval_is_variant(is_tag: &IsVariant, env: Env, cont: Cont, _ir: &Ir) -> MachineState {
    let IsVariant {
        source,
        tag,
        on_success,
        on_failure,
    } = *is_tag;

    // Get the source value from the environment
    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
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
fn eval_is_record(is_record: &IsRecord, env: Env, cont: Cont, _ir: &Ir) -> MachineState {
    let IsRecord {
        source,
        on_success,
        on_failure,
    } = *is_record;

    // Get the source value from the environment
    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
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
    env: Env,
    cont: Cont,
    _ir: &Ir,
) -> MachineState {
    let RecordHasField {
        source,
        field,
        on_success,
        on_failure,
    } = *record_has_field;

    // Get the source value from the environment
    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    let next_matcher = match source_val {
        Value::Record(record) if record.contains_key(field) => on_success,
        _ => on_failure,
    };

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: next_matcher,
        env,
        cont,
    })
}

fn eval_is_list(is_list: &IsList, env: Env, cont: Cont, _ir: &Ir) -> MachineState {
    let IsList {
        source,
        on_success,
        on_failure,
    } = *is_list;

    // Get the source value from the environment
    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
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

fn eval_list_is_exact(list_is_exact: &ListIsExact, env: Env, cont: Cont, _ir: &Ir) -> MachineState {
    let ListIsExact {
        source,
        length,
        on_success,
        on_failure,
    } = *list_is_exact;

    // Get the source value from the environment
    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    let next_matcher = match source_val {
        Value::List(ref list) if list.len() as u32 == length => on_success,
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
    env: Env,
    cont: Cont,
    _ir: &Ir,
) -> MachineState {
    let ListIsAtLeast {
        source,
        min_length,
        on_success,
        on_failure,
    } = *list_is_atleast;

    // Get the source value from the environment
    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    let next_matcher = match source_val {
        Value::List(ref list) if list.len() as u32 >= min_length => on_success,
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
fn eval_identity(extract: &Identity, mut env: Env, cont: Cont, _ir: &Ir) -> MachineState {
    let Identity {
        bind: extract_bind,
        source,
        next: pattern_next,
    } = *extract;

    // Get the source value from the environment
    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    // Bind the source value to the target variable
    env.insert(extract_bind, source_val);

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: pattern_next,
        env,
        cont,
    })
}
fn eval_variant_get(extract: &VariantGet, mut env: Env, cont: Cont, _ir: &Ir) -> MachineState {
    let VariantGet {
        bind: extract_bind,
        source,
        next: pattern_next,
    } = *extract;

    // Get the source value from the environment
    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    // Ensure the source value is a variant
    let Value::Variant(variant) = source_val else {
        return MachineState::Error(format!("Expected a variant, got: {:?}", source_val));
    };

    // Bind the payload to the target variable
    env.insert(extract_bind, variant.value().clone());

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: pattern_next,
        env,
        cont,
    })
}

fn eval_record_get_at(extract: &RecordGetAt, mut env: Env, cont: Cont, _ir: &Ir) -> MachineState {
    let RecordGetAt {
        bind: extract_bind,
        source,
        field,
        next: pattern_next,
    } = *extract;

    // Get the source value from the environment
    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    // Ensure the source value is a record
    let Value::Record(record) = source_val else {
        return MachineState::Error(format!("Expected a record, got: {:?}", source_val));
    };

    // Get the field value from the record
    if let Some(value) = record.get(field).cloned() {
        // Bind the field value to the target variable
        env.insert(extract_bind, value);
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
    mut env: Env,
    cont: Cont,
    _ir: &Ir,
) -> MachineState {
    let ListSplitHead {
        head,
        tail_list,
        source,
        next: pattern_next,
    } = *extract;

    // Get the source value from the environment
    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    // Ensure the source value is a list
    let Value::List(list) = source_val else {
        return MachineState::Error(format!("Expected a list, got: {:?}", source_val));
    };

    // Get the head of the list
    if let Some((h, t)) = list.split_first() {
        // Bind the head to the target variable
        env.insert(head, h);
        env.insert(tail_list, t);
    } else {
        return MachineState::Error(format!("List is empty, cannot extract head"));
    }

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: pattern_next,
        env,
        cont,
    })
}

fn eval_list_split_tail(
    extract: &ListSplitTail,
    mut env: Env,
    cont: Cont,
    _ir: &Ir,
) -> MachineState {
    let ListSplitTail {
        tail,
        head_list,
        source,
        next: pattern_next,
    } = *extract;

    // Get the source value from the environment
    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    // Ensure the source value is a list
    let Value::List(list) = source_val else {
        return MachineState::Error(format!("Expected a list, got: {:?}", source_val));
    };

    // Get the tail of the list
    if let Some((h, t)) = list.split_last() {
        // Bind the tail to the target variable
        env.insert(tail, t);
        env.insert(head_list, h);
    } else {
        return MachineState::Error(format!("List is empty, cannot extract tail"));
    }

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: pattern_next,
        env,
        cont,
    })
}

fn eval_list_get_at(extract: &ListGetAt, mut env: Env, cont: Cont, _ir: &Ir) -> MachineState {
    let ListGetAt {
        bind: extract_bind,
        source,
        index,
        next: pattern_next,
    } = *extract;

    // Get the source value from the environment
    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    // Ensure the source value is a list
    let Value::List(list) = source_val else {
        return MachineState::Error(format!("Expected a list, got: {:?}", source_val));
    };

    // Get the item at the specified index
    if let Some(item) = list.get(index as usize) {
        // Bind the item to the target variable
        env.insert(extract_bind, item.clone());
    } else {
        return MachineState::Error(format!("Index out of bounds for list: {:?}", list));
    }

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: pattern_next,
        env,
        cont,
    })
}

fn eval_list_split_at(extract: &ListSplitAt, mut env: Env, cont: Cont, _ir: &Ir) -> MachineState {
    let ListSplitAt {
        head,
        tail,
        source,
        index,
        next: pattern_next,
    } = *extract;

    // Get the source value from the environment
    let source_val = match env.get(&source) {
        Some(val) => val.clone(),
        None => return MachineState::Error(format!("Unbound variable: {}", source)),
    };

    // Ensure the source value is a list
    let Value::List(list) = source_val else {
        return MachineState::Error(format!("Expected a list, got: {:?}", source_val));
    };

    // Get the slices from the specified range
    let (h, t) = list.split_at(index as usize);
    env.insert(head, h);
    env.insert(tail, t);

    // Continue with the next pattern matcher
    MachineState::Pattern(PatternConfig {
        matcher: pattern_next,
        env,
        cont,
    })
}

/// Evaluates PatternSuccess - pattern matching succeeded, continue to the matched expression
fn eval_pattern_success(success: &PatternSuccess, env: Env, cont: Cont, ir: &Ir) -> MachineState {
    // TODO either use PatternMatcherExpr to setup continuation and use it here,
    // or use the next id of PatternSuccess directly but do not do both
    let PatternSuccess { next } = *success;

    // Bind the original source value to the result binding
    // env.insert(bind, source_value); // TODO this would only be necessary if I would actually mutate variables I guess

    // Continue with the matched branch expression
    MachineState::Standard(StandardConfig {
        control: next.get(ir).clone(),
        env,
        cont,
    })
}
