use std::fs;

use crate::{
    closure::Closure,
    config::{MachineState, OperationConfig, PatternConfig, StandardConfig},
    cont::{Cont, ContFrame},
    env::EnvIdx,
    handler::{RawHandler, ReturnClause},
    heap::Heap,
    machine::MachineContext,
    string::StringIdx,
    value::{Value, to_usize_exact},
};
use kola_builtins::BuiltinId;
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
use kola_protocol::TypeProtocol;

#[inline]
pub fn eval_symbol(symbol: Symbol, env: EnvIdx, heap: &Heap) -> Result<Value, String> {
    // Look up the symbol in the environment
    heap.envs
        .get(env, symbol)
        .ok_or_else(|| format!("Unbound variable: {}", symbol))
}

pub fn eval_atom(atom: Atom, env: EnvIdx, heap: &mut Heap) -> Result<Value, String> {
    match atom {
        Atom::Noop => Ok(Value::None),
        Atom::Bool(b) => Ok(Value::Bool(b)),
        Atom::Char(c) => Ok(Value::Char(c)),
        Atom::Num(n) => Ok(Value::Num(n)),
        Atom::Str(s) => Ok(Value::Str(StringIdx::Static(s))),
        Atom::Func(f) => {
            // Create a closure by capturing the current environment
            Ok(Value::Closure(Closure::new(env, f)))
        }
        Atom::Symbol(s) => eval_symbol(s, env, heap),
        Atom::Builtin(b) => Ok(Value::Builtin(b)),
        Atom::Tag(t) => Ok(Value::Tag(t)),
        Atom::Witness(w) => Ok(Value::Witness(heap.alloc_type_key(w.0))),
    }
}

pub trait Eval {
    fn eval(
        &self,
        env: EnvIdx,
        cont: Cont,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState;
}

impl Eval for Expr {
    fn eval(
        &self,
        env: EnvIdx,
        cont: Cont,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
        match self {
            Expr::Ret(ret_expr) => ret_expr.eval(env, cont, context, heap),
            Expr::Call(call) => call.eval(env, cont, context, heap),
            Expr::Handle(handle) => handle.eval(env, cont, context, heap),
            Expr::Do(do_expr) => do_expr.eval(env, cont, context, heap),
            Expr::Let(let_expr) => let_expr.eval(env, cont, context, heap),
            Expr::If(if_expr) => if_expr.eval(env, cont, context, heap),
            Expr::Unary(unary_expr) => unary_expr.eval(env, cont, context, heap),
            Expr::Binary(binary_expr) => binary_expr.eval(env, cont, context, heap),
            Expr::List(list_expr) => list_expr.eval(env, cont, context, heap),
            Expr::Record(record_expr) => record_expr.eval(env, cont, context, heap),
            Expr::RecordExtend(record_extend_expr) => {
                record_extend_expr.eval(env, cont, context, heap)
            }
            Expr::RecordRestrict(record_restrict_expr) => {
                record_restrict_expr.eval(env, cont, context, heap)
            }
            Expr::RecordUpdate(record_update_expr) => {
                record_update_expr.eval(env, cont, context, heap)
            }
            Expr::RecordAccess(record_access_expr) => {
                record_access_expr.eval(env, cont, context, heap)
            }
            Expr::PatternMatch(pattern_match_expr) => {
                pattern_match_expr.eval(env, cont, context, heap)
            }
        }
    }
}

// M-RET : Return with a value
impl Eval for RetExpr {
    fn eval(
        &self,
        env: EnvIdx,
        mut cont: Cont,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
        // Evaluate the return machine state of value
        let value = match eval_atom(self.arg.get(&context.ir), env, heap) {
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
                    control: body.get(&context.ir),
                    env,
                    cont,
                })
            }
            ContFrame::Handler { handler, env } => {
                // M-RETHANDLER: <return V | γ | ([], (γ', H)) :: κ> --> <M | γ'[x ↦ V] | κ>,
                // if H(return) = {return x ↦ M}
                //
                // When a return expression with value V reaches a handler frame with
                // an empty pure continuation, we:
                // 1. Extract the return clause from the handler H
                // 2. Bind the returned value V to parameter x in the handler's environment γ'
                // 3. Continue by evaluating the return handler's body M with the updated environment
                // 4. Use the continuation κ below the current handler frame

                match handler.return_clause {
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
                        let env = heap.envs.insert(env, param, value);

                        // Evaluate the return handler body
                        MachineState::Standard(StandardConfig {
                            control: body.get(&context.ir),
                            env,
                            cont,
                        })
                    }
                }
            }
            ContFrame::NumRec { data, step } => {
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
                let acc = (heap.intern_str("acc"), value); // value = result from previous step
                let head = (heap.intern_str("head"), Value::Num(data));
                let record = heap.records.alloc(&[acc, head]);

                let env = heap.envs.insert(env, param, Value::Record(record));

                MachineState::Standard(StandardConfig {
                    control: body.get(&context.ir),
                    env,
                    cont,
                })
            }
            ContFrame::ListRec { data, step } => {
                let Some((head, tail)) = heap.lists.split_front(data) else {
                    // Empty list — continue with next frame
                    return MachineState::Standard(StandardConfig {
                        control: Expr::from(*self),
                        env,
                        cont,
                    });
                };

                // Continue with the next frame
                cont.push(ContFrame::ListRec { data: tail, step });

                let Closure {
                    env,
                    func: Func { param, body },
                } = step;

                // Create argument for the step function
                let acc = (heap.intern_str("acc"), value); // value = result from previous step
                let head = (heap.intern_str("head"), head);
                let record = heap.records.alloc(&[acc, head]);

                let env = heap.envs.insert(env, param, Value::Record(record));

                MachineState::Standard(StandardConfig {
                    control: body.get(&context.ir),
                    env,
                    cont,
                })
            }
            ContFrame::RecordRec { data, step } => {
                let Some((first, tail)) = heap.records.split_front(data) else {
                    // Empty record — continue with next frame
                    return MachineState::Standard(StandardConfig {
                        control: Expr::from(*self),
                        env,
                        cont,
                    });
                };

                let key = (
                    heap.intern_str("key"),
                    Value::Str(StringIdx::Static(first.0)),
                );
                let val = (heap.intern_str("value"), first.1);
                let head = heap.records.alloc(&[key, val]);

                // Continue with the next frame
                cont.push(ContFrame::RecordRec { data: tail, step });

                let Closure {
                    env,
                    func: Func { param, body },
                } = step;

                // Create argument for the step function
                let acc = (heap.intern_str("acc"), value); // value = result from previous step
                let head = (heap.intern_str("head"), Value::Record(head));
                let record = heap.records.alloc(&[acc, head]);

                let env = heap.envs.insert(env, param, Value::Record(record));

                MachineState::Standard(StandardConfig {
                    control: body.get(&context.ir),
                    env,
                    cont,
                })
            }
            ContFrame::StrRec { data, step } => {
                let Some((head, tail)) = heap.strings.split_front(data) else {
                    // Empty string — continue with next frame
                    return MachineState::Standard(StandardConfig {
                        control: Expr::from(*self),
                        env,
                        cont,
                    });
                };

                // Continue with the next frame
                cont.push(ContFrame::StrRec { data: tail, step });

                let Closure {
                    env,
                    func: Func { param, body },
                } = step;

                // Create argument for the step function
                let acc = (heap.intern_str("acc"), value); // value = result from previous step
                let head = (heap.intern_str("head"), Value::Str(head));
                let record = heap.records.alloc(&[acc, head]);

                let env = heap.envs.insert(env, param, Value::Record(record));

                MachineState::Standard(StandardConfig {
                    control: body.get(&context.ir),
                    env,
                    cont,
                })
            }
        }
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
    fn eval(
        &self,
        env: EnvIdx,
        mut cont: Cont,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
        let Self {
            bind,
            func,
            arg,
            next,
        } = *self;

        // Get the function and argument
        let func_val = match eval_atom(func.get(&context.ir), env, heap) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        let arg_val = match eval_atom(arg.get(&context.ir), env, heap) {
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
                    control: body.get(&context.ir),
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
                eval_builtin(builtin, bind, arg_val, env, cont, next, context, heap)
            }
            Value::Tag(tag) => {
                let value = Value::Variant(heap.variants.alloc(tag, arg_val));

                // Create a new environment with the result bound to the variable
                let env = heap.envs.insert(env, bind, value);

                // Continue with the next expression
                MachineState::Standard(StandardConfig {
                    control: next.get(&context.ir),
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
    mut cont: Cont,
    next: Id<Expr>,
    context: &mut MachineContext,
    heap: &mut Heap,
) -> MachineState {
    let value = match (builtin, arg) {
        (BuiltinId::IoDebug, value) => {
            println!("Debug: {:?}", value);
            value
        }
        (BuiltinId::IoReadFile, Value::Str(path)) => {
            let flattened = heap.strings.flatten(path);
            let path = heap.strings.try_get(&flattened).unwrap();

            match fs::read_to_string(context.join_path(path)) {
                Err(err) => {
                    let err = heap.strings.alloc(&err.to_string());
                    Value::Variant(heap.alloc_builtin_variant("Err", Value::Str(err)))
                }
                Ok(contents) => {
                    let contents = heap.strings.alloc(&contents);
                    Value::Variant(heap.alloc_builtin_variant("Ok", Value::Str(contents)))
                }
            }
        }
        (BuiltinId::IoWriteFile, Value::Record(record)) => {
            let Some(Value::Str(path)) = heap.get_record_value(record, "path") else {
                return MachineState::Error(
                    "io_write_file requires 'path' field with a string".to_owned(),
                );
            };

            let Some(Value::Str(contents)) = heap.get_record_value(record, "contents") else {
                return MachineState::Error(
                    "io_write_file requires 'contents' field with a string".to_owned(),
                );
            };

            let path_result = heap.strings.flatten(path);
            let contents_result = heap.strings.flatten(contents);

            let path = heap.strings.try_get(&path_result).unwrap();
            let contents = heap.strings.try_get(&contents_result).unwrap();

            match fs::write(context.join_path(path), contents) {
                Err(err) => {
                    let err = heap.strings.alloc(&err.to_string());
                    Value::Variant(heap.alloc_builtin_variant("Err", Value::Str(err)))
                }
                Ok(_) => Value::Variant(heap.alloc_builtin_variant("Ok", Value::None)),
            }
        }
        (BuiltinId::ListLength, Value::List(list)) => Value::Num(heap.lists.len(list) as f64),
        (BuiltinId::ListIsEmpty, Value::List(list)) => Value::Bool(heap.lists.is_empty(list)),
        (BuiltinId::ListReverse, Value::List(list)) => Value::List(heap.lists.reverse(list)),
        (BuiltinId::ListSum, Value::List(list)) => {
            match heap.lists.try_fold(list, 0.0, |acc, v| {
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
            let Some(Value::List(list)) = heap.get_record_value(record, "list") else {
                return MachineState::Error(
                    "list_contains requires 'list' field with a list".to_owned(),
                );
            };

            let Some(value) = heap.get_record_value(record, "value") else {
                return MachineState::Error("list_contains requires 'value' field".to_owned());
            };

            Value::Bool(heap.lists.contains(list, value))
        }
        (BuiltinId::ListAt, Value::Record(record)) => {
            let Some(Value::List(list)) = heap.get_record_value(record, "list") else {
                return MachineState::Error(
                    "list_get requires 'list' field with a list".to_owned(),
                );
            };

            let Some(Value::Num(index)) = heap.get_record_value(record, "index") else {
                return MachineState::Error("list_get requires 'index' field".to_owned());
            };

            match to_usize_exact(index).and_then(|idx| heap.lists.get_element(list, idx)) {
                Some(value) => Value::Variant(heap.alloc_builtin_variant("Some", value)),
                None => Value::Variant(heap.alloc_builtin_variant("None", Value::None)),
            }
        }
        (BuiltinId::ListFirst, Value::List(list)) => match heap.lists.first(list) {
            Some(head) => Value::Variant(heap.alloc_builtin_variant("Some", head)),
            None => Value::Variant(heap.alloc_builtin_variant("None", Value::None)),
        },
        (BuiltinId::ListLast, Value::List(list)) => match heap.lists.last(list) {
            Some(tail) => Value::Variant(heap.alloc_builtin_variant("Some", tail)),
            None => Value::Variant(heap.alloc_builtin_variant("None", Value::None)),
        },
        (BuiltinId::ListPrepend, Value::Record(record)) => {
            let Some(head_value) = heap.get_record_value(record, "head") else {
                return MachineState::Error("list_prepend requires 'head' field".to_owned());
            };

            let Some(Value::List(tail_list)) = heap.get_record_value(record, "tail") else {
                return MachineState::Error(
                    "list_prepend requires 'tail' field with a list".to_owned(),
                );
            };

            let result = heap.lists.push_front(tail_list, head_value);
            Value::List(Some(result))
        }
        (BuiltinId::ListAppend, Value::Record(record)) => {
            let Some(Value::List(head_list)) = heap.get_record_value(record, "head") else {
                return MachineState::Error(
                    "list_append requires 'head' field with a list".to_owned(),
                );
            };

            let Some(tail_value) = heap.get_record_value(record, "tail") else {
                return MachineState::Error("list_append requires 'tail' field".to_owned());
            };

            let result = heap.lists.push_back(head_list, tail_value);
            Value::List(Some(result))
        }
        (BuiltinId::ListConcat, Value::Record(record)) => {
            let Some(Value::List(head_list)) = heap.get_record_value(record, "head") else {
                return MachineState::Error(
                    "list_concat requires 'head' field with a list".to_owned(),
                );
            };

            let Some(Value::List(tail_list)) = heap.get_record_value(record, "tail") else {
                return MachineState::Error(
                    "list_concat requires 'tail' field with a list".to_owned(),
                );
            };

            let result = heap.lists.concat(head_list, tail_list);
            Value::List(result)
        }
        // list_rec([], base, step) = base
        // list_rec([head, ...tail], base, step) = step(head, list_rec(tail, base step))
        (BuiltinId::ListRec, Value::Record(record)) => {
            let Some(Value::List(list)) = heap.get_record_value(record, "list") else {
                return MachineState::Error(
                    "list_rec requires 'list' field with a list".to_owned(),
                );
            };

            let Some(base) = heap.get_record_value(record, "base") else {
                return MachineState::Error("list_rec requires 'base' field".to_owned());
            };

            let Some(Value::Closure(step)) = heap.get_record_value(record, "step") else {
                return MachineState::Error(
                    "list_rec requires 'step' field with a func".to_owned(),
                );
            };

            let Some((head, tail)) = heap.lists.split_front(list) else {
                // Empty list — bind base directly, go to next
                let env = heap.envs.insert(env, bind, base);

                return MachineState::Standard(StandardConfig {
                    control: next.get(&context.ir),
                    env,
                    cont,
                });
            };

            // Create a pure continuation frame for the next expression
            let pure_frame = ContFrame::pure(bind, next, env);
            cont.push(pure_frame);

            // Create a primitive recursion frame for the list_rec operation,
            // which will handle the recursive processing of the list.
            let rec_frame = ContFrame::list_rec(tail, step);
            cont.push(rec_frame);

            let Closure {
                env,
                func: Func { param, body },
            } = step;

            // Create argument for the step function
            let acc = (heap.intern_str("acc"), base); // value = result from previous step
            let head = (heap.intern_str("head"), head);
            let record = heap.records.alloc(&[acc, head]);

            let env = heap.envs.insert(env, param, Value::Record(record));

            return MachineState::Standard(StandardConfig {
                control: body.get(&context.ir),
                env,
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
            let Some(Value::Num(base)) = heap.get_record_value(record, "base") else {
                return MachineState::Error("num_pow requires 'base' field".to_owned());
            };
            let Some(Value::Num(exp)) = heap.get_record_value(record, "exp") else {
                return MachineState::Error("num_pow requires 'exp' field".to_owned());
            };

            Value::Num(base.powf(exp))
        }
        (BuiltinId::NumRec, Value::Record(record)) => {
            let Some(Value::Num(n)) = heap.get_record_value(record, "num") else {
                return MachineState::Error(
                    "num_rec requires 'num' field with a number".to_owned(),
                );
            };

            let Some(base) = heap.get_record_value(record, "base") else {
                return MachineState::Error("num_rec requires 'base' field".to_owned());
            };

            let Some(Value::Closure(step)) = heap.get_record_value(record, "step") else {
                return MachineState::Error("num_rec requires 'step' field with a func".to_owned());
            };

            if n <= 0.0 {
                // Empty number — bind base directly, go to next
                let env = heap.envs.insert(env, bind, base);

                return MachineState::Standard(StandardConfig {
                    control: next.get(&context.ir),
                    env,
                    cont,
                });
            }

            // Create a pure continuation frame for the next expression
            let pure_frame = ContFrame::pure(bind, next, env);
            cont.push(pure_frame);

            // Create a primitive recursion frame
            if n >= 2.0 {
                let rec_frame = ContFrame::num_rec(n - 2.0, step);
                cont.push(rec_frame);
            }

            let Closure {
                env,
                func: Func { param, body },
            } = step;

            // Create argument for the step function
            let acc = (heap.intern_str("acc"), base); // value = result from previous step
            let head = (heap.intern_str("head"), Value::Num(n - 1.0));
            let record = heap.records.alloc(&[acc, head]);

            let env = heap.envs.insert(env, param, Value::Record(record));

            return MachineState::Standard(StandardConfig {
                control: body.get(&context.ir),
                env,
                cont,
            });
        }
        (BuiltinId::RecordSelect, Value::Record(record)) => {
            let Some(Value::Witness(wit)) = heap.get_record_value(record, "label") else {
                return MachineState::Error(
                    "record_select requires 'label' field with a string".to_owned(),
                );
            };

            let TypeProtocol::Label(label) = heap.witnesses.get(wit).clone() else {
                return MachineState::Error(
                    "record_select requires 'label' field with a string".to_owned(),
                );
            };

            let Some(Value::Record(record)) = heap.get_record_value(record, "record") else {
                return MachineState::Error(
                    "record_select requires 'record' field with a record".to_owned(),
                );
            };

            match heap.get_record_value(record, &label) {
                Some(value) => value,
                None => {
                    return MachineState::Error(format!("Field '{}' not found in record", label));
                }
            }
        }
        (BuiltinId::RecordInsert, Value::Record(record)) => {
            let Some(Value::Witness(wit)) = heap.get_record_value(record, "label") else {
                return MachineState::Error(
                    "record_insert requires 'label' field with a string".to_owned(),
                );
            };

            let TypeProtocol::Label(label) = heap.witnesses.get(wit).clone() else {
                return MachineState::Error(
                    "record_insert requires 'label' field with a string".to_owned(),
                );
            };

            let Some(value) = heap.get_record_value(record, "value") else {
                return MachineState::Error("record_insert requires 'value' field".to_owned());
            };

            let Some(Value::Record(record)) = heap.get_record_value(record, "record") else {
                return MachineState::Error(
                    "record_insert requires 'record' field with a record".to_owned(),
                );
            };

            // Insert the value into the record
            let label = heap.intern_str(label);
            heap.records.insert(record, label, value);

            Value::Record(record)
        }
        (BuiltinId::RecordRemove, Value::Record(record)) => {
            let Some(Value::Witness(wit)) = heap.get_record_value(record, "label") else {
                return MachineState::Error(
                    "record_remove requires 'label' field with a string".to_owned(),
                );
            };

            let TypeProtocol::Label(label) = heap.witnesses.get(wit).clone() else {
                return MachineState::Error(
                    "record_remove requires 'label' field with a string".to_owned(),
                );
            };

            let Some(Value::Record(record)) = heap.get_record_value(record, "record") else {
                return MachineState::Error(
                    "record_remove requires 'record' field with a record".to_owned(),
                );
            };

            // Remove the field from the record
            let label = heap.intern_str(label);
            heap.records.remove(record, label);

            Value::Record(record)
        }
        (BuiltinId::RecordRename, Value::Record(record)) => {
            let Some(Value::Witness(wit)) = heap.get_record_value(record, "from") else {
                return MachineState::Error(
                    "record_rename requires 'from' field with a string".to_owned(),
                );
            };

            let TypeProtocol::Label(from) = heap.witnesses.get(wit).clone() else {
                return MachineState::Error(
                    "record_rename requires 'from' field with a string".to_owned(),
                );
            };

            let Some(Value::Witness(wit)) = heap.get_record_value(record, "to") else {
                return MachineState::Error(
                    "record_rename requires 'to' field with a string".to_owned(),
                );
            };

            let TypeProtocol::Label(to) = heap.witnesses.get(wit).clone() else {
                return MachineState::Error(
                    "record_rename requires 'to' field with a string".to_owned(),
                );
            };

            let Some(Value::Record(record)) = heap.get_record_value(record, "record") else {
                return MachineState::Error(
                    "record_rename requires 'record' field with a record".to_owned(),
                );
            };

            // Rename the field in the record
            let from = heap.intern_str(from);
            let to = heap.intern_str(to);
            if let Some((value, record)) = heap.records.remove(record, from) {
                heap.records.insert(record, to, value);
            } else {
                return MachineState::Error(format!("Field '{}' not found in record", from));
            }

            Value::Record(record)
        }
        (BuiltinId::RecordContains, Value::Record(record)) => {
            let Some(Value::Witness(wit)) = heap.get_record_value(record, "label") else {
                return MachineState::Error(
                    "record_contains requires 'label' field with a string".to_owned(),
                );
            };

            let TypeProtocol::Label(label) = heap.witnesses.get(wit).clone() else {
                return MachineState::Error(
                    "record_contains requires 'label' field with a string".to_owned(),
                );
            };

            let Some(Value::Record(record)) = heap.get_record_value(record, "record") else {
                return MachineState::Error(
                    "record_contains requires 'record' field with a record".to_owned(),
                );
            };

            let label = heap.intern_str(label);
            Value::Bool(heap.records.contains_key(record, label))
        }
        (BuiltinId::RecordKeys, Value::Record(record)) => Value::List(heap.record_keys(record)),
        (BuiltinId::RecordSize, Value::Record(record)) => {
            // Return the number of fields in the record
            Value::Num(heap.records.len(record) as f64)
        }
        (BuiltinId::RecordMergeLeft, Value::Record(record)) => {
            let Some(Value::Record(left)) = heap.get_record_value(record, "left") else {
                return MachineState::Error(
                    "record_merge_left requires 'left' field with a record".to_owned(),
                );
            };

            let Some(Value::Record(right)) = heap.get_record_value(record, "right") else {
                return MachineState::Error(
                    "record_merge_left requires 'right' field with a record".to_owned(),
                );
            };

            // Merge the two records, with left taking precedence
            let merged = heap.records.merge_left(left, right);

            Value::Record(merged)
        }
        (BuiltinId::RecordMergeRight, Value::Record(record)) => {
            let Some(Value::Record(left)) = heap.get_record_value(record, "left") else {
                return MachineState::Error(
                    "record_merge_left requires 'left' field with a record".to_owned(),
                );
            };

            let Some(Value::Record(right)) = heap.get_record_value(record, "right") else {
                return MachineState::Error(
                    "record_merge_left requires 'right' field with a record".to_owned(),
                );
            };

            // Merge the two records, with right taking precedence
            let merged = heap.records.merge_right(left, right);

            Value::Record(merged)
        }
        (BuiltinId::RecordRec, Value::Record(record)) => {
            let Some(Value::Record(record)) = heap.get_record_value(record, "record") else {
                return MachineState::Error(
                    "record_rec requires 'record' field with a record".to_owned(),
                );
            };

            let Some(base) = heap.get_record_value(record, "base") else {
                return MachineState::Error("record_rec requires 'base' field".to_owned());
            };

            let Some(Value::Closure(step)) = heap.get_record_value(record, "step") else {
                return MachineState::Error(
                    "record_rec requires 'step' field with a func".to_owned(),
                );
            };

            let Some((first, tail)) = heap.records.split_front(record) else {
                // Empty record — bind base directly, go to next
                let env = heap.envs.insert(env, bind, base);

                return MachineState::Standard(StandardConfig {
                    control: next.get(&context.ir),
                    env,
                    cont,
                });
            };

            let key = (
                heap.intern_str("key"),
                Value::Str(StringIdx::Static(first.0)),
            );
            let val = (heap.intern_str("value"), first.1);
            let head = heap.records.alloc(&[key, val]);

            // Create a pure continuation frame for the next expression
            let pure_frame = ContFrame::pure(bind, next, env);
            cont.push(pure_frame);

            // Create a primitive recursion frame for the list_rec operation,
            // which will handle the recursive processing of the list.
            let rec_frame = ContFrame::record_rec(tail, step);
            cont.push(rec_frame);

            let Closure {
                env,
                func: Func { param, body },
            } = step;

            // Create argument for the step function
            let acc = (heap.intern_str("acc"), base); // value = result from previous step
            let head = (heap.intern_str("head"), Value::Record(head));
            let record = heap.records.alloc(&[acc, head]);

            let env = heap.envs.insert(env, param, Value::Record(record));

            return MachineState::Standard(StandardConfig {
                control: body.get(&context.ir),
                env,
                cont,
            });
        }
        // TODO these serde methods do not enforce type annotations
        (BuiltinId::SerdeFromJson, Value::Record(record)) => {
            let Some(Value::Witness(wit)) = heap.get_record_value(record, "proto") else {
                return MachineState::Error(
                    "serde_from_json requires 'proto' field with a TypeRep".to_owned(),
                );
            };

            let Some(Value::Str(json)) = heap.get_record_value(record, "json") else {
                return MachineState::Error(
                    "serde_from_json requires 'json' field with a string".to_owned(),
                );
            };

            let proto = heap.witnesses.get(wit).clone();
            let json_result = heap.strings.flatten(json);
            let json_str = heap.strings.try_get(&json_result).unwrap().to_owned();

            match Value::from_json(&proto, &json_str, heap) {
                Ok(value) => Value::Variant(heap.alloc_builtin_variant("Ok", value)),
                Err(err) => {
                    let err = heap.strings.alloc(&err.to_string());
                    Value::Variant(heap.alloc_builtin_variant("Err", Value::Str(err)))
                }
            }
        }
        (BuiltinId::SerdeToJson, value) => match heap.to_json(&value) {
            Ok(json_str) => {
                let json_str = heap.strings.alloc(&json_str);
                Value::Variant(heap.alloc_builtin_variant("Ok", Value::Str(json_str)))
            }
            Err(err) => {
                let err = heap.strings.alloc(&err.to_string());
                Value::Variant(heap.alloc_builtin_variant("Err", Value::Str(err)))
            }
        },
        (BuiltinId::StrLength, Value::Str(s)) => Value::Num(heap.strings.len(s) as f64),
        (BuiltinId::StrIsEmpty, Value::Str(s)) => Value::Bool(heap.strings.is_empty(s)),
        (BuiltinId::StrReverse, Value::Str(s)) => Value::Str(heap.strings.reverse(s)),
        (BuiltinId::StrFirst, Value::Str(s)) => {
            if let Some(first) = heap.strings.first(s) {
                Value::Variant(heap.alloc_builtin_variant("Some", Value::Char(first)))
            } else {
                Value::Variant(heap.alloc_builtin_variant("None", Value::None))
            }
        }
        (BuiltinId::StrLast, Value::Str(s)) => {
            if let Some(last) = heap.strings.last(s) {
                Value::Variant(heap.alloc_builtin_variant("Some", Value::Char(last)))
            } else {
                Value::Variant(heap.alloc_builtin_variant("None", Value::None))
            }
        }
        (BuiltinId::StrContains, Value::Record(record)) => {
            let Some(Value::Str(mut s)) = heap.get_record_value(record, "str") else {
                return MachineState::Error(
                    "str_contains requires 'str' field with a string".to_owned(),
                );
            };

            let Some(Value::Char(c)) = heap.get_record_value(record, "value") else {
                return MachineState::Error(
                    "str_contains requires 'value' field with a char".to_owned(),
                );
            };

            let mut buf = [0; 4];
            let needle: &str = c.encode_utf8(&mut buf);

            let is_contained = heap.strings.contains_mut(&mut s, needle);

            // TODO the unflattened string is still in the record.

            Value::Bool(is_contained)
        }
        (BuiltinId::StrAt, Value::Record(record)) => {
            let Some(Value::Str(mut s)) = heap.get_record_value(record, "str") else {
                return MachineState::Error("str_at requires 'str' field with a string".to_owned());
            };

            let Some(Value::Num(index)) = heap.get_record_value(record, "index") else {
                return MachineState::Error(
                    "str_at requires 'index' field with a number".to_owned(),
                );
            };

            match to_usize_exact(index).and_then(|idx| heap.strings.at_mut(&mut s, idx)) {
                Some(c) => Value::Variant(heap.alloc_builtin_variant("Some", Value::Char(c))),
                None => Value::Variant(heap.alloc_builtin_variant("None", Value::None)),
            }

            // TODO the unflattened string is still in the record.
        }
        (BuiltinId::StrPrepend, Value::Record(record)) => {
            let Some(Value::Char(c)) = heap.get_record_value(record, "head") else {
                return MachineState::Error(
                    "str_prepend requires 'head' field with a char".to_owned(),
                );
            };

            let Some(Value::Str(s)) = heap.get_record_value(record, "tail") else {
                return MachineState::Error(
                    "str_prepend requires 'tail' field with a string".to_owned(),
                );
            };

            let mut buf = [0; 4];
            let c: &str = c.encode_utf8(&mut buf);

            Value::Str(heap.strings.push_front(s, c))
        }
        (BuiltinId::StrAppend, Value::Record(record)) => {
            let Some(Value::Str(s)) = heap.get_record_value(record, "head") else {
                return MachineState::Error(
                    "str_append requires 'head' field with a string".to_owned(),
                );
            };

            let Some(Value::Char(c)) = heap.get_record_value(record, "tail") else {
                return MachineState::Error(
                    "str_append requires 'tail' field with a char".to_owned(),
                );
            };

            let mut buf = [0; 4];
            let c: &str = c.encode_utf8(&mut buf);

            Value::Str(heap.strings.push_back(s, c))
        }
        (BuiltinId::StrConcat, Value::Record(record)) => {
            let Some(Value::Str(s1)) = heap.get_record_value(record, "head") else {
                return MachineState::Error(
                    "str_concat requires 'head' field with a string".to_owned(),
                );
            };

            let Some(Value::Str(s2)) = heap.get_record_value(record, "tail") else {
                return MachineState::Error(
                    "str_concat requires 'tail' field with a string".to_owned(),
                );
            };

            Value::Str(heap.strings.concat(s1, s2))
        }

        (BuiltinId::StrRec, Value::Record(record)) => {
            let Some(Value::Str(s)) = heap.get_record_value(record, "str") else {
                return MachineState::Error("str_rec requires 's' field with a string".to_owned());
            };

            let Some(base) = heap.get_record_value(record, "base") else {
                return MachineState::Error("str_rec requires 'base' field".to_owned());
            };

            let Some(Value::Closure(step)) = heap.get_record_value(record, "step") else {
                return MachineState::Error("str_rec requires 'step' field with a func".to_owned());
            };

            let Some((head, tail)) = heap.strings.pop_front(s) else {
                // Empty string — bind base directly, go to next
                let env = heap.envs.insert(env, bind, base);

                return MachineState::Standard(StandardConfig {
                    control: next.get(&context.ir),
                    env,
                    cont,
                });
            };

            // Create a pure continuation frame for the next expression
            let pure_frame = ContFrame::pure(bind, next, env);
            cont.push(pure_frame);

            // Create a primitive recursion frame for the list_rec operation,
            // which will handle the recursive processing of the list.
            let rec_frame = ContFrame::str_rec(tail, step);
            cont.push(rec_frame);

            let Closure {
                env,
                func: Func { param, body },
            } = step;

            // Create argument for the step function
            let acc = (heap.intern_str("acc"), base); // value = result from previous step
            let head = (heap.intern_str("head"), Value::Char(head));
            let record = heap.records.alloc(&[acc, head]);

            let env = heap.envs.insert(env, param, Value::Record(record));

            return MachineState::Standard(StandardConfig {
                control: body.get(&context.ir),
                env,
                cont,
            });
        }
        (_, value) => {
            return MachineState::Error(format!("Cannot apply {builtin} to: {:?}", value));
        }
    };

    // Create a new environment with the result bound to the variable
    let env = heap.envs.insert(env, bind, value);

    // Continue with the next expression
    MachineState::Standard(StandardConfig {
        control: next.get(&context.ir),
        env,
        cont,
    })
}
//
// M-HANDLE: ⟨handle M with H | γ | κ⟩ → ⟨M | γ | ([], (γ, H)) :: κ⟩
//
// This rule pushes a handler frame onto the continuation stack and
// continues evaluating the source computation M with the handler available.
impl Eval for HandleExpr {
    fn eval(
        &self,
        env: EnvIdx,
        mut cont: Cont,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
        let Self {
            bind,
            source,
            clause,
            next,
        } = *self;

        // Create a pure continuation frame for the ANF binding
        let pure_frame = ContFrame::pure(bind, next, env);

        // Create handler from clauses (H in the rule)
        let handler = RawHandler::from_clauses(clause, &context.ir);
        let handler_frame = ContFrame::handler(handler.alloc(heap), env);

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
    fn eval(
        &self,
        env: EnvIdx,
        mut cont: Cont,
        _context: &mut MachineContext,
        _heap: &mut Heap,
    ) -> MachineState {
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
    fn eval(
        &self,
        env: EnvIdx,
        mut cont: Cont,
        _context: &mut MachineContext,
        _heap: &mut Heap,
    ) -> MachineState {
        // Create a pure continuation frame
        let pure_frame = ContFrame::pure(self.bind, self.next, env);
        cont.push(pure_frame);

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
    fn eval(
        &self,
        env: EnvIdx,
        mut cont: Cont,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
        let Self {
            bind,
            predicate,
            then,
            or,
            next,
        } = *self;

        // Evaluate the predicate
        let pred_val = match eval_atom(predicate.get(&context.ir), env, heap) {
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
            control: branch.get(&context.ir),
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
    fn eval(
        &self,
        env: EnvIdx,
        cont: Cont,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
        let Self {
            bind,
            op,
            arg,
            next,
        } = *self;

        // Evaluate the operand
        let arg_val = match eval_atom(arg.get(&context.ir), env, heap) {
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
            control: next.get(&context.ir),
            env,
            cont,
        })
    }
}

pub fn eval_binary_op(
    op: BinaryOp,
    left: Value,
    right: Value,
    heap: &mut Heap,
) -> Result<Value, String> {
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
        (BinaryOp::Merge, Value::Record(l), Value::Record(r)) => heap
            .records
            .merge(l, r)
            .map(Value::Record)
            .ok_or("Cannot merge records with conflicting fields".to_owned()),
        (BinaryOp::Concat, Value::List(l), Value::List(r)) => {
            Ok(Value::List(heap.lists.concat(l, r)))
        }
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
    fn eval(
        &self,
        env: EnvIdx,
        cont: Cont,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
        let Self {
            bind,
            op,
            lhs,
            rhs,
            next,
        } = *self;

        // Evaluate the left operand
        let left_val = match eval_atom(lhs.get(&context.ir), env, heap) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Evaluate the right operand
        let right_val = match eval_atom(rhs.get(&context.ir), env, heap) {
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
            control: next.get(&context.ir),
            env,
            cont,
        })
    }
}

impl Eval for ListExpr {
    fn eval(
        &self,
        env: EnvIdx,
        cont: Cont,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
        let ListExpr {
            bind,
            head,
            tail: _,
            next,
        } = *self;

        let items = match context
            .ir
            .iter_items(head)
            .map(|item| eval_atom(item.value.get(&context.ir), env, heap))
            .collect::<Result<Vec<_>, _>>()
        {
            Ok(items) => items,
            Err(err) => return MachineState::Error(err),
        };

        let list = heap.lists.alloc(&items);
        let env = heap.envs.insert(env, bind, Value::List(list));

        MachineState::Standard(StandardConfig {
            control: next.get(&context.ir),
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
    fn eval(
        &self,
        env: EnvIdx,
        cont: Cont,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
        let RecordExpr { bind, head, next } = *self;

        let pairs = match context
            .ir
            .iter_fields(head)
            .map(|RecordField { label, value, .. }| {
                eval_atom(value.get(&context.ir), env, heap)
                    .map(|val| (label, val))
                    .map_err(|err| format!("Error evaluating field '{}': {}", label, err))
            })
            .collect::<Result<Vec<_>, _>>()
        {
            Ok(pairs) => pairs,
            Err(err) => return MachineState::Error(err),
        };

        // Create a record value from the evaluated fields
        let record = heap.records.alloc(&pairs);
        let record_value = Value::Record(record);

        // Bind the record to the variable in the environment
        let env = heap.envs.insert(env, bind, record_value);

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(&context.ir),
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
    fn eval(
        &self,
        env: EnvIdx,
        cont: Cont,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
        let Self {
            bind,
            base,
            path,
            value,
            next,
        } = *self;

        // Evaluate the base record
        let record_val = match eval_atom(base.get(&context.ir), env, heap) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Evaluate the value to extend with
        let extend_value = match eval_atom(value.get(&context.ir), env, heap) {
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
        let field_path: Vec<_> = context
            .ir
            .iter_path(Some(path))
            .map(|fp| fp.label)
            .collect();

        // Use Record's extend_at_path method
        let record = match heap
            .records
            .extend_at_path(record, &field_path, extend_value)
        {
            Ok(record) => record,
            Err(err) => return MachineState::Error(err),
        };

        // Bind the extended record to the variable in the environment
        let env = heap.envs.insert(env, bind, Value::Record(record));

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(&context.ir),
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
    fn eval(
        &self,
        env: EnvIdx,
        cont: Cont,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
        let Self {
            bind,
            base,
            path,
            next,
        } = *self;

        // Evaluate the base record
        let record_val = match eval_atom(base.get(&context.ir), env, heap) {
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
        let field_path: Vec<_> = context
            .ir
            .iter_path(Some(path))
            .map(|fp| fp.label)
            .collect();

        // Use Record's restrict_at_path method
        let record = match heap.records.restrict_at_path(record, &field_path) {
            Ok(record) => record,
            Err(err) => return MachineState::Error(err),
        };

        // Bind the restricted record to the variable in the environment
        let env = heap.envs.insert(env, bind, Value::Record(record));

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(&context.ir),
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
    fn eval(
        &self,
        env: EnvIdx,
        cont: Cont,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
        let Self {
            bind,
            base,
            path,
            op,
            value,
            next,
        } = *self;

        // Evaluate the base record
        let record_val = match eval_atom(base.get(&context.ir), env, heap) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Evaluate the value to update with
        let update_val = match eval_atom(value.get(&context.ir), env, heap) {
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
        let field_path: Vec<_> = context
            .ir
            .iter_path(Some(path))
            .map(|fp| fp.label)
            .collect();

        // Use Record's update_at_path method
        let update_fn = |cur: Value| eval_record_op(op, cur, update_val);
        let record = match heap.records.update_at_path(record, &field_path, update_fn) {
            Ok(record) => record,
            Err(err) => return MachineState::Error(err),
        };

        // Bind the updated record to the variable in the environment
        let env = heap.envs.insert(env, bind, Value::Record(record));

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control: next.get(&context.ir),
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
    fn eval(
        &self,
        env: EnvIdx,
        cont: Cont,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
        let Self {
            bind,
            base,
            label,
            next,
        } = *self;

        // Evaluate the record
        let record_val = match eval_atom(base.get(&context.ir), env, heap) {
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
            control: next.get(&context.ir),
            env,
            cont,
        })
    }
}
impl Eval for PatternMatchExpr {
    fn eval(
        &self,
        env: EnvIdx,
        mut cont: Cont,
        _context: &mut MachineContext,
        _heap: &mut Heap,
    ) -> MachineState {
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
    fn eval(
        &self,
        env: EnvIdx,
        cont: Cont,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
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

            PatternMatcher::Success(success) => {
                eval_pattern_success(success, env, cont, &context.ir)
            }
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
        Value::Str(s) => {
            // Get the string from the interner to compare
            let s = heap.strings.flatten(s);
            let expected_str = &heap.strings.interner[payload];

            if heap.strings.try_get(&s).unwrap() == expected_str {
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
    let Value::List(list) = source_val else {
        return MachineState::Error(format!("Expected a list, got: {:?}", source_val));
    };

    // Get the head of the list
    if let Some((h, t)) = heap.lists.split_front(list) {
        // Bind the head to the target variable
        env = heap.envs.insert(env, head, h);
        env = heap.envs.insert(env, tail_list, t);
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
    let Value::List(list) = source_val else {
        return MachineState::Error(format!("Expected a list, got: {:?}", source_val));
    };

    // Get the tail of the list
    if let Some((h, t)) = heap.lists.split_back(list) {
        // Bind the tail to the target variable
        env = heap.envs.insert(env, tail, t);
        env = heap.envs.insert(env, head_list, h);
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
    let Value::List(list) = source_val else {
        return MachineState::Error(format!("Expected a list, got: {:?}", source_val));
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
