use crate::{
    config::{MachineState, StandardConfig},
    cont::{Cont, ContFrame, Handler, HandlerClosure, PureCont, PureContFrame, ReturnClause},
    env::Env,
    value::Value,
};
use kola_ir::{
    instr::{Atom, CallExpr, Expr, Func, Instr, LetExpr, RetExpr, Symbol},
    ir::Ir,
};

pub fn eval_symbol(symbol: Symbol, env: &Env) -> Result<Value, String> {
    // Look up the symbol in the environment
    env.lookup(&symbol)
        .cloned()
        .ok_or(format!("Unbound variable: {}", symbol))
}

pub fn eval_atom(atom: Atom, env: &Env) -> Result<Value, String> {
    match atom {
        Atom::Bool(b) => Ok(Value::Bool(b)),
        Atom::Char(c) => Ok(Value::Char(c)),
        Atom::Num(n) => Ok(Value::Num(n)),
        Atom::Str(s) => todo!(),
        Atom::Func(f) => {
            // Create a closure by capturing the current environment
            Ok(Value::Func(env.clone(), f))
        }
        Atom::Symbol(s) => eval_symbol(s, env),
    }
}

pub trait Eval {
    fn eval(&self, env: Env, cont: Cont, ir: &Ir) -> MachineState;
}

impl Eval for Instr {
    fn eval(&self, env: Env, cont: Cont, ir: &Ir) -> MachineState {
        match self {
            Instr::Expr(expr) => expr.eval(env, cont, ir),
            Instr::Atom(atom) => match eval_atom(*atom, &env) {
                Ok(value) => MachineState::Value(value),
                Err(err) => MachineState::Error(err),
            },
            Instr::Symbol(symbol) => match eval_symbol(*symbol, &env) {
                Ok(value) => MachineState::Value(value),
                Err(err) => MachineState::Error(err),
            },
        }
    }
}

impl Eval for Expr {
    fn eval(&self, env: Env, cont: Cont, ir: &Ir) -> MachineState {
        match self {
            Expr::Call(call) => call.eval(env, cont, ir),
            Expr::Let(let_expr) => let_expr.eval(env, cont, ir),
            Expr::Ret(ret_expr) => ret_expr.eval(env, cont, ir),
            _ => todo!(),
        }
    }
}

// M-RET : Return with a value
impl Eval for RetExpr {
    fn eval(&self, env: Env, mut cont: Cont, ir: &Ir) -> MachineState {
        // Evaluate the return machine state of value
        let value = match eval_atom(*self.arg.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Get the top continuation frame
        let [frame, ..] = cont.frames.as_slice() else {
            // If the continuation is empty, return the value
            return MachineState::Value(value);
        };

        // If there's a pure continuation, apply it
        let [.., pure_frame] = frame.pure.frames.as_slice() else {
            // M-RETHANDLER: Apply the return clause of the handler
            let handler_closure = &frame.handler;

            return match &handler_closure.handler.return_clause {
                ReturnClause::Identity => {
                    // Identity function just returns the value

                    // Remove the top continuation frame
                    cont.frames.remove(0);

                    if cont.frames.is_empty() {
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
                    let mut new_env = handler_closure.env.clone();
                    new_env.extend(*param, value);

                    let control = *body.get(ir);

                    // Remove the top continuation frame
                    cont.frames.remove(0);

                    // Evaluate the return handler body
                    MachineState::Standard(StandardConfig {
                        control,
                        env: new_env,
                        cont,
                    })
                }
            };
        };

        // M-RETCONT: Apply the pure continuation

        let control = *pure_frame.body.get(ir);

        // Create new environment with the bound variable
        let mut new_env = pure_frame.env.clone();
        new_env.extend(pure_frame.var, value);

        // Remove the pure frame from the continuation
        cont.frames[0].pure.frames.pop();

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control,
            env: new_env,
            cont,
        })
    }
}

// M-LET : Process a let binding
// < let x ← M in N | γ | (σ, χ) :: κ > −→ < M | γ | ((γ, x, N) :: σ, χ) :: κi >
impl Eval for LetExpr {
    fn eval(&self, env: Env, mut cont: Cont, ir: &Ir) -> MachineState {
        // Create a pure continuation frame
        let pure_frame = PureContFrame {
            var: self.bind,
            body: self.next,
            env: env.clone(),
        };

        let [.., frame] = cont.frames.as_mut_slice() else {
            // If the continuation has no frames, create one with identity handler
            let identity_frame = ContFrame {
                pure: PureCont::empty(),
                handler: HandlerClosure {
                    handler: Handler::identity(),
                    env: Env::empty(),
                },
            };

            cont.push(identity_frame);

            // TODO does the pure_frame need to be pushed here?

            return MachineState::Standard(StandardConfig {
                control: Expr::from(RetExpr { arg: self.value }),
                env,
                cont,
            });
        };

        frame.pure.push(pure_frame);

        // Evaluate the bound value
        MachineState::Standard(StandardConfig {
            control: Expr::from(RetExpr { arg: self.value }),
            env,
            cont,
        })
    }
}

// M-APP : Apply a function
// V W | γ | κ => M | γ'[x -> [| W |] γ ] | κ
impl Eval for CallExpr {
    fn eval(&self, env: Env, mut cont: Cont, ir: &Ir) -> MachineState {
        // Get the function and argument
        let func_val = match eval_atom(*self.func.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        let arg_val = match eval_atom(*self.arg.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Create a pure continuation frame for the next expression
        let frame = PureContFrame {
            var: self.bind,
            body: self.next,
            env: env.clone(),
        };

        // Add the frame to the continuation
        if let Some(frame_ref) = cont.frames.last_mut() {
            frame_ref.pure.push(frame);
        } else {
            // This shouldn't happen in a well-formed continuation
            return MachineState::Error("Continuation frames inconsistency".to_string());
        }

        // Apply the function to the argument
        if let Value::Func(func_env, Func { param, body }) = func_val {
            // Create a new environment with the bound parameter
            let mut new_env = func_env.clone();
            new_env.extend(param, arg_val);

            // Evaluate the function body
            MachineState::Standard(StandardConfig {
                control: *body.get(ir),
                env: new_env,
                cont,
            })
        } else if let Value::Cont(captured_cont) = func_val {
            // Apply the continuation to the argument
            MachineState::Standard(StandardConfig {
                control: *self.next.get(ir), // Skip to next after applying
                env,
                cont: captured_cont,
            })
        } else {
            MachineState::Error(format!("Cannot apply non-function value: {:?}", func_val))
        }
    }
}
