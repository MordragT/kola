use crate::{
    config::{OperationConfig, StandardConfig},
    cont::{
        Continuation, ContinuationFrame, Handler, HandlerClosure, PureContinuation,
        PureContinuationFrame, ReturnClause,
    },
    env::Environment,
    value::Value,
};
use kola_ir::{
    id::InstrId,
    instr::{Atom, BinaryOp, Expr, Symbol, UnaryOp},
    ir::Ir,
};

/// CEK-style abstract machine for interpreting the language
pub struct CekMachine {
    /// The IR being interpreted
    pub ir: Ir,
    /// The current state of the machine
    pub state: MachineState,
}

impl CekMachine {
    /// Create a new CEK machine to evaluate an expression
    pub fn new(ir: Ir, expr: InstrId<Expr>) -> Self {
        // Initial configuration (M-INIT in the paper)
        // C = hM | ∅ | κ0i
        let config = StandardConfig {
            control: expr,
            env: Environment::empty(),
            continuation: Continuation::identity(),
        };

        Self {
            ir,
            state: MachineState::Standard(config),
        }
    }

    /// Execute a single step of the machine
    /// Returns Ok(true) if the machine has reached a final state,
    /// Ok(false) if more steps are needed, or Err if an error occurred
    pub fn step(&mut self) -> Result<bool, String> {
        match &self.state {
            MachineState::Standard(config) => self.step_standard(config.clone()),
            MachineState::Operation(config) => self.step_operation(config.clone()),
            MachineState::Value(_) => {
                // Machine has already terminated with a value
                Ok(true)
            }
            MachineState::Error(_) => {
                // Machine has already terminated with an error
                Ok(true)
            }
        }
    }

    /// Run the machine until it reaches a terminal state
    pub fn run(&mut self) -> Result<Value, String> {
        loop {
            let done = self.step()?;
            if done {
                break;
            }
        }

        match &self.state {
            MachineState::Value(value) => Ok(value.clone()),
            MachineState::Error(msg) => Err(msg.clone()),
            _ => Err("Machine stopped in a non-terminal state".to_string()),
        }
    }

    /// Execute a standard configuration step
    fn step_standard(&mut self, config: StandardConfig) -> Result<bool, String> {
        // Get the current expression
        let expr = self.ir.get(config.control).clone();

        match expr {
            // M-RET : Return with a value
            Expr::Ret { arg } => {
                // Get the value of the argument
                self.handle_return(arg, config.env)?;
                Ok(false)
            }

            // M-LET : Process a let binding
            // < let x ← M in N | γ | (σ, χ) :: κi > −→ < M | γ | ((γ, x, N) :: σ, χ) :: κi >
            Expr::Let { bind, value, next } => {
                // Create a pure continuation frame
                let frame = PureContinuationFrame {
                    var: bind,
                    body: next,
                    env: config.env.clone(),
                };

                // If the continuation has no frames, create one with identity handler
                if config.continuation.frames.is_empty() {
                    let identity_frame = ContinuationFrame {
                        pure: PureContinuation::empty(),
                        handler: HandlerClosure {
                            handler: Handler::identity(),
                            env: Environment::empty(),
                        },
                    };

                    let mut new_continuation = Continuation::empty();
                    new_continuation.push(identity_frame);
                    self.state = MachineState::Standard(StandardConfig {
                        control: value,
                        env: config.env,
                        continuation: new_continuation,
                    });
                } else {
                    // Push the frame onto the current pure continuation
                    let mut new_continuation = config.continuation;
                    if let Some(frame_ref) = new_continuation.frames.last_mut() {
                        frame_ref.pure.push(frame);
                    } else {
                        // This shouldn't happen due to the check above, but being safe
                        return Err("Continuation frames inconsistency".to_string());
                    }

                    // Evaluate the bound value
                    self.state = MachineState::Standard(StandardConfig {
                        control: value,
                        env: config.env,
                        continuation: new_continuation,
                    });
                }

                Ok(false)
            }

            // M-APP : Apply a function
            Expr::Call {
                bind,
                func,
                arg,
                next,
            } => {
                // Get the function and argument
                let func_val = self.evaluate_atom(func, &config.env)?;
                let arg_val = self.evaluate_atom(arg, &config.env)?;

                // Create a pure continuation frame for the next expression
                let frame = PureContinuationFrame {
                    var: bind,
                    body: next,
                    env: config.env.clone(),
                };

                // Add the frame to the continuation
                let mut new_continuation = config.continuation;
                if let Some(frame_ref) = new_continuation.frames.last_mut() {
                    frame_ref.pure.push(frame);
                } else {
                    // This shouldn't happen in a well-formed continuation
                    return Err("Continuation frames inconsistency".to_string());
                }

                // Apply the function to the argument
                match func_val {
                    Value::Function(func_env, func_id) => {
                        // Get the function definition
                        let func_atom = self.ir.get(func_id).clone();
                        match func_atom {
                            Atom::Func { param, body } => {
                                // Create a new environment with the bound parameter
                                let mut new_env = func_env.clone();
                                new_env.extend(param, arg);

                                // Evaluate the function body
                                self.state = MachineState::Standard(StandardConfig {
                                    control: body,
                                    env: new_env,
                                    continuation: new_continuation,
                                });

                                Ok(false)
                            }
                            _ => Err(format!("Expected function, got: {:?}", func_atom)),
                        }
                    }
                    Value::Continuation(captured_cont) => {
                        // Apply the continuation to the argument
                        self.state = MachineState::Standard(StandardConfig {
                            control: next, // Skip to next after applying
                            env: config.env,
                            continuation: captured_cont,
                        });

                        Ok(false)
                    }
                    _ => Err(format!("Cannot apply non-function value: {:?}", func_val)),
                }
            }

            // Other expression types...
            // TODO: Implement remaining expression handlers
            _ => Err(format!("Unsupported expression: {:?}", expr)),
        }
    }

    /// Execute an operation configuration step
    fn step_operation(&mut self, config: OperationConfig) -> Result<bool, String> {
        // Check for handler in current continuation
        if config.continuation.frames.is_empty() {
            // No handler found
            return Err(format!("Unhandled effect operation: {}", config.operation));
        }

        // Check if the top handler can handle this operation
        let frame = &config.continuation.frames[0];
        if let Some(handler_id) = frame.handler.handler.find_operation(&config.operation) {
            // M-OP-HANDLE: Handler found, apply it

            // Get the handler function
            let handler_fn = self.ir.get(handler_id).clone();

            // Apply the handler to the argument and the captured continuation
            match handler_fn {
                Atom::Func { param: _, body: _ } => {
                    // TODO: Implement handler application
                    // This is complex and requires creating a continuation value

                    Err("Handler application not implemented yet".to_string())
                }
                _ => Err(format!("Handler is not a function: {:?}", handler_fn)),
            }
        } else {
            // M-OP-FORWARD: Handler doesn't handle this operation, forward it

            // Create a new forwarding continuation by adding current frame to it
            let current_frame = config.continuation.frames[0].clone();
            let mut new_forwarding = config.forwarding;
            new_forwarding.push(current_frame);

            // Remove the top frame from the current continuation
            let mut new_continuation = config.continuation;
            new_continuation.frames.remove(0);

            // Forward the operation
            self.state = MachineState::Operation(OperationConfig {
                operation: config.operation,
                argument: config.argument,
                env: config.env,
                continuation: new_continuation,
                forwarding: new_forwarding,
            });

            Ok(false)
        }
    }

    /// Handle returning a value (M-RET)
    fn handle_return(&mut self, arg_id: InstrId<Atom>, env: Environment) -> Result<(), String> {
        // Evaluate the return value
        let value = self.evaluate_atom(arg_id, &env)?;

        // Get the continuation
        let continuation = match &self.state {
            MachineState::Standard(config) => &config.continuation,
            _ => return Err("Expected standard configuration".to_string()),
        };

        // If the continuation is empty, return the value
        if continuation.frames.is_empty() {
            self.state = MachineState::Value(value);
            return Ok(());
        }

        // Get the top continuation frame
        let frame = &continuation.frames[0];

        // If there's a pure continuation, apply it
        if !frame.pure.is_empty() {
            // M-RETCONT: Apply the pure continuation
            let pure_frame = frame.pure.frames[frame.pure.frames.len() - 1].clone();

            // Create new environment with the bound variable
            let mut new_env = pure_frame.env.clone();
            new_env.extend(pure_frame.var, arg_id);

            // Remove the pure frame from the continuation
            let mut new_continuation = continuation.clone();
            new_continuation.frames[0].pure.frames.pop();

            // Continue with the next expression
            self.state = MachineState::Standard(StandardConfig {
                control: pure_frame.body,
                env: new_env,
                continuation: new_continuation,
            });
        } else {
            // M-RETHANDLER: Apply the return clause of the handler
            let handler_closure = &frame.handler;

            match &handler_closure.handler.return_clause {
                ReturnClause::Identity => {
                    // Identity function just returns the value

                    // Remove the top continuation frame
                    let mut new_continuation = continuation.clone();
                    new_continuation.frames.remove(0);

                    if new_continuation.frames.is_empty() {
                        // No more frames, return the final value
                        self.state = MachineState::Value(value);
                    } else {
                        // Continue with the next frame
                        self.state = MachineState::Standard(StandardConfig {
                            control: arg_id,
                            env: env,
                            continuation: new_continuation,
                        });
                    }
                }
                ReturnClause::Function(func_id) => {
                    // Get the return handler function
                    let func_atom = self.ir.get(*func_id).clone();

                    match func_atom {
                        Atom::Func { param, body } => {
                            // Create a new environment with the value bound to param
                            let mut new_env = handler_closure.env.clone();
                            new_env.extend(param, arg_id);

                            // Remove the top continuation frame
                            let mut new_continuation = continuation.clone();
                            new_continuation.frames.remove(0);

                            // Evaluate the return handler body
                            self.state = MachineState::Standard(StandardConfig {
                                control: body,
                                env: new_env,
                                continuation: new_continuation,
                            });
                        }
                        _ => {
                            return Err(format!(
                                "Return clause is not a function: {:?}",
                                func_atom
                            ));
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Evaluate an atom to a value
    fn evaluate_atom(&self, atom_id: InstrId<Atom>, env: &Environment) -> Result<Value, String> {
        let atom = self.ir.get(atom_id).clone();

        match atom {
            Atom::Bool(b) => Ok(Value::Bool(b)),
            Atom::Char(c) => Ok(Value::Char(c)),
            Atom::Num(n) => Ok(Value::Number(n)),
            Atom::Str(s) => Ok(Value::String(s.to_string())), // You may need to convert StrKey to String
            Atom::Func { param, body } => {
                // Create a closure by capturing the current environment
                Ok(Value::Function(env.clone(), atom_id))
            }
            Atom::Symbol(s) => {
                // Look up the symbol in the environment
                if let Some(value_id) = env.lookup(&s) {
                    self.evaluate_atom(value_id, env)
                } else {
                    Err(format!("Unbound variable: {}", s))
                }
            }
        }
    }

    /// Get the result value if the machine has terminated
    pub fn get_value(&self) -> Option<&Value> {
        match &self.state {
            MachineState::Value(value) => Some(value),
            _ => None,
        }
    }
}
