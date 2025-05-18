use crate::{
    config::{MachineState, StandardConfig},
    cont::{Cont, ContFrame, HandlerClosure, PureContFrame, ReturnClause},
    env::Env,
    value::Value,
};
use kola_ir::{
    instr::{
        Atom, BinaryExpr, BinaryOp, CallExpr, Expr, Func, IfExpr, LetExpr, LetInExpr, RetExpr,
        Symbol, UnaryExpr, UnaryOp,
    },
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

impl Eval for Expr {
    fn eval(&self, env: Env, cont: Cont, ir: &Ir) -> MachineState {
        match self {
            Expr::Ret(ret_expr) => ret_expr.eval(env, cont, ir),
            Expr::Let(let_expr) => let_expr.eval(env, cont, ir),
            Expr::LetIn(let_in_expr) => let_in_expr.eval(env, cont, ir),
            Expr::Call(call) => call.eval(env, cont, ir),
            Expr::If(if_expr) => if_expr.eval(env, cont, ir),
            Expr::Unary(unary_expr) => unary_expr.eval(env, cont, ir),
            Expr::Binary(binary_expr) => binary_expr.eval(env, cont, ir),
        }
    }
}

// pub fn eval_return_value

// M-RET : Return with a value
impl Eval for RetExpr {
    fn eval(&self, env: Env, mut cont: Cont, ir: &Ir) -> MachineState {
        // Evaluate the return machine state of value
        let value = match eval_atom(*self.arg.get(ir), &env) {
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
                    env.extend(param, value);

                    // Evaluate the return handler body
                    MachineState::Standard(StandardConfig {
                        control: *body.get(ir),
                        env,
                        cont,
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
        env.extend(var, value);

        // Continue with the next expression
        MachineState::Standard(StandardConfig {
            control: *body.get(ir),
            env,
            cont,
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

        let mut frame = cont.pop_or_identity();
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

// TODO maybe remove `LetInExpr` altogether and just use `LetExpr`
impl Eval for LetInExpr {
    fn eval(&self, env: Env, mut cont: Cont, ir: &Ir) -> MachineState {
        todo!()
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
    fn eval(&self, env: Env, mut cont: Cont, ir: &Ir) -> MachineState {
        let Self {
            bind,
            func,
            arg,
            next,
        } = *self;

        // Get the function and argument
        let func_val = match eval_atom(*func.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        let arg_val = match eval_atom(*arg.get(ir), &env) {
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
        let mut frame = cont.pop_or_identity();
        frame.pure.push(pure_frame);
        cont.push(frame);

        // Apply the function to the argument
        if let Value::Func(mut env, Func { param, body }) = func_val {
            // Create a new environment with the bound parameter
            env.extend(param, arg_val);

            // Evaluate the function body
            MachineState::Standard(StandardConfig {
                control: *body.get(ir),
                env,
                cont,
            })
        } else if let Value::Cont(mut captured) = func_val {
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

            captured.append(&mut cont);

            // Apply the captured continuation to the argument
            MachineState::Standard(StandardConfig {
                control: Expr::from(RetExpr { arg }),
                env,
                cont: captured,
            })
        } else {
            MachineState::Error(format!("Cannot apply non-function value: {:?}", func_val))
        }
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
        let pred_val = match eval_atom(*predicate.get(ir), &env) {
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
        let mut frame = cont.pop_or_identity();
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
            control: *branch.get(ir),
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
        let arg_val = match eval_atom(*arg.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Apply the unary operation
        let result = match eval_unary_op(op, arg_val) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Create a new environment with the result bound to the variable
        env.extend(bind, result);

        // Continue directly with the next expression
        MachineState::Standard(StandardConfig {
            control: *next.get(ir),
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
        (BinaryOp::Xor, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l ^ r)),
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
        let left_val = match eval_atom(*lhs.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Evaluate the right operand
        let right_val = match eval_atom(*rhs.get(ir), &env) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Apply the binary operation
        let result = match eval_binary_op(op, left_val, right_val) {
            Ok(value) => value,
            Err(err) => return MachineState::Error(err),
        };

        // Create a new environment with the result bound to the variable
        env.extend(bind, result);

        // Continue directly with the next expression
        MachineState::Standard(StandardConfig {
            control: *next.get(ir),
            env,
            cont,
        })
    }
}
