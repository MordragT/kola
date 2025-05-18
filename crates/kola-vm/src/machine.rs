use crate::{
    config::{MachineState, OperationConfig, StandardConfig},
    cont::Cont,
    env::Env,
    eval::Eval,
    value::Value,
};
use kola_ir::{instr::Func, ir::Ir};

/// CEK-style abstract machine for interpreting the language
#[derive(Debug, Clone)]
pub struct CekMachine {
    /// The IR being interpreted
    pub ir: Ir,
    /// The current state of the machine
    pub state: MachineState,
}

impl CekMachine {
    /// Create a new CEK machine to evaluate an expression
    pub fn new(ir: Ir) -> Self {
        // Initial configuration (M-INIT in the paper)
        // C = hM | ∅ | κ0i
        let config = StandardConfig {
            control: *ir.root().get(&ir),
            env: Env::empty(),
            cont: Cont::identity(),
        };

        Self {
            ir,
            state: MachineState::Standard(config),
        }
    }

    /// Execute a single step of the machine
    /// Returns Ok(true) if the machine has reached a final state,
    /// Ok(false) if more steps are needed, or Err if an error occurred
    pub fn step(&mut self) -> bool {
        dbg!(&self.state);

        let state = std::mem::replace(&mut self.state, MachineState::InProgress);

        self.state = match state {
            MachineState::Standard(config) => Self::step_standard(config, &self.ir),
            MachineState::Operation(config) => Self::step_operation(config, &self.ir),
            MachineState::Value(_) | MachineState::Error(_) => {
                // If the machine is in a terminal state, we don't need to do anything
                state
            }
            MachineState::InProgress => {
                // This should never happen
                panic!("Machine is in an invalid state: {:?}", state)
            }
        };

        matches!(&self.state, MachineState::Error(_) | MachineState::Value(_))
    }

    /// Run the machine until it reaches a terminal state
    pub fn run(&mut self) -> Result<Value, String> {
        while !self.step() {}

        match &self.state {
            MachineState::Value(value) => Ok(value.clone()),
            MachineState::Error(msg) => Err(msg.clone()),
            _ => Err("Machine stopped in a non-terminal state".to_string()),
        }
    }

    /// Execute a standard configuration step
    fn step_standard(config: StandardConfig, ir: &Ir) -> MachineState {
        let StandardConfig { control, env, cont } = config;
        control.eval(env, cont, ir)
    }

    /// Execute an operation configuration step
    fn step_operation(config: OperationConfig, ir: &Ir) -> MachineState {
        let OperationConfig {
            op,
            arg,
            env,
            mut cont,
            mut forward,
        } = config;

        // Check for handler in current continuation
        let [frame, ..] = cont.frames.as_slice() else {
            // No handler found
            return MachineState::Error(format!("Unhandled effect operation: {}", op,));
        };

        // Check if the top handler can handle this operation
        // And get the handler function
        if let Some(Func { param, body }) = frame.handler_closure.handler.find_operation(&op) {
            // M-OP-HANDLE: Handler found, apply it

            // Apply the handler to the argument and the captured continuation

            // TODO: Implement handler application
            // This is complex and requires creating a continuation value

            todo!()
        } else {
            // M-OP-FORWARD: Handler doesn't handle this operation, forward it

            // Create a new forwarding continuation by adding current frame to it
            let current_frame = frame.clone(); // TODO this clone is not needed
            forward.push(current_frame);

            // Remove the top frame from the current continuation
            cont.frames.remove(0);

            // Forward the operation
            MachineState::Operation(OperationConfig {
                op,
                arg,
                env,
                cont,
                forward,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use kola_ir::{
        instr::{Atom, CallExpr, Expr, Func, LetExpr, RetExpr, Symbol},
        ir::IrBuilder,
    };
    use kola_utils::interner::StrInterner;

    use crate::{machine::CekMachine, value::Value};

    #[test]
    fn test_simple_let_and_return() {
        let mut interner = StrInterner::new();
        let x = interner.intern("x");

        // Create symbols
        let x_sym = Symbol(x);

        // let x = 42 in
        //   return x
        let mut ir = IrBuilder::new();

        let num_42 = ir.add(Atom::Num(42.0));
        let x_ref = ir.add(Atom::Symbol(x_sym));
        let ret_expr = ir.add(Expr::Ret(RetExpr { arg: x_ref.into() }));
        let let_expr = ir.add(Expr::Let(LetExpr {
            bind: x_sym,
            value: num_42.into(),
            next: ret_expr.into(),
        }));

        let ir = ir.finish(let_expr);

        let mut machine = CekMachine::new(ir);
        let result = machine.run().unwrap();

        match result {
            Value::Num(n) => assert_eq!(n, 42.0),
            other => panic!("Expected Num(42), got {:?}", other),
        }
    }

    #[test]
    fn test_function_application() {
        let mut interner = StrInterner::new();
        let f = interner.intern("f");
        let x = interner.intern("x");
        let y = interner.intern("y");

        // Create symbols
        let f_sym = Symbol(f);
        let x_sym = Symbol(x);
        let y_sym = Symbol(y);

        // let f = (λx. return x+1) in
        //   let y = f(5) in
        //     return y
        let mut ir = IrBuilder::new();

        // Create constants
        let num_1 = ir.add(Atom::Num(1.0));
        let num_5 = ir.add(Atom::Num(5.0));

        // Create x reference
        let x_ref = ir.add(Atom::Symbol(x_sym));

        // Create x+1 operation (we don't have Binary in the test code, so we'll fake it)
        // In real code you'd use binary addition
        let x_plus_1 = ir.add(Atom::Num(6.0)); // Pretend this is x+1

        // Create return x+1
        let ret_x_plus_1 = ir.add(Expr::Ret(RetExpr {
            arg: x_plus_1.into(),
        }));

        // Create function: λx. return x+1
        let func = ir.add(Atom::Func(Func {
            param: x_sym,
            body: ret_x_plus_1.into(),
        }));

        // Create f reference
        let f_ref = ir.add(Atom::Symbol(f_sym));

        // Create y reference
        let y_ref = ir.add(Atom::Symbol(y_sym));

        // Create return y
        let ret_y = ir.add(Expr::Ret(RetExpr { arg: y_ref.into() }));

        // Create y = f(5)
        let call_f = ir.add(Expr::Call(CallExpr {
            bind: y_sym,
            func: f_ref.into(),
            arg: num_5.into(),
            next: ret_y.into(),
        }));

        // Create let f = function in ...
        let let_f = ir.add(Expr::Let(LetExpr {
            bind: f_sym,
            value: func.into(),
            next: call_f.into(),
        }));

        let ir = ir.finish(let_f);

        dbg!(&ir);

        // Run the machine
        let mut machine = CekMachine::new(ir);
        let result = machine.run().unwrap();

        // Check the result
        match result {
            Value::Num(n) => assert_eq!(n, 6.0), // Should be 5+1
            other => panic!("Expected Num(6), got {:?}", other),
        }
    }
}
