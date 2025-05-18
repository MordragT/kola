use crate::{
    config::{MachineState, OperationConfig, StandardConfig},
    cont::{Cont, ContFrame},
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
            env: Env::new(),
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

        // // Check for handler in current continuation
        // let Some(ContFrame { pure, handler_closure }) = cont.pop() else {
        //     // No handler found
        //     return MachineState::Error(format!("Unhandled effect operation: {}", op,));
        // };

        // let Some(Func { param, body }) = handler_closure.handler.find_operation(&op)

        // // Check if the top handler can handle this operation
        // // And get the handler function
        // if let Some(Func { param, body }) = frame.handler_closure.handler.find_operation(&op) {
        //     // M-OP-HANDLE: Handler found, apply it

        //     // Apply the handler to the argument and the captured continuation

        //     // TODO: Implement handler application
        //     // This is complex and requires creating a continuation value

        //     todo!()
        // } else {
        //     // M-OP-FORWARD: Handler doesn't handle this operation, forward it

        //     // Create a new forwarding continuation by adding current frame to it
        //     let current_frame = frame.clone(); // TODO this clone is not needed
        //     forward.push(current_frame);

        //     // Remove the top frame from the current continuation
        //     cont.frames.remove(0);

        //     // Forward the operation
        //     MachineState::Operation(OperationConfig {
        //         op,
        //         arg,
        //         env,
        //         cont,
        //         forward,
        //     })
        // }
        //
        todo!()
    }
}
#[cfg(test)]
mod tests {
    use kola_ir::{
        instr::{
            Atom, BinaryExpr, BinaryOp, CallExpr, Expr, Func, LetExpr, RecordAccessExpr,
            RecordExpr, RecordExtendExpr, RecordField, RecordUpdateExpr, RecordUpdateOp, RetExpr,
            Symbol,
        },
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

        // Using the new helper methods
        let x_ref = Atom::Symbol(x_sym);
        let ret_expr = RetExpr::new(x_ref, &mut ir);
        let let_expr = LetExpr::new(x_sym, Atom::Num(42.0), ret_expr, &mut ir);

        let root = ir.add(let_expr.into());

        let ir = ir.finish(root);

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
        let t = interner.intern("t");

        // Create symbols
        let f_sym = Symbol(f);
        let x_sym = Symbol(x);
        let y_sym = Symbol(y);
        let t_sym = Symbol(t);

        // let f = (λx. return x+1) in
        //   let y = f(5) in
        //     return y
        let mut ir = IrBuilder::new();

        // Return expressions
        let t_ref = Atom::Symbol(t_sym);
        let ret_t = RetExpr::new(t_ref, &mut ir);

        // Binary operation: t = x + 1
        let x_ref = Atom::Symbol(x_sym);
        let x_plus_1 = BinaryExpr::new(t_sym, BinaryOp::Add, x_ref, Atom::Num(1.0), ret_t, &mut ir);

        // Function: λx. x + 1
        let func = Func::new(x_sym, x_plus_1, &mut ir);

        // Return y
        let y_ref = Atom::Symbol(y_sym);
        let ret_y = RetExpr::new(y_ref, &mut ir);

        // Call f(5): y = f(5)
        let f_ref = Atom::Symbol(f_sym);
        let call_f = CallExpr::new(y_sym, f_ref, Atom::Num(5.0), ret_y, &mut ir);

        // Let binding: let f = λx. x + 1
        let let_f = LetExpr::new(f_sym, Atom::Func(func), call_f, &mut ir);

        let root = ir.add(let_f.into());

        let ir = ir.finish(root);

        // Run the machine
        let mut machine = CekMachine::new(ir);
        let result = machine.run().unwrap();

        // Check the result
        match result {
            Value::Num(n) => assert_eq!(n, 6.0), // Should be 5+1
            other => panic!("Expected Num(6), got {:?}", other),
        }
    }

    #[test]
    fn test_record_creation_and_access() {
        let mut interner = StrInterner::new();
        // Intern symbols
        let r = interner.intern("r");
        let x = interner.intern("x");
        let y = interner.intern("y");
        let z = interner.intern("z");

        // Create symbols
        let r_sym = Symbol(r);
        let x_sym = Symbol(x);
        let y_sym = Symbol(y);
        let z_sym = Symbol(z);

        let mut ir = IrBuilder::new();

        // Create a record {x: 10, y: 20}
        // Then access the x field
        // And return its value

        // z = r.x
        let r_ref = Atom::Symbol(r_sym);
        let ret_z = RetExpr::new(Atom::Symbol(z_sym), &mut ir);
        let access_x = RecordAccessExpr::new(z_sym, r_ref, x_sym, ret_z, &mut ir);

        // Create record fields
        let fields = vec![
            RecordField::new(x_sym, Atom::Num(10.0), &mut ir),
            RecordField::new(y_sym, Atom::Num(20.0), &mut ir),
        ];

        // r = {x: 10, y: 20}
        let record_expr = RecordExpr::new(r_sym, fields, access_x, &mut ir);

        let root = ir.add(record_expr.into());

        let ir = ir.finish(root);

        // Run the machine
        let mut machine = CekMachine::new(ir);
        let result = machine.run().unwrap();

        // Check the result - should be the value of the x field (10)
        match result {
            Value::Num(n) => assert_eq!(n, 10.0),
            other => panic!("Expected Num(10), got {:?}", other),
        }
    }

    #[test]
    fn test_record_extension() {
        let mut interner = StrInterner::new();
        // Intern symbols
        let r1 = interner.intern("r1");
        let r2 = interner.intern("r2");
        let x = interner.intern("x");
        let y = interner.intern("y");
        let z = interner.intern("z");

        // Create symbols
        let r1_sym = Symbol(r1);
        let r2_sym = Symbol(r2);
        let x_sym = Symbol(x);
        let y_sym = Symbol(y);
        let z_sym = Symbol(z);

        let mut ir = IrBuilder::new();

        // Create a record {x: 10} and extend it with {y: 20}
        // Then access the y field
        // And return its value

        // Return r2.y
        let r2_ref = Atom::Symbol(r2_sym);
        let ret_y = RetExpr::new(Atom::Symbol(y_sym), &mut ir);
        let access_y = RecordAccessExpr::new(y_sym, r2_ref, y_sym, ret_y, &mut ir);

        // r2 = r1.{z = 30}
        let r1_ref = Atom::Symbol(r1_sym);
        let extend_r1 =
            RecordExtendExpr::new(r2_sym, r1_ref, z_sym, Atom::Num(30.0), access_y, &mut ir);

        // Create record fields
        let fields = vec![
            RecordField::new(x_sym, Atom::Num(10.0), &mut ir),
            RecordField::new(y_sym, Atom::Num(20.0), &mut ir),
        ];

        // r1 = {x: 10, y: 20}
        let record_expr = RecordExpr::new(r1_sym, fields, extend_r1, &mut ir);

        let root = ir.add(record_expr.into());

        let ir = ir.finish(root);

        // Run the machine
        let mut machine = CekMachine::new(ir);
        let result = machine.run().unwrap();

        // Check the result - should be the value of the y field (20)
        match result {
            Value::Num(n) => assert_eq!(n, 20.0),
            other => panic!("Expected Num(20), got {:?}", other),
        }
    }

    #[test]
    fn test_record_update() {
        let mut interner = StrInterner::new();
        // Intern symbols for variables and fields
        let r1 = interner.intern("r1");
        let r2 = interner.intern("r2");
        let x = interner.intern("x");

        // Create symbols for use in the IR
        let r1_sym = Symbol(r1);
        let r2_sym = Symbol(r2);
        let x_sym = Symbol(x);

        let mut ir = IrBuilder::new();

        // This test creates and tests the following program:
        // let r1 = {x: 10} in
        //   let r2 = r1.{x = 20} in  // Update the x field from 10 to 20
        //     let x = r2.x in        // Access the updated field
        //       return x             // Should return 20

        // Return expression with x after access
        let ret_x = RetExpr::new(Atom::Symbol(x_sym), &mut ir);

        // Access the x field from r2 record
        let r2_ref = Atom::Symbol(r2_sym);
        let access_x = RecordAccessExpr::new(x_sym, r2_ref, x_sym, ret_x, &mut ir);

        // Update r1's x field to 20 and bind to r2
        let r1_ref = Atom::Symbol(r1_sym);
        let update_r1 = RecordUpdateExpr::new(
            r2_sym,
            r1_ref,
            x_sym,
            RecordUpdateOp::Assign, // Simple assignment
            Atom::Num(20.0),
            access_x,
            &mut ir,
        );

        // Create initial record {x: 10} and bind to r1
        let fields = vec![RecordField::new(x_sym, Atom::Num(10.0), &mut ir)];
        let record_expr = RecordExpr::new(r1_sym, fields, update_r1, &mut ir);

        // Add the expression to the IR and set as root
        let root = ir.add(record_expr.into());
        let ir = ir.finish(root);

        // Run the machine
        let mut machine = CekMachine::new(ir);
        let result = machine.run().unwrap();

        // Check the result - should be the updated value of x (20)
        match result {
            Value::Num(n) => assert_eq!(n, 20.0),
            other => panic!("Expected Num(20), got {:?}", other),
        }
    }
}
