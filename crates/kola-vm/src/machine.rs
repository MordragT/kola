use camino::{Utf8Path, Utf8PathBuf};

use crate::{
    config::{MachineState, OperationConfig, PatternConfig, StandardConfig},
    cont::{Cont, ContFrame},
    env::EnvArena,
    eval::{Eval, eval_atom},
    heap::Heap,
    value::Value,
};
use kola_ir::{
    instr::Func,
    ir::{Ir, IrView},
};

#[derive(Debug, Clone)]
pub struct MachineContext {
    /// The IR being interpreted
    pub ir: Ir,
    /// The working directory for the machine
    pub working_dir: Utf8PathBuf,
}

impl MachineContext {
    pub fn new(ir: Ir, working_dir: impl Into<Utf8PathBuf>) -> Self {
        Self {
            ir,
            working_dir: working_dir.into(),
        }
    }

    pub fn start_config(&self) -> StandardConfig {
        let control = self.ir.root().get(&self.ir);
        let env = EnvArena::ambient();
        let cont = Cont::identity(env);

        StandardConfig { control, env, cont }
    }

    pub fn join_path(&self, path: impl AsRef<Utf8Path>) -> Utf8PathBuf {
        self.working_dir.join(path)
    }
}

/// CEK-style abstract machine for interpreting the language
#[derive(Debug, Clone)]
pub struct CekMachine {
    /// The current state of the machine
    pub state: MachineState,
    /// The context of the machine
    pub context: MachineContext,
}

impl CekMachine {
    /// Create a new CEK machine to evaluate an expression
    /// Initial configuration (M-INIT in the paper)
    /// C = hM | ∅ | κ0i
    pub fn new(context: MachineContext) -> Self {
        let config = context.start_config();

        Self {
            context,
            state: MachineState::Standard(config),
        }
    }

    /// Execute a single step of the machine
    /// Returns Ok(true) if the machine has reached a final state,
    /// Ok(false) if more steps are needed, or Err if an error occurred
    pub fn step(&mut self, heap: &mut Heap) -> bool {
        let state = std::mem::replace(&mut self.state, MachineState::InProgress);

        self.state = match state {
            MachineState::Standard(config) => Self::step_standard(config, &mut self.context, heap),
            MachineState::Operation(config) => {
                Self::step_operation(config, &mut self.context, heap)
            }
            MachineState::Pattern(config) => Self::step_pattern(config, &mut self.context, heap),
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
    pub fn run(&mut self, heap: &mut Heap) -> Result<Value, String> {
        while !self.step(heap) {}

        match &self.state {
            MachineState::Value(value) => Ok(value.clone()),
            MachineState::Error(msg) => Err(msg.clone()),
            _ => Err("Machine stopped in a non-terminal state".to_string()),
        }
    }

    /// Execute a standard configuration step
    fn step_standard(
        config: StandardConfig,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
        let StandardConfig { control, env, cont } = config;
        control.eval(env, cont, context, heap)
    }

    /// Execute a pattern matching configuration step
    fn step_pattern(
        config: PatternConfig,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
        let PatternConfig { matcher, env, cont } = config;
        matcher.get(&context.ir).eval(env, cont, context, heap)
    }

    /// Execute an operation configuration step
    fn step_operation(
        config: OperationConfig,
        context: &mut MachineContext,
        heap: &mut Heap,
    ) -> MachineState {
        let OperationConfig {
            op,
            arg,
            env,
            mut cont,
            mut forward,
        } = config;

        let Some(frame) = cont.pop() else {
            return MachineState::Error(format!(
                "Unhandled effect operation: {} ({})",
                heap.strings.interner[op], op,
            ));
        };

        if let ContFrame::Handler {
            handler,
            env: handler_env,
        } = frame
        {
            // Check if the top handler can handle this operation
            // And get the handler function
            if let Some(Func { param, body }) = handler.find(op, &context.ir) {
                // M-OP-HANDLE: Handler found, apply it
                // ⟨(do ℓ V)^E | γ | (σ, (γ', H)) :: κ | κ'⟩^op → ⟨M | γ'[x ↦ ⟦V⟧_γ, k ↦ (κ' ++ [(σ, (γ', H))])^B] | κ⟩

                // Evaluate the operation argument
                let arg_value = match eval_atom(context.ir.instr(arg), env, &heap.envs) {
                    Ok(value) => value,
                    Err(err) => return MachineState::Error(err),
                };

                // Create the handler environment γ'[x ↦ ⟦V⟧_γ]
                let handler_env = heap.envs.insert(handler_env, param, arg_value);

                // Info: Continuation parameters are currently not present in the syntax, consider:
                // handle some_computation | read arg k => (k "Hello from read") # k resumes the computation
                // vs.
                // handle some_computation | read arg => ("Hello from read") # implicitly resumes the computation

                // For now, we ignore the continuation parameter 'k' since the syntax doesn't expose it
                // In a full implementation, you would create a continuation value representing:
                // k ↦ (κ' ++ [(σ, (γ', H))])^B (the forwarding continuation plus current frame)

                // Create the captured continuation by combining forwarding + current frame
                forward.append(&mut cont);

                // Continue with the handler body (M in the rule) and remaining continuation (κ)
                return MachineState::Standard(StandardConfig {
                    control: context.ir.instr(body),
                    env: handler_env,
                    cont: forward,
                });
            } else {
                // M-OP-FORWARD: Handler doesn't handle this operation, forward it

                // Create a new forwarding continuation by adding current frame to it
                forward.push(ContFrame::handler(handler, handler_env));
            }
        } else {
            forward.push(frame);
        }

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
#[cfg(test)]
mod tests {
    use kola_ir::{
        instr::{
            Atom, BinaryExpr, BinaryOp, CallExpr, Func, LetExpr, PatternFailure, PatternSuccess,
            RecordAccessExpr, RecordExpr, RecordExtendExpr, RecordUpdateExpr, RecordUpdateOp,
            RetExpr, Symbol,
        },
        ir::IrBuilder,
    };
    use kola_utils::interner::StrInterner;

    use crate::{heap::Heap, machine::MachineContext};
    use crate::{machine::CekMachine, value::Value};
    use kola_protocol::TypeInterner;

    fn run_machine(context: MachineContext, heap: &mut Heap) -> Result<Value, String> {
        let mut machine = CekMachine::new(context);
        machine.run(heap)
    }

    #[test]
    fn test_simple_let_and_return() {
        // Create symbols
        let x_sym = Symbol::new(1);

        // let x = 42 in
        //   return x
        let mut ir = IrBuilder::new();

        // Using the new helper methods
        let x_ref = Atom::Symbol(x_sym);
        let ret_expr = RetExpr::new(x_ref, &mut ir);
        let let_expr = LetExpr::new(x_sym, Atom::Num(42.0), ret_expr, &mut ir);

        let root = ir.add(let_expr.into());

        let ir = ir.finish(root);

        let context = MachineContext::new(ir, "/mocked/path");
        let mut heap = Heap::new(StrInterner::default(), TypeInterner::new());
        let result = run_machine(context, &mut heap).unwrap();

        match result {
            Value::Num(n) => assert_eq!(n, 42.0),
            other => panic!("Expected Num(42), got {:?}", other),
        }
    }

    #[test]
    fn test_function_application() {
        // Create symbols
        let f_sym = Symbol::new(1);
        let x_sym = Symbol::new(2);
        let y_sym = Symbol::new(3);
        let t_sym = Symbol::new(4);

        // let f = (x => return x+1) in
        //   let y = f(5) in
        //     return y
        let mut ir = IrBuilder::new();

        // Return expressions
        let t_ref = Atom::Symbol(t_sym);
        let ret_t = RetExpr::new(t_ref, &mut ir);

        // Binary operation: t = x + 1
        let x_ref = Atom::Symbol(x_sym);
        let x_plus_1 = BinaryExpr::new(t_sym, BinaryOp::Add, x_ref, Atom::Num(1.0), ret_t, &mut ir);

        // Function: x => x + 1
        let func = Func::new(x_sym, x_plus_1, &mut ir);

        // Return y
        let y_ref = Atom::Symbol(y_sym);
        let ret_y = RetExpr::new(y_ref, &mut ir);

        // Call f(5): y = f(5)
        let f_ref = Atom::Symbol(f_sym);
        let call_f = CallExpr::new(y_sym, f_ref, Atom::Num(5.0), ret_y, &mut ir);

        // Let binding: let f = x => x + 1
        let let_f = LetExpr::new(f_sym, Atom::Func(func), call_f, &mut ir);

        let root = ir.add(let_f.into());

        let ir = ir.finish(root);

        // Run the machine
        let mut heap = Heap::new(StrInterner::default(), TypeInterner::new());
        let context = MachineContext::new(ir, "/mocked/path");
        let result = run_machine(context, &mut heap).unwrap();

        // Check the result
        match result {
            Value::Num(n) => assert_eq!(n, 6.0), // Should be 5+1
            other => panic!("Expected Num(6), got {:?}", other),
        }
    }

    #[test]
    fn test_record_creation_and_access() {
        let mut interner = StrInterner::default();

        // Create labels for record fields
        let x_label = interner.intern("x");
        let y_label = interner.intern("y");

        // Create symbols
        let r_sym = Symbol::new(1);
        let z_sym = Symbol::new(2);

        let mut ir = IrBuilder::new();

        // Create a record {x: 10, y: 20}
        // Then access the x field
        // And return its value

        // z = r.x
        let r_ref = Atom::Symbol(r_sym);
        let ret_z = RetExpr::new(Atom::Symbol(z_sym), &mut ir);
        let access_x = RecordAccessExpr::new(z_sym, r_ref, x_label, ret_z, &mut ir);

        // Create record fields
        let fields = [(x_label, Atom::Num(10.0)), (y_label, Atom::Num(20.0))];

        // r = {x: 10, y: 20}
        let mut record_expr = RecordExpr::new(r_sym, access_x, &mut ir);
        record_expr.extend(fields, &mut ir);

        let root = ir.add(record_expr.into());

        let ir = ir.finish(root);

        // Run the machine
        let mut heap = Heap::new(interner, TypeInterner::new());
        let context = MachineContext::new(ir, "/mocked/path");
        let result = run_machine(context, &mut heap).unwrap();

        // Check the result - should be the value of the x field (10)
        match result {
            Value::Num(n) => assert_eq!(n, 10.0),
            other => panic!("Expected Num(10), got {:?}", other),
        }
    }

    #[test]
    fn test_record_extension() {
        let mut interner = StrInterner::default();

        // Create labels for record fields
        let x_label = interner.intern("x");
        let y_label = interner.intern("y");
        let z_label = interner.intern("z");

        // Create symbols
        let r1_sym = Symbol::new(1);
        let r2_sym = Symbol::new(2);
        let result_sym = Symbol::new(3);

        let mut ir = IrBuilder::new();

        // Create a record {x: 10, y: 20} and extend it with {z: 30}
        // Then access the z field
        // And return its value

        // Return r2.z
        let r2_ref = Atom::Symbol(r2_sym);
        let ret_result = RetExpr::new(Atom::Symbol(result_sym), &mut ir);
        let access_z = RecordAccessExpr::new(result_sym, r2_ref, z_label, ret_result, &mut ir);

        // r2 = r1.{z = 30}
        let r1_ref = Atom::Symbol(r1_sym);
        let extend_r1 =
            RecordExtendExpr::new(r2_sym, r1_ref, z_label, Atom::Num(30.0), access_z, &mut ir);

        // Create record fields
        let fields = [(x_label, Atom::Num(10.0)), (y_label, Atom::Num(20.0))];

        // r1 = {x: 10, y: 20}
        let mut record_expr = RecordExpr::new(r1_sym, extend_r1, &mut ir);
        record_expr.extend(fields, &mut ir);

        let root = ir.add(record_expr.into());

        let ir = ir.finish(root);

        // Run the machine
        let mut heap = Heap::new(interner, TypeInterner::new());
        let context = MachineContext::new(ir, "/mocked/path");
        let result = run_machine(context, &mut heap).unwrap();

        // Check the result - should be the value of the z field (30)
        match result {
            Value::Num(n) => assert_eq!(n, 30.0),
            other => panic!("Expected Num(30), got {:?}", other),
        }
    }

    #[test]
    fn test_record_update() {
        let mut interner = StrInterner::default();

        // Create labels for record fields
        let x_label = interner.intern("x");

        // Create symbols for use in the IR
        let r1_sym = Symbol::new(1);
        let r2_sym = Symbol::new(2);
        let result_sym = Symbol::new(3);

        let mut ir = IrBuilder::new();

        // This test creates and tests the following program:
        // let r1 = {x: 10} in
        //   let r2 = { r1 | x = 20 } in  // Update the x field from 10 to 20
        //     let result = r2.x in        // Access the updated field
        //       return result             // Should return 20

        // Return expression with result after access
        let ret_result = RetExpr::new(Atom::Symbol(result_sym), &mut ir);

        // Access the x field from r2 record
        let r2_ref = Atom::Symbol(r2_sym);
        let access_x = RecordAccessExpr::new(result_sym, r2_ref, x_label, ret_result, &mut ir);

        // Update r1's x field to 20 and bind to r2
        let r1_ref = Atom::Symbol(r1_sym);
        let update_r1 = RecordUpdateExpr::new(
            r2_sym,
            r1_ref,
            x_label,
            RecordUpdateOp::Assign, // Simple assignment
            Atom::Num(20.0),
            access_x,
            &mut ir,
        );

        // Create initial record {x: 10} and bind to r1
        let fields = [(x_label, Atom::Num(10.0))];
        let mut record_expr = RecordExpr::new(r1_sym, update_r1, &mut ir);
        record_expr.extend(fields, &mut ir);

        // Add the expression to the IR and set as root
        let root = ir.add(record_expr.into());
        let ir = ir.finish(root);

        // Run the machine
        let mut heap = Heap::new(interner, TypeInterner::new());
        let context = MachineContext::new(ir, "/mocked/path");
        let result = run_machine(context, &mut heap).unwrap();

        // Check the result - should be the updated value of x (20)
        match result {
            Value::Num(n) => assert_eq!(n, 20.0),
            other => panic!("Expected Num(20), got {:?}", other),
        }
    }

    #[test]
    fn test_simple_pattern_matching() {
        use kola_ir::instr::{PatternMatchExpr, PatternMatcher};

        let interner = StrInterner::default();

        // Create symbols
        let source_sym = Symbol::new(1);
        let result_sym = Symbol::new(2);

        let mut ir = IrBuilder::new();

        // This test creates a simple pattern match:
        // let source = unit in
        //   match source with
        //   | unit -> return 42
        //   (basically: if source is unit, return 42)

        // Return 42
        let ret_42 = RetExpr::new(Atom::Num(42.0), &mut ir);
        let ret_42_expr = ir.add(ret_42.into());

        // Success pattern: bind result and continue to return 42
        let success = PatternMatcher::Success(PatternSuccess::new(ret_42_expr));

        // Extract identity (for wildcard/unit binding)
        let extract = PatternMatcher::identity(result_sym, source_sym, success, &mut ir);

        // Test if source is unit
        let failure = PatternMatcher::Failure(PatternFailure);
        let is_unit = PatternMatcher::is_unit(source_sym, extract, failure, &mut ir);

        // Pattern match expression
        let pattern_match = PatternMatchExpr::new(result_sym, is_unit, ret_42, &mut ir);

        // Let binding: source = unit
        let let_source = LetExpr::new(source_sym, Atom::Noop, pattern_match, &mut ir);

        let root = ir.add(let_source.into());
        let ir = ir.finish(root);

        // Run the machine
        let mut heap = Heap::new(interner, TypeInterner::new());
        let context = MachineContext::new(ir, "/mocked/path");
        let result = run_machine(context, &mut heap).unwrap();

        // Check the result - should be 42
        match result {
            Value::Num(n) => assert_eq!(n, 42.0),
            other => panic!("Expected Num(42), got {:?}", other),
        }
    }
}
