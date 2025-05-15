use crate::{cont::Cont, env::Env, value::Value};
use derive_more::From;
use kola_ir::{
    id::InstrId,
    instr::{Atom, Expr, Symbol},
};

/// The standard configuration in the CEK machine
/// C = hM | γ | κi, where:
/// - M is the computation term (control)
/// - γ is the environment (binds free variables)
/// - κ is the continuation (what to do next)
#[derive(Debug, Clone, PartialEq)]
pub struct StandardConfig {
    /// M - The expression currently being evaluated
    pub control: InstrId<Expr>,
    /// γ - Environment binding free variables
    pub env: Env,
    /// κ - Continuation instructing what to do next
    pub continuation: Cont,
}

/// The operation handling configuration in the CEK machine
/// C = hM | γ | κ | κ0iop, where:
/// - M is the operation being performed
/// - γ is the environment (binds free variables)
/// - κ is the continuation (what to do next)
/// - κ0 is the forwarding continuation (handlers already tried)
#[derive(Debug, Clone, PartialEq)]
pub struct OperationConfig {
    /// The operation name being performed
    pub operation: Symbol,
    /// The argument to the operation
    pub argument: InstrId<Atom>,
    /// Environment for the operation
    pub env: Env,
    /// κ - Current continuation
    pub continuation: Cont,
    /// κ0 - Forwarding continuation (handlers already tried)
    pub forwarding: Cont,
}

/// Machine states represent the different configurations of the CEK machine
#[derive(Debug, From, Clone, PartialEq)]
pub enum MachineState {
    /// Normal evaluation state hM | γ | κi
    Standard(StandardConfig),
    /// Operation handling state hM | γ | κ | κ0iop
    Operation(OperationConfig),
    /// The machine has produced a final value
    Value(Value),
    /// The machine has encountered an error
    Error(String),
}
