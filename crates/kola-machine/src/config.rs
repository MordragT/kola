use derive_more::From;
use kola_ir::{
    id::Id,
    instr::{Atom, Expr, PatternMatcher},
};
use kola_runtime::{env::EnvIdx, value::Value};
use kola_utils::interner::StrKey;

use crate::cont::Cont;

/// The standard configuration in the CEK machine
/// C = M | γ | κi, where:
/// - M is the computation term (control)
/// - γ is the environment (binds free variables)
/// - κ is the continuation (what to do next)
#[derive(Debug, Clone, PartialEq)]
pub struct StandardConfig {
    /// M - The expression currently being evaluated
    pub control: Expr,
    /// γ - Environment binding free variables
    pub env: EnvIdx,
    /// κ - Continuation instructing what to do next
    pub cont: Cont,
}

const _: () = assert!(std::mem::size_of::<StandardConfig>() <= 56);

/// The operation handling configuration in the CEK machine
/// C = M | γ | κ | κ0iop, where:
/// - M is the operation being performed
/// - γ is the environment (binds free variables)
/// - κ is the continuation (what to do next)
/// - κ0 is the forwarding continuation (handlers already tried)
#[derive(Debug, Clone, PartialEq)]
pub struct OperationConfig {
    /// The operation name being performed
    pub op: StrKey,
    /// The argument to the operation
    pub arg: Id<Atom>,
    /// Environment for the operation
    pub env: EnvIdx,
    /// κ - Current continuation
    pub cont: Cont,
    /// κ0 - Forwarding continuation (handlers already tried)
    pub forward: Cont,
}

const _: () = assert!(std::mem::size_of::<OperationConfig>() <= 64);

/// Pattern matching configuration in the CEK machine
/// C = P | γ | κi, where:
/// - P is the pattern matcher instruction currently being evaluated
/// - γ is the environment (binds free variables)
/// - κ is the continuation (what to do next)
#[derive(Debug, Clone, PartialEq)]
pub struct PatternConfig {
    /// The pattern matcher instruction currently being evaluated
    pub matcher: Id<PatternMatcher>,
    /// Environment for pattern matching
    pub env: EnvIdx,
    /// Continuation to use after pattern matching completes
    pub cont: Cont,
}

const _: () = assert!(std::mem::size_of::<PatternConfig>() <= 32);

/// Machine states represent the different configurations of the CEK machine
#[derive(Debug, From, Clone, PartialEq)]
pub enum MachineState {
    /// Normal evaluation state M | γ | κi
    Standard(StandardConfig),
    /// Operation handling state M | γ | κ | κ0iop
    Operation(OperationConfig),
    /// Pattern matching state P | v | γ | κi
    Pattern(PatternConfig),
    /// The machine has produced a final value
    Value(Value),
    /// The machine has encountered an error
    Error(String),
    /// The machine is in the middle of an operation
    InProgress,
}

const _: () = assert!(std::mem::size_of::<MachineState>() <= 64);
