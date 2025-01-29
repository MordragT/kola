// cps : continuation passing style
// ssa
// https://www.reddit.com/r/ProgrammingLanguages/comments/1ejyr0u/why_dont_we_use_continuation_passing_style_as/

/*
https://en.wikipedia.org/wiki/Continuation-passing_style
The key to CPS is to remember that
(a) every function takes an extra argument known as its continuation,
and (b) every argument in a function call must be either a variable
or a lambda expression (not a more complex expression).
This has the effect of turning expressions "inside-out"
because the innermost parts of the expression must be evaluated first,
thus CPS makes explicit the order of evaluation as well as the control flow.

https://overreacted.io/algebraic-effects-for-the-rest-of-us/
https://gist.github.com/yelouafi/57825fdd223e5337ba0cd2b6ed757f53
https://en.m.wikipedia.org/wiki/Delimited_continuation

https://en.wikipedia.org/wiki/A-normal_form
https://matt.might.net/articles/a-normalization/
*/

use std::marker::PhantomData;

pub use kola_tree::node::Literal;

#[derive(Debug, Clone)]
pub struct IrBuilder {
    instructions: Vec<Instr>,
}

impl IrBuilder {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    // TODO return InstrId ?
    pub fn push<T>(&mut self, instr: T) -> InstrId<T>
    where
        T: Into<Instr>,
    {
        let id = self.instructions.len() as u32;

        let instr = instr.into();
        self.instructions.push(instr);

        InstrId { id, t: PhantomData }
    }
}

#[derive(Debug, Clone)]
pub struct Ir {
    instructions: Vec<Instr>,
}

#[derive(Debug, Clone)]
pub enum Instr {
    Symbol(Symbol),
    Literal(Literal),
    Func(Func),
    Atomic(Atomic),
    Call(Call),
    If(If),
    Complex(Complex),
}

impl From<Symbol> for Instr {
    fn from(value: Symbol) -> Self {
        Self::Symbol(value)
    }
}

impl From<Literal> for Instr {
    fn from(value: Literal) -> Self {
        Self::Literal(value)
    }
}

impl From<Func> for Instr {
    fn from(value: Func) -> Self {
        Self::Func(value)
    }
}

impl From<Atomic> for Instr {
    fn from(value: Atomic) -> Self {
        Self::Atomic(value)
    }
}

impl From<Call> for Instr {
    fn from(value: Call) -> Self {
        Self::Call(value)
    }
}

impl From<If> for Instr {
    fn from(value: If) -> Self {
        Self::If(value)
    }
}

impl From<Complex> for Instr {
    fn from(value: Complex) -> Self {
        Self::Complex(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InstrId<T> {
    id: u32,
    t: PhantomData<T>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub param: InstrId<Symbol>,
    pub body: InstrId<Expr>,
}

/// An expression is atomic if:
/// - it is guaranteed to terminate
/// - it causes no side effects
/// - it causes no control effects
/// - it never produces an error
#[derive(Debug, Clone, PartialEq)]
pub enum Atomic {
    Literal(InstrId<Literal>),
    Symbol(InstrId<Symbol>),
    Func(InstrId<Func>),
    // Unary
    // Binary (atleast for some)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub func: InstrId<Atomic>,
    pub arg: InstrId<Atomic>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    predicate: InstrId<Atomic>,
    then: InstrId<Expr>,
    or: InstrId<Expr>,
}

/// An expression is complex if it is not atomic
/// Complex expressions must be in tail position
#[derive(Debug, Clone, PartialEq)]
pub enum Complex {
    Call(InstrId<Call>),
    If(InstrId<If>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub name: InstrId<Symbol>,
    pub value: InstrId<Complex>,
    pub inside: InstrId<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Atomic(InstrId<Atomic>),
    Complex(InstrId<Complex>),
    Let(InstrId<Let>),
}
