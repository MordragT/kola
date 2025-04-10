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

use core::fmt;
use kola_print::prelude::*;
use std::marker::PhantomData;

pub use kola_tree::node;

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

    pub fn finish(self, root: InstrId<Expr>) -> Ir {
        let Self { instructions } = self;
        Ir { instructions, root }
    }
}

#[derive(Debug, Clone)]
pub struct Ir {
    instructions: Vec<Instr>,
    root: InstrId<Expr>,
}

impl Printable<()> for Ir {
    fn notate<'a>(&'a self, with: &'a (), arena: &'a Bump) -> Notation<'a> {
        self.root.notate(self, arena)
    }
}

impl Ir {
    fn get<T>(&self, id: InstrId<T>) -> &T
    where
        T: InnerInstr,
    {
        let instr = &self.instructions[id.as_usize()];
        T::to_inner_ref(instr).unwrap()
    }
}

pub trait InnerInstr: Into<Instr> {
    fn to_inner_ref(instr: &Instr) -> Option<&Self>;
}

#[derive(Debug, Clone)]
pub enum Instr {
    Symbol(Symbol),
    Atom(Atom),
    Expr(Expr),
}

impl From<Symbol> for Instr {
    fn from(value: Symbol) -> Self {
        Self::Symbol(value)
    }
}

impl From<Atom> for Instr {
    fn from(value: Atom) -> Self {
        Self::Atom(value)
    }
}

impl From<Expr> for Instr {
    fn from(value: Expr) -> Self {
        Self::Expr(value)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InstrId<T> {
    id: u32,
    t: PhantomData<T>,
}

impl<T> Clone for InstrId<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            t: PhantomData,
        }
    }
}

impl<T> Copy for InstrId<T> {}

impl<T> Printable<Ir> for InstrId<T>
where
    T: Printable<Ir> + InnerInstr,
{
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        with.get(*self).notate(with, arena)
    }
}

impl<T> InstrId<T> {
    pub fn as_usize(&self) -> usize {
        self.id as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(pub u32);

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.0)
    }
}

/// An expression is atomic if:
/// - it is guaranteed to terminate
/// - it causes no side effects
/// - it causes no control effects
/// - it never produces an error
#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Bool(bool),
    Char(char),
    Num(f64),
    Str(node::Symbol),
    Func { param: Symbol, body: InstrId<Expr> },
    Symbol(Symbol),
}

impl InnerInstr for Atom {
    fn to_inner_ref(instr: &Instr) -> Option<&Self> {
        match instr {
            Instr::Atom(a) => Some(a),
            _ => None,
        }
    }
}

impl Printable<Ir> for Atom {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        match self {
            Self::Bool(b) => b.display_in(arena).enclose_by(arena.just('"'), arena),
            Self::Char(c) => c.display_in(arena).enclose_by(arena.just('"'), arena),
            Self::Num(n) => n.display_in(arena).enclose_by(arena.just('"'), arena),
            Self::Str(s) => s.display_in(arena).enclose_by(arena.just('"'), arena),
            Self::Func { param, body } => {
                let param = param.display_in(arena);
                let body = body.notate(with, arena);

                param.then(body.clone().flatten(arena).or(body, arena), arena)
            }
            Self::Symbol(s) => s.display_in(arena),
        }
    }
}

impl From<node::Literal> for Atom {
    fn from(value: node::Literal) -> Self {
        match value {
            node::Literal::Bool(b) => Self::Bool(b),
            node::Literal::Char(c) => Self::Char(c),
            node::Literal::Num(n) => Self::Num(n),
            node::Literal::Str(s) => Self::Str(s),
        }
    }
}

impl From<Symbol> for Atom {
    fn from(value: Symbol) -> Self {
        Self::Symbol(value)
    }
}

// essentially a linked list
/// An expression is complex if it is not atomic
/// Complex expressions must be in tail position
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ret {
        arg: InstrId<Atom>,
    },
    Call {
        bind: Symbol,
        func: InstrId<Atom>,
        arg: InstrId<Atom>,
        next: InstrId<Expr>,
    },
    If {
        bind: Symbol,
        predicate: InstrId<Atom>,
        then: InstrId<Expr>,
        or: InstrId<Expr>,
        next: InstrId<Expr>,
    },
    Let {
        bind: Symbol,
        value: InstrId<Atom>,
        next: InstrId<Expr>,
    },
    LetIn {
        bind: Symbol,
        value: InstrId<Expr>,
        inside: InstrId<Expr>,
        next: InstrId<Expr>,
    },
    Unary {
        bind: Symbol,
        op: node::UnaryOp,
        arg: InstrId<Atom>,
        next: InstrId<Expr>,
    },
    Binary {
        bind: Symbol,
        op: node::BinaryOp,
        lhs: InstrId<Atom>,
        rhs: InstrId<Atom>,
        next: InstrId<Expr>,
    },
}

impl InnerInstr for Expr {
    fn to_inner_ref(instr: &Instr) -> Option<&Self> {
        match instr {
            Instr::Expr(e) => Some(e),
            _ => None,
        }
    }
}

impl Printable<Ir> for Expr {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        match self {
            Self::Ret { arg } => {
                let arg = arg.notate(with, arena);
                let single = arena.just(' ').then(arg.clone().flatten(arena), arena);
                let multi = arena.newline().then(arg, arena);

                arena.notate("return").then(single.or(multi, arena), arena)
            }
            Self::Call {
                bind,
                func,
                arg,
                next,
            } => {
                let bind = bind.display_in(arena);
                let func = func.notate(with, arena);
                let arg = arg.notate(with, arena);
                let next = arena.newline().then(next.notate(with, arena), arena);

                let single = [
                    bind.clone().flatten(arena),
                    arena.notate(" = "),
                    func.clone().flatten(arena),
                    arena.just('('),
                    arg.clone().flatten(arena),
                    arena.just(')'),
                ]
                .concat_in(arena)
                .flatten(arena);

                let multi = [
                    bind,
                    arena.newline(),
                    arena.notate("= "),
                    func,
                    arena.just('('),
                    arg,
                    arena.just(')'),
                ]
                .concat_in(arena);

                single.or(multi, arena).then(next, arena)
            }
            Self::If {
                bind,
                predicate,
                then,
                or,
                next,
            } => todo!(),
            Self::Let { bind, value, next } => {
                let bind = bind.display_in(arena);
                let value = value.notate(with, arena);
                let next = arena.newline().then(next.notate(with, arena), arena);

                let single = [
                    bind.clone().flatten(arena),
                    arena.notate(" = "),
                    value.clone().flatten(arena),
                ]
                .concat_in(arena)
                .flatten(arena);

                let multi = [bind, arena.newline(), arena.notate("= "), value].concat_in(arena);

                single.or(multi, arena).then(next, arena)
            }
            Self::LetIn {
                bind,
                value,
                inside,
                next,
            } => {
                let bind = bind.display_in(arena);
                let value = value.notate(with, arena);
                let inside = inside.notate(with, arena);
                let next = arena.newline().then(next.notate(with, arena), arena);

                let single = [
                    bind.clone().flatten(arena),
                    arena.notate(" = "),
                    value.clone().flatten(arena),
                    arena.notate(" in "),
                    inside.clone().flatten(arena),
                ]
                .concat_in(arena)
                .flatten(arena);

                let multi = [
                    bind,
                    arena.newline(),
                    arena.notate("= "),
                    value,
                    arena.newline(),
                    arena.notate("in "),
                    inside,
                ]
                .concat_in(arena);

                single.or(multi, arena).then(next, arena)
            }
            Self::Unary {
                bind,
                op,
                arg,
                next,
            } => todo!(),
            Self::Binary {
                bind,
                op,
                lhs,
                rhs,
                next,
            } => todo!(),
        }
    }
}
