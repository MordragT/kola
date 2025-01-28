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

use std::ops::Deref;

use crate::syntax::Span;

pub use kola_tree::{Literal, Symbol};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Node<T> {
    pub inner: Box<T>,
    pub span: Span,
}

impl<T> Deref for Node<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> Node<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Self {
            inner: Box::new(inner),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub param: Symbol,
    pub body: Expr,
}

impl Func {
    pub fn new(param: Symbol, body: Expr) -> Self {
        Self { param, body }
    }
}

/// An expression is atomic if:
/// - it is guaranteed to terminate
/// - it causes no side effects
/// - it causes no control effects
/// - it never produces an error
#[derive(Debug, Clone, PartialEq)]
pub enum Atomic {
    Literal(Node<Literal>),
    Ident(Node<Symbol>),
    Func(Node<Func>),
    // Unary
    // Binary (atleast for some)
}

impl Atomic {
    pub fn literal(l: Literal, span: Span) -> Self {
        Self::Literal(Node::new(l, span))
    }

    pub fn ident(i: Symbol, span: Span) -> Self {
        Self::Ident(Node::new(i, span))
    }

    pub fn func(param: Symbol, body: Expr, span: Span) -> Self {
        let f = Func::new(param, body);
        Self::Func(Node::new(f, span))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub func: Atomic,
    pub arg: Atomic,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    predicate: Atomic,
    then: Expr,
    or: Expr,
}

/// An expression is complex if it is not atomic
/// Complex expressions must be in tail position
#[derive(Debug, Clone, PartialEq)]
pub enum Complex {
    Call(Node<Call>),
    If(Node<If>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub name: Symbol,
    pub value: Complex,
    pub inside: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Atomic(Atomic),
    Complex(Complex),
    Let(Node<Let>),
}

impl From<Atomic> for Expr {
    fn from(value: Atomic) -> Self {
        Self::Atomic(value)
    }
}

impl From<Complex> for Expr {
    fn from(value: Complex) -> Self {
        Self::Complex(value)
    }
}

impl From<Node<Let>> for Expr {
    fn from(value: Node<Let>) -> Self {
        Self::Let(value)
    }
}
