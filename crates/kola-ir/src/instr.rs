use std::fmt;

use derive_more::{Display, From};
use kola_print::prelude::*;
use kola_utils::{impl_try_as, interner::StrKey};

use crate::{id::Id, ir::IrBuilder, print::IrPrinter};

// TODO Symbol scoping
#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(pub u32);

#[derive(Debug, From, Clone, PartialEq)]
pub enum Instr {
    Symbol(Symbol),
    Atom(Atom),
    Expr(Expr),
    RecordFields(Vec<RecordField>),
}

impl_try_as!(
    Instr,
    Symbol(Symbol),
    Atom(Atom),
    Expr(Expr),
    RecordFields(Vec<RecordField>)
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Func {
    pub param: Symbol,
    pub body: Id<Expr>,
}

impl Func {
    pub fn new(param: Symbol, body: impl Into<Expr>, builder: &mut IrBuilder) -> Self {
        let body = builder.add(body.into());
        Self { param, body }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, Func> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let Func { param, body } = self.node;

        let param = param.display_in(arena);
        let body = self.to(body).notate(arena);

        param.then(body.clone().flatten(arena).or(body, arena), arena)
    }
}

/// An expression is atomic if:
/// - it is guaranteed to terminate
/// - it causes no side effects
/// - it causes no control effects
/// - it never produces an error
#[derive(Debug, From, Clone, Copy, PartialEq)]
pub enum Atom {
    Bool(bool),
    Char(char),
    Num(f64),
    Str(StrKey),
    Func(Func),
    Symbol(Symbol),
}

impl_try_as!(
    Atom,
    Bool(bool),
    Char(char),
    Num(f64),
    Str(StrKey),
    Func(Func),
    Symbol(Symbol)
);

impl<'a> Notate<'a> for IrPrinter<'a, Id<Atom>> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.ir.instr(self.node) {
            Atom::Bool(b) => b.display_in(arena).enclose_by(arena.just('"'), arena),
            Atom::Char(c) => c.display_in(arena).enclose_by(arena.just('"'), arena),
            Atom::Num(n) => n.display_in(arena).enclose_by(arena.just('"'), arena),
            Atom::Str(s) => s.display_in(arena).enclose_by(arena.just('"'), arena),
            Atom::Func(f) => self.to(f).notate(arena),
            Atom::Symbol(s) => s.display_in(arena),
        }
    }
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // Comparison
    Less,
    Greater,
    LessEq,
    GreaterEq,
    // Logical
    And,
    Or,
    Xor,
    // Equality
    Eq,
    NotEq,
    // Record
    Merge,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RetExpr {
    pub arg: Id<Atom>,
}

impl RetExpr {
    pub fn new(arg: impl Into<Atom>, builder: &mut IrBuilder) -> Self {
        let arg = builder.add(arg.into());
        Self { arg }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, RetExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RetExpr { arg } = self.node;

        let arg = self.to(arg).notate(arena);
        let single = arena.just(' ').then(arg.clone().flatten(arena), arena);
        let multi = arena.newline().then(arg, arena);

        arena.notate("return").then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CallExpr {
    pub bind: Symbol,
    pub func: Id<Atom>,
    pub arg: Id<Atom>,
    pub next: Id<Expr>,
}

impl CallExpr {
    pub fn new(
        bind: Symbol,
        func: impl Into<Atom>,
        arg: impl Into<Atom>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let func = builder.add(func.into());
        let arg = builder.add(arg.into());
        let next = builder.add(next.into());

        Self {
            bind,
            func,
            arg,
            next,
        }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, CallExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let CallExpr {
            bind,
            func,
            arg,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let func = self.to(func).notate(arena);
        let arg = self.to(arg).notate(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            func.clone(),
            arena.just('('),
            arg.clone(),
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IfExpr {
    pub bind: Symbol,
    pub predicate: Id<Atom>,
    pub then: Id<Expr>,
    pub or: Id<Expr>,
    pub next: Id<Expr>,
}

impl IfExpr {
    pub fn new(
        bind: Symbol,
        predicate: impl Into<Atom>,
        then: impl Into<Expr>,
        or: impl Into<Expr>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let predicate = builder.add(predicate.into());
        let then = builder.add(then.into());
        let or = builder.add(or.into());
        let next = builder.add(next.into());

        Self {
            bind,
            predicate,
            then,
            or,
            next,
        }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, IfExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let IfExpr {
            bind,
            predicate,
            then,
            or,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let predicate = self.to(predicate).notate(arena);
        let then = self.to(then).notate(arena);
        let or = self.to(or).notate(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = if "),
            predicate.clone(),
            arena.notate(" then "),
            then.clone(),
            arena.notate(" else "),
            or.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= if "),
            predicate,
            arena.newline(),
            arena.notate("then "),
            then,
            arena.newline(),
            arena.notate("else "),
            or,
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LetExpr {
    pub bind: Symbol,
    pub value: Id<Atom>,
    pub next: Id<Expr>,
}

impl LetExpr {
    pub fn new(
        bind: Symbol,
        value: impl Into<Atom>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let value = builder.add(value.into());
        let next = builder.add(next.into());

        Self { bind, value, next }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, LetExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let LetExpr { bind, value, next } = self.node;

        let bind = bind.display_in(arena);
        let value = self.to(value).notate(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [bind.clone(), arena.notate(" = "), value.clone()]
            .concat_in(arena)
            .flatten(arena);

        let multi = [bind, arena.newline(), arena.notate("= "), value].concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LetInExpr {
    pub bind: Symbol,
    pub value: Id<Expr>,
    pub inside: Id<Expr>,
    pub next: Id<Expr>,
}

impl LetInExpr {
    pub fn new(
        bind: Symbol,
        value: impl Into<Expr>,
        inside: impl Into<Expr>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let value = builder.add(value.into());
        let inside = builder.add(inside.into());
        let next = builder.add(next.into());

        Self {
            bind,
            value,
            inside,
            next,
        }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, LetInExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let LetInExpr {
            bind,
            value,
            inside,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let value = self.to(value).notate(arena);
        let inside = self.to(inside).notate(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            value.clone(),
            arena.notate(" in "),
            inside.clone(),
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnaryExpr {
    pub bind: Symbol,
    pub op: UnaryOp,
    pub arg: Id<Atom>,
    pub next: Id<Expr>,
}

impl UnaryExpr {
    pub fn new(
        bind: Symbol,
        op: UnaryOp,
        arg: impl Into<Atom>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let arg = builder.add(arg.into());
        let next = builder.add(next.into());

        Self {
            bind,
            op,
            arg,
            next,
        }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, UnaryExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let UnaryExpr {
            bind,
            op,
            arg,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let op = op.display_in(arena);
        let arg = self.to(arg).notate(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [bind.clone(), arena.notate(" = "), op.clone(), arg.clone()]
            .concat_in(arena)
            .flatten(arena);

        let multi = [bind, arena.newline(), arena.notate("= "), op, arg].concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BinaryExpr {
    pub bind: Symbol,
    pub op: BinaryOp,
    pub lhs: Id<Atom>,
    pub rhs: Id<Atom>,
    pub next: Id<Expr>,
}

impl BinaryExpr {
    pub fn new(
        bind: Symbol,
        op: BinaryOp,
        lhs: impl Into<Atom>,
        rhs: impl Into<Atom>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let lhs = builder.add(lhs.into());
        let rhs = builder.add(rhs.into());
        let next = builder.add(next.into());

        Self {
            bind,
            op,
            lhs,
            rhs,
            next,
        }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, BinaryExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let BinaryExpr {
            bind,
            op,
            lhs,
            rhs,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let op = op.display_in(arena);
        let lhs = self.to(lhs).notate(arena);
        let rhs = self.to(rhs).notate(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            lhs.clone(),
            op.clone(),
            rhs.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [bind, arena.newline(), arena.notate("= "), lhs, op, rhs].concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordField {
    pub label: Symbol,
    pub value: Id<Atom>,
}

impl RecordField {
    pub fn new(label: Symbol, value: impl Into<Atom>, builder: &mut IrBuilder) -> Self {
        let value = builder.add(value.into());
        Self { label, value }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, RecordField> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordField { label, value } = self.node;

        let label = label.display_in(arena);
        let value = self.to(value).notate(arena);

        [label, arena.notate(" = "), value].concat_in(arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordExpr {
    pub bind: Symbol,
    pub fields: Id<Vec<RecordField>>,
    pub next: Id<Expr>,
}

impl RecordExpr {
    pub fn new(
        bind: Symbol,
        fields: impl Into<Vec<RecordField>>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let fields = builder.add(fields.into());
        let next = builder.add(next.into());

        Self { bind, fields, next }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, RecordExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordExpr { bind, fields, next } = self.node;

        let bind = bind.display_in(arena);
        let fields = fields
            .get(self.ir)
            .iter()
            .copied()
            .map(|field| self.to(field).notate(arena))
            .collect_in::<BumpVec<_>>(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            fields.clone().concat_in(arena).flatten(arena),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= "),
            fields.concat_in(arena),
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordExtendExpr {
    pub bind: Symbol,
    pub base: Id<Atom>,
    pub label: Symbol,
    pub value: Id<Atom>,
    pub next: Id<Expr>,
}

impl RecordExtendExpr {
    pub fn new(
        bind: Symbol,
        base: impl Into<Atom>,
        label: Symbol,
        value: impl Into<Atom>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let base = builder.add(base.into());
        let value = builder.add(value.into());
        let next = builder.add(next.into());

        Self {
            bind,
            base,
            label,
            value,
            next,
        }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, RecordExtendExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordExtendExpr {
            bind,
            base,
            label,
            value,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let base = self.to(base).notate(arena);
        let label = label.display_in(arena);
        let value = self.to(value).notate(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            base.clone(),
            arena.just('.'),
            label.clone(),
            arena.notate(" = "),
            value.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= +"),
            base,
            arena.just('.'),
            label,
            arena.newline(),
            arena.notate("= "),
            value,
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordRestrictExpr {
    pub bind: Symbol,
    pub base: Id<Atom>,
    pub label: Symbol,
    pub next: Id<Expr>,
}

impl RecordRestrictExpr {
    pub fn new(
        bind: Symbol,
        base: impl Into<Atom>,
        label: Symbol,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let base = builder.add(base.into());
        let next = builder.add(next.into());

        Self {
            bind,
            base,
            label,
            next,
        }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, RecordRestrictExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordRestrictExpr {
            bind,
            base,
            label,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let base = self.to(base).notate(arena);
        let label = label.display_in(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            base.clone(),
            arena.just('.'),
            label.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= -"),
            base,
            arena.just('.'),
            label,
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RecordUpdateOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
}

impl fmt::Display for RecordUpdateOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Assign => write!(f, "="),
            Self::AddAssign => write!(f, "+="),
            Self::SubAssign => write!(f, "-="),
            Self::MulAssign => write!(f, "*="),
            Self::DivAssign => write!(f, "/="),
            Self::RemAssign => write!(f, "%="),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordUpdateExpr {
    pub bind: Symbol,
    pub base: Id<Atom>,
    pub label: Symbol,
    pub op: RecordUpdateOp,
    pub value: Id<Atom>,
    pub next: Id<Expr>,
}

impl RecordUpdateExpr {
    pub fn new(
        bind: Symbol,
        base: impl Into<Atom>,
        label: Symbol,
        op: RecordUpdateOp,
        value: impl Into<Atom>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let base = builder.add(base.into());
        let value = builder.add(value.into());
        let next = builder.add(next.into());

        Self {
            bind,
            base,
            label,
            op,
            value,
            next,
        }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, RecordUpdateExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordUpdateExpr {
            bind,
            base,
            label,
            op,
            value,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let base = self.to(base).notate(arena);
        let label = label.display_in(arena);
        let op = op.display_in(arena);
        let value = self.to(value).notate(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            base.clone(),
            arena.just('.'),
            label.clone(),
            op.clone(),
            value.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= "),
            base,
            arena.just('.'),
            label,
            op,
            value,
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordAccessExpr {
    pub bind: Symbol,
    pub base: Id<Atom>,
    pub label: Symbol,
    pub next: Id<Expr>,
}

impl RecordAccessExpr {
    pub fn new(
        bind: Symbol,
        base: impl Into<Atom>,
        label: Symbol,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let base = builder.add(base.into());
        let next = builder.add(next.into());

        Self {
            bind,
            base,
            label,
            next,
        }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, RecordAccessExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordAccessExpr {
            bind,
            base,
            label,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let base = self.to(base).notate(arena);
        let label = label.display_in(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            base.clone(),
            arena.just('.'),
            label.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= "),
            base,
            arena.just('.'),
            label,
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

// essentially a linked list
/// An expression is complex if it is not atomic
/// Complex expressions must be in tail position
#[derive(Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr {
    Ret(RetExpr),
    Call(CallExpr),
    If(IfExpr),
    Let(LetExpr),
    LetIn(LetInExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Record(RecordExpr),
    RecordExtend(RecordExtendExpr),
    RecordRestrict(RecordRestrictExpr),
    RecordUpdate(RecordUpdateExpr),
    RecordAccess(RecordAccessExpr),
    // // Capture a continuation
    // Callcc {
    //     bind: Symbol,
    //     body: InstrId<Expr>,
    //     next: InstrId<Expr>,
    // },

    // // Jump with a value to a continuation
    // Jump {
    //     continuation: InstrId<Atom>,
    //     value: InstrId<Atom>,
    // },
}

impl_try_as!(
    Expr,
    Ret(RetExpr),
    Call(CallExpr),
    If(IfExpr),
    Let(LetExpr),
    LetIn(LetInExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr)
);

impl<'a> Notate<'a> for IrPrinter<'a, Id<Expr>> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.ir.instr(self.node) {
            Expr::Ret(expr) => self.to(expr).notate(arena),
            Expr::Call(expr) => self.to(expr).notate(arena),
            Expr::If(expr) => self.to(expr).notate(arena),
            Expr::Let(expr) => self.to(expr).notate(arena),
            Expr::LetIn(expr) => self.to(expr).notate(arena),
            Expr::Unary(expr) => self.to(expr).notate(arena),
            Expr::Binary(expr) => self.to(expr).notate(arena),
            Expr::Record(expr) => self.to(expr).notate(arena),
            Expr::RecordExtend(expr) => self.to(expr).notate(arena),
            Expr::RecordRestrict(expr) => self.to(expr).notate(arena),
            Expr::RecordUpdate(expr) => self.to(expr).notate(arena),
            Expr::RecordAccess(expr) => self.to(expr).notate(arena),
        }
    }
}
