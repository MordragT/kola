use derive_more::{Display, From};
use std::fmt;

use kola_print::prelude::*;
use kola_utils::{impl_try_as, interner::StrKey};

use super::{Atom, PatternMatcher, Symbol};
use crate::{
    id::Id,
    ir::{IrBuilder, IrView},
    print::IrPrinter,
};

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Rem => "%",
            Self::Less => "<",
            Self::Greater => ">",
            Self::LessEq => "<=",
            Self::GreaterEq => ">=",
            Self::And => "and",
            Self::Or => "or",
            Self::Xor => "xor",
            Self::Eq => "==",
            Self::NotEq => "!=",
            Self::Merge => "&",
        };
        write!(f, "{}", op.blue())
    }
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

// return <arg>;
// return
//    <arg>;
impl<'a> Notate<'a> for IrPrinter<'a, RetExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RetExpr { arg } = self.node;

        let arg = self.to(arg).notate(arena);
        let single = arena.just(' ').then(arg.clone().flatten(arena), arena);
        let multi = arena.newline().then(arg, arena).indent(arena);

        "return"
            .red()
            .display_in(arena)
            .then(single.or(multi, arena), arena)
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

// <bind> = (<func> <arg>);
// <bind> =
//     ( <func>
//     <arg> );
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
            arena.notate(" = ("),
            func.clone().flatten(arena),
            arena.just(' '),
            arg.clone().flatten(arena),
            arena.just(')'),
        ]
        .concat_in(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= ("),
            func,
            arena.newline(),
            arg,
            arena.just(')'),
        ]
        .concat_in(arena)
        .indent(arena);

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

// <bind> = if <predicate> then <then> else <or>;
// <bind> =
//     if <predicate>
//     then <then>
//     else <or>;
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
            predicate.clone().flatten(arena),
            arena.notate(" then "),
            then.clone().flatten(arena),
            arena.notate(" else "),
            or.clone().flatten(arena),
        ]
        .concat_in(arena);

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
        .concat_in(arena)
        .indent(arena);

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

// <bind> = <value>;
// <bind> =
//    <value>;
impl<'a> Notate<'a> for IrPrinter<'a, LetExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let LetExpr { bind, value, next } = self.node;

        let bind = bind.display_in(arena);
        let value = self.to(value).notate(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            value.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [bind, arena.newline(), arena.notate("= "), value]
            .concat_in(arena)
            .indent(arena);

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

// <bind> = <op> <arg>;
// <bind> =
//      <op>
//      <arg>;
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

        let single = [
            bind.clone(),
            arena.notate(" = "),
            op.clone(),
            arena.just(' '),
            arg.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= "),
            op,
            arena.newline(),
            arg,
        ]
        .concat_in(arena)
        .indent(arena);

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

// <bind> = <lhs> <op> <rhs>;
// <bind> =
//      <lhs>
//      <op>
//      <rhs>;
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
            lhs.clone().flatten(arena),
            arena.just(' '),
            op.clone(),
            arena.just(' '),
            rhs.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= "),
            lhs,
            arena.newline(),
            op,
            arena.newline(),
            rhs,
        ]
        .concat_in(arena)
        .indent(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ListItem {
    pub value: Id<Atom>,
    pub next: Option<Id<ListItem>>,
}

impl ListItem {
    pub fn new(value: impl Into<Atom>, builder: &mut IrBuilder) -> Self {
        let value = builder.add(value.into());
        Self { value, next: None }
    }

    pub fn with_next(value: impl Into<Atom>, next: Id<ListItem>, builder: &mut IrBuilder) -> Self {
        let value = builder.add(value.into());
        Self {
            value,
            next: Some(next),
        }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, ListItem> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ListItem { value, next, .. } = self.node;

        let value = self.to(value).notate(arena);
        let next = next
            .map(|next| {
                let next = self.to(self.ir.instr(next)).notate(arena);

                let single = ", "
                    .display_in(arena)
                    .then(next.clone(), arena)
                    .flatten(arena);
                let multi = [arena.newline(), arena.just(','), next]
                    .concat_in(arena)
                    .indent(arena);

                single.or(multi, arena)
            })
            .or_not(arena);

        [value, next].concat_in(arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ListExpr {
    pub bind: Symbol,
    pub head: Option<Id<ListItem>>,
    pub tail: Option<Id<ListItem>>,
    pub next: Id<Expr>,
}

impl ListExpr {
    pub fn new(bind: Symbol, next: impl Into<Expr>, builder: &mut IrBuilder) -> Self {
        let next = builder.add(next.into());

        Self {
            bind,
            head: None,
            tail: None,
            next,
        }
    }

    pub fn prepend(&mut self, item: impl Into<Atom>, builder: &mut IrBuilder) {
        let item_id = builder.prepend_item(item, self.head);

        if self.tail.is_none() {
            self.tail = Some(item_id);
        }
        self.head = Some(item_id);
    }

    pub fn append(&mut self, item: impl Into<Atom>, builder: &mut IrBuilder) {
        let item_id = builder.append_item(item, self.tail);

        if self.head.is_none() {
            self.head = Some(item_id);
        }
        self.tail = Some(item_id);
    }

    pub fn prepend_all<I>(&mut self, items: I, builder: &mut IrBuilder)
    where
        I: IntoIterator<Item = Atom>,
    {
        let mut items = items.into_iter();

        if let Some(first) = items.next() {
            self.prepend(first, builder);
        } else {
            return;
        }

        let head_id = builder.prepend_all_items(items, self.head);
        self.head = head_id;
    }
}

// <bind> = [ <item>, ... ];
// <bind> =
//      [ <item>
//      , ...
//      , <item> ];
impl<'a> Notate<'a> for IrPrinter<'a, ListExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ListExpr {
            bind, head, next, ..
        } = self.node;

        let bind = bind.display_in(arena);
        let items = head
            .map(|head| self.to(self.ir.instr(head)).notate(arena))
            .or_not(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = [ "),
            items.clone().flatten(arena),
            arena.notate(" ]"),
        ]
        .concat_in(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= ["),
            arena.newline(),
            items.indent(arena),
            arena.newline(),
            arena.just(']'),
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

// #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct VariantExpr {
//     pub bind: Symbol,
//     pub tag: StrKey,
//     pub value: Id<Atom>,
//     pub next: Id<Expr>,
// }

// impl VariantExpr {
//     pub fn new(
//         bind: Symbol,
//         tag: StrKey,
//         value: impl Into<Atom>,
//         next: impl Into<Expr>,
//         builder: &mut IrBuilder,
//     ) -> Self {
//         let value = builder.add(value.into());
//         let next = builder.add(next.into());

//         Self {
//             bind,
//             tag,
//             value,
//             next,
//         }
//     }
// }

// // <bind> = <tag> <value>;
// // <bind> =
// //        <tag>
// //        <value>;
// impl<'a> Notate<'a> for IrPrinter<'a, VariantExpr> {
//     fn notate(&self, arena: &'a Bump) -> Notation<'a> {
//         let VariantExpr {
//             bind,
//             tag,
//             value,
//             next,
//         } = self.node;

//         let bind = bind.display_in(arena);
//         let tag = self.interner[tag].display_in(arena);
//         let value = self.to(value).notate(arena);
//         let next = arena.newline().then(self.to(next).notate(arena), arena);

//         let single = [
//             bind.clone(),
//             arena.notate(" = "),
//             tag.clone(),
//             arena.just(' '),
//             value.clone().flatten(arena),
//         ]
//         .concat_in(arena);

//         let multi = [
//             bind,
//             arena.newline(),
//             arena.notate("= "),
//             tag,
//             arena.newline(),
//             value,
//         ]
//         .concat_in(arena)
//         .indent(arena);

//         single.or(multi, arena).then(next, arena)
//     }
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordField {
    pub label: StrKey,
    pub value: Id<Atom>,
    pub next: Option<Id<RecordField>>,
}

impl RecordField {
    pub fn new(label: StrKey, value: impl Into<Atom>, builder: &mut IrBuilder) -> Self {
        let value = builder.add(value.into());
        Self {
            label,
            value,
            next: None,
        }
    }

    pub fn with_next(
        label: StrKey,
        value: impl Into<Atom>,
        next: Id<RecordField>,
        builder: &mut IrBuilder,
    ) -> Self {
        let value = builder.add(value.into());
        Self {
            label,
            value,
            next: Some(next),
        }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, RecordField> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordField { label, value, next } = self.node;

        let label = self.interner[label].display_in(arena);
        let value = self.to(value).notate(arena);

        let field = [label, arena.notate(" = "), value].concat_in(arena);

        let next = next
            .map(|next| {
                let next = self.to(self.ir.instr(next)).notate(arena);

                let single = ", "
                    .display_in(arena)
                    .then(next.clone(), arena)
                    .flatten(arena);
                let multi = [arena.newline(), arena.just(','), next]
                    .concat_in(arena)
                    .indent(arena);

                single.or(multi, arena)
            })
            .or_not(arena);

        [field, next].concat_in(arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordExpr {
    pub bind: Symbol,
    pub head: Option<Id<RecordField>>,
    pub next: Id<Expr>,
}

impl RecordExpr {
    pub fn new(bind: Symbol, next: impl Into<Expr>, builder: &mut IrBuilder) -> Self {
        let next = builder.add(next.into());

        Self {
            bind,
            next,
            head: None,
        }
    }

    pub fn add_field(&mut self, field: (StrKey, Atom), builder: &mut IrBuilder) {
        self.head = Some(builder.add_field(field, self.head));
    }

    pub fn extend<I>(&mut self, fields: I, builder: &mut IrBuilder)
    where
        I: IntoIterator<Item = (StrKey, Atom)>,
    {
        self.head = builder.extend_fields(fields, self.head);
    }
}

// <bind> = { <label> = <value>, ... };
// <bind> =
//      { <label> = <value>
//      , ...
//      , <label> = <value> };
impl<'a> Notate<'a> for IrPrinter<'a, RecordExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordExpr { bind, head, next } = self.node;

        let bind = bind.display_in(arena);
        let fields = head
            .map(|head| self.to(self.ir.instr(head)).notate(arena))
            .or_not(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = { "),
            fields.clone().flatten(arena),
            arena.notate(" }"),
        ]
        .concat_in(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= {"),
            arena.newline(),
            fields.indent(arena),
            arena.newline(),
            arena.just('}'),
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldPath {
    pub label: StrKey,
    pub next: Option<Id<FieldPath>>,
}

impl FieldPath {
    pub fn new(label: StrKey, next: Option<Id<FieldPath>>) -> Self {
        Self { label, next }
    }

    pub fn with_next(label: StrKey, next: Id<FieldPath>) -> Self {
        Self {
            label,
            next: Some(next),
        }
    }
}

impl<'a> Notate<'a> for IrPrinter<'a, FieldPath> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let FieldPath { label, next } = self.node;

        let label = self.interner[label].display_in(arena);

        let next = next
            .map(|next| {
                let next = self.to(self.ir.instr(next)).notate(arena);

                let single = "."
                    .display_in(arena)
                    .then(next.clone(), arena)
                    .flatten(arena);
                let multi = [arena.newline(), arena.just('.'), next]
                    .concat_in(arena)
                    .indent(arena);

                single.or(multi, arena)
            })
            .or_not(arena);

        [label, next].concat_in(arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordExtendExpr {
    pub bind: Symbol,
    pub base: Id<Atom>,
    pub path: Id<FieldPath>,
    pub value: Id<Atom>,
    pub next: Id<Expr>,
}

impl RecordExtendExpr {
    pub fn new(
        bind: Symbol,
        base: impl Into<Atom>,
        label: StrKey,
        value: impl Into<Atom>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let base = builder.add(base.into());
        let value = builder.add(value.into());
        let next = builder.add(next.into());

        let path = builder.add(FieldPath::new(label, None));

        Self {
            bind,
            base,
            path,
            value,
            next,
        }
    }
}

// <bind> = { <base> | + <label> = <value> };
// <bind> =
//      { <base>
//      | + <label>
//      = <value> };
impl<'a> Notate<'a> for IrPrinter<'a, RecordExtendExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordExtendExpr {
            bind,
            base,
            path,
            value,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let base = self.to(base).notate(arena);
        let path = self.to(self.ir.instr(path)).notate(arena);
        let value = self.to(value).notate(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = { "),
            base.clone().flatten(arena),
            arena.notate(" | +"),
            path.clone(),
            arena.notate(" = "),
            value.clone().flatten(arena),
            arena.notate(" }"),
        ]
        .concat_in(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= { "),
            base,
            arena.newline(),
            arena.notate("  | +"),
            path,
            arena.newline(),
            arena.notate("  = "),
            value,
            arena.newline(),
            arena.notate("}"),
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordRestrictExpr {
    pub bind: Symbol,
    pub base: Id<Atom>,
    pub path: Id<FieldPath>,
    pub next: Id<Expr>,
}

impl RecordRestrictExpr {
    pub fn new(
        bind: Symbol,
        base: impl Into<Atom>,
        label: StrKey,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let base = builder.add(base.into());
        let next = builder.add(next.into());

        let path = builder.add(FieldPath::new(label, None));

        Self {
            bind,
            base,
            path,
            next,
        }
    }
}

// <bind> = { <base> | - <label> };
// <bind> =
//      { <base>
//      | - <label> };
impl<'a> Notate<'a> for IrPrinter<'a, RecordRestrictExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordRestrictExpr {
            bind,
            base,
            path,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let base = self.to(base).notate(arena);
        let path = self.to(self.ir.instr(path)).notate(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = { "),
            base.clone().flatten(arena),
            arena.notate(" | -"),
            path.clone(),
            arena.notate(" }"),
        ]
        .concat_in(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= { "),
            base,
            arena.newline(),
            arena.notate("  | -"),
            path,
            arena.newline(),
            arena.notate("}"),
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
        let op = match self {
            Self::Assign => "=",
            Self::AddAssign => "+=",
            Self::SubAssign => "-=",
            Self::MulAssign => "*=",
            Self::DivAssign => "/=",
            Self::RemAssign => "%=",
        };
        write!(f, "{}", op.blue())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordUpdateExpr {
    pub bind: Symbol,
    pub base: Id<Atom>,
    pub path: Id<FieldPath>,
    pub op: RecordUpdateOp,
    pub value: Id<Atom>,
    pub next: Id<Expr>,
}

impl RecordUpdateExpr {
    pub fn new(
        bind: Symbol,
        base: impl Into<Atom>,
        label: StrKey,
        op: RecordUpdateOp,
        value: impl Into<Atom>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let base = builder.add(base.into());
        let value = builder.add(value.into());
        let next = builder.add(next.into());

        let path = builder.add(FieldPath::new(label, None));

        Self {
            bind,
            base,
            path,
            op,
            value,
            next,
        }
    }
}

// <bind> = { <base> | <label> <op> value };
// <bind> =
//      { <base>
//      | <label>
//      <op> value };
impl<'a> Notate<'a> for IrPrinter<'a, RecordUpdateExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordUpdateExpr {
            bind,
            base,
            path,
            op,
            value,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let base = self.to(base).notate(arena);
        let path = self.to(self.ir.instr(path)).notate(arena);
        let op = op.display_in(arena);
        let value = self.to(value).notate(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = { "),
            base.clone().flatten(arena),
            arena.notate(" | "),
            path.clone(),
            arena.just(' '),
            op.clone(),
            arena.just(' '),
            value.clone().flatten(arena),
            arena.notate(" }"),
        ]
        .concat_in(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= { "),
            base,
            arena.newline(),
            arena.notate("  | "),
            path,
            arena.newline(),
            arena.notate("  "),
            op,
            arena.just(' '),
            value,
            arena.newline(),
            arena.notate("}"),
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordAccessExpr {
    pub bind: Symbol,
    pub base: Id<Atom>,
    pub label: StrKey,
    pub next: Id<Expr>,
}

impl RecordAccessExpr {
    pub fn new(
        bind: Symbol,
        base: impl Into<Atom>,
        label: StrKey,
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

// <bind> = <record>.<field>;
// <bind> =
//      <record>
//      .<field>;
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
        let label = self.interner[label].display_in(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            base.clone().flatten(arena),
            arena.just('.'),
            label.clone(),
        ]
        .concat_in(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= "),
            base,
            arena.newline(),
            arena.just('.'),
            label,
        ]
        .concat_in(arena)
        .indent(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PatternMatchExpr {
    pub bind: Symbol,
    pub matcher: Id<PatternMatcher>,
    pub next: Id<Expr>,
}

impl PatternMatchExpr {
    pub fn new(
        bind: Symbol,
        matcher: impl Into<PatternMatcher>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let matcher = builder.add(matcher.into());
        let next = builder.add(next.into());

        Self {
            bind,
            matcher,
            next,
        }
    }
}

// <bind> = match <source>
//      [ <matcher> ]
impl<'a> Notate<'a> for IrPrinter<'a, PatternMatchExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let PatternMatchExpr {
            bind,
            matcher,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let matcher = self.to(matcher).notate(arena);
        let next = arena.newline().then(self.to(next).notate(arena), arena);

        let head = [bind, arena.notate(" = match ")].concat_in(arena);

        let body = [
            arena.newline(),
            arena.notate("[ "),
            matcher,
            arena.notate(" ]"),
        ]
        .concat_in(arena)
        .indent(arena);

        [head, body, next].concat_in(arena)
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
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    List(ListExpr),
    Record(RecordExpr),
    RecordExtend(RecordExtendExpr),
    RecordRestrict(RecordRestrictExpr),
    RecordUpdate(RecordUpdateExpr),
    RecordAccess(RecordAccessExpr),
    PatternMatch(PatternMatchExpr),
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

impl<T> From<&T> for Expr
where
    T: Into<Expr> + Copy,
{
    fn from(value: &T) -> Self {
        (*value).into()
    }
}

impl_try_as!(
    Expr,
    Ret(RetExpr),
    Call(CallExpr),
    If(IfExpr),
    Let(LetExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr)
);

impl<'a> Notate<'a> for IrPrinter<'a, Id<Expr>> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let expr = match self.ir.instr(self.node) {
            Expr::Ret(expr) => self.to(expr).notate(arena),
            Expr::Call(expr) => self.to(expr).notate(arena),
            Expr::If(expr) => self.to(expr).notate(arena),
            Expr::Let(expr) => self.to(expr).notate(arena),
            Expr::Unary(expr) => self.to(expr).notate(arena),
            Expr::Binary(expr) => self.to(expr).notate(arena),
            Expr::List(expr) => self.to(expr).notate(arena),
            Expr::Record(expr) => self.to(expr).notate(arena),
            Expr::RecordExtend(expr) => self.to(expr).notate(arena),
            Expr::RecordRestrict(expr) => self.to(expr).notate(arena),
            Expr::RecordUpdate(expr) => self.to(expr).notate(arena),
            Expr::RecordAccess(expr) => self.to(expr).notate(arena),
            Expr::PatternMatch(expr) => self.to(expr).notate(arena),
        };

        [
            format_args!("{}:\t", self.node_label()).display_in(arena),
            expr,
            ';'.display_in(arena),
        ]
        .concat_in(arena)
    }
}
