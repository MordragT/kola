use derive_more::From;
use std::fmt::{self, Display};

use kola_builtins::{BuiltinId, TypeInterner, TypeKey, TypeProtocol};
use kola_print::prelude::*;
use kola_utils::{
    impl_try_as,
    interner::{StrInterner, StrKey},
    interner_ext::{DisplayWithInterner, InternerExt},
};

use super::{Expr, Symbol};
use crate::{
    id::Id,
    ir::{IrBuilder, IrView},
    print::IrPrinter,
};

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

// fn <param> => <body>
// fn <param>
//      => <body>;
impl<'a> Notate<'a> for IrPrinter<'a, Func> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let Func { param, body } = self.node;

        let param = param.display_in(arena);
        let body = self.to(body).notate(arena);

        // let single = arena
        //     .just(' ')
        //     .then("=> ".blue().display_in(arena), arena)
        //     .then(body.clone().flatten(arena), arena);
        // let multi = arena
        //     .newline()
        //     .then("=> ".blue().display_in(arena), arena)
        //     .then(body, arena)
        //     .indent(arena);

        // "fn "
        //     .red()
        //     .display_in(arena)
        //     .then(param, arena)
        //     .then(single.or(multi, arena), arena)

        let head = [
            "fn ".red().display_in(arena),
            param,
            " =>".blue().display_in(arena),
        ]
        .concat_in(arena)
        .flatten(arena);

        let body = arena.newline().then(body, arena).indent(arena);

        head.then(body, arena)
    }
}

#[derive(Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tag(pub StrKey);

impl DisplayWithInterner<str> for Tag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        write!(f, "#{}", interner[self.0].blue())
    }
}

#[derive(Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeRep(pub TypeKey);

impl DisplayWithInterner<TypeProtocol> for TypeRep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &TypeInterner) -> fmt::Result {
        interner[self.0].to_json().unwrap().fmt(f)
    }
}

/// An expression is atomic if:
/// - it is guaranteed to terminate
/// - it causes no side effects
/// - it causes no control effects
/// - it never produces an error
#[derive(Debug, From, Clone, Copy, PartialEq)]
pub enum Atom {
    Noop,
    Bool(bool),
    Char(char),
    Num(f64),
    Str(StrKey),
    Func(Func),
    Symbol(Symbol),
    Builtin(BuiltinId),
    Tag(Tag),
    TypeRep(TypeRep),
}

impl<T> From<&T> for Atom
where
    T: Into<Atom> + Copy,
{
    fn from(value: &T) -> Self {
        (*value).into()
    }
}

impl_try_as!(
    Atom,
    Bool(bool),
    Char(char),
    Num(f64),
    Str(StrKey),
    Func(Func),
    Symbol(Symbol),
    Builtin(BuiltinId),
    Tag(Tag),
    TypeRep(TypeRep)
);

impl<'a> Notate<'a> for IrPrinter<'a, Id<Atom>> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let notation = match self.ir.instr(self.node) {
            Atom::Noop => "noop".red().display_in(arena),
            Atom::Bool(b) => b.green().display_in(arena),
            Atom::Char(c) => format!("'{}'", c.green()).display_in(arena),
            Atom::Num(n) => n.green().display_in(arena),
            Atom::Str(s) => format_args!("\"{}\"", self.interner[s].green()).display_in(arena),
            Atom::Func(f) => self.to(f).notate(arena),
            Atom::Symbol(s) => s.display_in(arena),
            Atom::Builtin(b) => b.display_in(arena),
            Atom::Tag(t) => self.interner.with(&t).display_in(arena),
            Atom::TypeRep(tr) => tr.0.display_in(arena),
        };

        notation
            .clone()
            .flatten(arena)
            .or(notation.indent(arena), arena)
    }
}
