use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, Name};
use crate::{id::NodeId, print::TreePrinter, tree::NodeContainer};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Property {
    pub key: NodeId<Name>,
    pub value: NodeId<Expr>,
}

impl Property {
    pub fn key<'a>(&self, tree: &'a impl NodeContainer) -> &'a Name {
        self.key.get(tree)
    }

    pub fn value<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.value.get(tree)
    }
}

impl Printable<TreePrinter> for Property {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { key, value } = self;

        let head = "Property".blue().display_in(arena);

        let key = key.notate(with, arena);
        let value = value.notate(with, arena);

        let single = [
            arena.notate(" key = "),
            key.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("key = "),
            key,
            arena.newline(),
            arena.notate("value = "),
            value,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// { x = 10, y = 20 }
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Record {
    pub fields: Vec<NodeId<Property>>,
}

impl Record {
    pub fn get<'a>(
        &self,
        name: impl AsRef<str>,
        tree: &'a impl NodeContainer,
    ) -> Option<&'a Property> {
        self.fields.iter().find_map(|p| {
            let p = p.get(tree);
            (p.key.get(tree) == name.as_ref()).then_some(p)
        })
    }
}

impl Printable<TreePrinter> for Record {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "Record".blue().display_in(arena);

        let fields = self.fields.gather(with, arena);

        let single = fields.clone().concat_map(
            |field| arena.just(' ').then(field.flatten(arena), arena),
            arena,
        );
        let multi = fields.concat_map(|field| arena.newline().then(field, arena), arena);

        head.then(single.or(multi, arena), arena)
    }
}

// { y | +x = 10 }
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordExtend {
    pub source: NodeId<Expr>,
    pub field: NodeId<Name>,
    pub value: NodeId<Expr>,
}

impl RecordExtend {
    pub fn source<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.source.get(tree)
    }

    pub fn field<'a>(&self, tree: &'a impl NodeContainer) -> &'a Name {
        self.field.get(tree)
    }

    pub fn value<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.value.get(tree)
    }
}

impl Printable<TreePrinter> for RecordExtend {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self {
            source,
            field,
            value,
        } = self;

        let head = "RecordExtend".blue().display_in(arena);

        let source = source.notate(with, arena);
        let field = field.notate(with, arena);
        let value = value.notate(with, arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", field = "),
            field.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("source = "),
            source,
            arena.newline(),
            arena.notate("field = "),
            field,
            arena.newline(),
            arena.notate("value = "),
            value,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// { y | -x }
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordRestrict {
    pub source: NodeId<Expr>,
    pub field: NodeId<Name>,
}

impl RecordRestrict {
    pub fn source<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.source.get(tree)
    }

    pub fn field<'a>(&self, tree: &'a impl NodeContainer) -> &'a Name {
        self.field.get(tree)
    }
}

impl Printable<TreePrinter> for RecordRestrict {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { source, field } = self;

        let head = "RecordRestrict".blue().display_in(arena);

        let source = source.notate(with, arena);
        let field = field.notate(with, arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", field = "),
            field.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("source = "),
            source,
            arena.newline(),
            arena.notate("field = "),
            field,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// x.y.z
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordSelect {
    pub source: NodeId<Expr>,
    pub field: NodeId<Name>,
}

impl RecordSelect {
    pub fn source<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.source.get(tree)
    }

    pub fn field<'a>(&self, tree: &'a impl NodeContainer) -> &'a Name {
        self.field.get(tree)
    }
}

impl Printable<TreePrinter> for RecordSelect {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { source, field } = self;

        let head = "RecordSelect".blue().display_in(arena);

        let source = source.notate(with, arena);
        let field = field.notate(with, arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", field = "),
            field.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("source = "),
            source,
            arena.newline(),
            arena.notate("field = "),
            field,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// { y | x = 10 }
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordUpdate {
    pub source: NodeId<Expr>,
    pub field: NodeId<Name>,
    pub value: NodeId<Expr>,
}

impl RecordUpdate {
    pub fn source<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.source.get(tree)
    }

    pub fn field<'a>(&self, tree: &'a impl NodeContainer) -> &'a Name {
        self.field.get(tree)
    }

    pub fn value<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.value.get(tree)
    }
}

impl Printable<TreePrinter> for RecordUpdate {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self {
            source,
            field,
            value,
        } = self;

        let head = "RecordUpdate".blue().display_in(arena);

        let source = source.notate(with, arena);
        let field = field.notate(with, arena);
        let value = value.notate(with, arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", field = "),
            field.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("source = "),
            source,
            arena.newline(),
            arena.notate("field = "),
            field,
            arena.newline(),
            arena.notate("value = "),
            value,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}
