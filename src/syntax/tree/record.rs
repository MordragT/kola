use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, Metadata, Name, Node, NodeId, Tree};
use crate::syntax::print::prelude::*;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Property {
    pub key: NodeId<Name>,
    pub value: NodeId<Expr>,
}

impl<M> Printable<Tree<M>> for Property
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
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

impl TryFrom<Node> for Property {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Property(p) => Ok(p),
            _ => Err(()),
        }
    }
}

// { x = 10, y = 20 }
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Record {
    pub fields: Vec<NodeId<Property>>,
}

impl<M> Printable<Tree<M>> for Record
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
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

impl TryFrom<Node> for Record {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Record(r) => Ok(r),
            _ => Err(()),
        }
    }
}

// impl Record {
//     pub fn get(&self, name: impl AsRef<str>) -> Option<&Property> {
//         self.fields.iter().find(|p| &p.key.name == name.as_ref())
//     }
// }

// { y | +x = 10 }
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RecordExtend {
    pub source: NodeId<Expr>,
    pub field: NodeId<Name>,
    pub value: NodeId<Expr>,
}

impl<M> Printable<Tree<M>> for RecordExtend
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
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

impl TryFrom<Node> for RecordExtend {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::RecordExtend(r) => Ok(r),
            _ => Err(()),
        }
    }
}

// { y | -x }
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RecordRestrict {
    pub source: NodeId<Expr>,
    pub field: NodeId<Name>,
}

impl<M> Printable<Tree<M>> for RecordRestrict
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
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

impl TryFrom<Node> for RecordRestrict {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::RecordRestrict(r) => Ok(r),
            _ => Err(()),
        }
    }
}

// x.y.z
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RecordSelect {
    pub source: NodeId<Expr>,
    pub field: NodeId<Name>,
}

impl<M> Printable<Tree<M>> for RecordSelect
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
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

impl TryFrom<Node> for RecordSelect {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::RecordSelect(r) => Ok(r),
            _ => Err(()),
        }
    }
}

// { y | x = 10 }
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RecordUpdate {
    pub source: NodeId<Expr>,
    pub field: NodeId<Name>,
    pub value: NodeId<Expr>,
}

impl<M> Printable<Tree<M>> for RecordUpdate
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
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

impl TryFrom<Node> for RecordUpdate {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::RecordUpdate(r) => Ok(r),
            _ => Err(()),
        }
    }
}
