use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, InnerNode, Name, Node};
use crate::{
    Phase,
    id::NodeId,
    meta::{Attached, Meta},
    print::TreePrinter,
};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Property {
    pub key: NodeId<Name>,
    pub value: NodeId<Expr>,
}

impl InnerNode for Property {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::Property(p) => Some(p),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::Property(p) => Some(p),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for Property {
    type Meta = P::Property;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::Property(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::Property(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::Property(m) => Some(m),
            _ => None,
        }
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

impl InnerNode for Record {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::Record(r) => Some(r),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::Record(r) => Some(r),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for Record {
    type Meta = P::Record;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::Record(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::Record(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::Record(m) => Some(m),
            _ => None,
        }
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

impl InnerNode for RecordExtend {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::RecordExtend(r) => Some(r),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::RecordExtend(r) => Some(r),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for RecordExtend {
    type Meta = P::RecordExtend;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::RecordExtend(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::RecordExtend(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::RecordExtend(m) => Some(m),
            _ => None,
        }
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

impl InnerNode for RecordRestrict {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::RecordRestrict(r) => Some(r),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::RecordRestrict(r) => Some(r),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for RecordRestrict {
    type Meta = P::RecordRestrict;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::RecordRestrict(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::RecordRestrict(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::RecordRestrict(m) => Some(m),
            _ => None,
        }
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

impl InnerNode for RecordSelect {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::RecordSelect(s) => Some(s),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::RecordSelect(s) => Some(s),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for RecordSelect {
    type Meta = P::RecordSelect;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::RecordSelect(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::RecordSelect(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::RecordSelect(m) => Some(m),
            _ => None,
        }
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

impl InnerNode for RecordUpdate {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::RecordUpdate(u) => Some(u),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::RecordUpdate(u) => Some(u),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for RecordUpdate {
    type Meta = P::RecordUpdate;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::RecordUpdate(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::RecordUpdate(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::RecordUpdate(m) => Some(m),
            _ => None,
        }
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

impl TryFrom<Node> for RecordUpdate {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::RecordUpdate(r) => Ok(r),
            _ => Err(()),
        }
    }
}
