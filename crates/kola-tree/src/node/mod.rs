use crate::{id::NodeId, print::TreePrinter};

use derive_more::{From, TryInto};
use kola_print::prelude::*;
use kola_utils::impl_try_as;
use owo_colors::OwoColorize;
use paste::paste;
use serde::{Deserialize, Serialize};

mod expr;
mod module;
mod pat;
mod ty;

pub use expr::*;
pub use module::*;
pub use pat::*;
pub use ty::*;

pub type Symbol = ecow::EcoString;

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[from(forward)]
pub struct Name(pub Symbol);

impl Name {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl PartialEq<Symbol> for Name {
    fn eq(&self, other: &Symbol) -> bool {
        &self.0 == other
    }
}

impl PartialEq<str> for Name {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl Printable<TreePrinter> for Name {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "Name".cyan().display_in(arena);

        let name = self
            .0
            .yellow()
            .display_in(arena)
            .enclose_by(arena.just('"'), arena);

        let single = [arena.just(' '), name.clone()].concat_in(arena);
        let multi = [arena.newline(), name].concat_in(arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

macro_rules! define_nodes {
    ($($variant:ident),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum NodeKind {
            $($variant),*
        }

        #[derive(Clone, Debug, PartialEq, From, TryInto)]
        pub enum Node {
            $($variant($variant)),*
        }

        impl Node {
            pub fn kind(&self) -> NodeKind {
                match self {
                    $(Self::$variant(_) => NodeKind::$variant,)*
                }
            }
        }

        impl_try_as!(
            Node,
            $($variant($variant)),*
        );

        pub trait Handler {
            type Error;

            paste! {
                fn handle_node(&mut self, node: &Node, id: usize) -> Result<(), Self::Error> {
                    match node {
                        $(Node::$variant(v) => self.[<handle_ $variant:snake>](v, NodeId::from_usize(id)),)*
                    }
                }

                $(
                    fn [<handle_ $variant:snake>](&mut self, [<_ $variant:snake>]: &$variant, _id: NodeId<$variant>) -> Result<(), Self::Error> {
                        Ok(())
                    }
                )*
            }
        }
    }
}

define_nodes!(
    Name,
    // Patterns
    AnyPat,
    LiteralPat,
    IdentPat,
    RecordFieldPat,
    RecordPat,
    VariantCasePat,
    VariantPat,
    PatError,
    Pat,
    // Expressions
    LiteralExpr,
    PathExpr,
    ListExpr,
    RecordField,
    RecordExpr,
    RecordFieldPath,
    RecordExtendExpr,
    RecordRestrictExpr,
    RecordUpdateOp,
    RecordUpdateExpr,
    UnaryOp,
    UnaryExpr,
    BinaryOp,
    BinaryExpr,
    LetExpr,
    CaseBranch,
    CaseExpr,
    IfExpr,
    LambdaExpr,
    CallExpr,
    ExprError,
    Expr,
    // Types
    TypePath,
    TypeVar,
    RecordFieldType,
    RecordType,
    VariantCaseType,
    VariantType,
    FuncType,
    TypeApplication,
    TypeExpr,
    TypeError,
    Type,
    // Modules
    ValueBind,
    TypeBind,
    OpaqueTypeBind,
    ModuleBind,
    ModuleTypeBind,
    Bind,
    Module,
    ValueSpec,
    OpaqueTypeKind,
    OpaqueTypeSpec,
    ModuleSpec,
    Spec,
    ModuleType
);
