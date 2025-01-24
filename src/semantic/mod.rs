//! Algorithm W with addition in Polymoprhic Type System for Extensible Records
//! https://github.com/miguel-nascimento/algorithm-j-rs/
//! https://github.com/nwoeanhinnogaehr/algorithmw-rust/

// pub use context::*;
pub use env::*;
pub use infer::*;
pub use substitute::*;
pub use unify::*;

// mod context;
mod env;
pub mod error;
mod infer;
mod substitute;
pub mod types;
mod unify;

use crate::syntax::{
    tree::{Metadata, Tree},
    Span,
};
use types::MonoType;

pub type Meta = (Span, Option<MonoType>);

impl Metadata for Meta {
    fn span(&self) -> Span {
        self.0.clone()
    }

    fn ty(&self) -> Option<&MonoType> {
        self.1.as_ref()
    }
}

impl Substitutable for Meta {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        let t = self.1.as_ref()?.try_apply(s)?;

        Some((self.0, Some(t)))
    }
}

pub type SemanticTree = Tree<Meta>;
