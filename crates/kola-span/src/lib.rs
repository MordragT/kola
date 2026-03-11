#![feature(const_trait_impl)]
#![feature(array_try_from_fn)]

//! Source positions and related helper functions.
//!
//! Important concepts in this module include:
//!
//! - the *span*, represented by [`Span`] and related types;
//! - source code as represented by a [`Source`];

mod diag;
mod loc;
mod source;
mod span;

pub mod combinator;
pub mod input;
pub mod parser;
pub mod primitive;

pub use diag::{Diagnostic, IntoDiagnostic, IntoIssue, Issue, Report, Severity};
pub use loc::{Loc, Located};
pub use source::{Source, SourceId, SourceManager};
pub use span::{Span, Spanned};
