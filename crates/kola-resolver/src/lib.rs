//! # Kola Resolver: Forward Reference Symbol Resolution
//!
//! This crate implements a **forward reference symbol system** for resolving module dependencies,
//! imports, and symbol bindings in the Kola language. The design is inspired by LLVM IR's approach
//! to forward declarations, where symbols are created eagerly during discovery and resolved lazily
//! during a separate resolution phase.
//!
//! ## The Forward Reference Solution
//!
//! Our approach treats symbols as **stable identities** that can be created immediately
//! but resolved later:
//!
//! ### 1. Discovery Phase: Eager Symbol Creation
//!
//! During AST traversal, create symbols immediately for every binding and reference:
//! - Module definitions → `ModuleSym`
//! - Value bindings → `ValueSym`
//! - Type definitions → `TypeSym`
//! - Import statements → `ModuleSym` (same type!)
//! - Path references → `ModuleSym` (same type!)
//!
//! The key insight: **the same symbol can serve multiple roles simultaneously**.
//! A `ModuleSym` might represent both a module definition and all references to that module.
//!
//! ### 2. Resolution Phase: Lazy Target Resolution
//!
//! After discovery, resolve what each symbol actually points to:
//! - Map import symbols to their target modules
//! - Resolve path references to concrete definitions
//! - Build the final dependency graph
//! - Perform type checking and validation
//!
//! ## Conceptual Model
//!
//! Think of symbols as **forwarding addresses**:
//!
//! ```text
//! ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
//! │  Symbol Created │───▶│ Multiple Uses   │───▶│    Resolved     │
//! │                 │    │                 │    │                 │
//! │ ModuleSym(42)   │    │ • Module def    │    │ All point to    │
//! │                 │    │ • Import ref    │    │ concrete scope  │
//! │                 │    │ • Path ref      │    │                 │
//! └─────────────────┘    └─────────────────┘    └─────────────────┘
//! ```
//!
//! ## Symbol Identity vs. Symbol Roles
//!
//! Each symbol has a **stable identity** (unique ID) but can play **multiple roles**:
//!
//! - **Identity**: `ModuleSym(42)` - never changes, used for equality/hashing
//! - **Roles**: Definition, import target, path reference - determined during discovery
//! - **Resolution**: All roles eventually point to the same concrete entity
//!
//! This allows the same symbol to be used before we know what it will resolve to,
//! greatly simplifying the discovery algorithm.
//!
//! ## Architecture Overview
//!
//! ```text
//! ┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
//! │    Discovery    │───▶│   Symbol Table   │◀───│   Resolution    │
//! │                 │    │                  │    │                 │
//! │ • Create symbols│    │ • ModuleSym      │    │ • Resolve paths │
//! │ • Build scopes  │    │ • ValueSym       │    │ • Link imports  │
//! │ • Track deps    │    │ • TypeSym        │    │ • Build graph   │
//! │ • Thread context│    │ • QualId maps    │    │ • Validate refs │
//! └─────────────────┘    └──────────────────┘    └─────────────────┘
//!          │                       │                       │
//!          ▼                       ▼                       ▼
//! ┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
//! │   Module Tree   │    │  Forward Refs    │    │ Dependency Graph│
//! │   (AST nodes)   │    │  (Unresolved)    │    │  (Resolved)     │
//! └─────────────────┘    └──────────────────┘    └─────────────────┘
//! ```
//!

#![feature(never_type)]
#![feature(exhaustive_patterns)]
#![feature(iter_intersperse)]

pub mod constraints;
pub mod defs;
pub mod error;
pub mod forest;
pub mod functor;
pub mod info;
pub mod phase;
pub mod print;
pub mod resolver;
pub mod scope;
pub mod shape;
pub mod symbol;
pub mod topography;

pub mod prelude {
    pub use crate::forest::Forest;
    pub use crate::resolver::{ResolveOutput, resolve};
    pub use crate::topography::Topography;
}

#[derive(Debug)]
pub struct GlobalId<T> {
    pub source: kola_span::SourceId,
    pub id: kola_tree::id::Id<T>,
}

impl<T> GlobalId<T> {
    pub fn new(source: kola_span::SourceId, id: kola_tree::id::Id<T>) -> Self {
        Self { source, id }
    }
}

impl<T> Clone for GlobalId<T> {
    fn clone(&self) -> Self {
        Self {
            source: self.source,
            id: self.id,
        }
    }
}

impl<T> Copy for GlobalId<T> {}

impl<T> PartialEq for GlobalId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.source == other.source && self.id == other.id
    }
}

impl<T> Eq for GlobalId<T> {}

impl<T> PartialOrd for GlobalId<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for GlobalId<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.source.cmp(&other.source).then(self.id.cmp(&other.id))
    }
}

impl<T> std::hash::Hash for GlobalId<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.source.hash(state);
        self.id.hash(state);
    }
}
