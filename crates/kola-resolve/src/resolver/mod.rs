//! nameresolution/mod.rs - Defines the name resolution `declare` and
//! `define` passes via the Resolvable trait. Name resolution follows
//! parsing and is followed by type inference in the compiler pipeline.
//!
//! The goal of the name resolution passes are to handle Scopes + imports and link
//! each variable to its definition (via setting its DefinitionInfoId field)
//! so that no subsequent pass needs to care about scoping.
//!
//! Name resolution is split up into two passes:
//! 1. `declare` collects the publically exported symbols of every module
//!    to enable mutually recursive functions that may be defined in separate
//!    modules. Since this pass only needs to collect these symbols, it skips
//!    over most Ast node types, looking for only top-level Definition nodes.
//! 2. `define` does the bulk of name resolution, creating DefinitionInfos for
//!    each variable definition, linking each variable use to the corresponding
//!    DefinitionInfo it was defined in, creating a TypeInfo for each type
//!    definition, and a TraitInfo for each trait definition. This will also
//!    issue unused variable warnings at the end of a scope for any unused symbols.
//!
//! Both of these passes walk the Ast in a flat manor compared to subsequent
//! passes like codegen which uses the results of name resolution to walk the Ast
//! and follow definition links to compile definitions lazily as they're used.

// /// Specifies how far a particular module is in name resolution.
// /// Keeping this properly up to date for each module is the
// /// key for preventing infinite recursion when declaring recursive imports.
// ///
// /// For example, if we're in the middle of defining a module, and we
// /// try to import another file that has DefineInProgress, we know not
// /// to recurse into that module. In this case, since the module has already
// /// finished the declare phase, we can still import any needed public symbols
// /// and continue resolving the current module. The other module will finish
// /// being defined sometime after the current one is since we detected a cycle.
// #[derive(Debug, Copy, Clone, PartialEq, Eq)]
// pub enum ResolutionState {
//     NotStarted,
//     DeclareInProgress,
//     Declared,
//     DefineInProgress,
//     Defined,
// }

use kola_tree::{
    id::Id,
    node::{self, Node},
    tree::TreeView,
};
use kola_utils::{interner::PathKey, io::FileSystem};

use crate::{forest::Forest, module::ModuleKey};

mod declare;
mod define;

pub use declare::{Declare, Scope};
pub use define::Define;

/// The Resolver struct contains the context needed for resolving
/// a single file of the program. There is 1 NameResolver per file, and
/// different files may be in different `ResolutionState`s.
#[derive(Debug, Clone)]
pub struct Resolver<S> {
    state: S,
}

pub struct Declared;

impl Resolver<Declare> {
    pub fn declare<Io>(path_key: PathKey, forest: &mut Forest<Io>) -> Resolver<Declared>
    where
        Io: FileSystem,
    {
        let mut declare = Declare::new(path_key);
        declare.declare(forest);

        Resolver { state: Declared }
    }
}

impl Resolver<Declared> {
    pub fn define<Io>(self, forest: &mut Forest<Io>) -> Resolver<Define>
    where
        Io: FileSystem,
    {
        todo!()
    }
}
