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

use std::collections::{HashMap, HashSet};

use kola_utils::{interner::PathKey, io::FileSystem};

use crate::{
    forest::Forest,
    module::{ModuleKey, UnresolvedModuleScope},
};

mod declare;
mod define;

pub use declare::Declare;
pub use define::Define;

/// The Resolver struct contains the context needed for resolving
/// a single file of the program. There is 1 NameResolver per file, and
/// different files may be in different `ResolutionState`s.
#[derive(Debug, Clone)]
pub struct Resolver<S> {
    state: S,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Declared {
    pub module_key: ModuleKey,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Defined;

impl Resolver<Declare> {
    pub fn declare<C>(path_key: PathKey, ctx: C) -> Resolver<Declared> {
        let mut unresolved = HashMap::new();
        let mut in_progress = HashSet::new();

        let state = Declared {
            module_key: Declare::new(path_key).declare(ctx),
        };

        Resolver { state }
    }
}

impl Resolver<Declared> {
    pub fn to_define(self, forest: &mut Forest) -> Resolver<Define> {
        let state = Define::new(self.state.module_key, forest);

        Resolver { state }
    }
}

impl Resolver<Define> {
    pub fn define(self, forest: &mut Forest) -> Resolver<Defined> {
        self.state.define(forest);

        Resolver { state: Defined }
    }
}
