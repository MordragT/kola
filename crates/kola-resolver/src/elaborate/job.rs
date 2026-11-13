use std::collections::{HashMap, HashSet, VecDeque};

use kola_span::{Diagnostic, Loc};
use kola_tree::{
    node::{self, ModuleName, Vis},
    prelude::Id,
};

use crate::{
    env::ModuleMap,
    lookup::Lookups,
    name::Binding,
    symbol::{AnySym, ModuleGraph, ModuleSym, Substitution},
};

/// A work list of pending elaboration jobs.
pub type ElabJobs = VecDeque<ElabJob>;

/// Track un-elaborated names globally via their structural placement
pub type UnresolvedBinds = HashSet<(ModuleSym, ModuleName)>;

pub struct ElabCtx<'a> {
    pub modules: &'a mut ModuleMap,
    pub unresolved_binds: &'a mut UnresolvedBinds,
    pub worklist: &'a mut ElabJobs,
    pub lookups: &'a mut Lookups,
    pub module_graph: &'a mut ModuleGraph,
    pub super_name: ModuleName,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StepResult {
    /// The step completed perfectly.
    Done(ModuleSym),
    /// The step is blocked waiting on an un-elaborated name binding.
    Blocked,
    /// A hard semantic error occurred. The diagnostic is captured here.
    Error(Diagnostic),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElabJob {
    pub id: Id<node::ModuleBind>,
    pub loc: Loc,
    pub vis: Vis,
    pub name: ModuleName,
    pub definition_scope: ModuleSym,
    pub expr: ElabExpr,
}

impl ElabJob {
    pub fn new(
        id: Id<node::ModuleBind>,
        loc: Loc,
        vis: Vis,
        name: ModuleName,
        definition_scope: ModuleSym,
        expr: ElabExpr,
    ) -> Self {
        Self {
            id,
            loc,
            vis,
            name,
            definition_scope,
            expr,
        }
    }

    pub fn path(
        id: Id<node::ModuleBind>,
        loc: Loc,
        vis: Vis,
        name: ModuleName,
        definition_scope: ModuleSym,
        path_id: Id<node::ModulePath>,
        path_loc: Loc,
        remaining_path: VecDeque<ModuleName>,
    ) -> Self {
        let expr = ElabExpr::Path(ElabPath::new(
            path_id,
            path_loc,
            definition_scope,
            remaining_path,
        ));
        Self::new(id, loc, vis, name, definition_scope, expr)
    }
}


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElabPath {
    pub id: Id<node::ModulePath>,
    pub loc: Loc,
    pub current_scope: ModuleSym,
    pub remaining_path: VecDeque<ModuleName>,
}

impl ElabPath {
    #[inline]
    pub fn new(
        id: Id<node::ModulePath>,
        loc: Loc,
        current_scope: ModuleSym,
        remaining_path: VecDeque<ModuleName>,
    ) -> Self {
        Self {
            id,
            current_scope,
            loc,
            remaining_path,
        }
    }

    fn with_current_scope(mut self, current_scope: ModuleSym) -> Self {
        self.current_scope = current_scope;
        self
    }

    pub fn step(&mut self, definition_scope: ModuleSym, ctx: &mut ElabCtx) -> StepResult {
        while let Some(&next_segment) = self.remaining_path.front() {
            if ctx
                .unresolved_binds
                .contains(&(self.current_scope, next_segment))
            {
                return StepResult::Blocked;
            }

            let current_module = &ctx.modules[&self.current_scope];

            if let Some(Binding { sym, vis }) = current_module.names.get_module(next_segment) {
                // Enforce visibility checking
                if self.current_scope != definition_scope && vis == Vis::Private {
                    let err = Diagnostic::error(
                        self.loc,
                        format!("Module '{:?}' is private", next_segment), // TODO: interner retrieval
                    );
                    return StepResult::Error(err);
                }

                // Advance the frontier and consume the segment
                self.current_scope = sym;
                self.remaining_path.pop_front();
            } else {
                let err =
                    Diagnostic::error(self.loc, format!("Module '{:?}' not found", next_segment)); // TODO: interner retrieval
                return StepResult::Error(err);
            }
        }

        StepResult::Done(self.current_scope)
    }
}


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ElabExpr {
    Path(ElabPath),
    Done(ModuleSym),
}

impl ElabExpr {
    pub fn step(&mut self, definition_scope: ModuleSym, ctx: &mut ElabCtx) -> StepResult {
        match self {
            // Fast Path: Already evaluated! Immediately pass the symbol up.
            Self::Done(sym) => StepResult::Done(*sym),

            Self::Path(path_job) => {
                match path_job.step(definition_scope, ctx) {
                    StepResult::Done(resolved_sym) => {
                        // Mutate self into a permanent cached pointer slot
                        *self = Self::Done(resolved_sym);
                        StepResult::Done(resolved_sym)
                    }
                    other => other,
                }
            }

        }
    }
}


impl From<ElabPath> for ElabExpr {
    fn from(job: ElabPath) -> Self {
        Self::Path(job)
    }
}

