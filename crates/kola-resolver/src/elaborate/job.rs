use std::collections::{HashMap, HashSet, VecDeque};

use kola_span::{Diagnostic, Loc};
use kola_subst::{Substitutable, merge};
use kola_tree::{
    id::Id,
    node::{self, FunctorName, ModuleName, Vis},
};

use crate::{
    env::{FunctorMap, ModuleMap},
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
    pub functors: &'a FunctorMap,
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

    pub fn functor_appl(
        id: Id<node::ModuleBind>,
        loc: Loc,
        vis: Vis,
        name: ModuleName,
        definition_scope: ModuleSym,
        functor_id: Id<node::FunctorApp>,
        functor_loc: Loc,
        path: Option<Box<ElabExpr>>,
        functor: FunctorName,
        args: Vec<ElabExpr>,
    ) -> Self {
        let expr = ElabExpr::FunctorAppl(ElabFunctorAppl::new(
            functor_id,
            functor_loc,
            path,
            functor,
            args,
        ));
        Self::new(id, loc, vis, name, definition_scope, expr)
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

impl Substitutable<Substitution> for ElabJob {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        let Self {
            id,
            loc,
            vis,
            name,
            definition_scope,
            expr,
        } = self;

        let definition_scope_opt = definition_scope.try_apply(s);
        let expr_opt = expr.try_apply(s);

        merge(
            definition_scope_opt,
            || definition_scope.clone(),
            expr_opt,
            || expr.clone(),
        )
        .map(|(definition_scope, expr)| Self::new(*id, *loc, *vis, *name, definition_scope, expr))
    }

    fn apply_mut(&mut self, s: &mut HashMap<AnySym, AnySym>) {
        self.definition_scope.apply_mut(s);
        self.expr.apply_mut(s);
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
                if self.current_scope != definition_scope && vis != Vis::Export {
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

impl Substitutable<Substitution> for ElabPath {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        let Self {
            id,
            loc,
            current_scope,
            remaining_path: path,
        } = self;

        if let Some(current_scope) = current_scope.try_apply(s) {
            Some(Self::new(*id, *loc, current_scope, path.clone()))
        } else {
            None
        }
    }

    fn apply_mut(&mut self, s: &mut HashMap<AnySym, AnySym>) {
        self.current_scope.apply_mut(s);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElabFunctorAppl {
    pub id: Id<node::FunctorApp>,
    pub loc: Loc,
    pub path: Option<Box<ElabExpr>>,
    pub functor: FunctorName,
    pub args: Vec<ElabExpr>,
}

impl ElabFunctorAppl {
    #[inline]
    pub fn new(
        id: Id<node::FunctorApp>,
        loc: Loc,
        path: Option<Box<ElabExpr>>,
        functor: FunctorName,
        args: Vec<ElabExpr>,
    ) -> Self {
        Self {
            id,
            loc,
            path,
            functor,
            args,
        }
    }

    pub fn step(&mut self, definition_scope: ModuleSym, ctx: &mut ElabCtx) -> StepResult {
        // 1. Resolve the container module where the functor template is declared
        let container_sym = if let Some(ref mut prefix_expr) = self.path {
            // Because prefix_expr is an ElabExpr, once it hits StepResult::Done,
            // it mutates into ElabExpr::Done(sym) and will fast-path out instantly next pass!
            match prefix_expr.step(definition_scope, ctx) {
                StepResult::Blocked => return StepResult::Blocked,
                StepResult::Error(e) => return StepResult::Error(e),
                StepResult::Done(sym) => sym,
            }
        } else {
            definition_scope
        };

        // 2. Fetch the functor template symbol out of that resolved container module
        let container_module = &ctx.modules[&container_sym];
        let Some(functor_binding) = container_module.names.get_functor(self.functor) else {
            let err =
                Diagnostic::error(self.loc, format!("Functor '{:?}' not found", self.functor));
            return StepResult::Error(err);
        };

        // 3. Progressively step through argument expressions in-place
        let mut any_blocked = false;

        for arg_expr in &mut self.args {
            match arg_expr.step(definition_scope, ctx) {
                StepResult::Blocked => any_blocked = true,
                StepResult::Error(e) => return StepResult::Error(e),
                StepResult::Done(_sym) => (),
            }
        }

        if any_blocked {
            StepResult::Blocked
        } else {
            // 4. All dependencies are completely Ready! Trigger instantiation.
            let fresh_instance_sym = ModuleSym::new();
            let functor_def = ctx.functors[&functor_binding.sym].clone();

            let resolved_args = self
                .args
                .iter()
                .map(|arg_expr| match arg_expr {
                    ElabExpr::Done(sym) => *sym,
                    _ => unreachable!("Argument expressions should have been fully stepped by now"),
                })
                .collect();

            let (mut inst_body, inst_lookups, inst_elab_jobs) =
                functor_def.apply(fresh_instance_sym, resolved_args);

            inst_body
                .names
                .insert_module(ctx.super_name, Vis::None, definition_scope);

            // Queue the downstream compiler tasks returned by the functor body instantiation
            for inner_job in inst_elab_jobs {
                ctx.unresolved_binds
                    .insert((inner_job.definition_scope, inner_job.name));
                ctx.worklist.push_back(inner_job);
            }

            ctx.lookups.append(inst_lookups);
            ctx.modules.insert(fresh_instance_sym, inst_body);
            ctx.module_graph
                .add_dependency(definition_scope, fresh_instance_sym);

            StepResult::Done(fresh_instance_sym)
        }
    }
}

impl Substitutable<Substitution> for ElabFunctorAppl {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        let Self {
            id,
            loc,
            path,
            functor,
            args,
        } = self;

        let path_opt = path.as_ref().map(|p| p.try_apply(s).map(Box::new));
        let args_opt = args.try_apply(s);

        merge(path_opt, || path.clone(), args_opt, || args.clone())
            .map(|(path, args)| Self::new(*id, *loc, path, *functor, args))
    }

    fn apply_mut(&mut self, s: &mut HashMap<AnySym, AnySym>) {
        if let Some(ref mut path) = self.path {
            path.apply_mut(s);
        }
        self.args.apply_mut(s);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ElabExpr {
    FunctorAppl(ElabFunctorAppl),
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

            Self::FunctorAppl(funct_job) => match funct_job.step(definition_scope, ctx) {
                StepResult::Done(resolved_sym) => {
                    *self = Self::Done(resolved_sym);
                    StepResult::Done(resolved_sym)
                }
                other => other,
            },
        }
    }
}

impl From<ElabFunctorAppl> for ElabExpr {
    fn from(job: ElabFunctorAppl) -> Self {
        Self::FunctorAppl(job)
    }
}

impl From<ElabPath> for ElabExpr {
    fn from(job: ElabPath) -> Self {
        Self::Path(job)
    }
}

impl Substitutable<Substitution> for ElabExpr {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        match self {
            Self::FunctorAppl(job) => job.try_apply(s).map(Self::FunctorAppl),
            Self::Path(job) => job.try_apply(s).map(Self::Path),
            Self::Done(sym) => sym.try_apply(s).map(Self::Done),
        }
    }

    fn apply_mut(&mut self, s: &mut HashMap<AnySym, AnySym>) {
        match self {
            Self::FunctorAppl(job) => job.apply_mut(s),
            Self::Path(job) => job.apply_mut(s),
            Self::Done(sym) => sym.apply_mut(s),
        }
    }
}
