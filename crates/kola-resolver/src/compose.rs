use std::collections::HashMap;

use kola_span::{Diagnostic, Loc, Report, SourceId};
use kola_tree::{
    node::{self, ModuleOp, Vis},
    prelude::*,
    source::{ModuleKey, TreeMap},
};

use crate::{
    env::ModuleMap,
    surface::{Definitions, ModuleDefinitions, ModuleMembers, ModuleObligations, Obligations},
    symbol::{FileMap, ModuleGraph, ModuleSym},
};

#[derive(Debug, Clone, Copy)]
pub struct ComposeJob {
    pub sym: ModuleSym,
    pub parent: ModuleSym,
    pub source: SourceId,
    pub key: ModuleKey,
    pub merge: Id<node::ModuleMergeExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ComposeState {
    Visiting,
    Done,
}

pub fn compose(
    modules: &mut ModuleMap,
    jobs: &[ComposeJob],
    files: &FileMap,
    module_of: &HashMap<(SourceId, ModuleKey), ModuleSym>,
    tree_map: &TreeMap,
    module_graph: &mut ModuleGraph,
    report: &mut Report,
) {
    let jobs = jobs
        .iter()
        .map(|job| (job.sym, *job))
        .collect::<HashMap<_, _>>();

    let mut composer = Composer {
        modules,
        jobs: &jobs,
        files,
        module_of,
        tree_map,
        module_graph,
        state: HashMap::new(),
    };

    for job in jobs.values() {
        if let Err(error) = composer.compose(job.sym) {
            report.add_diagnostic(error);
            break;
        }
    }
}

struct Composer<'a> {
    modules: &'a mut ModuleMap,
    jobs: &'a HashMap<ModuleSym, ComposeJob>,
    files: &'a FileMap,
    module_of: &'a HashMap<(SourceId, ModuleKey), ModuleSym>,
    tree_map: &'a TreeMap,
    module_graph: &'a mut ModuleGraph,
    state: HashMap<ModuleSym, ComposeState>,
}

impl Composer<'_> {
    fn compose(&mut self, sym: ModuleSym) -> Result<(), Diagnostic> {
        match self.state.get(&sym) {
            Some(ComposeState::Done) => return Ok(()),
            Some(ComposeState::Visiting) => {
                let loc = self
                    .jobs
                    .get(&sym)
                    .expect("compose job missing for a cycle")
                    .loc;

                return Err(
                    Diagnostic::error(loc, "Module composition cycle").with_help(
                        "A module composition expression depends on itself. Break the cycle.",
                    ),
                );
            }
            None => {}
        }

        let job = *self
            .jobs
            .get(&sym)
            .expect("compose job missing for a composed module");

        self.state.insert(sym, ComposeState::Visiting);

        let result = self.compose_job(job);

        match result {
            Ok(()) => {
                self.state.insert(sym, ComposeState::Done);
                Ok(())
            }
            Err(error) => {
                self.state.remove(&sym);
                Err(error)
            }
        }
    }

    fn compose_job(&mut self, job: ComposeJob) -> Result<(), Diagnostic> {
        let tree = self.tree_map[&job.source].trees[&job.key].clone();
        let merge = *tree.get(job.merge);

        let lhs = self.resolve_expr(merge.lhs, job.parent, job.source, &tree, job.loc)?;
        let rhs = self.resolve_expr(merge.rhs, job.parent, job.source, &tree, job.loc)?;

        let (Some(lhs), Some(rhs)) = (lhs, rhs) else {
            return Ok(());
        };

        self.module_graph.add_dependency(job.sym, lhs);
        self.module_graph.add_dependency(job.sym, rhs);

        let members = compose_op(
            merge.op,
            &self.modules[&lhs].members,
            &self.modules[&rhs].members,
            job.loc,
        )?;

        self.modules
            .get_mut(&job.sym)
            .expect("composed module missing")
            .members = members;

        self.populate_names(job.sym);

        Ok(())
    }

    fn resolve_expr(
        &mut self,
        id: Id<node::ModuleExpr>,
        parent: ModuleSym,
        source: SourceId,
        tree: &Tree,
        loc: Loc,
    ) -> Result<Option<ModuleSym>, Diagnostic> {
        match *tree.get(id) {
            node::ModuleExpr::Error(_) => Ok(None),
            node::ModuleExpr::Import(import_id) => {
                let target = import_id.get(tree).0;

                self.files.get(&target).copied().map(Some).ok_or_else(|| {
                    Diagnostic::error(loc, "Imported module not found")
                        .with_help("Check the import path.")
                })
            }
            node::ModuleExpr::Body(module_ref) => {
                let key = module_ref.get(tree).0;

                self.module_of
                    .get(&(source, key))
                    .copied()
                    .map(Some)
                    .ok_or_else(|| {
                        Diagnostic::error(loc, "Module body not found")
                            .with_help("The inline module body was not discovered.")
                    })
            }
            node::ModuleExpr::Path(path_id) => {
                self.resolve_path(path_id, parent, tree, loc).map(Some)
            }
            node::ModuleExpr::Merge(merge_id) => {
                let target = self.modules[&parent]
                    .nodes
                    .get_opt(merge_id)
                    .copied()
                    .ok_or_else(|| {
                        Diagnostic::error(loc, "Composed module not found")
                            .with_help("The nested composition was not collected.")
                    })?;

                self.compose(target)?;
                Ok(Some(target))
            }
        }
    }

    fn resolve_path(
        &mut self,
        path_id: Id<node::ModulePath>,
        parent: ModuleSym,
        tree: &Tree,
        loc: Loc,
    ) -> Result<ModuleSym, Diagnostic> {
        let path = tree.get(path_id).0.get(tree);
        let mut current = parent;

        for name_id in path.iter() {
            let name = *name_id.get(tree);

            if self.jobs.contains_key(&current) {
                self.compose(current)?;
            }

            let Some(binding) = self.modules[&current].names.get_module(name) else {
                return Err(Diagnostic::error(loc, "Module not found")
                    .with_help("Check that the module is defined in this scope."));
            };

            if binding.vis == Vis::Private && current != parent {
                return Err(Diagnostic::error(loc, "Module not exported")
                    .with_help("Only exported modules can be used in module composition."));
            }

            current = binding.sym;
        }

        if self.jobs.contains_key(&current) {
            self.compose(current)?;
        }

        Ok(current)
    }

    fn populate_names(&mut self, sym: ModuleSym) {
        let members = self.modules[&sym].members.clone();
        let module = self.modules.get_mut(&sym).expect("composed module missing");

        for &(name, definition) in &members.definitions.values.entries {
            module.names.insert_value(name, Vis::Public, definition.sym);
        }

        for &(name, definition) in &members.definitions.types.entries {
            module.names.insert_type(name, Vis::Public, definition.sym);
        }

        for &(name, definition) in &members.definitions.modules.entries {
            module
                .names
                .insert_module(name, Vis::Public, definition.sym);
        }

        for &(name, definition) in &members.definitions.module_types.entries {
            module
                .names
                .insert_module_type(name, Vis::Public, definition.sym);
        }
    }
}

fn compose_op(
    op: ModuleOp,
    lhs: &ModuleMembers,
    rhs: &ModuleMembers,
    loc: Loc,
) -> Result<ModuleMembers, Diagnostic> {
    match op {
        ModuleOp::With => compose_with(lhs, rhs, loc),
        ModuleOp::Over => Ok(compose_over(lhs, rhs)),
        ModuleOp::Using => Ok(compose_using(lhs, rhs)),
    }
}

fn compose_with(
    lhs: &ModuleMembers,
    rhs: &ModuleMembers,
    loc: Loc,
) -> Result<ModuleMembers, Diagnostic> {
    Ok(ModuleMembers {
        definitions: ModuleDefinitions {
            values: merge_with(&lhs.definitions.values, &rhs.definitions.values, loc)?,
            types: merge_with(&lhs.definitions.types, &rhs.definitions.types, loc)?,
            modules: merge_with(&lhs.definitions.modules, &rhs.definitions.modules, loc)?,
            module_types: merge_with(
                &lhs.definitions.module_types,
                &rhs.definitions.module_types,
                loc,
            )?,
        },
        obligations: ModuleObligations {
            values: merge_obligations(&lhs.obligations.values, &rhs.obligations.values),
            modules: merge_obligations(&lhs.obligations.modules, &rhs.obligations.modules),
        },
    })
}

fn compose_over(lhs: &ModuleMembers, rhs: &ModuleMembers) -> ModuleMembers {
    ModuleMembers {
        definitions: ModuleDefinitions {
            values: merge_over(&lhs.definitions.values, &rhs.definitions.values),
            types: merge_over(&lhs.definitions.types, &rhs.definitions.types),
            modules: merge_over(&lhs.definitions.modules, &rhs.definitions.modules),
            module_types: merge_over(&lhs.definitions.module_types, &rhs.definitions.module_types),
        },
        obligations: ModuleObligations {
            values: merge_obligations(&lhs.obligations.values, &rhs.obligations.values),
            modules: merge_obligations(&lhs.obligations.modules, &rhs.obligations.modules),
        },
    }
}

fn compose_using(lhs: &ModuleMembers, rhs: &ModuleMembers) -> ModuleMembers {
    let mut members = lhs.clone();

    supply(
        &mut members.definitions.values,
        &lhs.obligations.values,
        &rhs.definitions.values,
    );

    supply(
        &mut members.definitions.modules,
        &lhs.obligations.modules,
        &rhs.definitions.modules,
    );

    members
}

fn merge_with<K, S, B>(
    lhs: &Definitions<K, S, B>,
    rhs: &Definitions<K, S, B>,
    loc: Loc,
) -> Result<Definitions<K, S, B>, Diagnostic>
where
    K: Ord + Copy,
    S: Copy + PartialEq,
    B: Copy,
{
    let mut merged = lhs.clone();

    for &(key, definition) in &rhs.entries {
        match merged.definition(key) {
            Some(existing) if existing.origin != definition.origin => {
                return Err(Diagnostic::error(loc, "Conflicting module definitions").with_help(
                    "The two sides define the same public member. Use `over` if the left-hand definition should win.",
                ));
            }
            Some(_) => {}
            None => merged.define(key, definition),
        }
    }

    Ok(merged)
}

fn merge_over<K, S, B>(
    lhs: &Definitions<K, S, B>,
    rhs: &Definitions<K, S, B>,
) -> Definitions<K, S, B>
where
    K: Ord + Copy,
    S: Copy,
    B: Copy,
{
    let mut merged = lhs.clone();

    for &(key, definition) in &rhs.entries {
        if merged.definition(key).is_none() {
            merged.define(key, definition);
        }
    }

    merged
}

fn merge_obligations<K, D>(lhs: &Obligations<K, D>, rhs: &Obligations<K, D>) -> Obligations<K, D>
where
    K: Ord + Copy,
    D: Copy,
{
    let mut merged = lhs.clone();

    for &(key, obligation) in &rhs.entries {
        if merged.obligation(key).is_none() {
            merged.obligate(key, obligation);
        }
    }

    merged
}

fn supply<K, S, B, D>(
    definitions: &mut Definitions<K, S, B>,
    obligations: &Obligations<K, D>,
    provider: &Definitions<K, S, B>,
) where
    K: Ord + Copy,
    S: Copy,
    B: Copy,
{
    for &(key, _) in &obligations.entries {
        if definitions.definition(key).is_none() {
            if let Some(definition) = provider.definition(key) {
                definitions.define(key, *definition);
            }
        }
    }
}
