use kola_span::{Diagnostic, Report};
use kola_tree::node::{ModuleName, Vis};
use kola_utils::interner::StrInterner;

use crate::{
    def::DefMap,
    env::{FunctorMap, ModuleMap},
    lookup::Lookups,
    symbol::ModuleGraph,
};

mod job;

pub use job::*;

pub fn elaborate_modules(
    elab_jobs: ElabJobs,
    lookups: &mut Lookups,
    modules: &mut ModuleMap,
    functors: &FunctorMap,
    def_map: &mut DefMap,
    module_graph: &mut ModuleGraph,
    interner: &mut StrInterner,
    report: &mut Report,
) {
    // Standard name for parent scope access
    let super_name = ModuleName::new(interner.intern("super"));

    // 1. SETUP PARENT "SUPER" SCOPES
    // Wire up parent module links using your updated 3-argument signature
    for (sym, module) in modules.iter_mut() {
        let mut dependents = module_graph.dependents_of(*sym);
        if let Some(parent) = dependents.next() {
            module.names.insert_module(super_name, Vis::None, *parent);
        }
        assert!(dependents.next().is_none(), "Multiple parent modules found");
    }

    // 2. INITIALIZE WORKLIST & STRUCTURAL TRACKING
    let mut worklist = elab_jobs;

    // Track unresolved jobs globally by their parent scope and bound name
    let mut unresolved_binds: UnresolvedBinds = worklist
        .iter()
        .map(|job| (job.definition_scope, job.name))
        .collect();

    // 3. EXECUTION FIXED-POINT LOOP
    while !worklist.is_empty() {
        let mut progress_made_this_pass = false;
        let current_pass_size = worklist.len();

        // Process the current generation snapshot
        for _ in 0..current_pass_size {
            let mut job = worklist.pop_front().unwrap();
            let tracking_key = (job.definition_scope, job.name);

            // Isolate the context borrow so it drops cleanly before we mutate
            // the worklist or environments outside of the step execution
            let step_result = {
                let mut ctx = ElabCtx {
                    modules,
                    functors,
                    unresolved_binds: &mut unresolved_binds,
                    worklist: &mut worklist,
                    lookups,
                    module_graph,
                    super_name,
                };
                job.expr.step(job.definition_scope, &mut ctx)
            };

            match step_result {
                StepResult::Blocked => {
                    // Re-queue the job. Its internal state machine mutations are saved in-place!
                    worklist.push_back(job);
                }
                StepResult::Error(err) => {
                    report.add_diagnostic(err);
                    unresolved_binds.remove(&tracking_key);
                }
                StepResult::Done(final_evaluated_sym) => {
                    // Success! Bind the name in its parent scope to the canonical symbol
                    modules[&job.definition_scope].names.insert_module(
                        job.name,
                        job.vis,
                        final_evaluated_sym,
                    );
                    def_map.insert_module(final_evaluated_sym, job.id, job.loc);

                    // Maintain dependency mapping integrity across true module aliases
                    module_graph.add_dependency(job.definition_scope, final_evaluated_sym);

                    unresolved_binds.remove(&tracking_key);
                    progress_made_this_pass = true;
                }
            }
        }

        // 4. FIXED-POINT STAGNATION (DEADLOCK) CHECK
        if !progress_made_this_pass && !worklist.is_empty() {
            for blocked in &worklist {
                let loc = match &blocked.expr {
                    ElabExpr::Path(p) => p.loc,
                    ElabExpr::FunctorAppl(f) => f.loc,
                    ElabExpr::Done(_) => continue,
                };

                report.add_diagnostic(
                    Diagnostic::error(loc, "Module dependency cycle detected").with_notes([
                        format!(
                            "The binding for module '{:?}' is deadlocked in a circular chain.",
                            blocked.name
                        ),
                    ]),
                );
            }
            break;
        }
    }
}
