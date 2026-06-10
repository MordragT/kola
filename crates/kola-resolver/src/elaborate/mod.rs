use std::collections::HashSet;

use kola_span::{Diagnostic, Report};
use kola_tree::node::{ModuleName, Vis};
use kola_utils::interner::StrInterner;

use crate::{
    def::DefMap,
    env::{FunctorMap, ModuleMap},
    lookup::Lookups,
    symbol::{ModuleGraph, ModuleSym},
};

mod job;

pub use job::{ElabJob, ElabJobs};

pub fn elaborate_modules(
    elab_jobs: ElabJobs,
    lookups: &mut Lookups,
    modules: &mut ModuleMap,
    functors: &FunctorMap,
    defs: &mut DefMap,
    module_graph: &mut ModuleGraph,
    interner: &mut StrInterner,
    report: &mut Report,
) {
    // Standard name for parent scope access
    let super_name = ModuleName::new(interner.intern("super"));

    // 1. SETUP PARENT "SUPER" SCOPES
    // Wire up parent module links in the environment graph based on the known module hierarchy.
    for (sym, module) in modules.iter_mut() {
        let mut dependents = module_graph.dependents_of(*sym);
        if let Some(parent) = dependents.next() {
            module.names.insert_module(super_name, *parent);
        }
        assert!(dependents.next().is_none(), "Multiple parent modules found");
    }

    // 2. INITIALIZE WORKLIST & TRACKING
    let mut worklist = elab_jobs;

    // Track symbols whose bindings are still being actively evaluated.
    // If a path or functor points here, it must block until the target is removed from this set.
    let mut unresolved_binds: HashSet<ModuleSym> = worklist.iter().map(|c| c.bind()).collect();

    // 3. EXECUTION FIXED-POINT LOOP (OUTER)
    // Split processing into discrete "generations".
    // Each pass processes a snapshot of the current worklist size.
    while !worklist.is_empty() {
        let mut progress_made_this_pass = false;
        let current_pass_size = worklist.len();

        // INNER Pass: Process the Current Generation
        for _ in 0..current_pass_size {
            let job = worklist.pop_front().unwrap();
            let parent = job.parent();
            let bind = job.bind();
            let loc = job.loc();

            match job {
                ElabJob::Path { id, path, .. } => {
                    let mut current_sym = parent;
                    let mut is_blocked = false;

                    // Iteratively walk through the sub-module segments
                    for name in &path {
                        let current_module = &modules[&current_sym];

                        if let Some(next_sym) = current_module.names.get_module(*name) {
                            // If a segment depends on an un-evaluated module, this entire path is blocked
                            // Check this first so that its sound to access `defs` in the next step,
                            // without worrying about missing entries due to constraint ordering
                            if unresolved_binds.contains(&next_sym) {
                                is_blocked = true;
                                break;
                            }

                            // Enforce export visibility rules if we cross past our local module boundary
                            if current_sym != parent && defs[next_sym].vis != Vis::Export {
                                report
                                    .add_diagnostic(Diagnostic::error(loc, "Module not exported"));
                                break;
                            }

                            current_sym = next_sym;
                        } else {
                            report.add_diagnostic(Diagnostic::error(loc, "Module not found"));
                            break;
                        }
                    }

                    if is_blocked {
                        // Re-queue the intact path to try again during the next generation pass
                        worklist.push_back(ElabJob::Path {
                            id,
                            parent,
                            bind,
                            loc,
                            path,
                        });
                    } else if modules.contains_key(&current_sym) {
                        // Success! Clone the targeted module structure into this local alias binding slot
                        let targeted_module = modules[&current_sym].clone();
                        modules.insert(bind, targeted_module);

                        let def = defs[current_sym];
                        defs.insert_module(bind, def);

                        // TODO this could break things I guess because the symbol is not its unique identity,
                        // meaning, that the module graph might become useless here.
                        module_graph.add_dependency(parent, current_sym);

                        unresolved_binds.remove(&bind);
                        progress_made_this_pass = true;
                    }
                }
                ElabJob::Functor {
                    id,
                    path,
                    functor,
                    args,
                    ..
                } => {
                    // Resolve the prefix path container module where the functor declaration resides
                    let container_sym = if let Some(p_sym) = path {
                        if unresolved_binds.contains(&p_sym) {
                            worklist.push_back(ElabJob::Functor {
                                id,
                                parent,
                                bind,
                                path,
                                loc,
                                functor,
                                args,
                            });
                            continue; // Native loop control safe: skips rest of body
                        }
                        p_sym
                    } else {
                        parent
                    };

                    let container_module = &modules[&container_sym];
                    let Some(functor_sym) = container_module.names.get_functor(functor) else {
                        report.add_diagnostic(Diagnostic::error(loc, "Functor not found"));
                        unresolved_binds.remove(&bind);
                        continue;
                    };

                    // Check if any argument modules passed to the functor are still unresolved
                    let any_arg_blocked = args.iter().any(|arg| unresolved_binds.contains(arg));
                    if any_arg_blocked {
                        worklist.push_back(ElabJob::Functor {
                            id,
                            parent,
                            bind,
                            path,
                            loc,
                            functor,
                            args,
                        });
                    } else {
                        let functor_def = functors[&functor_sym].clone();
                        if functor_def.params.len() != args.len() {
                            report.add_diagnostic(Diagnostic::error(
                                loc,
                                "Wrong number of arguments",
                            ));
                            unresolved_binds.remove(&bind);
                            continue;
                        }

                        // --- INSTANTIATION ---
                        // Evaluate the functor body. This generates a fresh module structure with all parameter references substituted for the provided argument modules.
                        let (mut inst_body, inst_lookups, inst_elab_jobs) =
                            functor_def.apply(bind, args);

                        // Set up the local instance's "super" pointer dynamically to its callsite parent
                        inst_body.names.insert_module(super_name, parent);

                        // Feed the newly discovered internal jobs to the back of the worklist.
                        // They will wait safely to be evaluated in subsequent generation passes.
                        for job in inst_elab_jobs {
                            unresolved_binds.insert(job.bind());
                            worklist.push_back(job);
                        }
                        lookups.append(inst_lookups);

                        // Cache the fully elaborated module body into the global module map under this instance's unique symbol.
                        modules.insert(bind, inst_body);
                        module_graph.add_dependency(parent, bind);

                        unresolved_binds.remove(&bind);
                        progress_made_this_pass = true;
                    }
                }
            }
        }

        // 4. FIXED-POINT STAGNATION CHECK
        // If an entire generation completed and we were unable to resolve even a single item,
        // we have reached complete stagnation due to circular dependencies.
        if !progress_made_this_pass && !worklist.is_empty() {
            for blocked in &worklist {
                report.add_diagnostic(
                    Diagnostic::error(blocked.loc(), "Module dependency cycle detected")
                        .with_notes([format!(
                            "Module binding is deadlocked due to cyclic dependencies."
                        )]),
                );
            }
            break;
        }
    }
}
