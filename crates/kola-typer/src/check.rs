//! This module implements the main type checking algorithm that processes modules
//! in dependency order and coordinates constraint generation, solving, and generalization.
//!
//! ## Algorithm Implementation
//!
//! The type checker follows this precise algorithm for each module:
//!
//! ### Type Binds Processing
//! 1. Create local Constraint-Set and Typer with fresh LexicalScope
//! 2. Gather constraints for the bind and add to Constraint-Set
//! 3. Solve constraints immediately (update module-local Substitution and KindEnv)
//! 4. Apply substitution to the bind
//! 5. Insert resolved type into module-local TypeEnv (available for subsequent binds)
//!
//! ### Value Binds Processing
//! 1. Create local Constraint-Set and Typer with fresh LexicalScope
//! 2. Gather constraints for the bind and add to fresh Constraint-Set
//! 3. Solve constraints immediately (update module-local Substitution and KindEnv)
//! 4. Apply substitution to the inferred type
//! 5. Generalize the type immediately
//! 6. Insert generalized type into module-local TypeEnv (available for subsequent binds)
//!
//! ### Module Finalization
//! 1. Apply final substitution to all annotations
//! 2. Merge generalized types into global TypeEnv
//!
//! ## Key Design Decisions
//!
//! - **Incremental constraint solving**: Each bind's constraints are solved immediately,
//!   allowing subsequent binds to use the fully resolved and generalized types
//! - **Immediate generalization**: Both type and value binds are generalized as soon as
//!   they are processed, ensuring consistent polymorphic availability
//! - **Module-local substitution accumulation**: Substitutions accumulate across binds
//!   within a module, preserving type variable relationships
//! - **No let-bind generalization**: Local let-binds remain monomorphic for algorithmic
//!   simplicity while maintaining expressiveness through top-level polymorphism
//!

use kola_builtins::BuiltinLexicon;
use kola_print::prelude::*;
use kola_resolver::{
    def::DefMap,
    env::ModuleMap,
    print::ResolutionDecorator,
    resolve::{TypeOrders, ValueOrders},
    symbol::{ModuleSym, ValueSym},
};
use kola_span::{IntoDiagnostic, Report};
use kola_syntax::loc::LocMap;
use kola_tree::{
    meta::MetaView,
    print::{Decorators, TreePrinter},
    tree::TreeMap,
};
use kola_types::{
    class::TypeClassEnv,
    env::TypeEnv,
    substitute::{Substitutable, Substitution},
};
use kola_utils::{interner::StrInterner, interner_ext::InternerExt};
use log::trace;

use crate::{
    analysis::exhaust_check_all,
    constraints::Constraints,
    phase::{TypeAnnotations, TypedNodes},
    print::TypeDecorator,
    typer::Typer,
};

#[derive(Debug, Clone, Default)]
pub struct TypeCheckOutput {
    pub global_env: TypeEnv,
    pub type_annotations: TypeAnnotations,
}

/// Performs type checking on a collection of modules using constraint-based inference.
///
/// This function implements the main type checking algorithm that processes modules in
/// dependency order, ensuring that each module's dependencies are fully typed before
/// processing dependent modules.
///
/// # Algorithm Steps
///
/// For each module in topological dependency order:
/// 1. **Type bind processing**: Immediately solve constraints and generalize type definitions
/// 2. **Value bind processing**: For each bind, generate constraints, solve immediately, and generalize
/// 3. **Module finalization**: Apply final substitutions and merge into global environment
///
/// This incremental approach ensures that each binding is available in its fully generalized
/// form to subsequent bindings within the same module, maintaining consistency with cross-module usage.
pub fn type_check(
    tree_map: &TreeMap,
    loc_map: &LocMap,
    modules: &ModuleMap,
    defs: &DefMap,
    entry_points: &[ValueSym],
    module_order: &[ModuleSym],
    type_orders: &TypeOrders,
    value_orders: &ValueOrders,
    arena: &Bump,
    str_interner: &mut StrInterner,
    lexicon: &BuiltinLexicon,
    report: &mut Report,
    print_options: PrintOptions,
) -> TypeCheckOutput {
    let mut global_env = TypeEnv::new();
    let mut class_env = TypeClassEnv::new();
    let mut type_annotations = TypeAnnotations::new();

    for &module_sym in module_order {
        let module = modules[&module_sym].clone();
        let module_def = defs[module_sym];

        let tree = &tree_map[&module_def.loc.path];
        let spans = &loc_map[&module_def.loc.path];
        let value_order = &value_orders[&module_sym];
        let type_order = &type_orders[&module_sym];

        let mut subs = Substitution::empty(); // TODO better if this is bind local ?
        let mut module_annotations = TypedNodes::new();
        let mut module_env = TypeEnv::new();

        for &type_sym in type_order {
            let def = defs[type_sym];

            let mut constraints = Constraints::new();

            let typer = Typer::new(
                def.id(),
                spans,
                &module_env,
                &global_env,
                &modules,
                &defs,
                &module.nodes,
                entry_points,
                &mut constraints,
                str_interner,
                lexicon,
            );

            let Some((typed_nodes, _)) = typer.run(tree, report) else {
                // If there are errors in type checking types, break out early
                break;
            };

            if let Err((errs, loc)) = constraints.solve(&mut subs, &mut class_env) {
                let diag = str_interner.with(&errs).into_diagnostic(loc);
                report.add_diagnostic(diag);
                break;
            }

            let type_ = typed_nodes.meta(def.id()).clone().apply(&mut subs);

            // TODO generalize ?

            module_env.insert_type(type_sym, def, type_);
            module_annotations.extend(typed_nodes);
        }

        if !report.is_empty() {
            // If there are errors in type checking types, break out early
            break;
        }

        for &value_sym in value_order {
            let def = defs[value_sym];

            let mut constraints = Constraints::new();

            let typer = Typer::new(
                def.id(),
                spans,
                &module_env,
                &global_env,
                &modules,
                &defs,
                &module.nodes,
                entry_points,
                &mut constraints,
                str_interner,
                lexicon,
            );

            let Some((mut typed_nodes, cases)) = typer.run(tree, report) else {
                // If there are errors in type checking types, break out early
                break;
            };

            if let Err((errs, loc)) = constraints.solve(&mut subs, &mut class_env) {
                let diag = str_interner.with(&errs).into_diagnostic(loc);
                report.add_diagnostic(diag);
                break;
            }

            // Apply the substitution to the typed nodes
            typed_nodes.apply_mut(&mut subs);

            // Check for exhaustiveness in case expressions
            if let Err(errs) = exhaust_check_all(&cases, tree, &typed_nodes, &*spans) {
                report.extend_diagnostics(errs.into_iter().map(Into::into));
                break;
            }

            // Generalize immediately (making it available for subsequent binds)
            let actual_t = typed_nodes.meta(def.id()).to_mono().unwrap();
            let poly_type = actual_t.generalize(&[]); // TODO should bound be something ? &type_env.bound_vars()
            module_env.insert_value(value_sym, def, poly_type.clone());

            // Update annotations with the final type
            *typed_nodes.meta_mut(def.id()) = poly_type;
            module_annotations.extend(typed_nodes);
        }

        if !report.is_empty() {
            // If there are errors in type checking values, break out early
            break;
        }

        module_annotations.apply_mut(&mut subs);
        global_env.merge(module_env);

        let resolution_decorator = ResolutionDecorator(&module.nodes);
        let type_decorator = TypeDecorator(&module_annotations);
        let decorators = Decorators::new()
            .with(&resolution_decorator)
            .with(&type_decorator);

        let tree_printer = TreePrinter::new(&tree, &str_interner, decorators, module_def.id);

        trace!(
            "{} SourceId {}, ModuleSym {}\n{}",
            "Typed Abstract Syntax Tree".bold().bright_white(),
            module_def.loc.path,
            module_sym,
            tree_printer.render(print_options, arena)
        );

        type_annotations.insert(module_sym, module_annotations);
    }

    TypeCheckOutput {
        global_env,
        type_annotations,
    }
}
