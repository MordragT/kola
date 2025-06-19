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
//! 1. Create Typer with fresh LexicalScope
//! 2. Gather constraints for the bind and add to module-local Constraint-Set
//! 3. Create fresh monotype variable and constrain it with bind's inferred type
//! 4. Insert type variable into module-local TypeEnv (available for subsequent binds)
//!
//! ### Module Finalization
//! 1. Solve all accumulated constraints for value binds
//! 2. Apply substitution to all value bind types
//! 3. Generalize all value binds (converting MonoTypes to PolyTypes)
//! 4. Merge finalized types into global TypeEnv
//!
//! ## Key Design Decisions
//!
//! - **Module boundaries as constraint solving points**: Each module's constraints
//!   are solved together, respecting semantic boundaries for type finalization
//! - **Immediate type bind resolution**: Type binds are solved immediately since
//!   subsequent binds need access to the resolved type definitions
//! - **Deferred value bind resolution**: Value binds use placeholders during constraint
//!   generation and are resolved together for better type inference
//! - **No let-bind generalization**: Local let-binds remain monomorphic for algorithmic
//!   simplicity while maintaining expressiveness through top-level polymorphism

use kola_print::prelude::*;
use kola_resolver::{
    forest::Forest,
    info::ModuleGraph,
    prelude::Topography,
    print::ResolutionDecorator,
    resolver::{TypeOrders, ValueOrders},
    scope::ModuleScopes,
};
use kola_span::{IntoDiagnostic, Issue, Report};
use kola_tree::{
    meta::MetaView,
    print::{Decorators, TreePrinter},
};
use kola_utils::{fmt::StrInternerExt, interner::StrInterner};
use log::debug;

use crate::{
    constraints::Constraints,
    env::{GlobalTypeEnv, KindEnv, ModuleTypeEnv},
    phase::{TypeAnnotations, TypedNodes},
    print::TypeDecorator,
    substitute::{Substitutable, Substitution},
    typer::Typer,
    types::{ModuleType, MonoType},
};

#[derive(Debug, Clone, Default)]
pub struct TypeCheckOutput {
    pub global_env: GlobalTypeEnv,
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
/// 1. **Type bind processing**: Immediately solve constraints for type definitions
/// 2. **Value bind constraint generation**: Create constraints and placeholder types
/// 3. **Module-level constraint solving**: Resolve all value bind constraints together
/// 4. **Generalization and finalization**: Convert to polymorphic types and merge into global environment
pub fn type_check(
    forest: &Forest,
    topography: &Topography,
    module_graph: &ModuleGraph,
    module_scopes: &ModuleScopes,
    value_orders: &ValueOrders,
    type_orders: &TypeOrders,
    arena: &Bump,
    interner: &mut StrInterner,
    report: &mut Report,
    print_options: PrintOptions,
) -> TypeCheckOutput {
    let mut global_env = GlobalTypeEnv::new();
    let mut kind_env = KindEnv::new();
    let mut type_annotations = TypeAnnotations::new();

    let module_order = match module_graph.topological_sort() {
        Ok(order) => order,
        Err(_cycle) => {
            // TODO error reporting isn't great here,
            // but it is hard to know where the exact error is,
            // still should report the cycle.
            report.add_issue(Issue::error("Module cycle detected", 0));
            return TypeCheckOutput::default();
        }
    };

    for module_sym in module_order {
        let module_scope = module_scopes[&module_sym].clone();
        let info = module_scope.info;

        let tree = &*forest[info.source];
        let spans = topography[info.source].clone();
        let value_order = &value_orders[&module_sym];
        let type_order = &type_orders[&module_sym];

        let mut subs = Substitution::empty();
        let mut module_annotations = TypedNodes::new();
        let mut module_env = ModuleTypeEnv::new();

        for &type_sym in type_order {
            let id = module_scope.defs[type_sym].id();

            let mut constraints = Constraints::new();

            let typer = Typer::new(
                id,
                spans.clone(),
                &mut constraints,
                &module_env,
                &global_env,
                interner,
                &module_scope.resolved,
            );

            let Some(typed_nodes) = typer.run(tree, report) else {
                // If there are errors in type checking types, break out early
                break;
            };

            if let Err((errs, loc)) = constraints.solve(&mut subs, &mut kind_env) {
                let diag = interner.display(&errs).into_diagnostic(loc);
                report.add_diagnostic(diag);
                break;
            }

            let type_ = typed_nodes.meta(id).clone().apply(&mut subs);

            // TODO generalize ?

            global_env.insert_type(type_sym, type_);
            module_annotations.extend(typed_nodes);
        }

        if !report.is_empty() {
            // If there are errors in type checking types, break out early
            break;
        }

        let mut constraints = Constraints::new();

        for &value_sym in value_order {
            let id = module_scope.defs[value_sym].id();

            let typer = Typer::new(
                id,
                spans.clone(),
                &mut constraints,
                &module_env,
                &global_env,
                interner,
                &module_scope.resolved,
            );

            let Some(typed_nodes) = typer.run(tree, report) else {
                // If there are errors in type checking types, break out early
                break;
            };

            let actual_type = typed_nodes.meta(id).to_mono().unwrap();
            let type_var = MonoType::variable();

            // Create constraint: type_var = actual_type
            constraints.constrain(type_var.clone(), actual_type, *spans.meta(id));

            // Insert type variable as placeholder
            module_env.insert(value_sym, type_var);
            module_annotations.extend(typed_nodes);
        }

        if !report.is_empty() {
            // If there are errors in type checking values, break out early
            break;
        }

        if let Err((errs, loc)) = constraints.solve(&mut subs, &mut kind_env) {
            let diag = interner.display(&errs).into_diagnostic(loc);
            report.add_diagnostic(diag);
            break;
        }

        module_annotations.apply_mut(&mut subs);

        for (value_sym, mono_t) in module_env {
            let poly_type = mono_t.apply(&mut subs).generalize(&[]); // TODO should bound be something ? &type_env.bound_vars()
            global_env.insert_value(value_sym, poly_type.clone()); // replace

            // Also update module annotations with the final type
            let id = module_scope.defs[value_sym].id();
            *module_annotations.meta_mut(id) = poly_type;
        }

        let module_type = ModuleType::from(module_scope.shape.clone());
        global_env.insert_module(module_sym, module_type);

        let resolution_decorator = ResolutionDecorator(&module_scope.resolved);
        let type_decorator = TypeDecorator(&module_annotations);
        let decorators = Decorators::new()
            .with(&resolution_decorator)
            .with(&type_decorator);

        let tree_printer = TreePrinter::new(&tree, &interner, decorators, info.id);

        debug!(
            "{} SourceId {}, ModuleSym {}\n{}",
            "Typed Abstract Syntax Tree".bold().bright_white(),
            info.source,
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
