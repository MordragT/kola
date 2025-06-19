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
    env::{KindEnv, TypeEnv},
    phase::{TypeAnnotations, TypedNodes},
    print::TypeDecorator,
    scope::{BoundVars, ModuleTypeScope},
    substitute::{Substitutable, Substitution},
    typer::{Constraints, Typer},
    types::{ModuleType, MonoType, PolyType},
};

#[derive(Debug, Clone, Default)]
pub struct TypeCheckOutput {
    pub type_env: TypeEnv,
    pub type_annotations: TypeAnnotations,
}

/*

- module boundaries are natural points for constraint solving,
    because they represent semantic boundaries
    where types need to be finalized for export.
- generalization must happen after constraint solving not before
    - TODO okay what about let-bindings then ?
    - Solution: don't generalize let-bindings, only value-binds
- instantiatiated types must be tracked inside a type environment or constraint set

-------------------------------------------------

ModuleEnv: Environment for Module data, holds per module: value types, type types, module types and their kinds
ConstraintSet: Set of constraints to be solved per module
Substitution: Substitutions for type variables, built up during constraint solving
KindEnv: Environment for kinds, holds per module: type kinds (numerical, equatable, row etc.)
TypeEnv: Environment for types, holds per module: value types, type types, module types
LexicalScope: Holds types referenced by name, not by symbol
Typer: Visits AST nodes, gathers constraints and fills module local TypeEnv

1. Create a global ModuleEnv (TODO my KindEnv and TypeEnv are global using stable identifiers (symbols), so no need anymore for ModuleEnv)

For each module in module-order:

    1. Create a module local Substitution, TypeEnv and KindEnv (use ModuleEnv for dependencies)

    For each type-bind in type-order:
        1. Create a Constraint-Set and Typer with a new Lexical Scope
        2. Gather constraints for the bind and add to Constraint-Set
        3. Solve constraints for this bind (and thus update the module local Substitution and KindEnv)
        4. Apply the module local substitution to the bind
        5. Generalize the bind (probably not needed because they are already PolyTypes)
        6. Insert into the module local TypeEnv (making it available for subsequent binds)

    2. Create a module local Constraint-Set for value-bind constraints.

    For each value-bind in value-order:
        1. Create a Typer with a new Lexical Scope
        2. Gather constraints for the bind and add to module local Constraint-Set
        3. Create a fresh monotype variable and constraint it with the bind's current type (insert into Constraint-Set)
        4. Insert this type variable (MonoType) into the module local TypeEnv (making it available for subsequent binds)

    3. Solve the module local Constraint-Set with the module local Substitution and KindEnv
    4. Apply the substitution to value-binds inside the module local TypeEnv
    5. Generalize all value-binds in the TypeEnv changing them from being MonoTypes to PolyTypes
    6. Insert the generalized value-binds and the type-binds data inside the TypeEnv into the global ModuleEnv (TODO: visibility)

 */
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
    let mut type_env = TypeEnv::new();
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
        let mut module_type_scope = ModuleTypeScope::new();

        for &type_sym in type_order {
            let id = module_scope.defs[type_sym].id();

            let mut constraints = Constraints::new();

            let typer = Typer::new(
                id,
                spans.clone(),
                &mut constraints,
                &module_type_scope,
                &type_env,
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

            type_env.insert_type(type_sym, type_);
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
                &module_type_scope,
                &type_env,
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
            module_type_scope.insert(value_sym, type_var);
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

        for (value_sym, mono_t) in module_type_scope {
            let poly_type = mono_t.apply(&mut subs).generalize(&[]); // TODO should bound be something ? &type_env.bound_vars()
            type_env.insert_value(value_sym, poly_type.clone()); // replace

            // Also update module annotations with the final type
            let id = module_scope.defs[value_sym].id();
            *module_annotations.meta_mut(id) = poly_type;
        }

        let module_type = ModuleType::from(module_scope.shape.clone());
        type_env.insert_module(module_sym, module_type);

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
        type_env,
        type_annotations,
    }
}
