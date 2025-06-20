use kola_span::{Diagnostic, Report};
use kola_tree::meta::MetaMapExt;

use crate::{
    phase::ResolvedValue,
    refs::ConstructorRef,
    scope::{ModuleScope, ModuleScopes},
};

pub fn resolve_constructors(scopes: &mut ModuleScopes, report: &mut Report) {
    for (_sym, scope) in scopes {
        resolve_constructors_in_module(scope, report);
    }
}

fn resolve_constructors_in_module(scope: &mut ModuleScope, report: &mut Report) {
    for &ConstructorRef {
        variant,
        name,
        id,
        source,
        loc,
    } in scope.refs.constructors()
    {
        // Resolve the type name
        if let Some(type_sym) = scope.shape.get_type(variant) {
            scope
                .resolved
                .insert_meta(id, ResolvedValue::Constructor(type_sym, name));
        } else {
            report.add_diagnostic(
                Diagnostic::error(loc, "Constructor not found")
                    .with_help("Check that the variant type is defined in this module."),
            );
        }
    }
}
