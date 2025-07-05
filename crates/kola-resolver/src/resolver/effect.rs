use kola_span::{Diagnostic, Report};
use kola_tree::meta::MetaMapExt;
use log::debug;

use super::EffectOrders;
use crate::{
    constraints::{EffectBindConst, EffectConst},
    phase::ResolvedEffect,
    scope::{ModuleScope, ModuleScopes},
};

pub struct EffectResolution {
    pub effect_orders: EffectOrders,
}

pub fn resolve_effects(scopes: &mut ModuleScopes, report: &mut Report) -> EffectResolution {
    let mut effect_orders = EffectOrders::new();

    for (sym, scope) in scopes {
        resolve_effects_in_module(scope, report);

        match scope.effect_graph.topological_sort() {
            Ok(order) => {
                effect_orders.insert(*sym, order);
            }
            Err(cycle) => {
                report.add_diagnostic(
                    Diagnostic::error(scope.info.loc, cycle.to_string())
                        .with_help("Check for circular dependencies in effect definitions."),
                );

                debug!("Cycle detected inside:\n{}", scope.effect_graph.to_dot());
            }
        }
    }

    EffectResolution { effect_orders }
}

fn resolve_effects_in_module(scope: &mut ModuleScope, report: &mut Report) {
    // Resolve effect references similar to value references
    for &EffectBindConst {
        name,
        id,
        source,
        loc,
    } in scope.cons.effect_binds()
    {
        if let Some(target) = scope.shape.get_effect(name) {
            scope.effect_graph.add_dependency(source, target);
            scope
                .resolved
                .insert_meta(id, ResolvedEffect::Reference(target));
        } else {
            report.add_diagnostic(
                Diagnostic::error(loc, "Effect not found")
                    .with_help("Check that the effect is defined in this module."),
            );
        }
    }

    for &EffectConst { name, id, loc } in scope.cons.effects() {
        if let Some(effect_sym) = scope.shape.get_effect(name) {
            scope
                .resolved
                .insert_meta(id, ResolvedEffect::Reference(effect_sym));
        } else {
            report.add_diagnostic(
                Diagnostic::error(loc, "Effect not found in annotation")
                    .with_help("Check that the effect is defined in this module."),
            );
        }
    }
}
