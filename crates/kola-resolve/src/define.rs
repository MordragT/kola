use std::rc::Rc;

use kola_span::Diagnostic;
use kola_tree::node::{self, Vis};
use kola_utils::interner::StrKey;

use crate::{
    context::ResolveContext,
    info::ModuleDef,
    scope::{ModuleScope, Rib},
    symbol::ModuleSymbol,
};

// pub fn define(scope: ModuleScope, ctx: &mut ResolveContext) {
//     let ModuleScope {
//         symbol,
//         loc,
//         modules,
//         types,
//         values,
//         mut paths,
//     } = scope;

//     ctx.pending.remove(&symbol);
//     ctx.active.insert(symbol);

//     // TODO I should probably track insertion order, to make the whole algorithm predictable
//     for (sym, bind) in &modules {
//         if let Some(scope) = ctx.module_scopes.get(sym) {
//             ctx.def_table.insert_module(*sym, ModuleDef::IsScope);
//         } else if let Some(path) = paths.remove_by_key(sym) {
//             // let dependents = ctx.module_graph.dependents_of(*sym);

//             // let parent = if dependents.is_empty() {
//             //     None
//             // } else if let [parent] = dependents {
//             //     Some(*parent)
//             // } else {
//             //     panic!("At this point every module must only have one parent or none")
//             // };

//             // let target_sym = match define_path(&path, parent, &modules, ctx).ok_or(
//             //     Diagnostic::error(bind.loc, "Module not found")
//             //         .with_help("Maybe the module is not marked to be exported."),
//             // ) {
//             //     Ok(key) => key,
//             //     Err(e) => {
//             //         ctx.report.add_diagnostic(e);
//             //         continue;
//             //     }
//             // };

//             if let Some(target_sym) = define_path(&path, &mut paths, &modules, ctx) {
//                 // TODO instead of def_table maybe some sort of substitution buildup,
//                 // which I then defer to one last stage where I actually substitute and define
//                 // so that later lookups do not need to do essentially a linked list lookup ?
//                 // Maybe just an Rc<ModuleScope> ??
//                 ctx.def_table
//                     .insert_module(*sym, ModuleDef::IsLink(target_sym));
//             } else {
//                 // Error already reported in define_path
//                 continue;
//             }
//         } else {
//             // This means that the symbol is neither a inline module, a import nor a path,
//             // which should not be possible at this point.
//             unreachable!()
//         }
//     }

//     ctx.active.remove(&symbol);
//     ctx.module_scopes.insert(
//         symbol,
//         ModuleScope {
//             symbol,
//             loc,
//             modules,
//             types,
//             values,
//             paths,
//         },
//     );
// }

pub fn define(scope: Rc<ModuleScope>, ctx: &mut ResolveContext) {
    ctx.pending.remove(&scope.symbol());
    ctx.active.insert(scope.symbol());

    // TODO It is a bit confusing that Symbol<ModuleBind> might have no ModuleBind (if it is a ModulePath) node.
    // Maybe I should really make the Symbol abstraction independent of the node type.
    for (sym, path) in &scope.paths {
        define_path(*sym, path, scope.clone(), ctx);
    }

    ctx.active.remove(&scope.symbol);
}

// TODO return error type instead of on the fly reporting.
pub fn define_path<'s>(
    module_sym: ModuleSymbol,
    mut path: &[StrKey],
    mut scope: Rc<ModuleScope>,
    ctx: &'s mut ResolveContext,
) {
    ctx.pending.remove(&module_sym);
    ctx.active.insert(module_sym);

    while let Some((name, rest)) = path.split_first() {
        // TODO do not special case super here, but rather insert it in the scope itsself.

        let Some(sym) = scope.modules.lookup_sym(*name) else {
            ctx.report
                .add_diagnostic(Diagnostic::error(scope.loc, "Module not found"));
            ctx.active.remove(&module_sym);
            return;
        };

        let bind = scope.modules[sym];

        if bind.vis != Vis::Export {
            ctx.report.add_diagnostic(
                Diagnostic::error(bind.loc, "Module not exported")
                    .with_help("Only exported modules can be used in paths."),
            );
            ctx.active.remove(&module_sym);
            return;
        }

        if ctx.active.contains(&sym) {
            ctx.report
                .add_diagnostic(Diagnostic::error(bind.loc, "Module cycle detected"));
            ctx.active.remove(&module_sym);
            return;
        }

        if ctx.pending.contains(&sym) {
            if let Some(scope) = ctx.module_scopes.get(&sym) {
                define(scope.clone(), ctx);
            } else if let Some(path) = scope.paths.get_by_key(&sym) {
                define_path(sym, &path, scope.clone(), ctx);
            } else {
                unreachable!()
            }
        }

        scope = if let Some(scope) = ctx.module_scopes.get(&sym) {
            scope.clone()
        } else {
            ctx.report
                .add_diagnostic(Diagnostic::error(bind.loc, "Module scope not found"));
            ctx.active.remove(&module_sym);
            return;
        };

        path = rest;
    }

    ctx.active.remove(&module_sym);
    ctx.module_scopes.insert(module_sym, scope);
}

// TODO This algorithm doesnt work exactly how I want it to.
// It doesnt handle visibility errors, therefore the error is too generic (can be not found or visibilty error).
// Secondly it doesnt handle the case where the target is a alias itsself that,
// was declared after this alias was declared.
// fn define_path(
//     path: &[StrKey],
//     paths: &mut Bi
//     modules: &Rib<node::ModuleBind>,
//     ctx: &mut ResolveContext,
// ) -> Option<ModuleSymbol> {
//     let sup = ctx.interner.intern("super");

//     let (name, path) = segments.split_first()?;
//     let mut module_sym = if *name == sup {
//         parent?
//     } else {
//         let symbol = modules.lookup_sym(*name)?;
//         let info = modules.get_by_sym(symbol)?;

//         if info.vis != Vis::Export {
//             return None; // if the module is not exported, we cannot resolve it
//         }

//         symbol
//     };

//     for name in path {
//         if ctx.pending.contains(&module_sym) {
//             let scope = ctx.module_scopes.remove(&module_sym)?;
//             define(scope, ctx);
//         }

//         if ctx.active.contains(&module_sym) {
//             // TODO this should probably return an cycle error
//             return None;
//         }

//         let scope = ctx.module_scopes.get(&module_sym)?; // when module_sym points to another module_path here it will fail
//         module_sym = scope.modules.lookup_sym(*name)?;

//         let info = scope.modules.get_by_sym(module_sym)?;
//         if info.vis != Vis::Export {
//             return None; // if the module is not exported, we cannot resolve ito safe to unwrap
//         }
//     }

//     // TODO instead of returning, insert into context def_table
//     Some(module_sym)
// }
