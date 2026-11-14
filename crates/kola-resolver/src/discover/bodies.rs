use std::ops::ControlFlow;

use kola_builtins::{BuiltinType, find_builtin_id};
use kola_collections::scope::LinearScope;
use kola_span::{Diagnostic, Loc};
use kola_tree::{
    node::{self, TypeName, ValueName},
    prelude::*,
};

use crate::{
    lookup::{Lookups, ModuleLookup},
    phase::{NodeMap, ResolvedModuleType, ResolvedType, ResolvedValue},
    symbol::{ModuleTypeSym, TypeSym, ValueSym},
};

use super::{Inputs, ModuleState};

pub(super) type ValueScope = LinearScope<ValueName, ValueSym>;
pub(super) type TypeScope = LinearScope<TypeName, TypeSym>;

#[derive(Debug, Clone, Default)]
struct Scopes {
    value: ValueScope,
    type_: TypeScope,
}

#[derive(Debug, Clone, Copy, Default)]
struct Bindings {
    module_type: Option<ModuleTypeSym>,
    type_: Option<TypeSym>,
    value: Option<ValueSym>,
}

pub(super) struct BodyVisitor<'a> {
    input: &'a Inputs<'a>,
    state: ModuleState,
    lookups: &'a mut Lookups,
    bindings: Bindings,
    scopes: Scopes,
}

impl<'a> BodyVisitor<'a> {
    pub(super) fn new(input: &'a Inputs<'a>, state: ModuleState, lookups: &'a mut Lookups) -> Self {
        Self {
            input,
            state,
            lookups,
            bindings: Bindings::default(),
            scopes: Scopes::default(),
        }
    }

    pub(super) fn into_state(self) -> ModuleState {
        self.state
    }

    #[inline]
    fn span<T: 'static>(&self, id: Id<T>) -> Loc
    where
        Tree: TotalSpan<T, Loc>,
    {
        self.input.span(id)
    }

    fn insert_symbol<T: 'static, S: 'static>(&mut self, id: Id<T>, sym: S)
    where
        NodeMap: PartialRelationSet<T, S>,
    {
        self.state.module.nodes.set(id, sym);
    }

    /// Visit every type scheme and expression now that all top-level names exist.
    pub(super) fn visit_bodies(&mut self) {
        let tree = self.input.tree;

        for (index, (bind, _)) in tree.value_bind.iter().enumerate() {
            let id: Id<node::ValueBind> = Id::new(index as u32);
            let sym: ValueSym = *self.state.module.nodes.get_unchecked(id);

            self.bindings.value = Some(sym);

            if let Some(ty_scheme) = bind.ty_scheme {
                let _ = self.visit_type_scheme(ty_scheme, tree);
            }

            if let Some(value) = bind.value {
                let _ = self.visit_expr(value, tree);
            }

            self.bindings.value = None;
        }

        for (index, (bind, _)) in tree.type_bind.iter().enumerate() {
            let id: Id<node::TypeBind> = Id::new(index as u32);
            let sym: TypeSym = *self.state.module.nodes.get_unchecked(id);

            self.bindings.type_ = Some(sym);
            let _ = self.visit_type_scheme(bind.ty_scheme, tree);
            self.bindings.type_ = None;
        }

        for (_, (bind, _)) in tree.module_bind.iter().enumerate() {
            if let Some(ty) = bind.ty {
                let _ = self.visit_module_type(ty, tree);
            }
        }

        for (index, (bind, _)) in tree.module_type_bind.iter().enumerate() {
            let id: Id<node::ModuleTypeBind> = Id::new(index as u32);
            let sym: ModuleTypeSym = *self.state.module.nodes.get_unchecked(id);

            self.bindings.module_type = Some(sym);
            let _ = self.visit_module_type(bind.ty, tree);
            self.bindings.module_type = None;
        }
    }
}

impl<'a> Visitor<Loc> for BodyVisitor<'a> {
    type BreakValue = !;

    fn visit_qualified_module_type(
        &mut self,
        id: Id<node::QualifiedModuleType>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedModuleType { path, ty } = *tree.get(id);

        let name = *ty.get(tree);
        let loc = self.span(id);

        if let Some(path) = path {
            // Module Type defined in another module - Just visit the module path if it exists
            self.visit_module_path(path, tree)?;
        } else if let Some(binding) = self.state.module.names.get_module_type(name) {
            // Found a module type binding in the current module scope
            // which was defined before this reference to it (no forward reference)

            self.insert_symbol(id, ResolvedModuleType(binding.sym));

            // Qualified Module Types can occur in both module type binds and module type annotations.
            // Only in the former case we need to add a dependency.
            if let Some(current_sym) = self.bindings.module_type {
                // Add dependency from the current type bind to this type
                self.state.module_type_graph.add_dependency(current_sym, binding.sym);
            }
        }
        else {
            // Every top-level name is collected before bodies are visited, so
            // this can no longer be a forward reference.
            self.state.report.add_diagnostic(
                Diagnostic::error(loc, "Module type not found").with_help(
                    "Check that the module type is defined in this module.",
                ),
            );
        }
        ControlFlow::Continue(())
    }

    fn visit_module_path(
        &mut self,
        id: Id<node::ModulePath>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let path = tree
            .get(id)
            .0
            .get(tree)
            .iter()
            .map(|id| *id.get(tree))
            .collect::<Vec<_>>();

        let loc = self.span(id);

        // This visit method will only be called from module path's in value position
        self.lookups
            .insert_module(ModuleLookup::new(path, id, self.input.sym, loc));

        ControlFlow::Continue(())
    }

    fn visit_let_expr(&mut self, id: Id<node::LetExpr>, tree: &Tree) -> ControlFlow<Self::BreakValue> {
        let node::LetExpr {
            name,
            value_type,
            value,
            body,
        } = *tree.get(id);

        let name = *tree.get(name);

        if let Some(type_) = value_type {
            self.visit_type_expr(type_, tree)?;
        }

        self.visit_expr(value, tree)?;

        let sym = ValueSym::new();
        self.insert_symbol(id, sym);

        self.scopes.value.enter(name, sym);
        self.visit_expr(body, tree)?;
        self.scopes.value.exit(&name);

        ControlFlow::Continue(())
    }

    fn visit_lambda_expr(
        &mut self,
        id: Id<node::LambdaExpr>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::LambdaExpr {
            param,
            param_type,
            body,
        } = *tree.get(id);

        let name = *tree.get(param);

        let sym = ValueSym::new();
        self.insert_symbol(id, sym);

        if let Some(param_type) = param_type {
            self.visit_type_expr(param_type, tree)?;
        }

        self.scopes.value.enter(name, sym);
        self.visit_expr(body, tree)?;
        self.scopes.value.exit(&name);

        ControlFlow::Continue(())
    }

    fn visit_case_branch(
        &mut self,
        id: Id<node::CaseBranch>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let depth = self.scopes.value.depth();
        self.walk_case_branch(id, tree)?;
        self.scopes.value.restore_depth(depth); // Restore former scope if patterns created binds

        ControlFlow::Continue(())
    }

    fn visit_handler_clause(
        &mut self,
        id: Id<node::HandlerClause>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::HandlerClause { op: _, param, body } = *tree.get(id);

        let param_name = *param.get(tree);
        let param_sym = ValueSym::new();

        self.insert_symbol(id, param_sym);
        self.scopes.value.enter(param_name, param_sym);

        self.visit_expr(body, tree)?;

        self.scopes.value.exit(&param_name);

        ControlFlow::Continue(())
    }

    fn visit_bind_pat(&mut self, id: Id<node::BindPat>, tree: &Tree) -> ControlFlow<Self::BreakValue> {
        let name = *id.get(tree).0.get(tree);
        let sym = ValueSym::new();
        self.insert_symbol(id, sym);

        self.scopes.value.enter(name, sym);
        ControlFlow::Continue(())
    }

    fn visit_list_pat(&mut self, id: Id<node::ListPat>, tree: &Tree) -> ControlFlow<Self::BreakValue> {
        let elements = &id.get(tree).0;

        for el in elements.get(tree).iter().copied() {
            match *el.get(tree) {
                node::ListElPat::Pat(pat) => self.visit_pat(pat, tree)?,
                node::ListElPat::Spread(Some(name_id)) => {
                    let name = *name_id.get(tree);
                    let sym = ValueSym::new();
                    self.insert_symbol(el, sym);

                    self.scopes.value.enter(name, sym);
                }
                _ => {
                    // Spread without name or empty list element - no action needed
                }
            }
        }

        ControlFlow::Continue(())
    }

    fn visit_record_pat(
        &mut self,
        id: Id<node::RecordPat>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordPat { fields, .. } = tree.get(id);

        for field_id in fields.get(tree).iter().copied() {
            let node::RecordFieldPat { field, pat } = *field_id.get(tree);

            if let Some(pat) = pat {
                self.visit_pat(pat, tree)?;
            } else {
                // No pattern means a binding: { a } => ...

                let name = *field.get(tree);
                let sym = ValueSym::new();
                self.insert_symbol(field_id, sym);

                self.scopes.value.enter(name, sym);
            }
        }

        ControlFlow::Continue(())
    }

    fn visit_variant_pat(
        &mut self,
        id: Id<node::VariantPat>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        // Actually I think nothing needs to be done here, for example:
        // < Some : { a } > => ...
        // This will create a binding but the binding part is handled by the Pat visitor,
        // nothing inside the VariantPat does any kind of binding.
        // However I suspect that I will need to revisit this for some kind of variant name analysis,
        // so I am keeping this here for now.

        self.walk_variant_pat(id, tree)
    }

    fn visit_qualified_expr(
        &mut self,
        id: Id<node::QualifiedExpr>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedExpr {
            module_path,
            source,
            ..
        } = *tree.get(id);

        let name = *source.get(tree);

        if let Some(path) = module_path {
            // Just visit the module path if it exists
            self.visit_module_path(path, tree)
        } else if let Some(value_sym) = self.scopes.value.get(&name) {
            // Local binding will not create value bind cycle either
            self.insert_symbol(id, ResolvedValue::Reference(*value_sym));
            ControlFlow::Continue(())
        } else if let Some(binding) = self.state.module.names.get_value(name) {
            // Found a value binding in the current module scope
            // which was defined before this path expression (no forward reference)

            self.insert_symbol(id, ResolvedValue::Reference(binding.sym));

            let current_sym = self.bindings.value.unwrap();
            self.state.value_graph.add_dependency(current_sym, binding.sym);

            ControlFlow::Continue(())
        } else if let Some(builtin) = self.input.interner.get(name.0).and_then(find_builtin_id) {
            // This is a builtin value - resolve immediately, no dependencies needed
            self.insert_symbol(id, ResolvedValue::Builtin(builtin));
            ControlFlow::Continue(())
        } else {
            // Every top-level name is collected before bodies are visited, so
            // this can no longer be a forward reference.
            self.state.report.add_diagnostic(
                Diagnostic::error(self.span(id), "Value not found").with_help(
                    "Check that the value is defined in this module or has been brought into scope.",
                ),
            );

            ControlFlow::Continue(())
        }
    }

    fn visit_type_var_bind(
        &mut self,
        id: Id<node::TypeVarBind>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeVarBind { var, .. } = *id.get(tree);

        let name = var.get(tree).0.into();
        let sym = TypeSym::new();

        self.insert_symbol(id, sym);
        self.scopes.type_.enter(name, sym);

        ControlFlow::Continue(())
    }

    fn visit_qualified_type(
        &mut self,
        id: Id<node::QualifiedType>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedType { path, ty } = *tree.get(id);

        let name = *ty.get(tree);

        let loc = self.span(id);

        if let Some(path) = path {
            // Just visit the module path if it exists
            self.visit_module_path(path, tree)
        } else if let Some(binding) = self.state.module.names.get_type(name) {
            // Found a type binding in the current module scope
            // which was defined before this type path (no forward reference)

            self.insert_symbol(id, ResolvedType::Reference(binding.sym));

            // Type Path's can occur in both type binds and type annotations.
            // Only in the former case we need to add a dependency.
            if let Some(current_sym) = self.bindings.type_ {
                // Add dependency from the current type bind to this type
                self.state.type_graph.add_dependency(current_sym, binding.sym);
            }

            ControlFlow::Continue(())
        } else if let Some(type_sym) = self.scopes.type_.get(&name) {
            // Local quantifier will not create type bind cycle either
            self.insert_symbol(id, ResolvedType::Reference(*type_sym));
            ControlFlow::Continue(())
        } else if let Some(builtin) = self.input.interner.get(name.0).and_then(BuiltinType::from_name) {
            // This is a builtin type - resolve immediately, no dependencies needed
            self.insert_symbol(id, ResolvedType::Builtin(builtin));
            ControlFlow::Continue(())
        }
        else {
            // Every top-level name is collected before bodies are visited, so
            // this can no longer be a forward reference.
            self.state.report.add_diagnostic(
                Diagnostic::error(loc, "Type not found").with_help(
                    "Check that the type is defined in this module or imported correctly.",
                ),
            );

            ControlFlow::Continue(())
        }
    }
}
