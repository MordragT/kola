use std::{ops::ControlFlow, rc::Rc};

use kola_builtins::{Builtin, BuiltinType};
use kola_resolver::phase::ResolvedNodes;
use kola_span::{Diagnostic, IntoDiagnostic, Loc, Report};
use kola_syntax::prelude::*;
use kola_tree::prelude::*;
use kola_utils::interner::StrInterner;

use crate::{
    constraints::Constraints,
    env::{GlobalTypeEnv, LocalTypeEnv, ModuleTypeEnv},
    pattern_typer::PatternTyper,
    phase::{TypePhase, TypedNodes},
    substitute::{Substitutable, Substitution},
    types::{Kind, LabeledType, MonoType, PolyType, TypeVar},
};

// https://blog.stimsina.com/post/implementing-a-hindley-milner-type-system-part-2

// ∆ = Kind Environment
// Γ = Type Environment

// Generalization
// Γ'(τ) quantifies all monotype variables not bound in Γ

pub struct Typer<'a, N> {
    root_id: Id<N>,
    spans: Rc<Locations>,
    types: TypedNodes,
    local_env: LocalTypeEnv,
    module_env: &'a ModuleTypeEnv,
    global_env: &'a GlobalTypeEnv,
    cons: &'a mut Constraints,
    interner: &'a StrInterner,
    resolved: &'a ResolvedNodes,
}

impl<'a, N> Typer<'a, N> {
    pub fn new(
        root_id: Id<N>,
        spans: Rc<Locations>,
        cons: &'a mut Constraints,
        module_scope: &'a ModuleTypeEnv,
        env: &'a GlobalTypeEnv,
        interner: &'a StrInterner,
        resolved: &'a ResolvedNodes,
    ) -> Self {
        Self {
            root_id,
            local_env: LocalTypeEnv::new(),
            module_env: module_scope,
            types: TypedNodes::new(),
            spans,
            global_env: env,
            interner,
            resolved,
            cons,
        }
    }

    pub fn run<Tree>(mut self, tree: &Tree, report: &mut Report) -> Option<TypedNodes>
    where
        Tree: TreeView,
        Id<N>: Visitable<Tree>,
    {
        let root = self.root_id;

        match root.visit_by(&mut self, tree) {
            ControlFlow::Break(e) => {
                report.add_diagnostic(e);
                return None;
            }
            ControlFlow::Continue(()) => (),
        }

        Some(self.types)
    }

    #[inline]
    fn insert_type<T>(&mut self, id: Id<T>, t: T::Meta)
    where
        T: MetaCast<TypePhase>,
    {
        self.types.insert_meta(id, t)
    }

    #[inline]
    fn span<T>(&self, id: Id<T>) -> Loc
    where
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        *self.spans.meta(id)
    }
}

impl<'a, T, N> Visitor<T> for Typer<'a, N>
where
    T: TreeView,
    Id<N>: Visitable<T>,
{
    type BreakValue = Diagnostic;

    /// Rule for Type Binding
    ///
    /// ```ignore
    /// ∆;Γ ⊢ ty : poly_t
    /// -----------------------
    /// ∆;Γ ⊢ type name = ty : poly_t
    /// ```
    ///
    /// Implementation:
    /// - Visits the type expression to compute its polymorphic type
    /// - Propagates the type to the binding node
    ///
    /// Type signature: Binds a name to a polymorphic type
    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeBind { ty, .. } = *id.get(tree);

        self.visit_type_scheme(ty, tree)?;
        let poly_t = self.types.meta(ty).clone();

        self.insert_type(id, poly_t);
        ControlFlow::Continue(())
    }

    /// Rule for Polymorphic Type with Type Variables
    ///
    /// ```ignore
    /// type_vars = [α₁, ..., αₙ]
    /// ∆, α₁, ..., αₙ; Γ, α₁ : *, ..., αₙ : * ⊢ ty_expr : mono_t
    /// -----------------------
    /// ∆;Γ ⊢ ∀α₁...αₙ. ty_expr : PolyType { vars: type_vars, ty: mono_t }
    /// ```
    ///
    /// Implementation:
    /// - Creates fresh type variables for each declared variable
    /// - Enters type variables into scope
    /// - Visits the type expression in the extended scope
    /// - Exits type variables from scope in reverse order
    /// - Constructs polymorphic type with collected variables
    ///
    /// Type signature: `∀α₁...αₙ. τ` where τ is the inner type
    fn visit_type_scheme(
        &mut self,
        id: Id<node::TypeScheme>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeScheme { vars, ty } = id.get(tree);

        let type_vars = vars
            .iter()
            .map(|var_node| {
                let var_name = var_node.get(tree).0;
                let type_var = TypeVar::new();

                self.local_env.enter(var_name, MonoType::Var(type_var));
                type_var
            })
            .collect::<Vec<_>>();

        self.visit_type(*ty, tree)?;
        let mono_t = match self.types.meta(*ty).to_mono() {
            Ok(t) => t,
            Err(e) => return ControlFlow::Break(e.into_diagnostic(self.span(id))),
        };

        for var_node in vars.iter().rev() {
            let var_name = var_node.get(tree).0;
            self.local_env.exit(&var_name);
        }

        let poly_t = PolyType {
            vars: type_vars,
            ty: mono_t,
        };
        self.insert_type(id, poly_t);

        ControlFlow::Continue(())
    }

    fn visit_type(&mut self, id: Id<node::Type>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_type(id, tree)?;

        let poly_t = match *id.get(tree) {
            node::Type::Error(_) => todo!(),
            node::Type::Qualified(path_id) => self.types.meta(path_id).clone(),
            node::Type::Func(func_id) => PolyType::new(self.types.meta(func_id).clone()),
            node::Type::Application(app_id) => self.types.meta(app_id).clone(),
            node::Type::Record(record_id) => PolyType::new(self.types.meta(record_id).clone()),
            node::Type::Variant(variant_id) => PolyType::new(self.types.meta(variant_id).clone()),
        };

        self.insert_type(id, poly_t);
        ControlFlow::Continue(())
    }

    /// Rule for Type Path Resolution
    ///
    /// Module-qualified Path:
    /// ```ignore
    /// module : σ ∈ Γ   type_name : τ ∈ module
    /// -----------------------
    /// ∆;Γ ⊢ module.type_name : τ
    /// ```
    ///
    /// Local Type Variable:
    /// ```ignore
    /// type_name : τ ∈ Γ
    /// -----------------------
    /// ∆;Γ ⊢ type_name : τ
    /// ```
    ///
    /// Builtin Type:
    /// ```ignore
    /// type_name ∈ {Bool, Char, Num, Str}
    /// -----------------------
    /// ∆;Γ ⊢ type_name : builtin_type(type_name)
    /// ```
    ///
    /// Implementation:
    /// - Resolves type names through module system, local scope, or builtins
    /// - Returns appropriate polymorphic type for the resolved name
    ///
    /// Type signature: Resolves names to their associated types
    fn visit_qualified_type(
        &mut self,
        id: Id<node::QualifiedType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedType { path, ty } = *id.get(tree);

        let type_name = ty.get(tree).0;
        let span = self.span(id);

        let poly_t = if let Some(path) = path {
            // Module-qualified path
            let module_sym = *self.resolved.meta(path);

            let type_sym = self.global_env[module_sym]
                .get_type(type_name)
                .expect("Type not found in module");

            let module_poly_t = &self.global_env[type_sym];
            module_poly_t.clone()
        } else if let Some(local_t) = self.local_env.get(&type_name) {
            // Local type parameter (like 'a' in forall a)
            PolyType::new(local_t.clone())
        } else if let Some(global_poly_t) = self
            .resolved
            .meta(id)
            .into_defined()
            .and_then(|sym| self.global_env.get_type(sym))
        {
            // Module-level type definition (like 'Person')
            // unlike module-level value binds, these are properly solved and ready to use
            global_poly_t.clone()
        } else if let Some(builtin_t) = self.resolved.meta(id).into_builtin() {
            match builtin_t {
                BuiltinType::Unit => PolyType::new(MonoType::UNIT),
                BuiltinType::Bool => PolyType::new(MonoType::BOOL),
                BuiltinType::Num => PolyType::new(MonoType::NUM),
                BuiltinType::Char => PolyType::new(MonoType::CHAR),
                BuiltinType::Str => PolyType::new(MonoType::STR),
                BuiltinType::List => {
                    let var = TypeVar::new();
                    PolyType {
                        vars: vec![var],
                        ty: MonoType::list(MonoType::Var(var)),
                    }
                }
            }
        } else {
            return ControlFlow::Break(Diagnostic::error(span, "Type not found in scope"));
        };

        self.insert_type(id, poly_t);
        ControlFlow::Continue(())
    }

    /// Rule for Function Type
    ///
    /// ```ignore
    /// ∆;Γ ⊢ input : input_t
    /// ∆;Γ ⊢ output : output_t
    /// -----------------------
    /// ∆;Γ ⊢ input → output : input_t → output_t
    /// ```
    ///
    /// Implementation:
    /// - Visits both input and output type expressions
    /// - Constructs function type from the resolved types
    ///
    /// Type signature: `α → β` where α is input type, β is output type
    fn visit_func_type(
        &mut self,
        id: Id<node::FuncType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::FuncType { input, output } = *id.get(tree);

        self.visit_type(input, tree)?;
        let input_t = match self.types.meta(input).to_mono() {
            Ok(t) => t,
            Err(e) => return ControlFlow::Break(e.into_diagnostic(self.span(id))),
        };

        self.visit_type(output, tree)?;
        let output_t = match self.types.meta(output).to_mono() {
            Ok(t) => t,
            Err(e) => return ControlFlow::Break(e.into_diagnostic(self.span(id))),
        };

        let func_t = MonoType::func(input_t, output_t);

        self.insert_type(id, func_t);
        ControlFlow::Continue(())
    }

    /// Rule for Type Application
    ///
    /// ```ignore
    /// ∆;Γ ⊢ constructor : PolyType { vars: [α₁, ..., αₙ], ty: constructor_t }
    /// ∆;Γ ⊢ arg : arg_t
    /// n ≥ 1
    /// substitution = [α₁ ↦ arg_t]
    /// result_t = constructor_t[α₁ ↦ arg_t]
    /// -----------------------
    /// ∆;Γ ⊢ constructor arg : PolyType { vars: [α₂, ..., αₙ], ty: result_t }
    /// ```
    ///
    /// Implementation:
    /// - Visits argument type expression (must be monomorphic)
    /// - Visits constructor type expression (must be polymorphic)
    /// - Applies first type variable to the argument type
    /// - Returns partially applied type constructor
    ///
    /// Type signature: `(∀α₁...αₙ. τ) → σ → (∀α₂...αₙ. τ[α₁ ↦ σ])`
    fn visit_type_application(
        &mut self,
        id: Id<node::TypeApplication>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeApplication { constructor, arg } = *id.get(tree);

        self.visit_type(arg, tree)?;
        let arg_t = match self.types.meta(arg).to_mono() {
            Ok(t) => t,
            Err(e) => return ControlFlow::Break(e.into_diagnostic(self.span(id))),
        };

        self.visit_type(constructor, tree)?;
        let PolyType { vars, ty } = self.types.meta(constructor);

        let [first_var, remaining_vars @ ..] = vars.as_slice() else {
            return ControlFlow::Break(Diagnostic::error(
                self.span(id),
                "Type application requires at least one type variable",
            ));
        };

        let mut substitution = Substitution::unit(*first_var, arg_t);
        let result_t = ty.clone().apply(&mut substitution);

        let poly_t = PolyType {
            ty: result_t,
            vars: remaining_vars.to_vec(),
        };

        self.insert_type(id, poly_t);
        ControlFlow::Continue(())
    }

    /// Rule for Record Type
    ///
    /// With extension:
    /// ```ignore
    /// ∆;Γ ⊢ field₁ : head_t₁
    /// ...
    /// ∆;Γ ⊢ fieldₙ : head_tₙ
    /// extension : tail_t ∈ Γ
    /// -----------------------
    /// ∆;Γ ⊢ { field₁, ..., fieldₙ | extension } : { head_t₁, ..., head_tₙ | tail_t }
    /// ```
    ///
    /// Without extension:
    /// ```ignore
    /// ∆;Γ ⊢ field₁ : head_t₁
    /// ...
    /// ∆;Γ ⊢ fieldₙ : head_tₙ
    /// -----------------------
    /// ∆;Γ ⊢ { field₁, ..., fieldₙ } : { head_t₁, ..., head_tₙ | {} }
    /// ```
    ///
    /// Implementation:
    /// - Resolves extension type variable if present, or uses empty row
    /// - Folds field types into a row type structure
    ///
    /// Type signature: `{ label₁ : τ₁, ..., labelₙ : τₙ | r }`
    fn visit_record_type(
        &mut self,
        id: Id<node::RecordType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_type(id, tree)?;

        let node::RecordType { fields, extension } = id.get(tree);

        let tail_t = if let Some(extension_var) = extension {
            let extension_name = extension_var.get(tree).0;
            let Some(extension_t) = self.local_env.get(&extension_name).cloned() else {
                return ControlFlow::Break(Diagnostic::error(
                    self.span(id),
                    "Type variable not found in scope",
                ));
            };
            extension_t
        } else {
            MonoType::empty_row()
        };

        let record_t = fields.iter().fold(tail_t, |acc_t, &field| {
            let head_t = self.types.meta(field).clone();
            MonoType::row(head_t, acc_t)
        });

        self.insert_type(id, record_t);
        ControlFlow::Continue(())
    }

    /// Rule for Record Field Type
    ///
    /// ```ignore
    /// ∆;Γ ⊢ ty : value_t
    /// head_t = ("label" : value_t)
    /// -----------------------
    /// ∆;Γ ⊢ (label : ty) : head_t
    /// ```
    ///
    /// Implementation:
    /// - Visits the type expression to get the field's type
    /// - Creates a labeled type associating the label with the type
    ///
    /// Type signature: Creates `LabeledType` for record type construction
    fn visit_record_field_type(
        &mut self,
        id: Id<node::RecordFieldType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordFieldType { name, ty } = *id.get(tree);

        let label = name.get(tree).0;

        self.visit_type(ty, tree)?;
        let value_t = match self.types.meta(ty).to_mono() {
            Ok(t) => t,
            Err(e) => return ControlFlow::Break(e.into_diagnostic(self.span(id))),
        };

        let head_t = LabeledType { label, ty: value_t };

        self.insert_type(id, head_t);
        ControlFlow::Continue(())
    }

    /// Rule for Variant Type
    ///
    /// With extension:
    /// ```ignore
    /// ∆;Γ ⊢ case₁ : head_t₁
    /// ...
    /// ∆;Γ ⊢ caseₙ : head_tₙ
    /// extension : tail_t ∈ Γ
    /// -----------------------
    /// ∆;Γ ⊢ [ case₁ | ... | caseₙ | extension ] : [ head_t₁ | ... | head_tₙ | tail_t ]
    /// ```
    ///
    /// Without extension:
    /// ```ignore
    /// ∆;Γ ⊢ case₁ : head_t₁
    /// ...
    /// ∆;Γ ⊢ caseₙ : head_tₙ
    /// -----------------------
    /// ∆;Γ ⊢ [ case₁ | ... | caseₙ ] : [ head_t₁ | ... | head_tₙ | {} ]
    /// ```
    ///
    /// Implementation:
    /// - Resolves extension type variable if present, or uses empty row
    /// - Folds case types into a row type structure
    ///
    /// Type signature: `[ label₁ : τ₁ | ... | labelₙ : τₙ | r ]`
    fn visit_variant_type(
        &mut self,
        id: Id<node::VariantType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_variant_type(id, tree)?;

        let node::VariantType { cases, extension } = id.get(tree);

        let tail_t = if let Some(extension_var) = extension {
            let extension_name = extension_var.get(tree).0;
            let Some(extensions_t) = self.local_env.get(&extension_name).cloned() else {
                return ControlFlow::Break(Diagnostic::error(
                    self.span(id),
                    "Type variable not found in scope",
                ));
            };
            extensions_t
        } else {
            MonoType::empty_row()
        };

        let variant_t = cases.iter().fold(tail_t, |acc_t, &case| {
            let head_t = self.types.meta(case).clone();
            MonoType::row(head_t, acc_t)
        });

        self.insert_type(id, variant_t);
        ControlFlow::Continue(())
    }

    /// Rule for Variant Case Type
    ///
    /// With payload:
    /// ```ignore
    /// ∆;Γ ⊢ ty : value_t
    /// head_t = ("label" : value_t)
    /// -----------------------
    /// ∆;Γ ⊢ (label : ty) : head_t
    /// ```
    ///
    /// Without payload:
    /// ```ignore
    /// head_t = ("label" : Unit)
    /// -----------------------
    /// ∆;Γ ⊢ label : head_t
    /// ```
    ///
    /// Implementation:
    /// - Visits the payload type if present, or uses Unit type
    /// - Creates a labeled type for the variant case
    ///
    /// Type signature: Creates `LabeledType` for variant type construction
    fn visit_variant_tag_type(
        &mut self,
        id: Id<node::VariantTagType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::VariantTagType { name, ty } = *id.get(tree);

        let label = name.get(tree).0;

        let value_t = if let Some(payload_ty) = ty {
            self.visit_type(payload_ty, tree)?;
            match self.types.meta(payload_ty).to_mono() {
                Ok(t) => t,
                Err(e) => return ControlFlow::Break(e.into_diagnostic(self.span(id))),
            }
        } else {
            MonoType::UNIT
        };

        let head_t = LabeledType { label, ty: value_t };

        self.insert_type(id, head_t);
        ControlFlow::Continue(())
    }

    /// Rule for Value Bindings
    ///
    /// In this constraint-based implementation, value bindings are processed in two phases:
    /// 1. **Constraint Generation**: Infers type from value expression and creates constraints
    /// 2. **Deferred Resolution**: Actual generalization happens later in the checker
    ///
    /// Implementation:
    /// - Infers type from value expression with fresh type variable scope
    /// - If type annotation present, constrains annotation against inferred type
    /// - Stores inferred type (not annotation) for later generalization
    /// - Does NOT generalize immediately (unlike traditional HM implementations)
    ///
    /// Note: The PolyType::new() wrapper is temporary for printing - actual generalization
    /// happens in the checker after constraint solving.
    fn visit_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ValueBind { ty, value, .. } = *id.get(tree);

        let span = self.span(id);

        TypeVar::enter();
        self.visit_expr(value, tree)?;
        TypeVar::exit();

        let value_t = self.types.meta(value).clone();

        if let Some(annotation_ty) = ty {
            self.visit_type_scheme(annotation_ty, tree)?;
            let expected_t = self.types.meta(annotation_ty).instantiate();

            self.cons.constrain_check(expected_t, value_t.clone(), span);
        }

        self.insert_type(id, PolyType::new(value_t)); // Use fake PolyType to aid printer

        ControlFlow::Continue(())
    }

    // Patterns

    // Expr

    /// Rule for Literal Expressions
    ///
    /// ```ignore
    /// -----------------------
    /// ∆;Γ ⊢ literal : literal_type
    /// ```
    ///
    /// Type signatures:
    /// - Bool literals: `Bool`
    /// - Char literals: `Char`
    /// - Num literals: `Num`
    /// - Str literals: `Str`
    fn visit_literal_expr(
        &mut self,
        id: Id<node::LiteralExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let actual = match id.get(tree) {
            &node::LiteralExpr::Unit => MonoType::UNIT,
            &node::LiteralExpr::Bool(_) => MonoType::BOOL,
            &node::LiteralExpr::Char(_) => MonoType::CHAR,
            &node::LiteralExpr::Num(_) => MonoType::NUM,
            &node::LiteralExpr::Str(_) => MonoType::STR,
        };

        self.insert_type(id, actual);
        ControlFlow::Continue(())
    }

    /// Rule for List Expression
    ///
    /// ```ignore
    /// ∆;Γ ⊢ e0 : element_t
    /// ∆;Γ ⊢ e1 : element_t
    /// ...
    /// ∆;Γ ⊢ en : element_t
    /// -----------------------
    /// ∆;Γ ⊢ [e0, e1, ..., en] : [element_t]
    /// ```
    ///
    /// Implementation:
    /// - All elements must have the same type
    /// - Empty lists are allowed but need type inference from context
    ///
    /// Type signature: `∀α. [α]` (list of elements of type α)
    fn visit_list_expr(
        &mut self,
        id: Id<node::ListExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_list_expr(id, tree)?;

        let node::ListExpr(list) = id.get(tree);

        if let Some(&first_elem) = list.first() {
            let first_t = self.types.meta(first_elem).clone();

            self.insert_type(id, MonoType::list(first_t.clone()));

            for &elem in list {
                let span = self.span(id);
                let elem_t = self.types.meta(elem).clone();

                self.cons.constrain(first_t.clone(), elem_t, span);
            }
        } else {
            todo!() // Handle empty lists by checking if it has annotations
        }

        ControlFlow::Continue(())
    }

    /// Rule for Record Selection and Variable Lookup
    ///
    /// Record Selection:
    /// ```ignore
    /// ∆;Γ ⊢ record : record_t
    /// value_t = newvar()
    /// tail_t = newvar()
    /// head_t = ("label" : value_t)
    /// unify(record_t, { head_t | tail_t })
    /// -----------------------
    /// ∆;Γ ⊢ record.label : value_t
    /// ```
    ///
    /// Variable Lookup:
    /// ```ignore
    /// x : σ ∈ Γ   base_t = inst(σ)
    /// -----------------------
    /// ∆;Γ ⊢ x : base_t
    /// ```
    ///
    /// Type signature: `∀r α. {l : α | r} → α` (for field selection)
    fn visit_qualified_expr(
        &mut self,
        id: Id<node::QualifiedExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let span = self.span(id);

        let node::QualifiedExpr {
            path,
            source,
            fields,
        } = *id.get(tree);

        let name = source.get(tree).0;

        let base_t = if let Some(path) = path {
            // Case 1: Other module (real PolyType)
            let module_sym = *self.resolved.meta(path);

            let value_sym = self.global_env[module_sym]
                .get_value(name)
                .expect("Value not found in module");

            let pt = &self.global_env[value_sym];
            pt.instantiate()
        } else if let Some(t) = self.local_env.get(&name) {
            // Case 2: Let-bind (MonoType)

            t.clone()
        } else if let Some(pt) = self
            .resolved
            .meta(id)
            .into_defined()
            .and_then(|sym| self.module_env.get(&sym))
        {
            // Case 3: Module local value-bind
            pt.instantiate()
        } else if let Some(id) = self.resolved.meta(id).into_builtin() {
            let builtin = Builtin::from_id(id);

            // TODO this instantiate isn't really necessary
            // from_protocols creates new variables anyway
            PolyType::from_protocol(builtin.type_, self.interner).instantiate()
        } else {
            return ControlFlow::Break(Diagnostic::error(span, "Value not found in scope"));
        };

        let Some(fields) = fields else {
            // Case 4: Variable lookup
            self.insert_type(id, base_t.clone());
            return ControlFlow::Continue(());
        };

        let result_t = fields.get(tree).iter().fold(base_t, |record_t, field| {
            self.cons
                .constrain_kind(Kind::Record, record_t.clone(), span);

            let value_t = MonoType::variable();
            let label = field.get(tree).0.clone();

            let head_t = LabeledType {
                label,
                ty: value_t.clone(),
            };

            self.cons.constrain(
                MonoType::row(head_t, MonoType::variable()),
                record_t.clone(),
                span,
            );

            value_t
        });

        self.insert_type(id, result_t);
        ControlFlow::Continue(())
    }

    /// Rule for Record Field
    ///
    /// ```ignore
    /// ∆;Γ ⊢ value : value_t
    /// head_t = ("label" : value_t)
    /// -----------------------
    /// ∆;Γ ⊢ (label : value) : head_t
    /// ```
    ///
    /// Implementation:
    /// - Creates a labeled type associating the field name with the value's type
    /// - Used as building blocks for record types
    ///
    /// Type signature: Creates `LabeledType` for record construction
    fn visit_record_field(
        &mut self,
        id: Id<node::RecordField>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_field(id, tree)?;

        let node::RecordField {
            field,
            type_,
            value,
        } = *id.get(tree);

        let label = field.get(tree).0.clone();
        let value_t = self.types.meta(value).clone();

        if let Some(type_) = type_ {
            self.visit_type(type_, tree)?;

            let expected_t = match self.types.meta(type_).to_mono() {
                Ok(t) => t,
                Err(e) => return ControlFlow::Break(e.into_diagnostic(self.span(id))),
            };

            self.cons
                .constrain_check(expected_t, value_t.clone(), self.span(id));
        }

        self.insert_type(id, LabeledType { label, ty: value_t });
        ControlFlow::Continue(())
    }

    /// Rule for Record Expression
    ///
    /// ```ignore
    /// ∆;Γ ⊢ v0 : t0
    /// head_t0 = ("l0" : t0)
    /// ...
    /// ∆;Γ ⊢ vn : tn
    /// head_tn = ("ln" : tn)
    /// -----------------------
    /// ∆;Γ ⊢ { l0 : v0, ..., ln : vn } : { head_t0, ..., head_tn | {} }
    /// ```
    ///
    /// Type signature: Build record type from field types
    fn visit_record_expr(
        &mut self,
        id: Id<node::RecordExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_expr(id, tree)?;

        let mut record_t = MonoType::empty_row();

        for &field in id.get(tree) {
            let head_t = self.types.meta(field).clone();
            record_t = MonoType::row(head_t, record_t);
        }

        self.insert_type(id, record_t);
        ControlFlow::Continue(())
    }

    /// Rule for Record Extension of `source` with a new field `label` and value `value`
    ///
    /// ```ignore
    /// ∆;Γ ⊢ value : value_t
    /// ∆;Γ ⊢ source : source_t where source_t is of kind Record
    /// head_t = ("label" : value_t)
    /// -----------------------
    /// ∆;Γ ⊢ { source | +label = value } : { head_t | source_t }
    /// ```
    ///
    /// Type signature: `∀r α. α → {r} → {l : α | r}`
    fn visit_record_extend_expr(
        &mut self,
        id: Id<node::RecordExtendExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_extend_expr(id, tree)?; // TODO maybe make this explicit ?

        let span = self.span(id);

        let node::RecordExtendExpr {
            source,
            source_type,
            select,
            value,
            value_type,
        } = *id.get(tree);

        let mut source_t = self.types.meta(source).clone();

        if let Some(type_) = source_type {
            self.visit_type(type_, tree)?;
            let expected_t = match self.types.meta(type_).to_mono() {
                Ok(t) => t,
                Err(e) => return ControlFlow::Break(e.into_diagnostic(span)),
            };

            self.cons
                .constrain_check(expected_t, source_t.clone(), span);
        }

        // Split path into navigation fields and final extension field
        // Safety: `select` is always non-empty
        let (field, fields) = select.get(tree).0.as_slice().split_last().unwrap();

        for field_id in fields {
            self.cons
                .constrain_kind(Kind::Record, source_t.clone(), span);

            let value_t = MonoType::variable();
            let label = field_id.get(tree).0;

            let head_t = LabeledType {
                label,
                ty: value_t.clone(),
            };

            self.cons.constrain(
                MonoType::row(head_t, MonoType::variable()),
                source_t.clone(),
                span,
            );

            source_t = value_t;
        }

        self.cons
            .constrain_kind(Kind::Record, source_t.clone(), span);

        let value_t = self.types.meta(value);

        if let Some(type_) = value_type {
            let expected_t = match self.types.meta(type_).to_mono() {
                Ok(t) => t,
                Err(e) => return ControlFlow::Break(e.into_diagnostic(span)),
            };

            self.cons.constrain_check(expected_t, value_t.clone(), span);
        }

        let label = field.get(tree).0.clone();
        let head_t = LabeledType {
            label,
            ty: value_t.clone(),
        };
        let result_t = MonoType::row(head_t, source_t);

        self.insert_type(id, result_t);
        ControlFlow::Continue(())
    }

    /// Rule for Record Restriction of `source` with label `field`
    ///
    /// ```ignore
    /// ∆;Γ ⊢ source : source_t
    /// value_t = newvar()
    /// tail_t = newvar()
    /// head_t = ("label" : value_t)
    /// unify(source_t, { head_t | tail_t })
    /// -----------------------
    /// ∆;Γ ⊢ { source | -label } : tail_t
    /// ```
    ///
    /// Implementation:
    /// - Constrain `source` to be of kind Record
    /// - Unify `source` with `{ field : field_t | tail_t }` to decompose the record
    /// - Result is `tail_t` (the remaining record without the restricted field)
    ///
    /// Type signature: `∀r α. {"label" : α | r} → r`
    fn visit_record_restrict_expr(
        &mut self,
        id: Id<node::RecordRestrictExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        // self.walk_record_restrict_expr(id, tree)?;

        let span = self.span(id);

        let node::RecordRestrictExpr {
            source,
            source_type,
            select,
            value_type,
        } = *id.get(tree);

        self.visit_expr(source, tree)?;
        let mut source_t = self.types.meta(source).clone();

        if let Some(type_) = source_type {
            self.visit_type(type_, tree)?;
            let expected_t = match self.types.meta(type_).to_mono() {
                Ok(t) => t,
                Err(e) => return ControlFlow::Break(e.into_diagnostic(span)),
            };

            self.cons
                .constrain_check(expected_t, source_t.clone(), span);
        }

        // Split path into navigation fields and final restriction field
        // Safety: `select` is always non-empty
        let (field, fields) = select.get(tree).0.as_slice().split_last().unwrap();

        // Navigate through all fields except the last
        for field_id in fields {
            self.cons
                .constrain_kind(Kind::Record, source_t.clone(), span);

            let value_t = MonoType::variable();
            let label = field_id.get(tree).0;

            let head_t = LabeledType {
                label,
                ty: value_t.clone(),
            };

            self.cons.constrain(
                MonoType::row(head_t, MonoType::variable()),
                source_t.clone(),
                span,
            );

            source_t = value_t;
        }

        // Restrict the final field
        self.cons
            .constrain_kind(Kind::Record, source_t.clone(), span);

        let value_t = MonoType::variable();

        if let Some(type_) = value_type {
            self.visit_type(type_, tree)?;
            let expected_t = match self.types.meta(type_).to_mono() {
                Ok(t) => t,
                Err(e) => return ControlFlow::Break(e.into_diagnostic(span)),
            };

            self.cons.constrain_check(expected_t, value_t.clone(), span);
        }

        let label = field.get(tree).0.clone();
        let head_t = LabeledType { label, ty: value_t };
        let tail_t = MonoType::variable();

        self.cons.constrain(
            MonoType::row(head_t, tail_t.clone()),
            source_t.clone(),
            span,
        );

        self.insert_type(id, tail_t);
        ControlFlow::Continue(())
    }

    /// Rule for Record Update Operations
    ///
    /// Type signatures:
    /// - `=`: `∀α. α → α`
    /// - `+=`: `∀α. α → α` where `α : Addable`
    /// - `-=`, `*=`, `/=`, `%=`: `Num → Num`
    fn visit_record_update_op(
        &mut self,
        id: Id<node::RecordUpdateOp>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let span = self.span(id);

        let value_t = match id.get(tree) {
            node::RecordUpdateOp::Assign => MonoType::variable(),
            node::RecordUpdateOp::AddAssign => {
                let addable_t = MonoType::variable();
                self.cons
                    .constrain_kind(Kind::Addable, addable_t.clone(), span);
                addable_t
            }
            node::RecordUpdateOp::SubAssign
            | node::RecordUpdateOp::MulAssign
            | node::RecordUpdateOp::DivAssign
            | node::RecordUpdateOp::RemAssign => MonoType::NUM,
        };

        let update_func_t = MonoType::func(value_t.clone(), value_t);

        self.insert_type(id, update_func_t);
        ControlFlow::Continue(())
    }

    /// Rule for Record Update of `source` with label `field` and value `value`
    ///
    /// ```ignore
    /// ∆;Γ ⊢ source : source_t
    /// tail_t = newvar()
    /// old_value_t = newvar()
    /// old_head_t = ("label" : old_value_t)
    /// unify(source_t, { old_head_t | tail_t })
    ///
    /// ∆;Γ ⊢ value : new_value_t
    /// ∆;Γ ⊢ op : old_value_t → new_value_t
    /// new_head_t = ("label" : new_value_t)
    /// -----------------------
    /// ∆;Γ ⊢ { source | label op value } : { new_head_t | tail_t }
    /// ```
    ///
    /// Implementation:
    /// - Unify `source_t` with `{ label : old_value_t | tail_t }` to decompose the record
    /// - Unify `op` with `old_value_t → new_value_t` to constrain the operation
    /// - Result is `{ label : new_value_t | tail_t }` (record with updated field type)
    ///
    /// Type signature: `∀r α β. {"label" : α | r} → (α → β) → β → {"label" : β | r}`
    fn visit_record_update_expr(
        &mut self,
        id: Id<node::RecordUpdateExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_update_expr(id, tree)?;

        let span = self.span(id);

        let node::RecordUpdateExpr {
            source,
            source_type,
            select,
            op,
            value,
            value_type,
        } = *id.get(tree);

        let mut source_t = self.types.meta(source).clone();

        if let Some(type_) = source_type {
            self.visit_type(type_, tree)?;
            let expected_t = match self.types.meta(type_).to_mono() {
                Ok(t) => t,
                Err(e) => return ControlFlow::Break(e.into_diagnostic(span)),
            };

            self.cons
                .constrain_check(expected_t, source_t.clone(), span);
        }

        // Split path into navigation fields and final update field
        // Safety: `select` is always non-empty
        let (field, fields) = select.get(tree).0.as_slice().split_last().unwrap();

        // Navigate through all fields except the last
        for field_id in fields {
            self.cons
                .constrain_kind(Kind::Record, source_t.clone(), span);

            let value_t = MonoType::variable();
            let label = field_id.get(tree).0;

            let head_t = LabeledType {
                label,
                ty: value_t.clone(),
            };

            self.cons.constrain(
                MonoType::row(head_t, MonoType::variable()),
                source_t.clone(),
                span,
            );

            source_t = value_t;
        }

        // Restrict the final field
        self.cons
            .constrain_kind(Kind::Record, source_t.clone(), span);

        let old_value_t = MonoType::variable();
        let label = field.get(tree).0.clone();
        let old_head_t = LabeledType {
            label: label.clone(),
            ty: old_value_t.clone(),
        };
        let tail_t = MonoType::variable();
        self.cons.constrain(
            MonoType::row(old_head_t, tail_t.clone()),
            source_t.clone(),
            span,
        );

        let new_value_t = self.types.meta(value).clone();

        if let Some(type_) = value_type {
            self.visit_type(type_, tree)?;
            let expected_t = match self.types.meta(type_).to_mono() {
                Ok(t) => t,
                Err(e) => return ControlFlow::Break(e.into_diagnostic(span)),
            };

            self.cons
                .constrain_check(expected_t, new_value_t.clone(), span);
        }

        // Treat the update operation as a function: old_value_t -> new_value_t
        let op_t = self.types.meta(op).clone();
        self.cons.constrain(
            op_t,
            MonoType::func(old_value_t.clone(), new_value_t.clone()),
            span,
        );

        let new_head_t = LabeledType {
            label,
            ty: new_value_t,
        };
        let result_t = MonoType::row(new_head_t, tail_t);

        self.insert_type(id, result_t);
        ControlFlow::Continue(())
    }

    /// Rule for Unary Operations
    ///
    /// Type signatures:
    /// - `-`: `Num → Num` (numeric negation)
    /// - `!`: `Bool → Bool` (logical negation)
    fn visit_unary_op(&mut self, id: Id<node::UnaryOp>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let operand_t = match id.get(tree) {
            &node::UnaryOp::Neg => MonoType::NUM,
            &node::UnaryOp::Not => MonoType::BOOL,
        };

        let unary_func_t = MonoType::func(operand_t.clone(), operand_t);

        self.insert_type(id, unary_func_t);
        ControlFlow::Continue(())
    }

    /// Rule for Unary Expression (Application of unary operator)
    ///
    /// ```ignore
    /// ∆;Γ ⊢ op : op_t
    /// ∆;Γ ⊢ operand : operand_t
    /// result_t = newvar()
    /// unify(op_t, operand_t → result_t)
    /// -----------------------
    /// ∆;Γ ⊢ op operand : result_t
    /// ```
    ///
    /// Type signature: `(α → β) → α → β`
    fn visit_unary_expr(
        &mut self,
        id: Id<node::UnaryExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_unary_expr(id, tree)?;

        let span = self.span(id);

        let node::UnaryExpr { op, operand } = *id.get(tree);

        let op_t = self.types.meta(op).clone();
        let operand_t = self.types.meta(operand).clone();

        let result_t = MonoType::variable();

        // Constrain the operator to be a function from operand type to result type
        self.cons
            .constrain(op_t, MonoType::func(operand_t, result_t.clone()), span);

        self.insert_type(id, result_t);
        ControlFlow::Continue(())
    }

    /// Rule for Binary Operations
    ///
    /// Type signatures:
    /// - `+`: `∀α. α → α → Num` where `α : Addable`
    /// - `-`, `*`, `/`, `%`: `Num → Num → Num`
    /// - `<`, `>`, `<=`, `>=`: `∀α. α → α → Bool` where `α : Comparable`
    /// - `&&`, `||`, `xor`: `Bool → Bool → Bool`
    /// - `==`, `!=`: `∀α. α → α → Bool` where `α : Equatable`
    fn visit_binary_op(
        &mut self,
        id: Id<node::BinaryOp>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let span = self.span(id);

        let (operand_t, result_t) = match id.get(tree) {
            node::BinaryOp::Add => {
                let addable_t = MonoType::variable();
                self.cons
                    .constrain_kind(Kind::Addable, addable_t.clone(), span);
                (addable_t, MonoType::NUM)
            }
            node::BinaryOp::Sub
            | node::BinaryOp::Mul
            | node::BinaryOp::Div
            | node::BinaryOp::Rem => (MonoType::NUM, MonoType::NUM),
            // Comparison
            node::BinaryOp::Less
            | node::BinaryOp::Greater
            | node::BinaryOp::LessEq
            | node::BinaryOp::GreaterEq => {
                let comparable_t = MonoType::variable();
                self.cons
                    .constrain_kind(Kind::Comparable, comparable_t.clone(), span);
                (comparable_t, MonoType::BOOL)
            }
            // Logical
            node::BinaryOp::And | node::BinaryOp::Or | node::BinaryOp::Xor => {
                (MonoType::BOOL, MonoType::BOOL)
            }
            // Equality
            node::BinaryOp::Eq | node::BinaryOp::NotEq => {
                let equatable_t = MonoType::variable();
                self.cons
                    .constrain_kind(Kind::Equatable, equatable_t.clone(), span);
                (equatable_t, MonoType::BOOL)
            }
            // Record
            node::BinaryOp::Merge => {
                todo!();
            }
        };

        let binary_func_t = MonoType::func(operand_t.clone(), MonoType::func(operand_t, result_t));

        self.insert_type(id, binary_func_t);
        ControlFlow::Continue(())
    }

    /// Rule for Binary Expression (Application of binary operator)
    ///
    /// ```ignore
    /// ∆;Γ ⊢ op : op_t
    /// ∆;Γ ⊢ left : left_t
    /// ∆;Γ ⊢ right : right_t
    /// result_t = newvar()
    /// unify(op_t, left_t → right_t → result_t)
    /// -----------------------
    /// ∆;Γ ⊢ left op right : result_t
    /// ```
    ///
    /// Type signature: `(α → β → γ) → α → β → γ`
    fn visit_binary_expr(
        &mut self,
        id: Id<node::BinaryExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_binary_expr(id, tree)?;

        let span = self.span(id);

        let node::BinaryExpr { left, op, right } = *id.get(tree);

        let op_t = self.types.meta(op).clone();
        let left_t = self.types.meta(left).clone();
        let right_t = self.types.meta(right).clone();

        let result_t = MonoType::variable();

        let expected_op_t = MonoType::func(left_t, MonoType::func(right_t, result_t.clone()));
        self.cons.constrain(op_t, expected_op_t, span);

        self.insert_type(id, result_t);
        ControlFlow::Continue(())
    }

    /// Rule for Let Expressions (Monomorphic Implementation)
    ///
    /// ```text
    /// ∆;Γ ⊢ value : value_t
    /// ∆;Γ, name : value_t ⊢ inside : result_t
    /// -----------------------
    /// ∆;Γ ⊢ let name = value in inside : result_t
    /// ```
    ///
    /// Note: This implementation does NOT generalize let-bindings, making them
    /// monomorphic. This simplifies the constraint-based algorithm while still
    /// supporting polymorphism at top-level value bindings.
    ///
    /// For polymorphic local bindings, use (non-exported) top-level value bindings instead.
    fn visit_let_expr(&mut self, id: Id<node::LetExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let &node::LetExpr {
            name,
            value_type,
            value,
            inside,
        } = id.get(tree);

        let name = name.get(tree).0.clone();

        TypeVar::enter();
        self.visit_expr(value, tree)?;
        TypeVar::exit();

        let value_t = self.types.meta(value).clone();

        // First: Handle type annotation constraint if present
        if let Some(type_) = value_type {
            self.visit_type(type_, tree)?;
            let expected_t = match self.types.meta(type_).to_mono() {
                Ok(t) => t,
                Err(e) => return ControlFlow::Break(e.into_diagnostic(self.span(id))),
            };

            // Constrain the value type against the annotation
            self.cons
                .constrain(expected_t, value_t.clone(), self.span(id));
        }
        self.local_env.enter(name, value_t);

        self.visit_expr(inside, tree)?;
        self.local_env.exit(&name);

        let result_t = self.types.meta(inside).clone();
        self.insert_type(id, result_t);

        ControlFlow::Continue(())
    }

    /// Rule for Case Expression (Pattern Matching)
    ///
    /// ```ignore
    /// ∆;Γ ⊢ source : source_t
    /// ∆;Γ ⊢ pattern₁ ⇒ source_t ⊣ Γ₁    Γ₁ ⊢ expr₁ : result_t
    /// ∆;Γ ⊢ pattern₂ ⇒ source_t ⊣ Γ₂    Γ₂ ⊢ expr₂ : result_t
    /// ...
    /// ∆;Γ ⊢ patternₙ ⇒ source_t ⊣ Γₙ    Γₙ ⊢ exprₙ : result_t
    /// -----------------------
    /// ∆;Γ ⊢ case source of pattern₁ => expr₁ | pattern₂ => expr₂ | ... | patternₙ => exprₙ : result_t
    /// ```
    ///
    /// Pattern Typing Rules:
    ///
    /// **Literal Patterns:**
    /// ```ignore
    /// ∆;Γ ⊢ 42 ⇒ Num ⊣ Γ
    /// ∆;Γ ⊢ true ⇒ Bool ⊣ Γ
    /// ∆;Γ ⊢ () ⇒ Unit ⊣ Γ
    /// ```
    ///
    /// **Wildcard Pattern:**
    /// ```ignore
    /// α fresh
    /// -----------------------
    /// ∆;Γ ⊢ _ ⇒ α ⊣ Γ
    /// ```
    ///
    /// **Bind Pattern:**
    /// ```ignore
    /// α fresh
    /// -----------------------
    /// ∆;Γ ⊢ x ⇒ α ⊣ Γ[x : α]
    /// ```
    ///
    /// **Record Patterns:**
    /// - Exact record: `{ l₁ : p₁, ..., lₙ : pₙ } ⇒ { l₁ : τ₁, ..., lₙ : τₙ }`
    /// - Polymorphic record: `{ l₁ : p₁, ..., lₙ : pₙ, ... } ⇒ { l₁ : τ₁, ..., lₙ : τₙ | ρ }`
    ///
    /// **List Patterns:**
    /// ```ignore
    /// ∆;Γ ⊢ p₁ ⇒ τ ⊣ Γ₁    ∆;Γ₁ ⊢ p₂ ⇒ τ ⊣ Γ₂    ...    ∆;Γₙ₋₁ ⊢ pₙ ⇒ τ ⊣ Γₙ
    /// -----------------------
    /// ∆;Γ ⊢ [p₁, p₂, ..., pₙ] ⇒ List τ ⊣ Γₙ
    /// ```
    ///
    /// **List Spread Patterns:**
    /// ```ignore
    /// ∆;Γ ⊢ p₁ ⇒ τ ⊣ Γ₁    ∆;Γ₁ ⊢ p₂ ⇒ τ ⊣ Γ₂    ...    ∆;Γₙ₋₁ ⊢ pₙ ⇒ τ ⊣ Γₙ
    /// -----------------------
    /// ∆;Γ ⊢ [p₁, p₂, ..., pₙ, ...x] ⇒ List τ ⊣ Γₙ[x : List τ]
    /// ```
    ///
    /// **Variant Patterns:**
    /// ```ignore
    /// ∆;Γ ⊢ p₁ ⇒ τ₁ ⊣ Γ₁    ∆;Γ₁ ⊢ p₂ ⇒ τ₂ ⊣ Γ₂    ...    ρ fresh
    /// -----------------------
    /// ∆;Γ ⊢ < l₁ : p₁, l₂ : p₂, ... > ⇒ < l₁ : τ₁, l₂ : τ₂, ... | ρ > ⊣ Γₙ
    /// ```
    ///
    /// Implementation:
    /// - Type source expression to get discriminant type
    /// - For each branch: type-check pattern against discriminant, bind pattern variables
    /// - Type-check branch expressions in extended environments
    /// - Constrain all branch expressions to have the same result type
    ///
    /// Type signature: `α → β` where α is discriminant type, β is unified result type
    fn visit_case_expr(
        &mut self,
        id: Id<node::CaseExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::CaseExpr { source, branches } = id.get(tree);
        let span = self.span(id);

        // Step 1: Type the source expression to get discriminant type
        self.visit_expr(*source, tree)?;
        let source_t = self.types.meta(*source).clone();

        // Step 2: Process each branch
        let mut result_types = Vec::new();

        for &branch_id in branches {
            let node::CaseBranch { pat, matches } = *branch_id.get(tree);

            // TODO: Implement proper pattern typing that:
            // - Type-checks pattern against source type
            // - Binds pattern variables in local environment
            // - Extends environment for expression typing
            //
            // Pattern typing requires:
            // 1. visit_pattern(pat, source_t) -> (pattern_t, env_extension)
            // 2. constrain(pattern_t, source_t)
            // 3. type check expression in extended environment
            //
            // For now, we provide a basic implementation that visits
            // patterns and expressions but doesn't handle variable binding

            // Save current environment to restore after pattern typing
            let env = self.local_env.clone();

            // Create fresh PatternTyper for this branch
            let pattern_typer = PatternTyper::new(&mut self.local_env, self.cons, source_t.clone());
            pattern_typer.run::<T, node::Pat>(pat, tree);

            // Type branch expression with extended environment
            self.visit_expr(matches, tree)?;

            // TODO use better mechanism to restore the env
            self.local_env = env;

            let branch_t = self.types.meta(matches).clone();
            result_types.push(branch_t);
        }

        // Step 3: Unify all branch result types
        let Some((first, remaining)) = result_types.split_first() else {
            return ControlFlow::Break(Diagnostic::error(
                span,
                "Case expression must have at least one branch",
            ));
        };

        for branch_t in remaining {
            // Constrain all branch expressions to have the same type
            self.cons.constrain(first.clone(), branch_t.clone(), span);
        }

        // Assign the unified type to the case expression
        self.insert_type(id, first.clone());

        ControlFlow::Continue(())
    }

    /// Rule for If Expression
    ///
    /// ```ignore
    /// ∆;Γ ⊢ predicate : predicate_t
    /// ∆;Γ ⊢ then : then_t
    /// ∆;Γ ⊢ or : or_t
    /// unify(predicate_t, Bool)
    /// unify(then_t, else_t)
    /// -----------------------
    /// ∆;Γ ⊢ if predicate then then else or : then_t
    /// ```
    ///
    /// Type signature: `Bool → α → α → α`
    fn visit_if_expr(&mut self, id: Id<node::IfExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_if_expr(id, tree)?;

        let span = self.span(id);

        let node::IfExpr {
            predicate,
            then,
            or,
        } = *id.get(tree);

        let predicate_t = self.types.meta(predicate).clone();

        self.cons.constrain(MonoType::BOOL, predicate_t, span);

        let then_t = self.types.meta(then).clone();
        let or_t = self.types.meta(or).clone();

        self.cons.constrain(then_t.clone(), or_t, span);

        self.insert_type(id, then_t);
        ControlFlow::Continue(())
    }

    /// Rule for Lambda Expression
    ///
    /// ```ignore
    /// param_t = newvar()
    /// ∆;Γ, param : param_t ⊢ body : body_t
    /// -----------------------
    /// ∆;Γ ⊢ λparam → body : param_t → body_t
    /// ```
    ///
    /// Type signature: `(α → β)` where α is parameter type, β is body type
    fn visit_lambda_expr(
        &mut self,
        id: Id<node::LambdaExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::LambdaExpr {
            param,
            param_type,
            body,
        } = *id.get(tree);

        let param_t = MonoType::variable();

        if let Some(param_type) = param_type {
            self.visit_type(param_type, tree)?;

            let expected_param_t = match self.types.meta(param_type).to_mono() {
                Ok(t) => t,
                Err(e) => return ControlFlow::Break(e.into_diagnostic(self.span(id))),
            };

            // Constrain the parameter type against the expected type
            self.cons
                .constrain_check(expected_param_t, param_t.clone(), self.span(id));
        }

        let name = param.get(tree).0.clone();

        self.local_env.enter(name, param_t.clone());
        self.visit_expr(body, tree)?;
        self.local_env.exit(&name);

        let body_t = self.types.meta(body).clone();
        let lambda_t = MonoType::func(param_t, body_t);

        self.insert_type(id, lambda_t);
        ControlFlow::Continue(())
    }

    /// Rule for Function Application
    ///
    /// ```ignore
    /// ∆;Γ ⊢ func : func_t
    /// ∆;Γ ⊢ arg : arg_t
    /// result_t = newvar()
    /// unify(func_t, arg_t → result_t)
    /// -----------------------
    /// ∆;Γ ⊢ func arg : result_t
    /// ```
    ///
    /// Type signature: `(α → β) → α → β`
    fn visit_call_expr(
        &mut self,
        id: Id<node::CallExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_call_expr(id, tree)?;

        let span = self.span(id);

        let node::CallExpr { func, arg } = *id.get(tree);

        let func_t = self.types.meta(func).clone();
        let arg_t = self.types.meta(arg).clone();
        let result_t = MonoType::variable();

        self.cons
            .constrain(func_t, MonoType::func(arg_t, result_t.clone()), span);

        self.insert_type(id, result_t);
        ControlFlow::Continue(())
    }

    fn visit_tag_expr(&mut self, id: Id<node::TagExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let tag = *id.get(tree).0.get(tree);

        // We want to create an open row type for the tag expression
        let row_var = MonoType::variable();

        let arg_t = MonoType::variable();
        let ret_t = MonoType::row(LabeledType::new(tag.0, arg_t.clone()), row_var);
        let tag_t = MonoType::func(arg_t, ret_t);

        self.insert_type(id, tag_t);

        ControlFlow::Continue(())
    }

    fn visit_expr(&mut self, id: Id<node::Expr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_expr(id, tree)?;

        let expr_t = match *id.get(tree) {
            node::Expr::Error(_) => todo!(),
            node::Expr::Literal(id) => self.types.meta(id),
            node::Expr::Qualified(id) => self.types.meta(id),
            node::Expr::List(id) => self.types.meta(id),
            node::Expr::Record(id) => self.types.meta(id),
            node::Expr::RecordExtend(id) => self.types.meta(id),
            node::Expr::RecordRestrict(id) => self.types.meta(id),
            node::Expr::RecordUpdate(id) => self.types.meta(id),
            node::Expr::Unary(id) => self.types.meta(id),
            node::Expr::Binary(id) => self.types.meta(id),
            node::Expr::Let(id) => self.types.meta(id),
            node::Expr::If(id) => self.types.meta(id),
            node::Expr::Case(id) => self.types.meta(id),
            node::Expr::Lambda(id) => self.types.meta(id),
            node::Expr::Call(id) => self.types.meta(id),
            node::Expr::Tag(id) => self.types.meta(id),
        }
        .clone();

        self.insert_type(id, expr_t);
        ControlFlow::Continue(())
    }
}

#[cfg(test)]
mod tests {
    use std::{ops::ControlFlow, rc::Rc};

    use camino::Utf8PathBuf;
    use kola_resolver::phase::ResolvedNodes;
    use kola_span::{Loc, Located, SourceId, Span};
    use kola_syntax::loc::Locations;
    use kola_tree::prelude::*;
    use kola_utils::interner::{PathInterner, StrInterner};

    use super::{TypedNodes, Typer};
    use crate::{
        env::{GlobalTypeEnv, KindEnv, ModuleTypeEnv},
        error::{TypeError, TypeErrors},
        prelude::{Substitutable, Substitution},
        typer::Constraints,
        types::*,
    };

    fn mocked_source() -> SourceId {
        let mut interner = PathInterner::new();
        interner.intern(Utf8PathBuf::from("test"))
    }

    fn mocked_spans(source_id: SourceId, tree: &impl TreeView) -> Locations {
        let span = Loc::new(source_id, Span::new(0, 0));
        tree.metadata_with(|node| Meta::default_with(span, node.kind()))
    }

    fn solve<T>(tree: TreeBuilder, root_id: Id<T>) -> Result<TypedNodes, Located<TypeErrors>>
    where
        Id<T>: Visitable<TreeBuilder>,
    {
        let source_id = mocked_source();
        let spans = Rc::new(mocked_spans(source_id, &tree));

        let type_env = GlobalTypeEnv::new();
        let module_scope = ModuleTypeEnv::new();
        let interner = StrInterner::new(); // TODO for tests with builtin types the interner should be passed
        let resolved = ResolvedNodes::new();

        let mut cons = Constraints::new();
        let mut typer = Typer::new(
            root_id,
            spans,
            &mut cons,
            &module_scope,
            &type_env,
            &interner,
            &resolved,
        );

        match root_id.visit_by(&mut typer, &tree) {
            ControlFlow::Break(e) => {
                panic!("Error during type checking: {}", e);
            }
            ControlFlow::Continue(()) => (),
        }

        let Typer { mut types, .. } = typer;

        let mut subs = Substitution::empty();
        let mut kind_env = KindEnv::new();

        cons.solve(&mut subs, &mut kind_env)?;
        types.apply_mut(&mut subs);

        Ok(types)
    }

    #[test]
    fn literal() {
        let mut builder = TreeBuilder::new();
        let lit = builder.insert(node::LiteralExpr::Num(10.0));

        let types = solve(builder, lit).unwrap();

        assert_eq!(types.meta(lit), &MonoType::NUM);
    }

    #[test]
    fn unary() {
        let mut builder = TreeBuilder::new();

        let target = builder.insert(node::LiteralExpr::Num(10.0));
        let unary = node::UnaryExpr::new_in(node::UnaryOp::Neg, target, &mut builder);

        let types = solve(builder, unary).unwrap();

        assert_eq!(types.meta(unary), &MonoType::NUM);
    }

    #[test]
    fn unary_err() {
        let mut builder = TreeBuilder::new();

        let target = builder.insert(node::LiteralExpr::Num(10.0));
        let unary = node::UnaryExpr::new_in(node::UnaryOp::Not, target, &mut builder);

        let (errors, _) = solve(builder, unary).unwrap_err();

        assert_eq!(
            errors[0],
            TypeError::CannotUnify {
                expected: MonoType::BOOL,
                actual: MonoType::NUM
            }
        );
    }

    #[test]
    fn binary_err() {
        let mut builder = TreeBuilder::new();

        let left = builder.insert(node::LiteralExpr::Bool(true));
        let right = builder.insert(node::LiteralExpr::Num(10.0));
        let binary = node::BinaryExpr::new_in(node::BinaryOp::Eq, left, right, &mut builder);

        let (errors, _) = solve(builder, binary).unwrap_err();

        assert_eq!(
            errors[0],
            TypeError::CannotUnify {
                expected: MonoType::BOOL,
                actual: MonoType::NUM
            }
        );
    }

    #[test]
    fn let_() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();

        let value = builder.insert(node::LiteralExpr::Num(10.0));
        let x = interner.intern("x");
        let inside = node::QualifiedExpr::new_in(None, x, None, &mut builder);
        let let_ = node::LetExpr::new_in(x, None, value, inside, &mut builder);

        let types = solve(builder, let_).unwrap();

        assert_eq!(types.meta(let_), &MonoType::NUM);
    }

    #[test]
    fn if_() {
        let mut builder = TreeBuilder::new();

        let predicate = builder.insert(node::LiteralExpr::Bool(true));
        let then = builder.insert(node::LiteralExpr::Num(5.0));
        let or = builder.insert(node::LiteralExpr::Num(10.0));
        let if_ = node::IfExpr::new_in(predicate, then, or, &mut builder);

        let types = solve(builder, if_).unwrap();

        assert_eq!(types.meta(if_), &MonoType::NUM);
    }

    #[test]
    fn if_err() {
        let mut builder = TreeBuilder::new();

        let predicate = builder.insert(node::LiteralExpr::Bool(true));
        let then = builder.insert(node::LiteralExpr::Num(5.0));
        let or = builder.insert(node::LiteralExpr::Char('x'));
        let if_ = node::IfExpr::new_in(predicate, then, or, &mut builder);

        let (errors, _) = solve(builder, if_).unwrap_err();

        assert_eq!(
            errors[0],
            TypeError::CannotUnify {
                expected: MonoType::NUM,
                actual: MonoType::CHAR,
            }
        );
    }
}
