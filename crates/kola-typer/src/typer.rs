use std::{ops::ControlFlow, rc::Rc};

use kola_resolver::phase::ResolvedNodes;
use kola_span::{Diagnostic, IntoDiagnostic, Loc, Located, Report};
use kola_syntax::prelude::*;
use kola_tree::prelude::*;
use kola_utils::{errors::Errors, fmt::StrInternerExt, interner::StrInterner};

use crate::{
    env::{KindEnv, TypeEnv},
    error::TypeErrors,
    phase::{TypePhase, TypedNodes},
    scope::{BoundVars, TypeScope},
    substitute::{Substitutable, Substitution},
    types::{Kind, LabeledType, MonoType, PolyType, TypeVar, Typed},
    unify::Unifiable,
};

// https://blog.stimsina.com/post/implementing-a-hindley-milner-type-system-part-2

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constraint {
    Kind {
        expected: Kind,
        actual: MonoType,
        span: Loc,
    },
    Ty {
        expected: MonoType,
        actual: MonoType,
        span: Loc,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constraints(Vec<Constraint>);

impl Constraints {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn constrain(&mut self, expected: MonoType, actual: MonoType, span: Loc) {
        let c = Constraint::Ty {
            expected,
            actual,
            span,
        };
        self.0.push(c);
    }

    pub fn constrain_kind(&mut self, expected: Kind, actual: MonoType, span: Loc) {
        let c = Constraint::Kind {
            expected,
            actual,
            span,
        };
        self.0.push(c);
    }

    // TODO error handling do not propagate but keep trace of errors
    pub fn solve(
        self,
        s: &mut Substitution,
        kind_env: &mut KindEnv,
    ) -> Result<(), Located<TypeErrors>> {
        // TODO infer Types first ?
        for c in self.0 {
            match c {
                Constraint::Kind {
                    expected,
                    actual,
                    span,
                } => {
                    actual
                        .apply_cow(s)
                        .constrain(expected, kind_env)
                        .map_err(|e| (Errors::unit(e), span))?;
                }
                Constraint::Ty {
                    expected,
                    actual,
                    span,
                } => {
                    expected
                        .try_unify(&actual, s)
                        .map_err(|errors| ((errors, span)))?;
                }
            }
        }

        Ok(())
    }
}

// ∆ = Kind Environment
// Γ = Type Environment

// Generalization
// Γ'(τ) quantifies all monotype variables not bound in Γ

pub struct Typer<'a, N> {
    root_id: Id<N>,
    subs: Substitution,
    cons: Constraints,
    type_scope: TypeScope,
    kind_scope: KindEnv,
    types: TypedNodes,
    spans: Rc<Locations>,
    env: &'a TypeEnv,
    interner: &'a StrInterner,
    resolved: &'a ResolvedNodes,
}

impl<'a, N> Typer<'a, N> {
    pub fn new(
        root_id: Id<N>,
        spans: Rc<Locations>,
        env: &'a TypeEnv,
        interner: &'a StrInterner,
        resolved: &'a ResolvedNodes,
    ) -> Self {
        Self {
            root_id,
            subs: Substitution::empty(),
            cons: Constraints::new(),
            type_scope: TypeScope::new(),
            kind_scope: KindEnv::new(),
            types: TypedNodes::new(),
            spans,
            env,
            interner,
            resolved,
        }
    }

    pub fn solve<Tree>(
        mut self,
        tree: &Tree,
        interner: &'a StrInterner,
        report: &mut Report,
    ) -> Option<TypedNodes>
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

        let Self {
            mut subs,
            cons,
            kind_scope: mut k_env,
            mut types,
            ..
        } = self;

        if let Err((errs, loc)) = cons.solve(&mut subs, &mut k_env) {
            let diag = interner.display(&errs).into_diagnostic(loc);
            report.add_diagnostic(diag);
            return None;
        }
        types.apply_mut(&mut subs);

        Some(types)
    }

    #[inline]
    fn update_type<T>(&mut self, id: Id<T>, t: T::Meta)
    where
        T: MetaCast<TypePhase>,
    {
        // self.types.update_meta(id, t)
        self.types.insert(id.as_usize(), T::upcast(t));
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

    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeBind { name, ty } = *id.get(tree);

        self.visit_type(ty, tree)?;
        let pt = self.types.meta(ty).clone();

        self.update_type(id, pt);
        ControlFlow::Continue(())
    }

    fn visit_type(&mut self, id: Id<node::Type>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let node::Type { vars, ty } = id.get(tree);

        let type_vars = vars
            .iter()
            .map(|v| {
                let type_var = TypeVar::new();

                let name = v.get(tree).0;
                let pt = PolyType::new(type_var.into());

                self.type_scope.enter(name, pt);
                type_var
            })
            .collect::<Vec<_>>();

        self.visit_type_expr(*ty, tree)?;
        let ty = self.types.meta(*ty).to_mono().unwrap();

        for var in vars.iter().rev() {
            let name = var.get(tree).0;
            self.type_scope.exit(&name);
        }

        let pt = PolyType {
            vars: type_vars,
            ty,
        };
        self.update_type(id, pt);

        ControlFlow::Continue(())
    }

    fn visit_type_expr(
        &mut self,
        id: Id<node::TypeExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_expr(id, tree)?;

        // Are these all MonoTypes?
        let pt = match *id.get(tree) {
            node::TypeExpr::Error(_) => todo!(),
            node::TypeExpr::Path(p) => self.types.meta(p).clone(),
            node::TypeExpr::Func(f) => PolyType::new(self.types.meta(f).clone()),
            node::TypeExpr::Application(a) => self.types.meta(a).clone(),
            node::TypeExpr::Record(r) => PolyType::new(self.types.meta(r).clone()),
            node::TypeExpr::Variant(v) => PolyType::new(self.types.meta(v).clone()),
        };

        self.update_type(id, pt);
        ControlFlow::Continue(())
    }

    fn visit_type_path(
        &mut self,
        id: Id<node::TypePath>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        fn builtin_type(name: &str) -> Option<MonoType> {
            let t = match name {
                "Bool" => MonoType::BOOL,
                "Char" => MonoType::CHAR,
                "Num" => MonoType::NUM,
                "Str" => MonoType::STR,
                _ => return None,
            };

            Some(t)
        }

        let node::TypePath { path, ty } = *id.get(tree);

        let name = ty.get(tree).0;
        let span = self.span(id);

        let pt = if let Some(path) = path {
            // Module-qualified path
            let module_sym = *self.resolved.meta(path);

            let type_sym = self.env[module_sym]
                .get_type(name)
                .expect("Value not found in module");

            let pt = &self.env[type_sym];
            pt.clone()
        } else if let Some(pt) = self.type_scope.get(&name) {
            // Local type parameter (like 'a' in forall a)
            pt.clone()
        }
        // Builtin's must be handled before checking the type env,
        // because they do not populate the "resolved" map (would cause a panic)
        else if let Some(t) = self.interner.get(name).and_then(builtin_type) {
            PolyType::new(t)
        } else if let Some(pt) = self.env.get_type(*self.resolved.meta(id)) {
            // Module-level type definition (like 'Person')
            pt.clone()
        } else {
            return ControlFlow::Break(Diagnostic::error(span, "Type not found in scope"));
        };

        self.update_type(id, pt);
        ControlFlow::Continue(())
    }

    fn visit_func_type(
        &mut self,
        id: Id<node::FuncType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::FuncType { input, output } = *id.get(tree);

        self.visit_type_expr(input, tree)?;
        let arg = self.types.meta(input).to_mono().unwrap();

        self.visit_type_expr(output, tree)?;
        let ret = self.types.meta(output).to_mono().unwrap();

        let func_t = MonoType::func(arg, ret);

        self.update_type(id, func_t);
        ControlFlow::Continue(())
    }

    fn visit_type_application(
        &mut self,
        id: Id<node::TypeApplication>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeApplication { constructor, arg } = *id.get(tree);

        // Okay here is a problem again, as far as I can see it would be great if the
        // ctor would be a PolyType and the arg a MonoType.

        self.visit_type_expr(arg, tree)?;
        let arg = self.types.meta(arg).to_mono().unwrap();

        self.visit_type_expr(constructor, tree)?;
        let PolyType { vars, ty } = self.types.meta(constructor);

        let [first, rest @ ..] = vars.as_slice() else {
            return ControlFlow::Break(Diagnostic::error(
                self.span(id),
                "Type application requires at least one type variable",
            ));
        };

        // TODO should I really eagerly substitute here if my typer is essentially lazy constraint based ?
        let mut subs = Substitution::unit(*first, arg);

        let ty = ty.clone().apply(&mut subs);

        let pt = PolyType {
            ty,
            vars: rest.to_vec(),
        };

        self.update_type(id, pt);
        ControlFlow::Continue(())
    }

    fn visit_record_type(
        &mut self,
        id: Id<node::RecordType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_type(id, tree)?;

        let node::RecordType { fields, extension } = id.get(tree);

        let tail = if let Some(ext) = extension {
            let name = ext.get(tree).0;
            let Some(pt) = self.type_scope.get(&name).cloned() else {
                return ControlFlow::Break(Diagnostic::error(
                    self.span(id),
                    format!("Type variable not found in scope"),
                ));
            };
            pt.to_mono().unwrap()
        } else {
            MonoType::empty_row()
        };

        let record_t = fields.iter().fold(tail, |acc, &field| {
            let head = self.types.meta(field).clone();
            MonoType::row(head, acc)
        });

        self.update_type(id, record_t);
        ControlFlow::Continue(())
    }

    fn visit_record_field_type(
        &mut self,
        id: Id<node::RecordFieldType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordFieldType { name, ty } = *id.get(tree);

        let label = name.get(tree).0;

        self.visit_type_expr(ty, tree)?;
        let ty = self.types.meta(ty).to_mono().unwrap();

        let labeled_t = LabeledType { label, ty };

        self.update_type(id, labeled_t);
        ControlFlow::Continue(())
    }

    fn visit_variant_type(
        &mut self,
        id: Id<node::VariantType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_variant_type(id, tree)?;

        let node::VariantType { cases, extension } = id.get(tree);

        let tail = if let Some(ext) = extension {
            let name = ext.get(tree).0;
            let Some(pt) = self.type_scope.get(&name).cloned() else {
                return ControlFlow::Break(Diagnostic::error(
                    self.span(id),
                    "Type variable not found in scope",
                ));
            };
            pt.to_mono().unwrap()
        } else {
            MonoType::empty_row()
        };

        let variant_t = cases.iter().fold(tail, |acc, &case| {
            let head = self.types.meta(case).clone();
            MonoType::row(head, acc)
        });

        self.update_type(id, variant_t);
        ControlFlow::Continue(())
    }

    fn visit_variant_case_type(
        &mut self,
        id: Id<node::VariantCaseType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::VariantCaseType { name, ty } = *id.get(tree);

        let label = name.get(tree).0;

        let ty = if let Some(ty) = ty {
            self.visit_type_expr(ty, tree)?;
            self.types.meta(ty).to_mono().unwrap()
        } else {
            MonoType::UNIT
        };

        let labeled_t = LabeledType { label, ty };

        self.update_type(id, labeled_t);
        ControlFlow::Continue(())
    }

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

        // due to explicit traversal this is known
        let t = self.types.meta(value).clone();
        // generalize with no bound type vars because this is a top-level binding.
        let pt = t.generalize(&[]);

        if let Some(ty) = ty {
            self.visit_type(ty, tree)?;
            let expected_pt = self.types.meta(ty).clone();

            // if !pt.alpha_equivalent(&expected_pt) {
            //     return ControlFlow::Break(Diagnostic::error(
            //         self.span(id),
            //         format!("Type mismatch: expected {}, found {}", expected_pt, pt),
            //     ));
            // }

            self.cons.constrain(expected_pt.instantiate(), t, span);
            self.update_type(id, expected_pt); // type ascription
        } else {
            self.update_type(id, pt);
        }

        ControlFlow::Continue(())
    }

    // Patterns

    // Expr

    /// Rule for Literal Expressions
    ///
    /// ```
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
            &node::LiteralExpr::Bool(_) => MonoType::BOOL,
            &node::LiteralExpr::Char(_) => MonoType::CHAR,
            &node::LiteralExpr::Num(_) => MonoType::NUM,
            &node::LiteralExpr::Str(_) => MonoType::STR,
        };

        self.update_type(id, actual);
        ControlFlow::Continue(())
    }

    /// Rule for List Expression
    ///
    /// ```
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

            self.update_type(id, MonoType::list(first_t.clone()));

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
    /// ```
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
    /// ```
    /// x : σ ∈ Γ   base_t = inst(σ)
    /// -----------------------
    /// ∆;Γ ⊢ x : base_t
    /// ```
    ///
    /// Type signature: `∀r α. {l : α | r} → α` (for field selection)
    fn visit_path_expr(
        &mut self,
        id: Id<node::PathExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let span = self.span(id);

        let node::PathExpr {
            path,
            binding,
            select,
        } = id.get(tree);

        let name = binding.get(tree).0;

        let base_t = if let Some(path) = *path {
            let module_sym = *self.resolved.meta(path);

            let value_sym = self.env[module_sym]
                .get_value(name)
                .expect("Value not found in module");

            let pt = &self.env[value_sym];
            pt.instantiate()
        } else if let Some(pt) = self.type_scope.get(&name) {
            pt.instantiate()
        }
        // For the future: Builtin's should be checked before the type env because they probably
        // do not populate the "resolved" map
        else if let Some(pt) = self.env.get_value(*self.resolved.meta(id)) {
            pt.instantiate()
        } else {
            return ControlFlow::Break(Diagnostic::error(span, "Value not found in scope"));
        };

        let result_t = select.iter().fold(base_t, |record_t, field| {
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

        self.update_type(id, result_t);
        ControlFlow::Continue(())
    }

    /// Rule for Record Field
    ///
    /// ```
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

        let node::RecordField { field, value } = *id.get(tree);

        let label = field.get(tree).0.clone();
        let value_t = self.types.meta(value).clone();

        self.update_type(id, LabeledType { label, ty: value_t });
        ControlFlow::Continue(())
    }

    /// Rule for Record Expression
    ///
    /// ```
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

        self.update_type(id, record_t);
        ControlFlow::Continue(())
    }

    /// Rule for Record Extension of `source` with a new field `label` and value `value`
    ///
    /// ```
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
        self.walk_record_extend_expr(id, tree)?;

        let span = self.span(id);

        let node::RecordExtendExpr {
            source,
            field,
            value,
        } = *id.get(tree);

        let value_t = self.types.meta(value);
        let source_t = self.types.meta(source);

        self.cons
            .constrain_kind(Kind::Record, source_t.clone(), span);

        let label = field.get(tree).0.clone();
        let head_t = LabeledType {
            label,
            ty: value_t.clone(),
        };
        let result_t = MonoType::row(head_t, source_t.clone());

        self.update_type(id, result_t);
        ControlFlow::Continue(())
    }

    /// Rule for Record Restriction of `source` with label `field`
    ///
    /// ```
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
        self.walk_record_restrict_expr(id, tree)?;

        let span = self.span(id);

        let node::RecordRestrictExpr { source, field } = *id.get(tree);

        let source_t = self.types.meta(source);
        self.cons
            .constrain_kind(Kind::Record, source_t.clone(), span);

        let value_t = MonoType::variable();
        let label = field.get(tree).0.clone();
        let head_t = LabeledType { label, ty: value_t };
        let tail_t = MonoType::variable();
        self.cons.constrain(
            MonoType::row(head_t, tail_t.clone()),
            source_t.clone(),
            span,
        );

        self.update_type(id, tail_t);
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

        self.update_type(id, update_func_t);
        ControlFlow::Continue(())
    }

    /// Rule for Record Update of `source` with label `field` and value `value`
    ///
    /// ```
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
            field,
            op,
            value,
        } = *id.get(tree);

        let source_t = self.types.meta(source);
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

        self.update_type(id, result_t);
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

        self.update_type(id, unary_func_t);
        ControlFlow::Continue(())
    }

    /// Rule for Unary Expression (Application of unary operator)
    ///
    /// ```
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

        self.update_type(id, result_t);
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

        self.update_type(id, binary_func_t);
        ControlFlow::Continue(())
    }

    /// Rule for Binary Expression (Application of binary operator)
    ///
    /// ```
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

        self.update_type(id, result_t);
        ControlFlow::Continue(())
    }

    /// Rule for Let Expression
    ///
    /// ```
    /// ∆;Γ ⊢ value : value_t
    /// value_pt = generalize(value_t, ftv(Γ))
    /// ∆;Γ, name : value_pt ⊢ inside : result_t
    /// -----------------------
    /// ∆;Γ ⊢ let name = value in inside : result_t
    /// ```
    ///
    /// Implementation uses generalization to allow polymorphic let-bindings
    fn visit_let_expr(&mut self, id: Id<node::LetExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let &node::LetExpr {
            name,
            value,
            inside,
        } = id.get(tree);

        let name = name.get(tree).0.clone();

        TypeVar::enter();
        self.visit_expr(value, tree)?;
        TypeVar::exit();

        // due to explicit traversal this is known
        let value_t = self.types.meta(value).clone();
        let value_pt = value_t.generalize(&self.type_scope.bound_vars());

        self.type_scope.enter(name, value_pt);
        self.visit_expr(inside, tree)?;
        self.type_scope.exit(&name);

        let result_t = self.types.meta(inside).clone();
        self.update_type(id, result_t);

        ControlFlow::Continue(())
    }

    fn visit_case_expr(
        &mut self,
        _id: Id<node::CaseExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    /// Rule for If Expression
    ///
    /// ```
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

        self.update_type(id, then_t);
        ControlFlow::Continue(())
    }

    /// Rule for Lambda Expression
    ///
    /// ```
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
        let node::LambdaExpr { param, body } = *id.get(tree);
        let param_t = MonoType::variable();

        // let name = self.types.meta(func.param).clone();
        let name = param.get(tree).0.clone();

        self.type_scope.enter(name, PolyType::from(param_t.clone()));
        self.visit_expr(body, tree)?;
        self.type_scope.exit(&name);

        let body_t = self.types.meta(body).clone();
        let lambda_t = MonoType::func(param_t, body_t);

        self.update_type(id, lambda_t);
        ControlFlow::Continue(())
    }

    /// Rule for Function Application
    ///
    /// ```
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

        self.update_type(id, result_t);
        ControlFlow::Continue(())
    }

    fn visit_expr(&mut self, id: Id<node::Expr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_expr(id, tree)?;

        let expr_t = match *id.get(tree) {
            node::Expr::Error(_) => todo!(),
            node::Expr::Literal(id) => self.types.meta(id),
            node::Expr::Path(id) => self.types.meta(id),
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
        }
        .clone();

        self.update_type(id, expr_t);
        ControlFlow::Continue(())
    }
}

#[cfg(test)]
mod tests {
    use std::{ops::ControlFlow, rc::Rc};

    use camino::Utf8PathBuf;
    use kola_resolver::phase::ResolvedNodes;
    use kola_span::{Loc, Located, Report, SourceId, Span};
    use kola_syntax::loc::Locations;
    use kola_tree::prelude::*;
    use kola_utils::interner::{PathInterner, StrInterner};

    use super::{TypedNodes, Typer};
    use crate::{
        env::TypeEnv,
        error::{TypeError, TypeErrors},
        prelude::Substitutable,
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

        let type_env = TypeEnv::new();
        let interner = StrInterner::new(); // TODO for tests with builtin types the interner should be passed
        let resolved = ResolvedNodes::new();

        let mut typer = Typer::new(root_id, spans, &type_env, &interner, &resolved);

        match root_id.visit_by(&mut typer, &tree) {
            ControlFlow::Break(e) => {
                panic!("Error during type checking: {}", e);
            }
            ControlFlow::Continue(()) => (),
        }

        let Typer {
            mut subs,
            cons,
            mut kind_scope,
            mut types,
            ..
        } = typer;

        cons.solve(&mut subs, &mut kind_scope)?;
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
        let unary = node::UnaryExpr::new_in(node::UnaryOp::Neg, target.into(), &mut builder);

        let types = solve(builder, unary).unwrap();

        assert_eq!(types.meta(unary), &MonoType::NUM);
    }

    #[test]
    fn unary_err() {
        let mut builder = TreeBuilder::new();

        let target = builder.insert(node::LiteralExpr::Num(10.0));
        let unary = node::UnaryExpr::new_in(node::UnaryOp::Not, target.into(), &mut builder);

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
        let binary =
            node::BinaryExpr::new_in(node::BinaryOp::Eq, left.into(), right.into(), &mut builder);

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
        let binding = builder.insert(node::Name::from(interner.intern("x")));
        let inside = builder.insert(node::PathExpr {
            path: None,
            binding,
            select: vec![],
        });
        let let_ = node::LetExpr::new_in(
            node::Name::from(interner.intern("x")),
            value.into(),
            inside.into(),
            &mut builder,
        );

        let types = solve(builder, let_).unwrap();

        assert_eq!(types.meta(let_), &MonoType::NUM);
    }

    #[test]
    fn if_() {
        let mut builder = TreeBuilder::new();

        let predicate = builder.insert(node::LiteralExpr::Bool(true));
        let then = builder.insert(node::LiteralExpr::Num(5.0));
        let or = builder.insert(node::LiteralExpr::Num(10.0));
        let if_ = node::IfExpr::new_in(predicate.into(), then.into(), or.into(), &mut builder);

        let types = solve(builder, if_).unwrap();

        assert_eq!(types.meta(if_), &MonoType::NUM);
    }

    #[test]
    fn if_err() {
        let mut builder = TreeBuilder::new();

        let predicate = builder.insert(node::LiteralExpr::Bool(true));
        let then = builder.insert(node::LiteralExpr::Num(5.0));
        let or = builder.insert(node::LiteralExpr::Char('x'));
        let if_ = node::IfExpr::new_in(predicate.into(), then.into(), or.into(), &mut builder);

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
