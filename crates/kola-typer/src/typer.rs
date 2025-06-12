use std::{ops::ControlFlow, rc::Rc};

use kola_resolver::{GlobalId, bind::Bindings};
use kola_span::{Diagnostic, Loc, Located, Report};
use kola_syntax::prelude::*;
use kola_tree::prelude::*;
use kola_utils::errors::Errors;

use crate::{
    env::{KindEnv, TypeEnv},
    error::TypeErrors,
    phase::{TypePhase, TypedNodes},
    scope::{BoundVars, TypeScope},
    substitute::{Substitutable, Substitution},
    types::{Kind, MonoType, PolyType, Property, TypeVar, Typed},
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

// TODO τ for normal types
// r for record types ?

// TODO: Since I have "open polytypes" that need final substitution,
// I should defer populating the public `TypeEnvironment`
// until **after** constraint solving and substitution.
// for that I need to cache the NodeIDs of the "public" types

pub struct Typer<'a, N> {
    root_id: GlobalId<N>,
    subs: Substitution,
    cons: Constraints,
    type_scope: TypeScope,
    kind_scope: KindEnv,
    types: TypedNodes,
    spans: Rc<Locations>,
    env: &'a TypeEnv,
    bindings: &'a Bindings,
}

impl<'a, N> Typer<'a, N> {
    pub fn new(
        root_id: GlobalId<N>,
        spans: Rc<Locations>,
        env: &'a TypeEnv,
        bindings: &'a Bindings,
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
            bindings,
        }
    }

    pub fn solve<Tree>(
        mut self,
        tree: &Tree,
        report: &mut Report,
    ) -> Result<TypedNodes, Located<TypeErrors>>
    where
        Tree: TreeView,
        Id<N>: Visitable<Tree>,
    {
        let root = self.root_id.id;

        match root.visit_by(&mut self, tree) {
            ControlFlow::Break(e) => {
                report.add_diagnostic(e);
                return Ok(self.types);
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

        cons.solve(&mut subs, &mut k_env)?;
        types.apply_mut(&mut subs);

        Ok(types)
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

    #[inline]
    pub fn qualify<T>(&self, id: Id<T>) -> GlobalId<T> {
        GlobalId::new(self.root_id.source, id)
    }

    fn partial_restrict(
        &mut self,
        source: Id<node::Expr>,
        field: Id<node::ValueName>,
        span: Loc,
        tree: &impl TreeView,
    ) -> MonoType {
        let t = self.types.meta(source);

        self.cons.constrain_kind(Kind::Record, t.clone(), span);

        let t_prime = MonoType::variable();

        // TODO handle field path
        let head = Property {
            k: field.get(tree).0.clone(),
            v: MonoType::variable(),
        };

        self.cons
            .constrain(MonoType::row(head, t_prime.clone()), t.clone(), span);

        t_prime
    }
}

impl<'a, T, N> Visitor<T> for Typer<'a, N>
where
    T: TreeView,
    Id<N>: Visitable<T>,
{
    type BreakValue = Diagnostic;

    fn visit_module_bind(
        &mut self,
        _id: Id<node::ModuleBind>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        // Skip module binds altogether
        ControlFlow::Continue(())
    }

    fn visit_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ValueBind { ty, value, .. } = *id.get(tree);

        TypeVar::enter();
        self.visit_expr(value, tree)?;
        TypeVar::exit();

        // due to explicit traversal this is known
        let t = self.types.meta(value).clone();
        // generalize with no bound type vars because this is a top-level binding.
        let pt = t.generalize(&[]);

        if let Some(ty) = ty {
            self.visit_type(ty, tree)?;
            let expected_pt = self.types.meta(ty);

            if !pt.alpha_equivalent(&expected_pt) {
                return ControlFlow::Break(Diagnostic::error(
                    self.span(id),
                    format!("Type mismatch: expected {}, found {}", expected_pt, pt),
                ));
            }
        }

        self.update_type(id, pt);
        ControlFlow::Continue(())
    }

    // Patterns

    // Expr

    // Unit rule
    // -----------------------
    // Γ ⊢ () : unit
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

    fn visit_list_expr(
        &mut self,
        id: Id<node::ListExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_list_expr(id, tree)?;

        let node::ListExpr(list) = id.get(tree);

        if let Some(&el) = list.first() {
            let expected = self.types.meta(el).clone();

            self.update_type(id, MonoType::list(expected.clone()));

            for &el in list {
                let span = self.span(id);
                let actual = self.types.meta(el).clone();

                self.cons.constrain(expected.clone(), actual, span);
            }
        }

        ControlFlow::Continue(())
    }

    // Rule for Record Selection of 'r' with label 'l'
    // τ' = newvar()
    // ∆;Γ ⊢ r : { τ0 | l : τ' } where τ0 is of kind Record
    // -----------------------
    // ∆;Γ ⊢ r.l : τ'
    //
    // Var rule to infer variable 'x'
    // x : σ ∈ Γ   τ = inst(σ)
    // -----------------------
    // ∆;Γ ⊢ x : τ
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

        let t = if let Some(path) = *path {
            let module_sym = self
                .bindings
                .lookup_module_path(self.qualify(path))
                .expect("Module path not found");

            let value_sym = self.env[module_sym]
                .get_value(name)
                .expect("Value not found in module");

            let pt = &self.env[value_sym];
            pt.instantiate()
        } else if let Some(pt) = self.type_scope.get(&name) {
            pt.instantiate()
        } else {
            return ControlFlow::Break(Diagnostic::error(span, "Value not found in scope"));
        };

        let field_t = select.iter().fold(t, |t, field| {
            self.cons.constrain_kind(Kind::Record, t.clone(), span);

            let field_t = MonoType::variable();

            let head = Property {
                k: field.get(tree).0.clone(),
                v: field_t.clone(),
            };

            self.cons
                .constrain(MonoType::row(head, MonoType::variable()), t.clone(), span);

            field_t
        });

        self.update_type(id, field_t);
        ControlFlow::Continue(())
    }

    fn visit_record_field(
        &mut self,
        id: Id<node::RecordField>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_field(id, tree)?;

        let node::RecordField { field, value } = *id.get(tree);

        let k = field.get(tree).0.clone();
        let v = self.types.meta(value).clone();

        self.update_type(id, Property { k, v });
        ControlFlow::Continue(())
    }

    // Rule for Record Instantiation via Induction
    // ∆;Γ ⊢ R : { l0 : t0, ..., ln : tn | {} }
    // -----------------------
    // ∆;Γ ⊢ R : { { l1 : t1, ..., ln : tn } | +l0 : τ0 | {} }
    fn visit_record_expr(
        &mut self,
        id: Id<node::RecordExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_expr(id, tree)?;

        let mut r = MonoType::empty_row();

        for &field in id.get(tree) {
            let head = self.types.meta(field).clone();
            r = MonoType::row(head, r);
        }

        self.update_type(id, r);
        ControlFlow::Continue(())
    }

    // Rule for Record Extension of 'r' with label 'l' and Value 'v'
    // ∆;Γ ⊢ v : τ0
    // ∆;Γ ⊢ r : τ1 where τ1 is of kind Record
    // -----------------------
    // ∆;Γ ⊢ { r | +l = v } : { τ1 | +l : τ0 }

    // old: ∀rα. α → {r} → {l :: α | r}
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

        let t0 = self.types.meta(value);
        let t1 = self.types.meta(source);

        self.cons.constrain_kind(Kind::Record, t1.clone(), span);

        // TODO handle record field path
        let head = Property {
            k: field.get(tree).0.clone(),
            v: t0.clone(),
        };
        let row = MonoType::row(head, t1.clone());

        self.update_type(id, row);
        ControlFlow::Continue(())
    }

    // Rule for Record Restriction of 'r' with label 'l'
    // ∆;Γ ⊢ r : { τ0 | l : τ1 }
    // -----------------------
    // ∆;Γ ⊢ { r | -l } : τ0
    // ∀rα. {l :: α | r} → {r}
    fn visit_record_restrict_expr(
        &mut self,
        id: Id<node::RecordRestrictExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_restrict_expr(id, tree)?;

        let span = self.span(id);

        let node::RecordRestrictExpr { source, field } = *id.get(tree);

        let t_prime = self.partial_restrict(source, field, span, tree);

        self.update_type(id, t_prime);
        ControlFlow::Continue(())
    }

    // Rule for Record Update of 'r' with label 'l' and value 'v'
    // ∆;Γ ⊢ r : { τ0 | l : τ1 }
    // ∆;Γ ⊢ v : τ2
    // -----------------------
    // ∆;Γ ⊢ { r | l = v } : { r | -l | +l : τ2 }
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

        let t0 = self.partial_restrict(source, field, span, tree);
        let t2 = self.types.meta(value).clone();

        let head = Property {
            k: field.get(tree).0.clone(),
            v: t2,
        };
        let row = MonoType::row(head, t0);

        self.update_type(id, row);
        ControlFlow::Continue(())
    }

    // Abstraction rule
    // τ = newvar()
    // Γ, x : τ ⊢ e : τ'
    // ___________________
    // Γ ⊢ \x -> e : τ -> τ'
    fn visit_unary_op(&mut self, id: Id<node::UnaryOp>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let t = match id.get(tree) {
            &node::UnaryOp::Neg => MonoType::NUM,
            &node::UnaryOp::Not => MonoType::BOOL,
        };

        let func = MonoType::func(t.clone(), t);

        self.update_type(id, func);
        ControlFlow::Continue(())
    }

    // Application rule
    // Γ ⊢ f : τ0
    // Γ ⊢ x : τ1
    // τ' = newvar()
    // unify(τ0, τ1 -> τ')
    // --------------------
    // Γ ⊢ f x : τ'
    fn visit_unary_expr(
        &mut self,
        id: Id<node::UnaryExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_unary_expr(id, tree)?;

        let span = self.span(id);

        let node::UnaryExpr { op, operand } = *id.get(tree);

        let t0 = self.types.meta(op).clone();
        let t1 = self.types.meta(operand).clone();

        let t_prime = MonoType::variable();

        self.cons
            .constrain(t0, MonoType::func(t1, t_prime.clone()), span);

        self.update_type(id, t_prime);
        ControlFlow::Continue(())
    }

    fn visit_binary_op(
        &mut self,
        id: Id<node::BinaryOp>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let span = self.span(id);

        let (t, t_prime) = match id.get(tree) {
            node::BinaryOp::Add => {
                let t = MonoType::variable();
                self.cons.constrain_kind(Kind::Addable, t.clone(), span);
                (t, MonoType::NUM)
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
                let t = MonoType::variable();
                self.cons.constrain_kind(Kind::Comparable, t.clone(), span);
                (t, MonoType::BOOL)
            }
            // Logical
            node::BinaryOp::And | node::BinaryOp::Or | node::BinaryOp::Xor => {
                (MonoType::BOOL, MonoType::BOOL)
            }
            // Equality
            node::BinaryOp::Eq | node::BinaryOp::NotEq => {
                let t = MonoType::variable();
                self.cons.constrain_kind(Kind::Equatable, t.clone(), span);
                (t, MonoType::BOOL)
            }
            // Record
            node::BinaryOp::Merge => {
                todo!();
            }
        };

        let func = MonoType::func(t.clone(), MonoType::func(t, t_prime));

        self.update_type(id, func);
        ControlFlow::Continue(())
    }

    // Application rule
    // Γ ⊢ f : τ0
    // Γ ⊢ x : τ1
    // τ' = newvar()
    // unify(τ0, τ1 -> τ')
    // --------------------
    // Γ ⊢ f x : τ'
    fn visit_binary_expr(
        &mut self,
        id: Id<node::BinaryExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_binary_expr(id, tree)?;

        let span = self.span(id);

        let node::BinaryExpr { left, op, right } = *id.get(tree);

        let t0 = self.types.meta(op).clone();
        let lhs = self.types.meta(left).clone();
        let rhs = self.types.meta(right).clone();

        let t_prime = MonoType::variable();

        let func = MonoType::func(lhs, MonoType::func(rhs, t_prime.clone()));
        self.cons.constrain(t0, func, span);

        self.update_type(id, t_prime);
        ControlFlow::Continue(())
    }

    // Let rule
    // Γ ⊢ e0 : τ
    // Γ, x : Γ'(τ) ⊢ e1 : τ'
    // --------------------
    // Γ ⊢ let x = e0 in e1 : τ'
    fn visit_let_expr(&mut self, id: Id<node::LetExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let &node::LetExpr {
            name,
            value,
            inside,
        } = id.get(tree);

        // TODO unsure if name is already populated by init inside self.types
        // otherwise implement traverse function for name

        let name = name.get(tree).0.clone();

        TypeVar::enter();
        self.visit_expr(value, tree)?;
        TypeVar::exit();

        // due to explicit traversal this is known
        let t = self.types.meta(value).clone();
        let pt = t.generalize(&self.type_scope.bound_vars());

        self.type_scope.enter(name, pt);
        self.visit_expr(inside, tree)?;
        self.type_scope.exit(&name);

        let t_prime = self.types.meta(inside).clone();
        self.update_type(id, t_prime);

        ControlFlow::Continue(())
    }

    fn visit_case_expr(
        &mut self,
        _id: Id<node::CaseExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    // If
    // Γ ⊢ predicate : Bool
    // Γ ⊢ e0 : τ
    // Γ ⊢ e1 : τ
    // --------------------------
    // Γ ⊢ if predicate then e0 else e1 : τ
    fn visit_if_expr(&mut self, id: Id<node::IfExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_if_expr(id, tree)?;

        let span = self.span(id);

        let node::IfExpr {
            predicate,
            then,
            or,
        } = *id.get(tree);

        let t0 = self.types.meta(predicate).clone();

        self.cons.constrain(MonoType::BOOL, t0, span);

        let then = self.types.meta(then).clone();
        let or = self.types.meta(or).clone();

        self.cons.constrain(then.clone(), or, span);

        self.update_type(id, then);
        ControlFlow::Continue(())
    }

    // Abstraction rule
    // τ = newvar()
    // Γ, x : τ ⊢ e : τ'
    // --------------------
    // Γ ⊢ \x -> e : t -> t'
    fn visit_lambda_expr(
        &mut self,
        id: Id<node::LambdaExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::LambdaExpr { param, body } = *id.get(tree);
        let t = MonoType::variable();

        // let name = self.types.meta(func.param).clone();
        let name = param.get(tree).0.clone();

        self.type_scope.enter(name, PolyType::from(t.clone()));
        self.visit_expr(body, tree)?;
        self.type_scope.exit(&name);

        let t_prime = self.types.meta(body).clone();
        let func = MonoType::func(t, t_prime);

        self.update_type(id, func);
        ControlFlow::Continue(())
    }

    // Application rule
    // Γ ⊢ f : τ0
    // Γ ⊢ x : τ1
    // τ' = newvar()
    // unify(τ0, τ1 -> τ')
    // --------------------
    // Γ ⊢ f x : τ'
    fn visit_call_expr(
        &mut self,
        id: Id<node::CallExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_call_expr(id, tree)?;

        let span = self.span(id);

        let node::CallExpr { func, arg } = *id.get(tree);

        let t0 = self.types.meta(func).clone();
        let t1 = self.types.meta(arg).clone();
        let t_prime = MonoType::variable();

        self.cons
            .constrain(t0, MonoType::func(t1, t_prime.clone()), span);

        self.update_type(id, t_prime);
        ControlFlow::Continue(())
    }

    fn visit_expr(&mut self, id: Id<node::Expr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_expr(id, tree)?;

        let t = match *id.get(tree) {
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

        self.update_type(id, t);
        ControlFlow::Continue(())
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use camino::Utf8PathBuf;
    use kola_resolver::{GlobalId, bind::Bindings};
    use kola_span::{Loc, Located, Report, SourceId, Span};
    use kola_syntax::loc::Locations;
    use kola_tree::prelude::*;
    use kola_utils::interner::{PathInterner, StrInterner};

    use super::{TypedNodes, Typer};
    use crate::{
        env::TypeEnv,
        error::{TypeError, TypeErrors},
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

    fn solve_expr<T>(
        mut builder: TreeBuilder,
        node: Id<T>,
        interner: &mut StrInterner,
    ) -> Result<TypedNodes, Located<TypeErrors>>
    where
        node::Expr: From<Id<T>>,
    {
        let source_id = mocked_source();

        let bind = node::Bind::value_in(
            node::Vis::None,
            interner.intern("_").into(),
            None,
            node::Expr::from(node),
            &mut builder,
        );
        let root_id = builder.insert(node::Module(vec![bind]));

        let tree = builder.finish(root_id);
        let spans = Rc::new(mocked_spans(source_id, &tree));

        let global_id = GlobalId::new(source_id, root_id);
        let mut report = Report::new();

        Typer::new(global_id, spans, &TypeEnv::new(), &Bindings::new()).solve(&tree, &mut report)
    }

    #[test]
    fn literal() {
        let mut builder = TreeBuilder::new();
        let lit = builder.insert(node::LiteralExpr::Num(10.0));

        let types = solve_expr(builder, lit, &mut StrInterner::new()).unwrap();

        assert_eq!(types.meta(lit), &MonoType::NUM);
    }

    #[test]
    fn unary() {
        let mut builder = TreeBuilder::new();

        let target = builder.insert(node::LiteralExpr::Num(10.0));
        let unary = node::UnaryExpr::new_in(node::UnaryOp::Neg, target.into(), &mut builder);

        let types = solve_expr(builder, unary, &mut StrInterner::new()).unwrap();

        assert_eq!(types.meta(unary), &MonoType::NUM);
    }

    #[test]
    fn unary_err() {
        let mut builder = TreeBuilder::new();

        let target = builder.insert(node::LiteralExpr::Num(10.0));
        let unary = node::UnaryExpr::new_in(node::UnaryOp::Not, target.into(), &mut builder);

        let (errors, _) = solve_expr(builder, unary, &mut StrInterner::new()).unwrap_err();

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

        let (errors, _) = solve_expr(builder, binary, &mut StrInterner::new()).unwrap_err();

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

        let types = solve_expr(builder, let_, &mut interner).unwrap();

        assert_eq!(types.meta(let_), &MonoType::NUM);
    }

    #[test]
    fn if_() {
        let mut builder = TreeBuilder::new();

        let predicate = builder.insert(node::LiteralExpr::Bool(true));
        let then = builder.insert(node::LiteralExpr::Num(5.0));
        let or = builder.insert(node::LiteralExpr::Num(10.0));
        let if_ = node::IfExpr::new_in(predicate.into(), then.into(), or.into(), &mut builder);

        let types = solve_expr(builder, if_, &mut StrInterner::new()).unwrap();

        assert_eq!(types.meta(if_), &MonoType::NUM);
    }

    #[test]
    fn if_err() {
        let mut builder = TreeBuilder::new();

        let predicate = builder.insert(node::LiteralExpr::Bool(true));
        let then = builder.insert(node::LiteralExpr::Num(5.0));
        let or = builder.insert(node::LiteralExpr::Char('x'));
        let if_ = node::IfExpr::new_in(predicate.into(), then.into(), or.into(), &mut builder);

        let (errors, _) = solve_expr(builder, if_, &mut StrInterner::new()).unwrap_err();

        assert_eq!(
            errors[0],
            TypeError::CannotUnify {
                expected: MonoType::NUM,
                actual: MonoType::CHAR,
            }
        );
    }
}
