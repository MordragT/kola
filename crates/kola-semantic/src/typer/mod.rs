use std::ops::ControlFlow;

use kola_syntax::prelude::*;
use kola_tree::prelude::*;
use kola_utils::Errors;

use crate::{
    env::{KindEnv, TypeEnv},
    error::SemanticError,
    file::FileInfo,
    module::{ModuleId, ModuleInfo, ModuleInfoTable},
    substitute::{Substitutable, Substitution},
    types::{Kind, MonoType, PolyType, Property, TypeVar, Typed},
    unify::Unifiable,
};

mod phase;
mod print;

pub use phase::{TypeInfo, TypeInfoTable, TypePhase};
pub use print::TypeDecorator;

// TODO rename to Typer ??

// https://blog.stimsina.com/post/implementing-a-hindley-milner-type-system-part-2

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constraint {
    Kind {
        expected: Kind,
        actual: MonoType,
        span: Span,
    },
    Ty {
        expected: MonoType,
        actual: MonoType,
        span: Span,
    },
}

pub type Error = Spanned<Errors<SemanticError>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constraints(Vec<Constraint>);

impl Constraints {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn constrain(&mut self, expected: MonoType, actual: MonoType, span: Span) {
        let c = Constraint::Ty {
            expected,
            actual,
            span,
        };
        self.0.push(c);
    }

    pub fn constrain_kind(&mut self, expected: Kind, actual: MonoType, span: Span) {
        let c = Constraint::Kind {
            expected,
            actual,
            span,
        };
        self.0.push(c);
    }

    // TODO error handling do not propagate but keep trace of errors
    pub fn solve(self, s: &mut Substitution, kind_env: &mut KindEnv) -> Result<(), Error> {
        // TODO infer Types first ?
        for c in self.0 {
            match c {
                Constraint::Kind {
                    expected,
                    actual,
                    span,
                } => actual
                    .apply_cow(s)
                    .constrain(expected, kind_env)
                    .map_err(|e| e.with(span))?,
                Constraint::Ty {
                    expected,
                    actual,
                    span,
                } => expected
                    .try_unify(&actual, s)
                    .map_err(|errs| (errs, span))?,
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

pub struct Typer<'a> {
    subs: Substitution,
    cons: Constraints,
    t_env: TypeEnv,
    k_env: KindEnv,
    types: Vec<Meta<TypePhase>>,
    id: ModuleId,
    module: ModuleInfo,
    spans: SpanInfo,
    module_infos: &'a ModuleInfoTable,
    type_infos: &'a TypeInfoTable,
}

impl<'a> Typer<'a> {
    pub fn new(
        id: ModuleId,
        module: ModuleInfo,
        spans: SpanInfo,
        module_infos: &'a ModuleInfoTable,
        type_infos: &'a TypeInfoTable,
    ) -> Self {
        let types = spans
            .iter()
            .map(|m| Meta::<TypePhase>::default_for(m.kind()))
            .collect();

        Self {
            subs: Substitution::empty(),
            cons: Constraints::new(),
            t_env: TypeEnv::new(),
            k_env: KindEnv::new(),
            types,
            id,
            module,
            spans,
            module_infos,
            type_infos,
        }
    }

    // pub fn solve()

    pub fn solve(mut self, tree: &Tree) -> Result<TypeInfo, Error> {
        match tree.root_id().visit_by(&mut self, tree) {
            ControlFlow::Break(_) => unreachable!(),
            ControlFlow::Continue(()) => (),
        }

        let Self {
            mut subs,
            cons,
            mut k_env,
            mut types,
            ..
        } = self;

        cons.solve(&mut subs, &mut k_env)?;
        types.apply_mut(&mut subs);

        Ok(types.into_metadata())
    }

    fn update_type<T>(&mut self, id: Id<T>, t: T::Meta) -> T::Meta
    where
        T: MetaCast<TypePhase>,
    {
        self.types.update_meta(id, t)
    }

    fn span<T>(&self, id: Id<T>) -> Span
    where
        T: MetaCast<SyntaxPhase, Meta = Span>,
    {
        *self.spans.meta(id)
    }

    fn partial_restrict(
        &mut self,
        source: Id<node::Expr>,
        field: Id<node::Name>,
        span: Span,
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

impl<'a, T> Visitor<T> for Typer<'a>
where
    T: TreeView,
{
    type BreakValue = !;

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

    // Var rule to infer variable 'x'
    // x : σ ∈ Γ   τ = inst(σ)
    // -----------------------
    // ∆;Γ ⊢ x : τ
    fn visit_path_expr(
        &mut self,
        id: Id<node::PathExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        // let path = todo!();

        // let t = self
        //     .t_env
        //     .try_lookup(&ident.0)
        //     .map_err(|e| e.with(self.span(id)))?
        //     .instantiate();

        // self.update_type(id, t);
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

        for &field in &id.get(tree).0 {
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

        self.t_env.enter();
        {
            let pt = t.generalize(&self.t_env.bound_vars());
            self.t_env.insert(name, pt);

            self.visit_expr(inside, tree)?;
        }
        self.t_env.exit();

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

        self.t_env.enter();
        {
            self.t_env.insert(name, PolyType::from(t.clone()));
            self.visit_expr(body, tree)?;
        }
        self.t_env.exit();

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

    use kola_syntax::span::{Span, SpanInfo};
    use kola_tree::prelude::*;

    use super::{Error, TypeInfo, TypeInfoTable, Typer};
    use crate::{
        error::SemanticError,
        module::{ModuleId, ModuleInfoBuilder, ModuleInfoTable},
        types::*,
    };

    fn mocked_spans(tree: &impl TreeView) -> SpanInfo {
        let span = Span {
            start: 0,
            end: 0,
            context: (),
        };
        tree.metadata_with(|node| Meta::default_with(span, node.kind()))
            .into_metadata()
    }
    fn solve_expr<T>(mut builder: TreeBuilder, node: Id<T>) -> Result<TypeInfo, Error>
    where
        node::Expr: From<Id<T>>,
    {
        let bind = node::Bind::value_in(
            node::Vis::None,
            "_".into(),
            None,
            node::Expr::from(node),
            &mut builder,
        );
        let root = builder.insert(node::Module(vec![bind]));
        let module_id = ModuleId::root("/mocked/path.kl", root.clone());
        let tree = builder.finish(root);

        Typer::new(
            module_id,
            ModuleInfoBuilder::new().finish(),
            mocked_spans(&tree),
            &ModuleInfoTable::new(),
            &TypeInfoTable::new(),
        )
        .solve(&tree)
    }

    #[test]
    fn literal() {
        let mut builder = TreeBuilder::new();
        let lit = builder.insert(node::LiteralExpr::Num(10.0));

        let types = solve_expr(builder, lit).unwrap();

        assert_eq!(types.meta(lit), &MonoType::NUM);
    }

    #[test]
    fn unary() {
        let mut builder = TreeBuilder::new();

        let target = builder.insert(node::LiteralExpr::Num(10.0));
        let unary = node::UnaryExpr::new_in(node::UnaryOp::Neg, target.into(), &mut builder);

        let types = solve_expr(builder, unary).unwrap();

        assert_eq!(types.meta(unary), &MonoType::NUM);
    }

    #[test]
    fn unary_err() {
        let mut builder = TreeBuilder::new();

        let target = builder.insert(node::LiteralExpr::Num(10.0));
        let unary = node::UnaryExpr::new_in(node::UnaryOp::Not, target.into(), &mut builder);

        let (errors, _) = solve_expr(builder, unary).unwrap_err();

        assert_eq!(
            errors[0],
            SemanticError::CannotUnify {
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

        let (errors, _) = solve_expr(builder, binary).unwrap_err();

        assert_eq!(
            errors[0],
            SemanticError::CannotUnify {
                expected: MonoType::BOOL,
                actual: MonoType::NUM
            }
        );
    }

    #[test]
    fn let_() {
        let mut builder = TreeBuilder::new();

        let value = builder.insert(node::LiteralExpr::Num(10.0));
        let segment = builder.insert(node::Name::from("x"));
        let inside = builder.insert(node::PathExpr(vec![segment]));
        let let_ = node::LetExpr::new_in(
            node::Name::from("x"),
            value.into(),
            inside.into(),
            &mut builder,
        );

        let types = solve_expr(builder, let_).unwrap();

        assert_eq!(types.meta(let_), &MonoType::NUM);
    }

    #[test]
    fn if_() {
        let mut builder = TreeBuilder::new();

        let predicate = builder.insert(node::LiteralExpr::Bool(true));
        let then = builder.insert(node::LiteralExpr::Num(5.0));
        let or = builder.insert(node::LiteralExpr::Num(10.0));
        let if_ = node::IfExpr::new_in(predicate.into(), then.into(), or.into(), &mut builder);

        let types = solve_expr(builder, if_).unwrap();

        assert_eq!(types.meta(if_), &MonoType::NUM);
    }

    #[test]
    fn if_err() {
        let mut builder = TreeBuilder::new();

        let predicate = builder.insert(node::LiteralExpr::Bool(true));
        let then = builder.insert(node::LiteralExpr::Num(5.0));
        let or = builder.insert(node::LiteralExpr::Char('x'));
        let if_ = node::IfExpr::new_in(predicate.into(), then.into(), or.into(), &mut builder);

        let (errors, _) = solve_expr(builder, if_).unwrap_err();

        assert_eq!(
            errors[0],
            SemanticError::CannotUnify {
                expected: MonoType::NUM,
                actual: MonoType::CHAR,
            }
        );
    }
}
