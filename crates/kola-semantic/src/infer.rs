use kola_syntax::prelude::*;
use kola_tree::prelude::*;
use kola_utils::Errors;

use crate::{
    SemanticPhase,
    env::{KindEnv, TypeEnv},
    error::SemanticError,
    meta::TypeMetadata,
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
        span: Span,
    },
    Ty {
        expected: MonoType,
        actual: MonoType,
        span: Span,
    },
}

type Error = Spanned<Errors<SemanticError>>;

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

// TODO τ for normal types
// r for record types ?

pub struct Inferer {
    subs: Substitution,
    cons: Constraints,
    t_env: TypeEnv,
    k_env: KindEnv,
    spans: SpanMetadata,
    types: Vec<Meta<SemanticPhase>>,
}

impl Inferer {
    pub fn new(tree: &Tree, spans: SpanMetadata) -> Self {
        // TODO init names with actual cloned names

        let types = tree.metadata_with(|node| match node {
            Node::Name(n) => Meta::Name(n.0.clone()),
            Node::Ident(_) => Meta::Ident(MonoType::variable()),
            Node::Literal(_) => Meta::Literal(MonoType::variable()),
            Node::List(_) => Meta::List(MonoType::variable()),
            Node::Property(_) => Meta::Property(Property {
                k: Default::default(),
                v: MonoType::variable(),
            }),
            Node::Record(_) => Meta::Record(MonoType::variable()),
            Node::RecordSelect(_) => Meta::RecordSelect(MonoType::variable()),
            Node::RecordExtend(_) => Meta::RecordExtend(MonoType::variable()),
            Node::RecordRestrict(_) => Meta::RecordRestrict(MonoType::variable()),
            Node::RecordUpdate(_) => Meta::RecordUpdate(MonoType::variable()),
            Node::UnaryOp(_) => Meta::UnaryOp(MonoType::variable()),
            Node::Unary(_) => Meta::Unary(MonoType::variable()),
            Node::BinaryOp(_) => Meta::BinaryOp(MonoType::variable()),
            Node::Binary(_) => Meta::Binary(MonoType::variable()),
            Node::Let(_) => Meta::Let(MonoType::variable()),
            Node::PatError(_) => Meta::PatError(()),
            Node::Wildcard(_) => Meta::Wildcard(MonoType::variable()),
            Node::LiteralPat(_) => Meta::LiteralPat(MonoType::variable()),
            Node::IdentPat(_) => Meta::IdentPat(MonoType::variable()),
            Node::PropertyPat(_) => Meta::PropertyPat(MonoType::variable()),
            Node::RecordPat(_) => Meta::RecordPat(MonoType::variable()),
            Node::Pat(_) => Meta::Pat(MonoType::variable()),
            Node::Branch(_) => Meta::Branch(()),
            Node::Case(_) => Meta::Case(MonoType::variable()),
            Node::If(_) => Meta::If(MonoType::variable()),
            Node::Func(_) => Meta::Func(MonoType::variable()),
            Node::Call(_) => Meta::Call(MonoType::variable()),
            Node::ExprError(_) => Meta::ExprError(()),
            Node::Expr(_) => Meta::Expr(MonoType::variable()),
        });

        Self {
            subs: Substitution::empty(),
            cons: Constraints::new(),
            t_env: TypeEnv::new(),
            k_env: KindEnv::new(),
            spans,
            types,
        }
    }

    pub fn solve(mut self, tree: &Tree) -> Result<TypeMetadata, Error> {
        self.visit_level_order(tree)?;

        let Self {
            mut subs,
            cons,
            t_env: _,
            mut k_env,
            spans: _,
            mut types,
        } = self;

        cons.solve(&mut subs, &mut k_env)?;
        types.apply_mut(&mut subs);

        Ok(types.into_metadata())
    }

    fn update_type<T>(&mut self, id: NodeId<T>, t: T::Meta) -> T::Meta
    where
        T: Attached<SemanticPhase>,
    {
        self.types.update_meta(id, t)
    }

    fn span<T>(&self, id: NodeId<T>) -> Span
    where
        T: Attached<SyntaxPhase, Meta = Span>,
    {
        *self.spans.meta(id)
    }

    fn partial_restrict(
        &mut self,
        source: NodeId<node::Expr>,
        field: NodeId<node::Name>,
        span: Span,
    ) -> Result<MonoType, Error> {
        let t = self.types.meta(source);

        self.cons.constrain_kind(Kind::Record, t.clone(), span);

        let t_prime = MonoType::variable();

        let head = Property {
            k: self.types.meta(field).clone(),
            v: MonoType::variable(),
        };

        self.cons
            .constrain(MonoType::row(head, t_prime.clone()), t.clone(), span);

        Ok(t_prime)
    }
}

impl Visitor for Inferer {
    // Generalization
    // Γ'(τ) quantifies all monotype variables not bound in Γ

    // Let rule
    // Γ ⊢ e0 : τ
    // Γ, x : Γ'(τ) ⊢ e1 : τ'
    // --------------------
    // Γ ⊢ let x = e0 in e1 : τ'
    fn walk_let(
        &mut self,
        id: NodeId<node::Let>,
        tree: &Tree,
        _stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let &node::Let {
            name,
            value,
            inside,
        } = id.get(tree);

        // TODO unsure if name is already populated by init inside self.types
        // otherwise implement traverse function for name

        let name = self.types.meta(name).clone();

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

        Ok(())
    }

    // Abstraction rule
    // τ = newvar()
    // Γ, x : τ ⊢ e : τ'
    // ___________________
    // Γ ⊢ \x -> e : t -> t'
    fn walk_func(
        &mut self,
        id: NodeId<node::Func>,
        tree: &Tree,
        _stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let func = id.get(tree);
        let t = MonoType::variable();

        // let name = self.types.meta(func.param).clone();
        let name = func.param.get(tree).0.clone();

        self.t_env.enter();
        {
            self.t_env.insert(name, PolyType::from(t.clone()));
            self.visit_expr(func.body, tree)?;
        }
        self.t_env.exit();

        let t_prime = self.types.meta(func.body).clone();
        let func = MonoType::func(t, t_prime);

        self.update_type(id, func);
        Ok(())
    }
}

impl Handler for Inferer {
    type Error = Error;

    // Unit rule
    // -----------------------
    // Γ ⊢ () : unit
    fn handle_literal(
        &mut self,
        literal: &node::Literal,
        id: NodeId<node::Literal>,
    ) -> Result<(), Self::Error> {
        let actual = match literal {
            &node::Literal::Bool(_) => MonoType::BOOL,
            &node::Literal::Char(_) => MonoType::CHAR,
            &node::Literal::Num(_) => MonoType::NUM,
            &node::Literal::Str(_) => MonoType::STR,
        };

        self.update_type(id, actual);
        Ok(())
    }

    // Var rule to infer variable 'x'
    // x : σ ∈ Γ   τ = inst(σ)
    // -----------------------
    // ∆;Γ ⊢ x : τ
    fn handle_ident(
        &mut self,
        ident: &node::Ident,
        id: NodeId<node::Ident>,
    ) -> Result<(), Self::Error> {
        let t = self
            .t_env
            .try_lookup(&ident.0)
            .map_err(|e| e.with(self.span(id)))?
            .instantiate();

        self.update_type(id, t);
        Ok(())
    }

    fn handle_property(
        &mut self,
        property: &node::Property,
        id: NodeId<node::Property>,
    ) -> Result<(), Self::Error> {
        let k = self.types.meta(property.key).clone();
        let v = self.types.meta(property.value).clone();

        self.update_type(id, Property { k, v });
        Ok(())
    }

    // Rule for Record Instantiation via Induction
    // ∆;Γ ⊢ R : { l0 : t0, ..., ln : tn | {} }
    // -----------------------
    // ∆;Γ ⊢ R : { { l1 : t1, ..., ln : tn } | +l0 : τ0 | {} }
    fn handle_record(
        &mut self,
        record: &node::Record,
        id: NodeId<node::Record>,
    ) -> Result<(), Self::Error> {
        let mut r = MonoType::empty_row();

        for &field in &record.fields {
            let head = self.types.meta(field).clone();
            r = MonoType::row(head, r);
        }

        self.update_type(id, r);
        Ok(())
    }

    // Rule for Record Selection of 'r' with label 'l'
    // τ' = newvar()
    // ∆;Γ ⊢ r : { τ0 | l : τ' } where τ0 is of kind Record
    // -----------------------
    // ∆;Γ ⊢ r.l : τ'

    // old: ∀rα. {l :: α | r} → α
    // TODO check if feasible to just iterate over record to get the selected type
    // and if not possible fallback to original behaviour
    fn handle_record_select(
        &mut self,
        select: &node::RecordSelect,
        id: NodeId<node::RecordSelect>,
    ) -> Result<(), Self::Error> {
        let span = self.span(id);

        let t0 = self.types.meta(select.source);

        self.cons.constrain_kind(Kind::Record, t0.clone(), span);

        let t_prime = MonoType::variable();

        let head = Property {
            k: self.types.meta(select.field).clone(),
            v: t_prime.clone(),
        };
        self.cons
            .constrain(MonoType::row(head, MonoType::variable()), t0.clone(), span);

        self.update_type(id, t_prime);
        Ok(())
    }

    // Rule for Record Extension of 'r' with label 'l' and Value 'v'
    // ∆;Γ ⊢ v : τ0
    // ∆;Γ ⊢ r : τ1 where τ1 is of kind Record
    // -----------------------
    // ∆;Γ ⊢ { r | +l = v } : { τ1 | +l : τ0 }

    // old: ∀rα. α → {r} → {l :: α | r}
    fn handle_record_extend(
        &mut self,
        extend: &node::RecordExtend,
        id: NodeId<node::RecordExtend>,
    ) -> Result<(), Self::Error> {
        let span = self.span(id);

        let t0 = self.types.meta(extend.value);
        let t1 = self.types.meta(extend.source);

        self.cons.constrain_kind(Kind::Record, t1.clone(), span);

        let head = Property {
            k: self.types.meta(extend.field).clone(),
            v: t0.clone(),
        };
        let row = MonoType::row(head, t1.clone());

        self.update_type(id, row);
        Ok(())
    }

    // Rule for Record Restriction of 'r' with label 'l'
    // ∆;Γ ⊢ r : { τ0 | l : τ1 }
    // -----------------------
    // ∆;Γ ⊢ { r | -l } : τ0
    // ∀rα. {l :: α | r} → {r}
    fn handle_record_restrict(
        &mut self,
        restrict: &node::RecordRestrict,
        id: NodeId<node::RecordRestrict>,
    ) -> Result<(), Self::Error> {
        let span = self.span(id);

        let t_prime = self.partial_restrict(restrict.source, restrict.field, span)?;

        self.update_type(id, t_prime);
        Ok(())
    }

    // Rule for Record Update of 'r' with label 'l' and value 'v'
    // ∆;Γ ⊢ r : { τ0 | l : τ1 }
    // ∆;Γ ⊢ v : τ2
    // -----------------------
    // ∆;Γ ⊢ { r | l = v } : { r | -l | +l : τ2 }
    fn handle_record_update(
        &mut self,
        update: &node::RecordUpdate,
        id: NodeId<node::RecordUpdate>,
    ) -> Result<(), Self::Error> {
        let span = self.span(id);

        let t0 = self.partial_restrict(update.source, update.field, span)?;
        let t2 = self.types.meta(update.value).clone();

        let head = Property {
            k: self.types.meta(update.field).clone(),
            v: t2,
        };
        let row = MonoType::row(head, t0);

        self.update_type(id, row);
        Ok(())
    }

    // Abstraction rule
    // τ = newvar()
    // Γ, x : τ ⊢ e : τ'
    // ___________________
    // Γ ⊢ \x -> e : τ -> τ'
    fn handle_unary_op(
        &mut self,
        unary_op: &node::UnaryOp,
        id: NodeId<node::UnaryOp>,
    ) -> Result<(), Self::Error> {
        let t = match unary_op {
            &node::UnaryOp::Neg => MonoType::NUM,
            &node::UnaryOp::Not => MonoType::BOOL,
        };

        let func = MonoType::func(t.clone(), t);

        self.update_type(id, func);
        Ok(())
    }

    // Application rule
    // Γ ⊢ f : τ0
    // Γ ⊢ x : τ1
    // τ' = newvar()
    // unify(τ0, τ1 -> τ')
    // --------------------
    // Γ ⊢ f x : τ'
    fn handle_unary(
        &mut self,
        unary: &node::Unary,
        id: NodeId<node::Unary>,
    ) -> Result<(), Self::Error> {
        let span = self.span(id);

        let t0 = self.types.meta(unary.op).clone();
        let t1 = self.types.meta(unary.target).clone();

        let t_prime = MonoType::variable();

        self.cons
            .constrain(t0, MonoType::func(t1, t_prime.clone()), span);

        self.update_type(id, t_prime);
        Ok(())
    }

    fn handle_binary_op(
        &mut self,
        binary_op: &node::BinaryOp,
        id: NodeId<node::BinaryOp>,
    ) -> Result<(), Self::Error> {
        let span = self.span(id);

        let (t, t_prime) = match binary_op {
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
        Ok(())
    }

    // Application rule
    // Γ ⊢ f : τ0
    // Γ ⊢ x : τ1
    // τ' = newvar()
    // unify(τ0, τ1 -> τ')
    // --------------------
    // Γ ⊢ f x : τ'
    fn handle_binary(
        &mut self,
        binary: &node::Binary,
        id: NodeId<node::Binary>,
    ) -> Result<(), Self::Error> {
        let span = self.span(id);

        let t0 = self.types.meta(binary.op).clone();
        let lhs = self.types.meta(binary.left).clone();
        let rhs = self.types.meta(binary.right).clone();

        let t_prime = MonoType::variable();

        let func = MonoType::func(lhs, MonoType::func(rhs, t_prime.clone()));
        self.cons.constrain(t0, func, span);

        self.update_type(id, t_prime);
        Ok(())
    }

    fn handle_let(&mut self, _let_: &node::Let, _id: NodeId<node::Let>) -> Result<(), Self::Error> {
        // handled by lower level walk_let
        Ok(())
    }

    // If
    // Γ ⊢ predicate : Bool
    // Γ ⊢ e0 : τ
    // Γ ⊢ e1 : τ
    // --------------------------
    // Γ ⊢ if predicate then e0 else e1 : τ
    fn handle_if(&mut self, if_: &node::If, id: NodeId<node::If>) -> Result<(), Self::Error> {
        let span = self.span(id);

        let t0 = self.types.meta(if_.predicate).clone();

        self.cons.constrain(MonoType::BOOL, t0, span);

        let then = self.types.meta(if_.then).clone();
        let or = self.types.meta(if_.or).clone();

        self.cons.constrain(then.clone(), or, span);

        self.update_type(id, then);
        Ok(())
    }

    fn handle_case(
        &mut self,
        _case: &node::Case,
        _id: NodeId<node::Case>,
    ) -> Result<(), Self::Error> {
        todo!()
    }

    fn handle_func(
        &mut self,
        _func: &node::Func,
        _id: NodeId<node::Func>,
    ) -> Result<(), Self::Error> {
        // handled by lower level walk_func
        Ok(())
    }

    // Application rule
    // Γ ⊢ f : τ0
    // Γ ⊢ x : τ1
    // τ' = newvar()
    // unify(τ0, τ1 -> τ')
    // --------------------
    // Γ ⊢ f x : τ'
    fn handle_call(
        &mut self,
        call: &node::Call,
        id: NodeId<node::Call>,
    ) -> Result<(), Self::Error> {
        let span = self.span(id);

        let t0 = self.types.meta(call.func).clone();
        let t1 = self.types.meta(call.arg).clone();
        let t_prime = MonoType::variable();

        self.cons
            .constrain(t0, MonoType::func(t1, t_prime.clone()), span);

        self.update_type(id, t_prime);
        Ok(())
    }

    fn handle_expr(
        &mut self,
        expr: &node::Expr,
        id: NodeId<node::Expr>,
    ) -> Result<(), Self::Error> {
        let t = match *expr {
            node::Expr::Error(_) => return Err((Errors::new(), self.span(id))),
            node::Expr::Literal(id) => self.types.meta(id),
            node::Expr::Ident(id) => self.types.meta(id),
            node::Expr::List(id) => self.types.meta(id),
            node::Expr::Record(id) => self.types.meta(id),
            node::Expr::RecordSelect(id) => self.types.meta(id),
            node::Expr::RecordExtend(id) => self.types.meta(id),
            node::Expr::RecordRestrict(id) => self.types.meta(id),
            node::Expr::RecordUpdate(id) => self.types.meta(id),
            node::Expr::Unary(id) => self.types.meta(id),
            node::Expr::Binary(id) => self.types.meta(id),
            node::Expr::Let(id) => self.types.meta(id),
            node::Expr::If(id) => self.types.meta(id),
            node::Expr::Case(id) => self.types.meta(id),
            node::Expr::Func(id) => self.types.meta(id),
            node::Expr::Call(id) => self.types.meta(id),
        }
        .clone();

        self.update_type(id, t);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use kola_syntax::span::{Span, SpanMetadata};
    use kola_tree::prelude::*;

    use super::Inferer;
    use crate::{error::SemanticError, types::*};

    fn mocked_spans(tree: &Tree) -> SpanMetadata {
        let span = Span::new(0, 0);

        tree.metadata_with(|kind| match kind {
            Node::Name(_) => Meta::Name(span),
            Node::Ident(_) => Meta::Ident(span),
            Node::Literal(_) => Meta::Literal(span),
            Node::List(_) => Meta::List(span),
            Node::Property(_) => Meta::Property(span),
            Node::Record(_) => Meta::Record(span),
            Node::RecordSelect(_) => Meta::RecordSelect(span),
            Node::RecordExtend(_) => Meta::RecordExtend(span),
            Node::RecordRestrict(_) => Meta::RecordRestrict(span),
            Node::RecordUpdate(_) => Meta::RecordUpdate(span),
            Node::UnaryOp(_) => Meta::UnaryOp(span),
            Node::Unary(_) => Meta::Unary(span),
            Node::BinaryOp(_) => Meta::BinaryOp(span),
            Node::Binary(_) => Meta::Binary(span),
            Node::Let(_) => Meta::Let(span),
            Node::PatError(_) => Meta::PatError(span),
            Node::Wildcard(_) => Meta::Wildcard(span),
            Node::LiteralPat(_) => Meta::LiteralPat(span),
            Node::IdentPat(_) => Meta::IdentPat(span),
            Node::PropertyPat(_) => Meta::PropertyPat(span),
            Node::RecordPat(_) => Meta::RecordPat(span),
            Node::Pat(_) => Meta::Pat(span),
            Node::Branch(_) => Meta::Branch(span),
            Node::Case(_) => Meta::Case(span),
            Node::If(_) => Meta::If(span),
            Node::Func(_) => Meta::Func(span),
            Node::Call(_) => Meta::Call(span),
            Node::ExprError(_) => Meta::ExprError(span),
            Node::Expr(_) => Meta::Expr(span),
        })
        .into_metadata()
    }

    #[test]
    fn literal() {
        let mut builder = TreeBuilder::new();
        let lit = builder.insert(node::Literal::Num(10.0));
        let root = builder.insert(node::Expr::Literal(lit));
        let tree = builder.finish(root);

        let types = Inferer::new(&tree, mocked_spans(&tree))
            .solve(&tree)
            .unwrap();

        assert_eq!(types.meta(lit), &MonoType::NUM);
    }

    #[test]
    fn unary() {
        let mut builder = TreeBuilder::new();

        let target = builder.insert(node::Literal::Num(10.0));
        let unary = node::Unary::new_in(node::UnaryOp::Neg, target.into(), &mut builder);
        let root = builder.insert(node::Expr::Unary(unary));

        let tree = builder.finish(root);

        let types = Inferer::new(&tree, mocked_spans(&tree))
            .solve(&tree)
            .unwrap();

        assert_eq!(types.meta(unary), &MonoType::NUM);
    }

    #[test]
    fn unary_err() {
        let mut builder = TreeBuilder::new();

        let target = builder.insert(node::Literal::Num(10.0));
        let unary = node::Unary::new_in(node::UnaryOp::Not, target.into(), &mut builder);
        let root = builder.insert(node::Expr::Unary(unary));

        let tree = builder.finish(root);

        let (errors, _) = Inferer::new(&tree, mocked_spans(&tree))
            .solve(&tree)
            .unwrap_err();

        assert_eq!(errors[0], SemanticError::CannotUnify {
            expected: MonoType::BOOL,
            actual: MonoType::NUM
        });
    }

    #[test]
    fn binary_err() {
        let mut builder = TreeBuilder::new();

        let left = builder.insert(node::Literal::Bool(true));
        let right = builder.insert(node::Literal::Num(10.0));
        let binary =
            node::Binary::new_in(node::BinaryOp::Eq, left.into(), right.into(), &mut builder);
        let root = builder.insert(node::Expr::Binary(binary));

        let tree = builder.finish(root);

        let (errors, _) = Inferer::new(&tree, mocked_spans(&tree))
            .solve(&tree)
            .unwrap_err();

        assert_eq!(errors[0], SemanticError::CannotUnify {
            expected: MonoType::BOOL,
            actual: MonoType::NUM
        });
    }

    #[test]
    fn let_() {
        let mut builder = TreeBuilder::new();

        let value = builder.insert(node::Literal::Num(10.0));
        let inside = builder.insert(node::Ident::from("x"));
        let let_ = node::Let::new_in(
            node::Name::from("x"),
            value.into(),
            inside.into(),
            &mut builder,
        );
        let root = builder.insert(node::Expr::Let(let_));

        let tree = builder.finish(root);

        let types = Inferer::new(&tree, mocked_spans(&tree))
            .solve(&tree)
            .unwrap();

        assert_eq!(types.meta(let_), &MonoType::NUM);
    }

    #[test]
    fn if_() {
        let mut builder = TreeBuilder::new();

        let predicate = builder.insert(node::Literal::Bool(true));
        let then = builder.insert(node::Literal::Num(5.0));
        let or = builder.insert(node::Literal::Num(10.0));
        let if_ = node::If::new_in(predicate.into(), then.into(), or.into(), &mut builder);
        let root = builder.insert(node::Expr::If(if_));

        let tree = builder.finish(root);

        let types = Inferer::new(&tree, mocked_spans(&tree))
            .solve(&tree)
            .unwrap();

        assert_eq!(types.meta(if_), &MonoType::NUM);
    }

    #[test]
    fn if_err() {
        let mut builder = TreeBuilder::new();

        let predicate = builder.insert(node::Literal::Bool(true));
        let then = builder.insert(node::Literal::Num(5.0));
        let or = builder.insert(node::Literal::Char('x'));
        let if_ = node::If::new_in(predicate.into(), then.into(), or.into(), &mut builder);
        let root = builder.insert(node::Expr::If(if_));

        let tree = builder.finish(root);

        let (errors, _) = Inferer::new(&tree, mocked_spans(&tree))
            .solve(&tree)
            .unwrap_err();

        assert_eq!(errors[0], SemanticError::CannotUnify {
            expected: MonoType::NUM,
            actual: MonoType::CHAR,
        });
    }
}
