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
        let types = tree.metadata_with(|kind| match kind {
            NodeKind::Name => Meta::Name(()),
            NodeKind::Ident => Meta::Ident(MonoType::variable()),
            NodeKind::Literal => Meta::Literal(MonoType::variable()),
            NodeKind::List => Meta::List(MonoType::variable()),
            NodeKind::Property => Meta::Property(()),
            NodeKind::Record => Meta::Record(MonoType::variable()),
            NodeKind::RecordSelect => Meta::RecordSelect(MonoType::variable()),
            NodeKind::RecordExtend => Meta::RecordExtend(MonoType::variable()),
            NodeKind::RecordRestrict => Meta::RecordRestrict(MonoType::variable()),
            NodeKind::RecordUpdate => Meta::RecordUpdate(MonoType::variable()),
            NodeKind::UnaryOp => Meta::UnaryOp(MonoType::variable()),
            NodeKind::Unary => Meta::Unary(MonoType::variable()),
            NodeKind::BinaryOp => Meta::BinaryOp(MonoType::variable()),
            NodeKind::Binary => Meta::Binary(MonoType::variable()),
            NodeKind::Let => Meta::Let(MonoType::variable()),
            NodeKind::PatError => Meta::PatError(()),
            NodeKind::Wildcard => Meta::Wildcard(MonoType::variable()),
            NodeKind::LiteralPat => Meta::LiteralPat(MonoType::variable()),
            NodeKind::IdentPat => Meta::IdentPat(MonoType::variable()),
            NodeKind::PropertyPat => Meta::PropertyPat(MonoType::variable()),
            NodeKind::RecordPat => Meta::RecordPat(MonoType::variable()),
            NodeKind::Pat => Meta::Pat(MonoType::variable()),
            NodeKind::Branch => Meta::Branch(()),
            NodeKind::Case => Meta::Case(MonoType::variable()),
            NodeKind::If => Meta::If(MonoType::variable()),
            NodeKind::Func => Meta::Func(MonoType::variable()),
            NodeKind::Call => Meta::Call(MonoType::variable()),
            NodeKind::ExprError => Meta::ExprError(()),
            NodeKind::Expr => Meta::Expr(MonoType::variable()),
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

    fn update_type<T>(&mut self, id: NodeId<T>, t: &MonoType) -> MonoType
    where
        T: Attached<SemanticPhase, Meta = MonoType>,
    {
        self.types.update_meta(id, t.clone())
    }

    fn span<T>(&self, id: NodeId<T>) -> Span
    where
        T: Attached<SyntaxPhase, Meta = Span>,
    {
        *self.spans.meta(id)
    }

    // Unit rule
    // -----------------------
    // Γ ⊢ () : unit
    fn infer_literal(&mut self, id: NodeId<node::Literal>, tree: &Tree) -> Result<MonoType, Error> {
        let actual = match id.get(tree) {
            &node::Literal::Bool(_) => MonoType::BOOL,
            &node::Literal::Char(_) => MonoType::CHAR,
            &node::Literal::Num(_) => MonoType::NUM,
            &node::Literal::Str(_) => MonoType::STR,
        };

        self.update_type(id, &actual);
        Ok(actual)
    }

    // Var rule to infer variable 'x'
    // x : σ ∈ Γ   τ = inst(σ)
    // -----------------------
    // ∆;Γ ⊢ x : τ
    fn infer_ident(&mut self, id: NodeId<node::Ident>, tree: &Tree) -> Result<MonoType, Error> {
        let t = self
            .t_env
            .try_lookup(&id.get(tree).0)
            .map_err(|e| e.with(self.span(id)))?
            .instantiate();

        self.update_type(id, &t);
        Ok(t)
    }

    fn infer_list(&mut self, id: NodeId<node::List>, tree: &Tree) -> Result<MonoType, Error> {
        todo!()
    }

    fn infer_property(
        &mut self,
        id: NodeId<node::Property>,
        tree: &Tree,
    ) -> Result<Property, Error> {
        let property = id.get(tree);

        let k = property.key.get(tree).0.clone();
        let v = self.infer_expr(property.value, tree)?;

        Ok(Property { k, v })
    }

    // Rule for Record Instantiation via Induction
    // ∆;Γ ⊢ R : { l0 : t0, ..., ln : tn | {} }
    // -----------------------
    // ∆;Γ ⊢ R : { { l1 : t1, ..., ln : tn } | +l0 : τ0 | {} }
    fn infer_record(&mut self, id: NodeId<node::Record>, tree: &Tree) -> Result<MonoType, Error> {
        let mut r = MonoType::empty_row();

        for &field in &id.get(tree).fields {
            let head = self.infer_property(field, tree)?;
            r = MonoType::row(head, r);
        }

        self.update_type(id, &r);
        Ok(r)
    }

    // Rule for Record Selection of 'r' with label 'l'
    // τ' = newvar()
    // ∆;Γ ⊢ r : { τ0 | l : τ' } where τ0 is of kind Record
    // -----------------------
    // ∆;Γ ⊢ r.l : τ'

    // old: ∀rα. {l :: α | r} → α
    // TODO check if feasible to just iterate over record to get the selected type
    // and if not possible fallback to original behaviour
    fn infer_record_select(
        &mut self,
        id: NodeId<node::RecordSelect>,
        tree: &Tree,
    ) -> Result<MonoType, Error> {
        let select = id.get(tree);
        let span = self.span(id);

        let t0 = self.infer_expr(select.source, tree)?;

        self.cons.constrain_kind(Kind::Record, t0.clone(), span);

        let t_prime = MonoType::variable();

        let head = Property {
            k: select.field.get(tree).0.clone(),
            v: t_prime.clone(),
        };
        self.cons
            .constrain(MonoType::row(head, MonoType::variable()), t0, span);

        self.update_type(id, &t_prime);
        Ok(t_prime)
    }

    // Rule for Record Extension of 'r' with label 'l' and Value 'v'
    // ∆;Γ ⊢ v : τ0
    // ∆;Γ ⊢ r : τ1 where τ1 is of kind Record
    // -----------------------
    // ∆;Γ ⊢ { r | +l = v } : { τ1 | +l : τ0 }

    // old: ∀rα. α → {r} → {l :: α | r}
    fn infer_record_extend(
        &mut self,
        id: NodeId<node::RecordExtend>,
        tree: &Tree,
    ) -> Result<MonoType, Error> {
        let extend = id.get(tree);
        let span = self.span(id);

        let t0 = self.infer_expr(extend.value, tree)?;
        let t1 = self.infer_expr(extend.source, tree)?;

        self.cons.constrain_kind(Kind::Record, t1.clone(), span);

        let head = Property {
            k: extend.field.get(tree).0.clone(),
            v: t0,
        };
        let row = MonoType::row(head, t1);

        self.update_type(id, &row);
        Ok(row)
    }

    fn partial_restrict(
        &mut self,
        source: NodeId<node::Expr>,
        field: NodeId<node::Name>,
        span: Span,
        tree: &Tree,
    ) -> Result<MonoType, Error> {
        let t = self.infer_expr(source, tree)?;

        self.cons.constrain_kind(Kind::Record, t.clone(), span);

        let t_prime = MonoType::variable();

        let head = Property {
            k: field.get(tree).0.clone(),
            v: MonoType::variable(),
        };

        self.cons
            .constrain(MonoType::row(head, t_prime.clone()), t, span);

        Ok(t_prime)
    }

    // Rule for Record Restriction of 'r' with label 'l'
    // ∆;Γ ⊢ r : { τ0 | l : τ1 }
    // -----------------------
    // ∆;Γ ⊢ { r | -l } : τ0
    // ∀rα. {l :: α | r} → {r}
    fn infer_record_restrict(
        &mut self,
        id: NodeId<node::RecordRestrict>,
        tree: &Tree,
    ) -> Result<MonoType, Error> {
        let restrict = id.get(tree);
        let span = self.span(id);

        let t_prime = self.partial_restrict(restrict.source, restrict.field, span, tree)?;

        self.update_type(id, &t_prime);
        Ok(t_prime)
    }

    // Rule for Record Update of 'r' with label 'l' and value 'v'
    // ∆;Γ ⊢ r : { τ0 | l : τ1 }
    // ∆;Γ ⊢ v : τ2
    // -----------------------
    // ∆;Γ ⊢ { r | l = v } : { r | -l | +l : τ2 }
    fn infer_record_update(
        &mut self,
        id: NodeId<node::RecordUpdate>,
        tree: &Tree,
    ) -> Result<MonoType, Error> {
        let update = id.get(tree);
        let span = self.span(id);

        let t0 = self.partial_restrict(update.source, update.field, span, tree)?;
        let t2 = self.infer_expr(update.value, tree)?;

        let head = Property {
            k: update.field.get(tree).0.clone(),
            v: t2,
        };
        let row = MonoType::row(head, t0);

        self.update_type(id, &row);
        Ok(row)
    }

    // Abstraction rule
    // τ = newvar()
    // Γ, x : τ ⊢ e : τ'
    // ___________________
    // Γ ⊢ \x -> e : τ -> τ'
    fn infer_unary_op(
        &mut self,
        id: NodeId<node::UnaryOp>,
        tree: &Tree,
    ) -> Result<MonoType, Error> {
        let t = match id.get(tree) {
            &node::UnaryOp::Neg => MonoType::NUM,
            &node::UnaryOp::Not => MonoType::BOOL,
        };

        let func = MonoType::func(t.clone(), t);

        self.update_type(id, &func);
        Ok(func)
    }

    // Application rule
    // Γ ⊢ f : τ0
    // Γ ⊢ x : τ1
    // τ' = newvar()
    // unify(τ0, τ1 -> τ')
    // --------------------
    // Γ ⊢ f x : τ'
    fn infer_unary(&mut self, id: NodeId<node::Unary>, tree: &Tree) -> Result<MonoType, Error> {
        let unary = id.get(tree);
        let span = self.span(id);

        let t0 = self.infer_unary_op(unary.op, tree)?;
        let t1 = self.infer_expr(unary.target, tree)?;

        let t_prime = MonoType::variable();

        self.cons
            .constrain(t0, MonoType::func(t1, t_prime.clone()), span);

        self.update_type(id, &t_prime);
        Ok(t_prime)
    }

    fn infer_binary_op(
        &mut self,
        id: NodeId<node::BinaryOp>,
        tree: &Tree,
    ) -> Result<MonoType, Error> {
        let op = id.get(tree);
        let span = self.span(id);

        let (t, t_prime) = match op {
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

        self.update_type(id, &func);
        Ok(func)
    }

    // Application rule
    // Γ ⊢ f : τ0
    // Γ ⊢ x : τ1
    // τ' = newvar()
    // unify(τ0, τ1 -> τ')
    // --------------------
    // Γ ⊢ f x : τ'
    fn infer_binary(&mut self, id: NodeId<node::Binary>, tree: &Tree) -> Result<MonoType, Error> {
        let binary = id.get(tree);
        let span = self.span(id);

        let t0 = self.infer_binary_op(binary.op, tree)?;
        let lhs = self.infer_expr(binary.left, tree)?;
        let rhs = self.infer_expr(binary.right, tree)?;

        let t_prime = MonoType::variable();

        let func = MonoType::func(lhs, MonoType::func(rhs, t_prime.clone()));
        self.cons.constrain(t0, func, span);

        self.update_type(id, &t_prime);
        Ok(t_prime)
    }

    // Generalization
    // Γ'(τ) quantifies all monotype variables not bound in Γ

    // Let rule
    // Γ ⊢ e0 : τ
    // Γ, x : Γ'(τ) ⊢ e1 : τ'
    // --------------------
    // Γ ⊢ let x = e0 in e1 : τ'
    fn infer_let(&mut self, id: NodeId<node::Let>, tree: &Tree) -> Result<MonoType, Error> {
        let let_ = id.get(tree);

        let t = TypeVar::branch(|| self.infer_expr(let_.value, tree))?;

        self.t_env.enter();

        let pty = t.generalize(&self.t_env.bound_vars());
        self.t_env.insert(let_.name.get(tree).0.clone(), pty);

        let t_prime = self.infer_expr(let_.inside, tree)?;

        self.t_env.exit();

        self.update_type(id, &t_prime);
        Ok(t_prime)
    }

    // If
    // Γ ⊢ predicate : Bool
    // Γ ⊢ e0 : τ
    // Γ ⊢ e1 : τ
    // --------------------------
    // Γ ⊢ if predicate then e0 else e1 : τ
    fn infer_if(&mut self, id: NodeId<node::If>, tree: &Tree) -> Result<MonoType, Error> {
        let if_ = id.get(tree);
        let span = self.span(id);

        let t0 = self.infer_expr(if_.predicate, tree)?;

        self.cons.constrain(MonoType::BOOL, t0, span);

        let then = self.infer_expr(if_.then, tree)?;
        let or = self.infer_expr(if_.or, tree)?;

        self.cons.constrain(then.clone(), or, span);

        self.update_type(id, &then);
        Ok(then)
    }

    fn infer_case(&mut self, id: NodeId<node::Case>, tree: &Tree) -> Result<MonoType, Error> {
        todo!()
    }

    // Abstraction rule
    // τ = newvar()
    // Γ, x : τ ⊢ e : τ'
    // ___________________
    // Γ ⊢ \x -> e : t -> t'
    fn infer_func(&mut self, id: NodeId<node::Func>, tree: &Tree) -> Result<MonoType, Error> {
        let func = id.get(tree);
        let t = MonoType::variable();

        self.t_env.enter();

        self.t_env
            .insert(func.param.get(tree).0.clone(), PolyType::from(t.clone()));
        let t_prime = self.infer_expr(func.body, tree)?;

        self.t_env.exit();

        let func = MonoType::func(t, t_prime);

        self.update_type(id, &func);
        Ok(func)
    }

    // Application rule
    // Γ ⊢ f : τ0
    // Γ ⊢ x : τ1
    // τ' = newvar()
    // unify(τ0, τ1 -> τ')
    // --------------------
    // Γ ⊢ f x : τ'
    fn infer_call(&mut self, id: NodeId<node::Call>, tree: &Tree) -> Result<MonoType, Error> {
        let call = id.get(tree);
        let span = self.span(id);

        let t0 = self.infer_expr(call.func, tree)?;
        let t1 = self.infer_expr(call.arg, tree)?;
        let t_prime = MonoType::variable();

        self.cons
            .constrain(t0, MonoType::func(t1, t_prime.clone()), span);

        self.update_type(id, &t_prime);
        Ok(t_prime)
    }

    fn infer_expr(&mut self, id: NodeId<node::Expr>, tree: &Tree) -> Result<MonoType, Error> {
        let t = match *id.get(tree) {
            node::Expr::Error(_) => Err((Errors::new(), self.span(id))),
            node::Expr::Literal(l) => self.infer_literal(l, tree),
            node::Expr::Ident(i) => self.infer_ident(i, tree),
            node::Expr::List(l) => self.infer_list(l, tree),
            node::Expr::Record(r) => self.infer_record(r, tree),
            node::Expr::RecordSelect(r) => self.infer_record_select(r, tree),
            node::Expr::RecordExtend(r) => self.infer_record_extend(r, tree),
            node::Expr::RecordRestrict(r) => self.infer_record_restrict(r, tree),
            node::Expr::RecordUpdate(r) => self.infer_record_update(r, tree),
            node::Expr::Unary(u) => self.infer_unary(u, tree),
            node::Expr::Binary(b) => self.infer_binary(b, tree),
            node::Expr::Let(l) => self.infer_let(l, tree),
            node::Expr::If(i) => self.infer_if(i, tree),
            node::Expr::Case(c) => self.infer_case(c, tree),
            node::Expr::Func(f) => self.infer_func(f, tree),
            node::Expr::Call(c) => self.infer_call(c, tree),
        }?;

        self.update_type(id, &t);
        Ok(t)
    }

    pub fn solve(mut self, tree: &Tree) -> Result<TypeMetadata, Error> {
        let root = tree.root_id();
        self.infer_expr(root, tree)?;

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
}

#[cfg(test)]
mod tests {
    // use crate::{
    //     semantic::{Infer, Substitution, error::SemanticError, types::MonoType},
    //     syntax::{Span, node::*},
    // };

    // fn node<T>(t: T) -> Node<T> {
    //     Node::new(t, Span::new(0, 0))
    // }

    use kola_syntax::span::{Span, SpanMetadata};
    use kola_tree::prelude::*;

    use super::Inferer;
    use crate::{error::SemanticError, types::*};

    fn mocked_spans(tree: &Tree) -> SpanMetadata {
        let span = Span::new(0, 0);

        tree.metadata_with(|kind| match kind {
            NodeKind::Name => Meta::Name(span),
            NodeKind::Ident => Meta::Ident(span),
            NodeKind::Literal => Meta::Literal(span),
            NodeKind::List => Meta::List(span),
            NodeKind::Property => Meta::Property(span),
            NodeKind::Record => Meta::Record(span),
            NodeKind::RecordSelect => Meta::RecordSelect(span),
            NodeKind::RecordExtend => Meta::RecordExtend(span),
            NodeKind::RecordRestrict => Meta::RecordRestrict(span),
            NodeKind::RecordUpdate => Meta::RecordUpdate(span),
            NodeKind::UnaryOp => Meta::UnaryOp(span),
            NodeKind::Unary => Meta::Unary(span),
            NodeKind::BinaryOp => Meta::BinaryOp(span),
            NodeKind::Binary => Meta::Binary(span),
            NodeKind::Let => Meta::Let(span),
            NodeKind::PatError => Meta::PatError(span),
            NodeKind::Wildcard => Meta::Wildcard(span),
            NodeKind::LiteralPat => Meta::LiteralPat(span),
            NodeKind::IdentPat => Meta::IdentPat(span),
            NodeKind::PropertyPat => Meta::PropertyPat(span),
            NodeKind::RecordPat => Meta::RecordPat(span),
            NodeKind::Pat => Meta::Pat(span),
            NodeKind::Branch => Meta::Branch(span),
            NodeKind::Case => Meta::Case(span),
            NodeKind::If => Meta::If(span),
            NodeKind::Func => Meta::Func(span),
            NodeKind::Call => Meta::Call(span),
            NodeKind::ExprError => Meta::ExprError(span),
            NodeKind::Expr => Meta::Expr(span),
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
