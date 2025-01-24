use crate::{
    errors::Errors,
    syntax::{
        tree::{self, Metadata, NodeId, NodePool, SyntaxTree},
        Span, Spanned,
    },
};

use super::{
    error::SemanticError,
    types::{Kind, MonoType, PolyType, Property, TypeVar, Typed},
    KindEnv, Meta, SemanticTree, Substitutable, Substitution, TypeEnv, Unifiable,
};

// https://blog.stimsina.com/post/implementing-a-hindley-milner-type-system-part-2

// Type and Kind Environments:
// Type environments map term variables to their types and kind environments map type variables to
// their kinds

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

// TODO use Kind Environment in Inference rules

pub struct Inferer<'a> {
    subs: Substitution,
    cons: Constraints,
    t_env: TypeEnv,
    k_env: KindEnv,
    nodes: &'a NodePool,
    meta: Vec<Meta>,
}

impl<'a> Inferer<'a> {
    pub fn new(tree: &'a SyntaxTree) -> Self {
        let SyntaxTree { nodes, meta } = tree;

        let meta = meta.iter().cloned().map(|s| (s, None)).collect();

        Self {
            subs: Substitution::empty(),
            cons: Constraints::new(),
            t_env: TypeEnv::new(),
            k_env: KindEnv::new(),
            nodes,
            meta,
        }
    }

    fn set_type(&mut self, id: usize, t: MonoType) {
        self.meta[id].1 = Some(t);
    }

    fn span(&self, id: usize) -> Span {
        self.meta[id].0
    }

    // Unit rule
    // -----------------------
    // Γ ⊢ () : unit
    fn infer_literal(&mut self, literal: &tree::Literal, id: usize) -> Result<MonoType, Error> {
        let actual = match literal {
            &tree::Literal::Bool(_) => MonoType::BOOL,
            &tree::Literal::Char(_) => MonoType::CHAR,
            &tree::Literal::Num(_) => MonoType::NUM,
            &tree::Literal::Str(_) => MonoType::STR,
        };

        self.set_type(id, actual.clone());
        Ok(actual)
    }

    // Var rule to infer variable 'x'
    // x : σ ∈ Γ   τ = inst(σ)
    // -----------------------
    // ∆;Γ ⊢ x : τ
    fn infer_ident(&mut self, ident: &tree::Ident, id: usize) -> Result<MonoType, Error> {
        let t = self
            .t_env
            .try_lookup(&ident.0)
            .map_err(|e| e.with(self.span(id)))?
            .instantiate();

        self.set_type(id, t.clone());
        Ok(t)
    }

    fn infer_list(&mut self, list: &tree::List, _id: usize) -> Result<MonoType, Error> {
        todo!()
    }

    fn infer_property(&mut self, property: &tree::Property) -> Result<Property, Error> {
        let k = property.key.get(&self.nodes).0.clone();

        let value = property.value.get(&self.nodes);
        let v = self.infer_expr(value, property.value.as_usize())?;

        Ok(Property { k, v })
    }

    // Rule for Record Instantiation via Induction
    // ∆;Γ ⊢ R : { l0 : t0, ..., ln : tn | {} }
    // -----------------------
    // ∆;Γ ⊢ R : { { l1 : t1, ..., ln : tn } | +l0 : τ0 | {} }
    fn infer_record(&mut self, record: &tree::Record, id: usize) -> Result<MonoType, Error> {
        let mut r = MonoType::empty_row();

        for field in &record.fields {
            let head = self.infer_property(field.get(&self.nodes))?;
            r = MonoType::row(head, r);
        }

        self.set_type(id, r.clone());
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
        select: &tree::RecordSelect,
        id: usize,
    ) -> Result<MonoType, Error> {
        let t0 = self.infer_expr(select.source.get(&self.nodes), select.source.as_usize())?;

        self.cons
            .constrain_kind(Kind::Record, t0.clone(), self.span(id));

        let t_prime = MonoType::variable();

        let head = Property {
            k: select.field.get(&self.nodes).0.clone(),
            v: t_prime.clone(),
        };
        self.cons
            .constrain(MonoType::row(head, MonoType::variable()), t0, self.span(id));

        self.set_type(id, t_prime.clone());
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
        extend: &tree::RecordExtend,
        id: usize,
    ) -> Result<MonoType, Error> {
        let span = self.span(id);

        let t0 = self.infer_expr(extend.value.get(&self.nodes), extend.value.as_usize())?;
        let t1 = self.infer_expr(extend.source.get(&self.nodes), extend.source.as_usize())?;

        self.cons.constrain_kind(Kind::Record, t1.clone(), span);

        let head = Property {
            k: extend.field.get(&self.nodes).0.clone(),
            v: t0,
        };
        let row = MonoType::row(head, t1);

        self.set_type(id, row.clone());
        Ok(row)
    }

    fn partial_restrict(
        &mut self,
        source: NodeId<tree::Expr>,
        field: NodeId<tree::Name>,
        span: Span,
    ) -> Result<MonoType, Error> {
        let t = self.infer_expr(source.get(&self.nodes), source.as_usize())?;

        self.cons.constrain_kind(Kind::Record, t.clone(), span);

        let t_prime = MonoType::variable();

        let head = Property {
            k: field.get(&self.nodes).0.clone(),
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
        restrict: &tree::RecordRestrict,
        id: usize,
    ) -> Result<MonoType, Error> {
        let span = self.span(id);

        let t_prime = self.partial_restrict(restrict.source, restrict.field, span)?;

        self.set_type(id, t_prime.clone());
        Ok(t_prime)
    }

    // Rule for Record Update of 'r' with label 'l' and value 'v'
    // ∆;Γ ⊢ r : { τ0 | l : τ1 }
    // ∆;Γ ⊢ v : τ2
    // -----------------------
    // ∆;Γ ⊢ { r | l = v } : { r | -l | +l : τ2 }
    fn infer_record_update(
        &mut self,
        update: &tree::RecordUpdate,
        id: usize,
    ) -> Result<MonoType, Error> {
        let span = self.span(id);

        let t0 = self.partial_restrict(update.source, update.field, span)?;
        let t2 = self.infer_expr(update.value.get(&self.nodes), update.value.as_usize())?;

        let head = Property {
            k: update.field.get(&self.nodes).0.clone(),
            v: t2,
        };
        let row = MonoType::row(head, t0);

        self.set_type(id, row.clone());
        Ok(row)
    }

    // Abstraction rule
    // τ = newvar()
    // Γ, x : τ ⊢ e : τ'
    // ___________________
    // Γ ⊢ \x -> e : τ -> τ'
    fn infer_unary_op(&mut self, op: &tree::UnaryOp, id: usize) -> Result<MonoType, Error> {
        let t = match op {
            &tree::UnaryOp::Neg => MonoType::NUM,
            &tree::UnaryOp::Not => MonoType::BOOL,
        };

        let func = MonoType::func(t.clone(), t);

        self.set_type(id, func.clone());
        Ok(func)
    }

    // Application rule
    // Γ ⊢ f : τ0
    // Γ ⊢ x : τ1
    // τ' = newvar()
    // unify(τ0, τ1 -> τ')
    // --------------------
    // Γ ⊢ f x : τ'
    fn infer_unary(&mut self, unary: &tree::Unary, id: usize) -> Result<MonoType, Error> {
        let span = self.span(id);

        let t0 = self.infer_unary_op(unary.op.get(&self.nodes), unary.op.as_usize())?;
        let t1 = self.infer_expr(unary.target.get(&self.nodes), unary.target.as_usize())?;

        let t_prime = MonoType::variable();

        self.cons
            .constrain(t0, MonoType::func(t1, t_prime.clone()), span);

        self.set_type(id, t_prime.clone());
        Ok(t_prime)
    }

    fn infer_binary_op(&mut self, op: &tree::BinaryOp, id: usize) -> Result<MonoType, Error> {
        let span = self.span(id);

        let (t, t_prime) = match op {
            tree::BinaryOp::Add => {
                let t = MonoType::variable();
                self.cons.constrain_kind(Kind::Addable, t.clone(), span);
                (t, MonoType::NUM)
            }
            tree::BinaryOp::Sub
            | tree::BinaryOp::Mul
            | tree::BinaryOp::Div
            | tree::BinaryOp::Rem => (MonoType::NUM, MonoType::NUM),
            // Comparison
            tree::BinaryOp::Less
            | tree::BinaryOp::Greater
            | tree::BinaryOp::LessEq
            | tree::BinaryOp::GreaterEq => {
                let t = MonoType::variable();
                self.cons.constrain_kind(Kind::Comparable, t.clone(), span);
                (t, MonoType::BOOL)
            }
            // Logical
            tree::BinaryOp::And | tree::BinaryOp::Or | tree::BinaryOp::Xor => {
                (MonoType::BOOL, MonoType::BOOL)
            }
            // Equality
            tree::BinaryOp::Eq | tree::BinaryOp::NotEq => {
                let t = MonoType::variable();
                self.cons.constrain_kind(Kind::Equatable, t.clone(), span);
                (t, MonoType::BOOL)
            }
            // Record
            tree::BinaryOp::Merge => {
                todo!();
            }
        };

        let func = MonoType::func(t.clone(), MonoType::func(t, t_prime));

        self.set_type(id, func.clone());
        Ok(func)
    }

    // Application rule
    // Γ ⊢ f : τ0
    // Γ ⊢ x : τ1
    // τ' = newvar()
    // unify(τ0, τ1 -> τ')
    // --------------------
    // Γ ⊢ f x : τ'
    fn infer_binary(&mut self, binary: &tree::Binary, id: usize) -> Result<MonoType, Error> {
        let span = self.span(id);

        let t0 = self.infer_binary_op(binary.op.get(&self.nodes), binary.op.as_usize())?;
        let lhs = self.infer_expr(binary.left.get(&self.nodes), binary.left.as_usize())?;
        let rhs = self.infer_expr(binary.right.get(&self.nodes), binary.right.as_usize())?;

        let t_prime = MonoType::variable();

        let func = MonoType::func(lhs, MonoType::func(rhs, t_prime.clone()));
        self.cons.constrain(t0, func, span);

        self.set_type(id, t_prime.clone());
        Ok(t_prime)
    }

    // Generalization
    // Γ'(τ) quantifies all monotype variables not bound in Γ

    // Let rule
    // Γ ⊢ e0 : τ
    // Γ, x : Γ'(τ) ⊢ e1 : τ'
    // --------------------
    // Γ ⊢ let x = e0 in e1 : τ'
    fn infer_let(&mut self, let_: &tree::Let, id: usize) -> Result<MonoType, Error> {
        let t = TypeVar::branch(|| {
            self.infer_expr(let_.value.get(&self.nodes), let_.value.as_usize())
        })?;

        self.t_env.enter();

        let pty = t.generalize(&self.t_env.bound_vars());
        self.t_env.insert(let_.name.get(&self.nodes).0.clone(), pty);

        let t_prime = self.infer_expr(let_.inside.get(&self.nodes), let_.inside.as_usize())?;

        self.t_env.exit();

        self.set_type(id, t_prime.clone());
        Ok(t_prime)
    }

    // If
    // Γ ⊢ predicate : Bool
    // Γ ⊢ e0 : τ
    // Γ ⊢ e1 : τ
    // --------------------------
    // Γ ⊢ if predicate then e0 else e1 : τ
    fn infer_if(&mut self, if_: &tree::If, id: usize) -> Result<MonoType, Error> {
        let span = self.span(id);

        let t0 = self.infer_expr(if_.predicate.get(&self.nodes), if_.predicate.as_usize())?;

        self.cons.constrain(MonoType::BOOL, t0, span);

        let then = self.infer_expr(if_.then.get(&self.nodes), if_.then.as_usize())?;
        let or = self.infer_expr(if_.or.get(&self.nodes), if_.or.as_usize())?;

        self.cons.constrain(then.clone(), or, span);

        self.set_type(id, then.clone());
        Ok(then)
    }

    fn infer_case(&mut self, case: &tree::Case, id: usize) -> Result<MonoType, Error> {
        todo!()
    }

    // Abstraction rule
    // τ = newvar()
    // Γ, x : τ ⊢ e : τ'
    // ___________________
    // Γ ⊢ \x -> e : t -> t'
    fn infer_func(&mut self, func: &tree::Func, id: usize) -> Result<MonoType, Error> {
        let t = MonoType::variable();

        self.t_env.enter();

        self.t_env.insert(
            func.param.get(&self.nodes).0.clone(),
            PolyType::from(t.clone()),
        );
        let t_prime = self.infer_expr(func.body.get(&self.nodes), func.body.as_usize())?;

        self.t_env.exit();

        let func = MonoType::func(t, t_prime);

        self.set_type(id, func.clone());
        Ok(func)
    }

    // Application rule
    // Γ ⊢ f : τ0
    // Γ ⊢ x : τ1
    // τ' = newvar()
    // unify(τ0, τ1 -> τ')
    // --------------------
    // Γ ⊢ f x : τ'
    fn infer_call(&mut self, call: &tree::Call, id: usize) -> Result<MonoType, Error> {
        let span = self.span(id);

        let t0 = self.infer_expr(call.func.get(&self.nodes), call.func.as_usize())?;
        let t1 = self.infer_expr(call.arg.get(&self.nodes), call.arg.as_usize())?;
        let t_prime = MonoType::variable();

        self.cons
            .constrain(t0, MonoType::func(t1, t_prime.clone()), span);

        self.set_type(id, t_prime.clone());
        Ok(t_prime)
    }

    fn infer_expr(&mut self, expr: &tree::Expr, id: usize) -> Result<MonoType, Error> {
        match expr {
            tree::Expr::Error(e) => Err((Errors::new(), self.span(id))),
            tree::Expr::Literal(l) => self.infer_literal(l, id),
            tree::Expr::Ident(i) => self.infer_ident(i, id),
            tree::Expr::List(l) => self.infer_list(l, id),
            tree::Expr::Record(r) => self.infer_record(r, id),
            tree::Expr::RecordSelect(r) => self.infer_record_select(r, id),
            tree::Expr::RecordExtend(r) => self.infer_record_extend(r, id),
            tree::Expr::RecordRestrict(r) => self.infer_record_restrict(r, id),
            tree::Expr::RecordUpdate(r) => self.infer_record_update(r, id),
            tree::Expr::Unary(u) => self.infer_unary(u, id),
            tree::Expr::Binary(b) => self.infer_binary(b, id),
            tree::Expr::Let(l) => self.infer_let(l, id),
            tree::Expr::If(i) => self.infer_if(i, id),
            tree::Expr::Case(c) => self.infer_case(c, id),
            tree::Expr::Func(f) => self.infer_func(f, id),
            tree::Expr::Call(c) => self.infer_call(c, id),
        }
    }

    pub fn solve(mut self) -> Result<SemanticTree, Error> {
        let root = self.nodes.root();

        self.infer_expr(root.get(&self.nodes), root.as_usize())?;

        let Self {
            mut subs,
            cons,
            t_env: _,
            mut k_env,
            nodes,
            mut meta,
        } = self;

        cons.solve(&mut subs, &mut k_env)?;
        meta.apply_mut(&mut subs);

        Ok(SemanticTree {
            nodes: nodes.clone(),
            meta,
        })
    }
}

// #[cfg(test)]
// mod tests {
//     use crate::{
//         semantic::{error::SemanticError, types::MonoType, Infer, Substitution},
//         syntax::{tree::*, Span},
//     };

//     fn node<T>(t: T) -> Node<T> {
//         Node::new(t, Span::new(0, 0))
//     }

//     #[test]
//     fn literal() {
//         let mut s = Substitution::empty();

//         let mut lit = node(Literal::Num(10.0));
//         lit.solve(&mut s).unwrap();
//         s.apply(&mut lit);

//         assert_eq!(lit.ty(), &MonoType::NUM);
//     }

//     #[test]
//     fn unary() {
//         let mut s = Substitution::empty();

//         let mut unary = node(Unary {
//             op: node(UnaryOpKind::Neg),
//             target: node(Literal::Num(10.0)).into(),
//         });

//         unary.solve(&mut s).unwrap();
//         s.apply(&mut unary);

//         assert_eq!(unary.ty(), &MonoType::NUM);

//         s.clear();

//         let unary = node(Unary {
//             op: node(UnaryOpKind::Not),
//             target: node(Literal::Num(10.0)).into(),
//         });

//         let (errors, _) = unary.solve(&mut s).unwrap_err();

//         assert_eq!(
//             errors[0],
//             SemanticError::CannotUnify {
//                 expected: MonoType::BOOL,
//                 actual: MonoType::NUM
//             }
//         );
//     }

//     #[test]
//     fn binary() {
//         let mut s = Substitution::empty();

//         let binary = node(Binary {
//             op: node(BinaryOpKind::Eq),
//             left: node(Literal::Bool(true)).into(),
//             right: node(Literal::Num(10.0)).into(),
//         });

//         let (errors, _) = binary.solve(&mut s).unwrap_err();

//         assert_eq!(
//             errors[0],
//             SemanticError::CannotUnify {
//                 expected: MonoType::BOOL,
//                 actual: MonoType::NUM
//             }
//         );
//     }

//     #[test]
//     fn let_() {
//         let mut s = Substitution::empty();

//         let mut let_ = node(Let {
//             name: Name {
//                 name: "x".into(),
//                 span: Span::new(0, 0),
//             },
//             value: node(Literal::Num(10.0)).into(),
//             inside: node(Symbol::from("x")).into(),
//         });

//         let_.solve(&mut s).unwrap();
//         s.apply(&mut let_);

//         assert_eq!(let_.ty(), &MonoType::NUM)
//     }

//     #[test]
//     fn if_() {
//         let mut s = Substitution::empty();

//         let mut if_ = node(If {
//             predicate: node(Literal::Bool(true)).into(),
//             then: node(Literal::Num(5.0)).into(),
//             or: node(Literal::Num(10.0)).into(),
//         });

//         if_.solve(&mut s).unwrap();
//         s.apply(&mut if_);

//         assert_eq!(if_.ty(), &MonoType::NUM);

//         s.clear();

//         let if_ = node(If {
//             predicate: node(Literal::Bool(true)).into(),
//             then: node(Literal::Num(5.0)).into(),
//             or: node(Literal::Char('x')).into(),
//         });

//         let (errors, _) = if_.solve(&mut s).unwrap_err();

//         assert_eq!(
//             errors[0],
//             SemanticError::CannotUnify {
//                 expected: MonoType::NUM,
//                 actual: MonoType::CHAR,
//             }
//         );
//     }
// }
