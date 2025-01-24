use super::ir;

use crate::syntax::tree;

// pub trait Normalize {
//     type Node;

//     fn normalize(&self) -> ir::Node<Self::Node>;
// }

// impl Normalize for ast::LiteralExpr {
//     type Node = ir::Literal;

//     fn normalize(&self) -> ir::Node<Self::Node> {
//         ir::Node::new(self.inner().clone(), self.span)
//     }
// }

// impl Normalize for ast::IdentExpr {
//     type Node = ir::Symbol;

//     fn normalize(&self) -> ir::Node<Self::Node> {
//         ir::Node::new(self.inner().clone(), self.span)
//     }
// }

// impl Normalize for ast::ListExpr {
//     type Node = ir::Complex;

//     fn normalize(&self) -> ir::Node<Self::Node> {
//         todo!()
//     }
// }

// impl Normalize for ast::RecordExpr {
//     type Node = ir::Complex;

//     fn normalize(&self) -> ir::Node<Self::Node> {
//         todo!()
//     }
// }

// impl Normalize for ast::RecordSelectExpr {
//     type Node = ir::Complex;

//     fn normalize(&self) -> ir::Node<Self::Node> {
//         todo!()
//     }
// }

// impl Normalize for ast::RecordExtendExpr {
//     type Node = ir::Complex;

//     fn normalize(&self) -> ir::Node<Self::Node> {
//         todo!()
//     }
// }

// impl Normalize for ast::RecordRestrictExpr {
//     type Node = ir::Complex;

//     fn normalize(&self) -> ir::Node<Self::Node> {
//         todo!()
//     }
// }

// impl Normalize for ast::RecordUpdateExpr {
//     type Node = ir::Complex;

//     fn normalize(&self) -> ir::Node<Self::Node> {
//         todo!()
//     }
// }

// impl Normalize for ast::UnaryExpr {
//     type Node = ir::Complex;

//     fn normalize(&self) -> ir::Node<Self::Node> {
//         todo!()
//     }
// }

// impl Normalize for ast::BinaryExpr {
//     type Node = ir::Complex;

//     fn normalize(&self) -> ir::Node<Self::Node> {
//         todo!()
//     }
// }

// impl Normalize for ast::LetExpr {
//     type Node = ir::Let;

//     fn normalize(&self) -> ir::Node<Self::Node> {
//         let value = self.value.normalize();
//         let inside = self.inside.normalize();
//     }
// }

// impl Normalize for ast::Expr {
//     type Node = ir::Complex;

//     fn normalize(&self) -> ir::Node<Self::Node> {
//         todo!()
//     }
// }

/*
*/

// Idea state machine over the input and output types of the cont to be created
pub type Cont<I, O> = Box<dyn FnOnce(I) -> O>;

struct Normalizer {
    atomics: Vec<ir::Atomic>,
}

// impl Normalizer {
//     fn normalize_expr(&mut self, expr: &tree::Expr) -> ir::Expr {
//         todo!()
//     }

//     fn normalize_if(&mut self, if_: &tree::IfExpr) -> ir::Complex {
//         todo!()
//     }

//     fn normalize_literal(&mut self, literal: &tree::LiteralExpr) {
//         self.atomics
//             .push(ir::Atomic::literal(literal.inner().clone(), literal.span))
//     }
// }

// If I use a flat list to represent all nodes in the ast
// then child nodes will always be in front of parent nodes
