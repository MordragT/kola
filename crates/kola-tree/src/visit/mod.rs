pub use event::*;
pub use id::*;

use crate::{
    id::NodeId,
    node::{self, Handler},
    tree::Tree,
};

mod event;
mod id;

// TODO use Arena for EventStack creation or just reuse it (probably hard I need a subslice) ??

pub trait Visitor: Handler {
    fn visit_post_order(&mut self, tree: &Tree) -> Result<(), Self::Error> {
        for (id, node) in tree.iter_nodes().enumerate() {
            self.handle_node(node, id)?;
        }

        Ok(())
    }

    fn visit_pre_order(&mut self, tree: &Tree) -> Result<(), Self::Error> {
        for (id, node) in tree.iter_nodes().enumerate().rev() {
            self.handle_node(node, id)?;
        }

        Ok(())
    }

    fn visit_level_order(&mut self, tree: &Tree) -> Result<(), Self::Error> {
        let root_id = tree.root_id();
        self.visit_expr(root_id, tree)
    }

    fn visit_expr(&mut self, id: NodeId<node::Expr>, tree: &Tree) -> Result<(), Self::Error> {
        let mut stack = EventStack::new();
        stack.push_branch(id);

        while let Some(event) = stack.next() {
            self.handle(event, tree, &mut stack)?;
        }

        Ok(())
    }

    fn handle(
        &mut self,
        event: Event,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        match event {
            Event::Enter(id) => match id {
                BranchId::List(id) => self.walk_list(id, tree, stack),
                BranchId::Property(id) => self.walk_property(id, tree, stack),
                BranchId::Record(id) => self.walk_record(id, tree, stack),
                BranchId::RecordSelect(id) => self.walk_record_select(id, tree, stack),
                BranchId::RecordExtend(id) => self.walk_record_extend(id, tree, stack),
                BranchId::RecordRestrict(id) => self.walk_record_restrict(id, tree, stack),
                BranchId::RecordUpdate(id) => self.walk_record_update(id, tree, stack),
                BranchId::Unary(id) => self.walk_unary(id, tree, stack),
                BranchId::Binary(id) => self.walk_binary(id, tree, stack),
                BranchId::Let(id) => self.walk_let(id, tree, stack),
                BranchId::PropertyPat(id) => self.walk_property_pat(id, tree, stack),
                BranchId::RecordPat(id) => self.walk_record_pat(id, tree, stack),
                BranchId::Pat(id) => self.walk_pat(id, tree, stack),
                BranchId::Branch(id) => self.walk_branch(id, tree, stack),
                BranchId::Case(id) => self.walk_case(id, tree, stack),
                BranchId::If(id) => self.walk_if(id, tree, stack),
                BranchId::Func(id) => self.walk_func(id, tree, stack),
                BranchId::Call(id) => self.walk_call(id, tree, stack),
                BranchId::Expr(id) => self.walk_expr(id, tree, stack),
            },
            Event::Exit(id) => match id {
                BranchId::List(id) => self.handle_list(id.get(tree), id),
                BranchId::Property(id) => self.handle_property(id.get(tree), id),
                BranchId::Record(id) => self.handle_record(id.get(tree), id),
                BranchId::RecordSelect(id) => self.handle_record_select(id.get(tree), id),
                BranchId::RecordExtend(id) => self.handle_record_extend(id.get(tree), id),
                BranchId::RecordRestrict(id) => self.handle_record_restrict(id.get(tree), id),
                BranchId::RecordUpdate(id) => self.handle_record_update(id.get(tree), id),
                BranchId::Unary(id) => self.handle_unary(id.get(tree), id),
                BranchId::Binary(id) => self.handle_binary(id.get(tree), id),
                BranchId::Let(id) => self.handle_let(id.get(tree), id),
                BranchId::PropertyPat(id) => self.handle_property_pat(id.get(tree), id),
                BranchId::RecordPat(id) => self.handle_record_pat(id.get(tree), id),
                BranchId::Pat(id) => self.handle_pat(id.get(tree), id),
                BranchId::Branch(id) => self.handle_branch(id.get(tree), id),
                BranchId::Case(id) => self.handle_case(id.get(tree), id),
                BranchId::If(id) => self.handle_if(id.get(tree), id),
                BranchId::Func(id) => self.handle_func(id.get(tree), id),
                BranchId::Call(id) => self.handle_call(id.get(tree), id),
                BranchId::Expr(id) => self.handle_expr(id.get(tree), id),
            },
            Event::Visit(id) => match id {
                LeafId::Name(id) => self.handle_name(id.get(tree), id),
                LeafId::Ident(id) => self.handle_ident(id.get(tree), id),
                LeafId::Literal(id) => self.handle_literal(id.get(tree), id),
                LeafId::UnaryOp(id) => self.handle_unary_op(id.get(tree), id),
                LeafId::BinaryOp(id) => self.handle_binary_op(id.get(tree), id),
                LeafId::PatError(id) => self.handle_pat_error(id.get(tree), id),
                LeafId::Wildcard(id) => self.handle_wildcard(id.get(tree), id),
                LeafId::LiteralPat(id) => self.handle_literal_pat(id.get(tree), id),
                LeafId::IdentPat(id) => self.handle_ident_pat(id.get(tree), id),
                LeafId::ExprError(id) => todo!(),
                // LeafNode::ExprError(id) => self.visit_expr_error(id.get(tree), id),
            },
        }
    }

    fn walk_list(
        &mut self,
        id: NodeId<node::List>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let list = id.get(tree);

        for id in &list.0 {
            stack.push_branch(*id);
        }

        Ok(())
    }

    fn walk_property(
        &mut self,
        id: NodeId<node::RecordField>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::RecordField { field: key, value } = id.get(tree);

        stack.push_leaf(*key);
        stack.push_branch(*value);

        Ok(())
    }

    fn walk_record(
        &mut self,
        id: NodeId<node::RecordExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let record = id.get(tree);

        for id in &record.fields {
            stack.push_branch(*id);
        }

        Ok(())
    }

    fn walk_record_select(
        &mut self,
        id: NodeId<node::RecordSelect>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::RecordSelect { source, field } = id.get(tree);

        stack.push_branch(*source);
        stack.push_leaf(*field);
        Ok(())
    }

    fn walk_record_extend(
        &mut self,
        id: NodeId<node::RecordExtendExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::RecordExtendExpr {
            source,
            field,
            value,
        } = id.get(tree);

        stack.push_branch(*source);
        stack.push_leaf(*field);
        stack.push_branch(*value);

        Ok(())
    }

    fn walk_record_restrict(
        &mut self,
        id: NodeId<node::RecordRestrictExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::RecordRestrictExpr { source, field } = id.get(tree);

        stack.push_branch(*source);
        stack.push_leaf(*field);

        Ok(())
    }

    fn walk_record_update(
        &mut self,
        id: NodeId<node::RecordUpdateExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::RecordUpdateExpr {
            source,
            field,
            value,
        } = id.get(tree);

        stack.push_branch(*source);
        stack.push_leaf(*field);
        stack.push_branch(*value);

        Ok(())
    }

    fn walk_unary(
        &mut self,
        id: NodeId<node::UnaryExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::UnaryExpr {
            op,
            operand: target,
        } = id.get(tree);

        stack.push_leaf(*op);
        stack.push_branch(*target);

        Ok(())
    }

    fn walk_binary(
        &mut self,
        id: NodeId<node::BinaryExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::BinaryExpr { op, left, right } = id.get(tree);

        stack.push_leaf(*op);
        stack.push_branch(*left);
        stack.push_branch(*right);

        Ok(())
    }

    fn walk_let(
        &mut self,
        id: NodeId<node::LetExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::LetExpr {
            name,
            value,
            inside,
        } = id.get(tree);

        stack.push_leaf(*name);
        stack.push_branch(*value);
        stack.push_branch(*inside);

        Ok(())
    }

    fn walk_if(
        &mut self,
        id: NodeId<node::IfExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::IfExpr {
            predicate,
            then,
            or,
        } = id.get(tree);

        stack.push_branch(*predicate);
        stack.push_branch(*then);
        stack.push_branch(*or);

        Ok(())
    }

    fn walk_property_pat(
        &mut self,
        id: NodeId<node::RecordFieldPat>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::RecordFieldPat {
            field: key,
            pat: value,
        } = id.get(tree);

        stack.push_leaf(*key);
        if let Some(value) = value {
            stack.push_branch(*value);
        }

        Ok(())
    }

    fn walk_record_pat(
        &mut self,
        id: NodeId<node::RecordPat>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let record_pat = id.get(tree);

        for id in &record_pat.fields {
            stack.push_branch(*id);
        }

        Ok(())
    }

    fn walk_pat(
        &mut self,
        id: NodeId<node::Pat>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        use node::Pat::*;

        let pat = id.get(tree);

        match pat {
            Error(e) => todo!(),
            Any(w) => todo!(),
            Literal(l) => todo!(),
            Ident(i) => todo!(),
            Record(r) => todo!(),
        }

        Ok(())
    }

    fn walk_branch(
        &mut self,
        id: NodeId<node::CaseBranch>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::CaseBranch { pat, matches } = id.get(tree);

        stack.push_branch(*pat);
        stack.push_branch(*matches);

        Ok(())
    }

    fn walk_case(
        &mut self,
        id: NodeId<node::CaseExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::CaseExpr { source, branches } = id.get(tree);

        stack.push_leaf(*source);
        for id in branches {
            stack.push_branch(*id);
        }

        Ok(())
    }

    fn walk_func(
        &mut self,
        id: NodeId<node::LambdaExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::LambdaExpr { param, body } = id.get(tree);

        stack.push_leaf(*param);
        stack.push_branch(*body);

        Ok(())
    }

    fn walk_call(
        &mut self,
        id: NodeId<node::CallExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::CallExpr { func, arg } = id.get(tree);

        stack.push_branch(*func);
        stack.push_branch(*arg);

        Ok(())
    }

    fn walk_expr(
        &mut self,
        id: NodeId<node::Expr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        use node::Expr::*;

        match id.get(tree) {
            Error(e) => todo!(),
            Literal(id) => stack.push_leaf(*id),
            Ident(id) => stack.push_leaf(*id),
            List(id) => stack.push_branch(*id),
            Record(id) => stack.push_branch(*id),
            RecordSelect(id) => stack.push_branch(*id),
            RecordExtend(id) => stack.push_branch(*id),
            RecordRestrict(id) => stack.push_branch(*id),
            RecordUpdate(id) => stack.push_branch(*id),
            Unary(id) => stack.push_branch(*id),
            Binary(id) => stack.push_branch(*id),
            Let(id) => stack.push_branch(*id),
            If(id) => stack.push_branch(*id),
            Case(id) => stack.push_branch(*id),
            Lambda(id) => stack.push_branch(*id),
            Call(id) => stack.push_branch(*id),
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::node::{self, NodeKind};
    use crate::tree::TreeBuilder;

    #[derive(Debug, Clone, Default)]
    struct TestVisitor {
        flow: Vec<NodeKind>,
    }

    impl Visitor for TestVisitor {}

    impl Handler for TestVisitor {
        type Error = ();

        fn handle_expr(
            &mut self,
            _expr: &node::Expr,
            _id: NodeId<node::Expr>,
        ) -> Result<(), Self::Error> {
            self.flow.push(NodeKind::Expr);
            Ok(())
        }

        fn handle_unary(
            &mut self,
            _unary: &node::UnaryExpr,
            _id: NodeId<node::UnaryExpr>,
        ) -> Result<(), Self::Error> {
            self.flow.push(NodeKind::Unary);
            Ok(())
        }

        fn handle_literal(
            &mut self,
            _literal: &node::LiteralExpr,
            _id: NodeId<node::LiteralExpr>,
        ) -> Result<(), Self::Error> {
            self.flow.push(NodeKind::Literal);
            Ok(())
        }
    }

    #[test]
    fn visitor() {
        let mut builder = TreeBuilder::new();

        let target = builder.insert(node::LiteralExpr::Num(10.0));
        let unary = node::UnaryExpr::new_in(node::UnaryOp::Neg, target.into(), &mut builder);
        let root = builder.insert(node::Expr::Unary(unary));

        // -10
        let tree = builder.finish(root);

        let mut visitor = TestVisitor::default();
        visitor.visit_post_order(&tree).unwrap();
        assert_eq!(visitor.flow, vec![
            NodeKind::Literal,
            NodeKind::Expr,
            NodeKind::Unary,
            NodeKind::Expr,
        ]);

        let mut visitor = TestVisitor::default();
        visitor.visit_pre_order(&tree).unwrap();
        assert_eq!(visitor.flow, vec![
            NodeKind::Expr,
            NodeKind::Unary,
            NodeKind::Expr,
            NodeKind::Literal,
        ]);
    }
}
