use crate::{
    id::NodeId,
    node::{self, Node},
};

pub trait Handler: Sized {
    type Error;

    fn handle_node(&mut self, node: &node::Node, id: usize) -> Result<(), Self::Error> {
        use Node::*;

        match node {
            Name(name) => self.handle_name(name, NodeId::from_usize(id)),
            Ident(ident) => self.handle_ident(ident, NodeId::from_usize(id)),
            Literal(literal) => self.handle_literal(literal, NodeId::from_usize(id)),
            List(list) => self.handle_list(list, NodeId::from_usize(id)),
            Property(property) => self.handle_property(property, NodeId::from_usize(id)),
            Record(record) => self.handle_record(record, NodeId::from_usize(id)),
            RecordSelect(select) => self.handle_record_select(select, NodeId::from_usize(id)),
            RecordExtend(extend) => self.handle_record_extend(extend, NodeId::from_usize(id)),
            RecordRestrict(restrict) => {
                self.handle_record_restrict(restrict, NodeId::from_usize(id))
            }
            RecordUpdate(update) => self.handle_record_update(update, NodeId::from_usize(id)),
            UnaryOp(unary_op) => self.handle_unary_op(unary_op, NodeId::from_usize(id)),
            Unary(unary) => self.handle_unary(unary, NodeId::from_usize(id)),
            BinaryOp(binary_op) => self.handle_binary_op(binary_op, NodeId::from_usize(id)),
            Binary(binary) => self.handle_binary(binary, NodeId::from_usize(id)),
            Let(let_) => self.handle_let(let_, NodeId::from_usize(id)),
            PatError(error) => self.handle_pat_error(error, NodeId::from_usize(id)),
            Wildcard(wildcard) => self.handle_wildcard(wildcard, NodeId::from_usize(id)),
            LiteralPat(literal_pat) => self.handle_literal_pat(literal_pat, NodeId::from_usize(id)),
            IdentPat(ident_pat) => self.handle_ident_pat(ident_pat, NodeId::from_usize(id)),
            PropertyPat(property_pat) => {
                self.handle_property_pat(property_pat, NodeId::from_usize(id))
            }
            RecordPat(record_pat) => self.handle_record_pat(record_pat, NodeId::from_usize(id)),
            Pat(pat) => self.handle_pat(pat, NodeId::from_usize(id)),
            Branch(branch) => self.handle_branch(branch, NodeId::from_usize(id)),
            Case(case) => self.handle_case(case, NodeId::from_usize(id)),
            If(if_) => self.handle_if(if_, NodeId::from_usize(id)),
            Func(func) => self.handle_func(func, NodeId::from_usize(id)),
            Call(call) => self.handle_call(call, NodeId::from_usize(id)),
            ExprError(error) => self.handle_expr_error(error, NodeId::from_usize(id)),
            Expr(expr) => self.handle_expr(expr, NodeId::from_usize(id)),
        }
    }

    fn handle_name(
        &mut self,
        _name: &node::Name,
        _id: NodeId<node::Name>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_ident(
        &mut self,
        _ident: &node::Ident,
        _id: NodeId<node::Ident>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_literal(
        &mut self,
        _literal: &node::Literal,
        _id: NodeId<node::Literal>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_unary_op(
        &mut self,
        _unary_op: &node::UnaryOp,
        _id: NodeId<node::UnaryOp>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_binary_op(
        &mut self,
        _binary_op: &node::BinaryOp,
        _id: NodeId<node::BinaryOp>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_pat_error(
        &mut self,
        _error: &node::PatError,
        _id: NodeId<node::PatError>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_wildcard(
        &mut self,
        _wildcard: &node::Wildcard,
        _id: NodeId<node::Wildcard>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_literal_pat(
        &mut self,
        _literal_pat: &node::LiteralPat,
        _id: NodeId<node::LiteralPat>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_ident_pat(
        &mut self,
        _ident_pat: &node::IdentPat,
        _id: NodeId<node::IdentPat>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_expr_error(
        &mut self,
        _error: &node::ExprError,
        _id: NodeId<node::ExprError>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_list(
        &mut self,
        _list: &node::List,
        _id: NodeId<node::List>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_property(
        &mut self,
        _property: &node::Property,
        _id: NodeId<node::Property>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_record(
        &mut self,
        _record: &node::Record,
        _id: NodeId<node::Record>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_record_select(
        &mut self,
        _select: &node::RecordSelect,
        _id: NodeId<node::RecordSelect>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_record_extend(
        &mut self,
        _extend: &node::RecordExtend,
        _id: NodeId<node::RecordExtend>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_record_restrict(
        &mut self,
        _restrict: &node::RecordRestrict,
        _id: NodeId<node::RecordRestrict>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_record_update(
        &mut self,
        _update: &node::RecordUpdate,
        _id: NodeId<node::RecordUpdate>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_unary(
        &mut self,
        _unary: &node::Unary,
        _id: NodeId<node::Unary>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_binary(
        &mut self,
        _binary: &node::Binary,
        _id: NodeId<node::Binary>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_let(&mut self, _let_: &node::Let, _id: NodeId<node::Let>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_if(&mut self, _if_: &node::If, _id: NodeId<node::If>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_property_pat(
        &mut self,
        _property_pat: &node::PropertyPat,
        _id: NodeId<node::PropertyPat>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_record_pat(
        &mut self,
        _record_pat: &node::RecordPat,
        _id: NodeId<node::RecordPat>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_pat(&mut self, _pat: &node::Pat, _id: NodeId<node::Pat>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_branch(
        &mut self,
        _branch: &node::Branch,
        _id: NodeId<node::Branch>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_case(
        &mut self,
        _case: &node::Case,
        _id: NodeId<node::Case>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_func(
        &mut self,
        _func: &node::Func,
        _id: NodeId<node::Func>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_call(
        &mut self,
        _call: &node::Call,
        _id: NodeId<node::Call>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn handle_expr(
        &mut self,
        _expr: &node::Expr,
        _id: NodeId<node::Expr>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
}
