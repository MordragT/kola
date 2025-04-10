use crate::{id::NodeId, node};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BranchId {
    List(NodeId<node::List>),
    Property(NodeId<node::Property>),
    Record(NodeId<node::Record>),
    RecordSelect(NodeId<node::RecordSelect>),
    RecordExtend(NodeId<node::RecordExtend>),
    RecordRestrict(NodeId<node::RecordRestrict>),
    RecordUpdate(NodeId<node::RecordUpdate>),
    Unary(NodeId<node::Unary>),
    Binary(NodeId<node::Binary>),
    Let(NodeId<node::Let>),
    PropertyPat(NodeId<node::PropertyPat>),
    RecordPat(NodeId<node::RecordPat>),
    Pat(NodeId<node::Pat>),
    Branch(NodeId<node::Branch>),
    Case(NodeId<node::Case>),
    If(NodeId<node::If>),
    Func(NodeId<node::Func>),
    Call(NodeId<node::Call>),
    Expr(NodeId<node::Expr>),
}

impl From<NodeId<node::List>> for BranchId {
    fn from(value: NodeId<node::List>) -> Self {
        Self::List(value)
    }
}

impl From<NodeId<node::Property>> for BranchId {
    fn from(value: NodeId<node::Property>) -> Self {
        Self::Property(value)
    }
}

impl From<NodeId<node::Record>> for BranchId {
    fn from(value: NodeId<node::Record>) -> Self {
        Self::Record(value)
    }
}

impl From<NodeId<node::RecordSelect>> for BranchId {
    fn from(value: NodeId<node::RecordSelect>) -> Self {
        Self::RecordSelect(value)
    }
}

impl From<NodeId<node::RecordExtend>> for BranchId {
    fn from(value: NodeId<node::RecordExtend>) -> Self {
        Self::RecordExtend(value)
    }
}

impl From<NodeId<node::RecordRestrict>> for BranchId {
    fn from(value: NodeId<node::RecordRestrict>) -> Self {
        Self::RecordRestrict(value)
    }
}

impl From<NodeId<node::RecordUpdate>> for BranchId {
    fn from(value: NodeId<node::RecordUpdate>) -> Self {
        Self::RecordUpdate(value)
    }
}

impl From<NodeId<node::Unary>> for BranchId {
    fn from(value: NodeId<node::Unary>) -> Self {
        Self::Unary(value)
    }
}

impl From<NodeId<node::Binary>> for BranchId {
    fn from(value: NodeId<node::Binary>) -> Self {
        Self::Binary(value)
    }
}

impl From<NodeId<node::Let>> for BranchId {
    fn from(value: NodeId<node::Let>) -> Self {
        Self::Let(value)
    }
}

impl From<NodeId<node::PropertyPat>> for BranchId {
    fn from(value: NodeId<node::PropertyPat>) -> Self {
        Self::PropertyPat(value)
    }
}

impl From<NodeId<node::RecordPat>> for BranchId {
    fn from(value: NodeId<node::RecordPat>) -> Self {
        Self::RecordPat(value)
    }
}

impl From<NodeId<node::Pat>> for BranchId {
    fn from(value: NodeId<node::Pat>) -> Self {
        Self::Pat(value)
    }
}

impl From<NodeId<node::Branch>> for BranchId {
    fn from(value: NodeId<node::Branch>) -> Self {
        Self::Branch(value)
    }
}

impl From<NodeId<node::Case>> for BranchId {
    fn from(value: NodeId<node::Case>) -> Self {
        Self::Case(value)
    }
}

impl From<NodeId<node::If>> for BranchId {
    fn from(value: NodeId<node::If>) -> Self {
        Self::If(value)
    }
}

impl From<NodeId<node::Func>> for BranchId {
    fn from(value: NodeId<node::Func>) -> Self {
        Self::Func(value)
    }
}

impl From<NodeId<node::Call>> for BranchId {
    fn from(value: NodeId<node::Call>) -> Self {
        Self::Call(value)
    }
}

impl From<NodeId<node::Expr>> for BranchId {
    fn from(value: NodeId<node::Expr>) -> Self {
        Self::Expr(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LeafId {
    Name(NodeId<node::Name>),
    Ident(NodeId<node::Ident>),
    Literal(NodeId<node::Literal>),
    UnaryOp(NodeId<node::UnaryOp>),
    BinaryOp(NodeId<node::BinaryOp>),
    PatError(NodeId<node::PatError>),
    Wildcard(NodeId<node::Wildcard>),
    LiteralPat(NodeId<node::LiteralPat>),
    IdentPat(NodeId<node::IdentPat>),
    ExprError(NodeId<node::ExprError>),
}

impl From<NodeId<node::Name>> for LeafId {
    fn from(value: NodeId<node::Name>) -> Self {
        Self::Name(value)
    }
}

impl From<NodeId<node::Ident>> for LeafId {
    fn from(value: NodeId<node::Ident>) -> Self {
        Self::Ident(value)
    }
}

impl From<NodeId<node::Literal>> for LeafId {
    fn from(value: NodeId<node::Literal>) -> Self {
        Self::Literal(value)
    }
}

impl From<NodeId<node::UnaryOp>> for LeafId {
    fn from(value: NodeId<node::UnaryOp>) -> Self {
        Self::UnaryOp(value)
    }
}

impl From<NodeId<node::BinaryOp>> for LeafId {
    fn from(value: NodeId<node::BinaryOp>) -> Self {
        Self::BinaryOp(value)
    }
}

impl From<NodeId<node::PatError>> for LeafId {
    fn from(value: NodeId<node::PatError>) -> Self {
        Self::PatError(value)
    }
}

impl From<NodeId<node::Wildcard>> for LeafId {
    fn from(value: NodeId<node::Wildcard>) -> Self {
        Self::Wildcard(value)
    }
}

impl From<NodeId<node::LiteralPat>> for LeafId {
    fn from(value: NodeId<node::LiteralPat>) -> Self {
        Self::LiteralPat(value)
    }
}

impl From<NodeId<node::IdentPat>> for LeafId {
    fn from(value: NodeId<node::IdentPat>) -> Self {
        Self::IdentPat(value)
    }
}

impl From<NodeId<node::ExprError>> for LeafId {
    fn from(value: NodeId<node::ExprError>) -> Self {
        Self::ExprError(value)
    }
}
