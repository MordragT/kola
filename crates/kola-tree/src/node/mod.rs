pub use binary::*;
pub use bind::*;
pub use call::*;
pub use cond::*;
pub use expr::*;
pub use func::*;
pub use ident::*;
pub use list::*;
pub use literal::*;
pub use name::*;
pub use pat::*;
pub use record::*;
pub use unary::*;

mod binary;
mod bind;
mod call;
mod cond;
mod expr;
mod func;
mod ident;
mod list;
mod literal;
mod name;
mod pat;
mod record;
mod unary;

use crate::kind::NodeKind;

pub trait InnerNode: Into<Node> {
    fn to_inner_ref(node: &Node) -> Option<&Self>; // try_to_ref ?
    fn to_inner_mut(node: &mut Node) -> Option<&mut Self>; // try_to_mut ?
}

pub type Symbol = ecow::EcoString;

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Name(Name),
    Ident(Ident),
    Literal(Literal),
    List(List),
    Property(Property),
    Record(Record),
    RecordSelect(RecordSelect),
    RecordExtend(RecordExtend),
    RecordRestrict(RecordRestrict),
    RecordUpdate(RecordUpdate),
    UnaryOp(UnaryOp),
    Unary(Unary),
    BinaryOp(BinaryOp),
    Binary(Binary),
    Let(Let),
    PatError(PatError),
    Wildcard(Wildcard),
    LiteralPat(LiteralPat),
    IdentPat(IdentPat),
    PropertyPat(PropertyPat),
    RecordPat(RecordPat),
    Pat(Pat),
    Branch(Branch),
    Case(Case),
    If(If),
    Func(Func),
    Call(Call),
    ExprError(ExprError),
    Expr(Expr),
}

impl Node {
    pub fn kind(&self) -> NodeKind {
        match self {
            Self::Name(_) => NodeKind::Name,
            Self::Ident(_) => NodeKind::Ident,
            Self::Literal(_) => NodeKind::Literal,
            Self::List(_) => NodeKind::List,
            Self::Property(_) => NodeKind::Property,
            Self::Record(_) => NodeKind::Record,
            Self::RecordSelect(_) => NodeKind::RecordSelect,
            Self::RecordExtend(_) => NodeKind::RecordExtend,
            Self::RecordRestrict(_) => NodeKind::RecordRestrict,
            Self::RecordUpdate(_) => NodeKind::RecordUpdate,
            Self::UnaryOp(_) => NodeKind::UnaryOp,
            Self::Unary(_) => NodeKind::Unary,
            Self::BinaryOp(_) => NodeKind::BinaryOp,
            Self::Binary(_) => NodeKind::Binary,
            Self::Let(_) => NodeKind::Let,
            Self::PatError(_) => NodeKind::PatError,
            Self::Wildcard(_) => NodeKind::Wildcard,
            Self::LiteralPat(_) => NodeKind::LiteralPat,
            Self::IdentPat(_) => NodeKind::IdentPat,
            Self::PropertyPat(_) => NodeKind::PropertyPat,
            Self::RecordPat(_) => NodeKind::RecordPat,
            Self::Pat(_) => NodeKind::Pat,
            Self::Branch(_) => NodeKind::Branch,
            Self::Case(_) => NodeKind::Case,
            Self::If(_) => NodeKind::If,
            Self::Func(_) => NodeKind::Func,
            Self::Call(_) => NodeKind::Call,
            Self::ExprError(_) => NodeKind::ExprError,
            Self::Expr(_) => NodeKind::Expr,
        }
    }
}

impl From<Name> for Node {
    fn from(value: Name) -> Self {
        Self::Name(value)
    }
}

impl From<Ident> for Node {
    fn from(value: Ident) -> Self {
        Self::Ident(value)
    }
}

impl From<Literal> for Node {
    fn from(value: Literal) -> Self {
        Self::Literal(value)
    }
}

impl From<List> for Node {
    fn from(value: List) -> Self {
        Self::List(value)
    }
}

impl From<Property> for Node {
    fn from(value: Property) -> Self {
        Self::Property(value)
    }
}

impl From<Record> for Node {
    fn from(value: Record) -> Self {
        Self::Record(value)
    }
}

impl From<RecordSelect> for Node {
    fn from(value: RecordSelect) -> Self {
        Self::RecordSelect(value)
    }
}

impl From<RecordExtend> for Node {
    fn from(value: RecordExtend) -> Self {
        Self::RecordExtend(value)
    }
}

impl From<RecordRestrict> for Node {
    fn from(value: RecordRestrict) -> Self {
        Self::RecordRestrict(value)
    }
}

impl From<RecordUpdate> for Node {
    fn from(value: RecordUpdate) -> Self {
        Self::RecordUpdate(value)
    }
}

impl From<UnaryOp> for Node {
    fn from(value: UnaryOp) -> Self {
        Self::UnaryOp(value)
    }
}

impl From<Unary> for Node {
    fn from(value: Unary) -> Self {
        Self::Unary(value)
    }
}

impl From<BinaryOp> for Node {
    fn from(value: BinaryOp) -> Self {
        Self::BinaryOp(value)
    }
}

impl From<Binary> for Node {
    fn from(value: Binary) -> Self {
        Self::Binary(value)
    }
}

impl From<Let> for Node {
    fn from(value: Let) -> Self {
        Self::Let(value)
    }
}

impl From<PatError> for Node {
    fn from(value: PatError) -> Self {
        Self::PatError(value)
    }
}

impl From<Wildcard> for Node {
    fn from(value: Wildcard) -> Self {
        Self::Wildcard(value)
    }
}

impl From<LiteralPat> for Node {
    fn from(value: LiteralPat) -> Self {
        Self::LiteralPat(value)
    }
}

impl From<IdentPat> for Node {
    fn from(value: IdentPat) -> Self {
        Self::IdentPat(value)
    }
}

impl From<PropertyPat> for Node {
    fn from(value: PropertyPat) -> Self {
        Self::PropertyPat(value)
    }
}

impl From<RecordPat> for Node {
    fn from(value: RecordPat) -> Self {
        Self::RecordPat(value)
    }
}

impl From<Pat> for Node {
    fn from(value: Pat) -> Self {
        Self::Pat(value)
    }
}

impl From<Branch> for Node {
    fn from(value: Branch) -> Self {
        Self::Branch(value)
    }
}

impl From<Case> for Node {
    fn from(value: Case) -> Self {
        Self::Case(value)
    }
}

impl From<If> for Node {
    fn from(value: If) -> Self {
        Self::If(value)
    }
}

impl From<Func> for Node {
    fn from(value: Func) -> Self {
        Self::Func(value)
    }
}

impl From<Call> for Node {
    fn from(value: Call) -> Self {
        Self::Call(value)
    }
}

impl From<ExprError> for Node {
    fn from(value: ExprError) -> Self {
        Self::ExprError(value)
    }
}

impl From<Expr> for Node {
    fn from(value: Expr) -> Self {
        Self::Expr(value)
    }
}
