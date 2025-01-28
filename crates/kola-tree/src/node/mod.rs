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

use crate::NodeKind;

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

pub trait InnerNode: Into<Node> {
    fn to_inner_ref(node: &Node) -> Option<&Self>;
    fn to_inner_mut(node: &mut Node) -> Option<&mut Self>;
}

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

// impl TryAsRef<Name> for Node {
//     fn try_as_ref(&self) -> Option<&Name> {
//         match self {
//             Self::Name(n) => Some(n),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<Ident> for Node {
//     fn try_as_ref(&self) -> Option<&Ident> {
//         match self {
//             Self::Ident(i) => Some(i),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<Literal> for Node {
//     fn try_as_ref(&self) -> Option<&Literal> {
//         match self {
//             Self::Literal(l) => Some(l),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<List> for Node {
//     fn try_as_ref(&self) -> Option<&List> {
//         match self {
//             Self::List(l) => Some(l),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<Property> for Node {
//     fn try_as_ref(&self) -> Option<&Property> {
//         match self {
//             Self::Property(p) => Some(p),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<Record> for Node {
//     fn try_as_ref(&self) -> Option<&Record> {
//         match self {
//             Self::Record(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<RecordSelect> for Node {
//     fn try_as_ref(&self) -> Option<&RecordSelect> {
//         match self {
//             Self::RecordSelect(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<RecordExtend> for Node {
//     fn try_as_ref(&self) -> Option<&RecordExtend> {
//         match self {
//             Self::RecordExtend(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<RecordRestrict> for Node {
//     fn try_as_ref(&self) -> Option<&RecordRestrict> {
//         match self {
//             Self::RecordRestrict(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<RecordUpdate> for Node {
//     fn try_as_ref(&self) -> Option<&RecordUpdate> {
//         match self {
//             Self::RecordUpdate(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<UnaryOp> for Node {
//     fn try_as_ref(&self) -> Option<&UnaryOp> {
//         match self {
//             Self::UnaryOp(u) => Some(u),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<Unary> for Node {
//     fn try_as_ref(&self) -> Option<&Unary> {
//         match self {
//             Self::Unary(u) => Some(u),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<BinaryOp> for Node {
//     fn try_as_ref(&self) -> Option<&BinaryOp> {
//         match self {
//             Self::BinaryOp(b) => Some(b),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<Binary> for Node {
//     fn try_as_ref(&self) -> Option<&Binary> {
//         match self {
//             Self::Binary(b) => Some(b),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<Let> for Node {
//     fn try_as_ref(&self) -> Option<&Let> {
//         match self {
//             Self::Let(l) => Some(l),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<PatError> for Node {
//     fn try_as_ref(&self) -> Option<&PatError> {
//         match self {
//             Self::PatError(p) => Some(p),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<Wildcard> for Node {
//     fn try_as_ref(&self) -> Option<&Wildcard> {
//         match self {
//             Self::Wildcard(w) => Some(w),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<LiteralPat> for Node {
//     fn try_as_ref(&self) -> Option<&LiteralPat> {
//         match self {
//             Self::LiteralPat(l) => Some(l),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<IdentPat> for Node {
//     fn try_as_ref(&self) -> Option<&IdentPat> {
//         match self {
//             Self::IdentPat(i) => Some(i),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<PropertyPat> for Node {
//     fn try_as_ref(&self) -> Option<&PropertyPat> {
//         match self {
//             Self::PropertyPat(p) => Some(p),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<RecordPat> for Node {
//     fn try_as_ref(&self) -> Option<&RecordPat> {
//         match self {
//             Self::RecordPat(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<Pat> for Node {
//     fn try_as_ref(&self) -> Option<&Pat> {
//         match self {
//             Self::Pat(p) => Some(p),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<Branch> for Node {
//     fn try_as_ref(&self) -> Option<&Branch> {
//         match self {
//             Self::Branch(b) => Some(b),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<Case> for Node {
//     fn try_as_ref(&self) -> Option<&Case> {
//         match self {
//             Self::Case(c) => Some(c),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<If> for Node {
//     fn try_as_ref(&self) -> Option<&If> {
//         match self {
//             Self::If(i) => Some(i),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<Func> for Node {
//     fn try_as_ref(&self) -> Option<&Func> {
//         match self {
//             Self::Func(f) => Some(f),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<Call> for Node {
//     fn try_as_ref(&self) -> Option<&Call> {
//         match self {
//             Self::Call(c) => Some(c),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<ExprError> for Node {
//     fn try_as_ref(&self) -> Option<&ExprError> {
//         match self {
//             Self::ExprError(e) => Some(e),
//             _ => None,
//         }
//     }
// }

// impl TryAsRef<Expr> for Node {
//     fn try_as_ref(&self) -> Option<&Expr> {
//         match self {
//             Self::Expr(e) => Some(e),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<Name> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut Name> {
//         match self {
//             Self::Name(n) => Some(n),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<Ident> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut Ident> {
//         match self {
//             Self::Ident(i) => Some(i),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<Literal> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut Literal> {
//         match self {
//             Self::Literal(l) => Some(l),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<List> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut List> {
//         match self {
//             Self::List(l) => Some(l),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<Property> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut Property> {
//         match self {
//             Self::Property(p) => Some(p),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<Record> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut Record> {
//         match self {
//             Self::Record(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<RecordSelect> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut RecordSelect> {
//         match self {
//             Self::RecordSelect(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<RecordExtend> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut RecordExtend> {
//         match self {
//             Self::RecordExtend(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<RecordRestrict> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut RecordRestrict> {
//         match self {
//             Self::RecordRestrict(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<RecordUpdate> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut RecordUpdate> {
//         match self {
//             Self::RecordUpdate(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<UnaryOp> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut UnaryOp> {
//         match self {
//             Self::UnaryOp(u) => Some(u),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<Unary> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut Unary> {
//         match self {
//             Self::Unary(u) => Some(u),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<BinaryOp> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut BinaryOp> {
//         match self {
//             Self::BinaryOp(b) => Some(b),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<Binary> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut Binary> {
//         match self {
//             Self::Binary(b) => Some(b),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<Let> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut Let> {
//         match self {
//             Self::Let(l) => Some(l),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<PatError> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut PatError> {
//         match self {
//             Self::PatError(p) => Some(p),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<Wildcard> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut Wildcard> {
//         match self {
//             Self::Wildcard(w) => Some(w),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<LiteralPat> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut LiteralPat> {
//         match self {
//             Self::LiteralPat(l) => Some(l),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<IdentPat> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut IdentPat> {
//         match self {
//             Self::IdentPat(i) => Some(i),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<PropertyPat> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut PropertyPat> {
//         match self {
//             Self::PropertyPat(p) => Some(p),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<RecordPat> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut RecordPat> {
//         match self {
//             Self::RecordPat(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<Pat> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut Pat> {
//         match self {
//             Self::Pat(p) => Some(p),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<Branch> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut Branch> {
//         match self {
//             Self::Branch(b) => Some(b),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<Case> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut Case> {
//         match self {
//             Self::Case(c) => Some(c),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<If> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut If> {
//         match self {
//             Self::If(i) => Some(i),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<Func> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut Func> {
//         match self {
//             Self::Func(f) => Some(f),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<Call> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut Call> {
//         match self {
//             Self::Call(c) => Some(c),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<ExprError> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut ExprError> {
//         match self {
//             Self::ExprError(e) => Some(e),
//             _ => None,
//         }
//     }
// }

// impl TryAsMut<Expr> for Node {
//     fn try_as_mut(&mut self) -> Option<&mut Expr> {
//         match self {
//             Self::Expr(e) => Some(e),
//             _ => None,
//         }
//     }
// }
