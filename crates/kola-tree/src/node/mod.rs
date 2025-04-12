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
pub use ty::*;
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
mod ty;
mod unary;

use derive_more::{From, TryInto};
use kola_utils::impl_try_as;

pub type Symbol = ecow::EcoString;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NodeKind {
    Name,
    Ident,
    Literal,
    List,
    Property,
    Record,
    RecordSelect,
    RecordExtend,
    RecordRestrict,
    RecordUpdate,
    UnaryOp,
    Unary,
    BinaryOp,
    Binary,
    Let,
    PatError,
    Wildcard,
    LiteralPat,
    IdentPat,
    PropertyPat,
    RecordPat,
    Pat,
    Branch,
    Case,
    If,
    Func,
    Call,
    ExprError,
    Expr,
    TypeError,
    TypeIdent,
    PropertyType,
    RecordType,
    FuncType,
    MonoType,
    PolyType,
    TypeAlias,
}

#[derive(Clone, Debug, PartialEq, From, TryInto)]
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
    TypeError(TypeError),
    TypeIdent(TypeIdent),
    PropertyType(PropertyType),
    RecordType(RecordType),
    FuncType(FuncType),
    MonoType(MonoType),
    PolyType(PolyType),
    TypeAlias(TypeAlias),
}

impl_try_as!(
    Node,
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
    TypeError(TypeError),
    TypeIdent(TypeIdent),
    PropertyType(PropertyType),
    RecordType(RecordType),
    FuncType(FuncType),
    MonoType(MonoType),
    PolyType(PolyType),
    TypeAlias(TypeAlias)
);

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
            Self::TypeError(_) => NodeKind::TypeError,
            Self::TypeIdent(_) => NodeKind::TypeIdent,
            Self::PropertyType(_) => NodeKind::PropertyType,
            Self::RecordType(_) => NodeKind::RecordType,
            Self::FuncType(_) => NodeKind::FuncType,
            Self::MonoType(_) => NodeKind::MonoType,
            Self::PolyType(_) => NodeKind::PolyType,
            Self::TypeAlias(_) => NodeKind::TypeAlias,
        }
    }
}
