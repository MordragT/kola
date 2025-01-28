use std::{fmt::Debug, marker::PhantomData, ops::Deref, rc::Rc};

use crate::NodeId;

use super::Phase;

#[derive(Debug, Clone)]
pub struct Metadata<P, C>
where
    P: Phase,
    C: MetaContainer<P>,
{
    container: Rc<C>,
    phase: PhantomData<P>,
}

impl<P, C> Deref for Metadata<P, C>
where
    P: Phase,
    C: MetaContainer<P>,
{
    type Target = Rc<C>;

    fn deref(&self) -> &Self::Target {
        &self.container
    }
}

pub type MetaVec<P> = Vec<Meta<P>>;

pub trait MetaContainer<P: Phase>: Sized {
    fn get<T>(&self, id: NodeId<T>) -> &Meta<P>;

    fn get_mut<T>(&mut self, id: NodeId<T>) -> &mut Meta<P>;

    fn meta<T>(&self, id: NodeId<T>) -> &T::Meta
    where
        T: Attached<P>,
    {
        let meta = self.get(id);
        T::to_attached_ref(meta).unwrap()
    }

    fn meta_mut<T>(&mut self, id: NodeId<T>) -> &mut T::Meta
    where
        T: Attached<P>,
    {
        let meta = self.get_mut(id);
        T::to_attached_mut(meta).unwrap()
    }

    fn update_meta<T>(&mut self, id: NodeId<T>, meta: T::Meta) -> T::Meta
    where
        T: Attached<P>,
    {
        std::mem::replace(self.meta_mut(id), meta)
    }

    fn into_metadata(self) -> Metadata<P, Self> {
        Metadata {
            container: Rc::new(self),
            phase: PhantomData,
        }
    }
}

impl<P: Phase> MetaContainer<P> for MetaVec<P> {
    fn get<T>(&self, id: NodeId<T>) -> &Meta<P> {
        &self[id.as_usize()]
    }

    fn get_mut<T>(&mut self, id: NodeId<T>) -> &mut Meta<P> {
        &mut self[id.as_usize()]
    }
}

pub trait Attached<P: Phase> {
    type Meta;

    fn into_meta(attached: Self::Meta) -> Meta<P>;
    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta>;
    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta>;
}

#[derive(Clone, Debug)]
pub enum Meta<P: Phase> {
    Name(<super::Name as Attached<P>>::Meta),
    Ident(<super::Ident as Attached<P>>::Meta),
    Literal(<super::Literal as Attached<P>>::Meta),
    List(<super::List as Attached<P>>::Meta),
    Property(<super::Property as Attached<P>>::Meta),
    Record(<super::Record as Attached<P>>::Meta),
    RecordSelect(<super::RecordSelect as Attached<P>>::Meta),
    RecordExtend(<super::RecordExtend as Attached<P>>::Meta),
    RecordRestrict(<super::RecordRestrict as Attached<P>>::Meta),
    RecordUpdate(<super::RecordUpdate as Attached<P>>::Meta),
    UnaryOp(<super::UnaryOp as Attached<P>>::Meta),
    Unary(<super::Unary as Attached<P>>::Meta),
    BinaryOp(<super::BinaryOp as Attached<P>>::Meta),
    Binary(<super::Binary as Attached<P>>::Meta),
    Let(<super::Let as Attached<P>>::Meta),
    PatError(<super::PatError as Attached<P>>::Meta),
    Wildcard(<super::Wildcard as Attached<P>>::Meta),
    LiteralPat(<super::LiteralPat as Attached<P>>::Meta),
    IdentPat(<super::IdentPat as Attached<P>>::Meta),
    PropertyPat(<super::PropertyPat as Attached<P>>::Meta),
    RecordPat(<super::RecordPat as Attached<P>>::Meta),
    Pat(<super::Pat as Attached<P>>::Meta),
    Branch(<super::Branch as Attached<P>>::Meta),
    Case(<super::Case as Attached<P>>::Meta),
    If(<super::If as Attached<P>>::Meta),
    Func(<super::Func as Attached<P>>::Meta),
    Call(<super::Call as Attached<P>>::Meta),
    ExprError(<super::ExprError as Attached<P>>::Meta),
    Expr(<super::Expr as Attached<P>>::Meta),
}

impl<P, M> Meta<P>
where
    M: Clone + Debug,
    P: Phase<
            Name = M,
            Ident = M,
            Literal = M,
            List = M,
            Property = M,
            Record = M,
            RecordSelect = M,
            RecordExtend = M,
            RecordRestrict = M,
            RecordUpdate = M,
            UnaryOp = M,
            Unary = M,
            BinaryOp = M,
            Binary = M,
            Let = M,
            PatError = M,
            Wildcard = M,
            LiteralPat = M,
            IdentPat = M,
            PropertyPat = M,
            RecordPat = M,
            Pat = M,
            Branch = M,
            Case = M,
            If = M,
            Func = M,
            Call = M,
            ExprError = M,
            Expr = M,
        >,
{
    pub fn inner_ref(&self) -> &M {
        match self {
            Self::Name(m) => m,
            Self::Ident(m) => m,
            Self::Literal(m) => m,
            Self::List(m) => m,
            Self::Property(m) => m,
            Self::Record(m) => m,
            Self::RecordSelect(m) => m,
            Self::RecordExtend(m) => m,
            Self::RecordRestrict(m) => m,
            Self::RecordUpdate(m) => m,
            Self::UnaryOp(m) => m,
            Self::Unary(m) => m,
            Self::BinaryOp(m) => m,
            Self::Binary(m) => m,
            Self::Let(m) => m,
            Self::PatError(m) => m,
            Self::Wildcard(m) => m,
            Self::LiteralPat(m) => m,
            Self::IdentPat(m) => m,
            Self::PropertyPat(m) => m,
            Self::RecordPat(m) => m,
            Self::Pat(m) => m,
            Self::Branch(m) => m,
            Self::Case(m) => m,
            Self::If(m) => m,
            Self::Func(m) => m,
            Self::Call(m) => m,
            Self::ExprError(m) => m,
            Self::Expr(m) => m,
        }
    }

    pub fn inner_mut(&mut self) -> &mut M {
        match self {
            Self::Name(m) => m,
            Self::Ident(m) => m,
            Self::Literal(m) => m,
            Self::List(m) => m,
            Self::Property(m) => m,
            Self::Record(m) => m,
            Self::RecordSelect(m) => m,
            Self::RecordExtend(m) => m,
            Self::RecordRestrict(m) => m,
            Self::RecordUpdate(m) => m,
            Self::UnaryOp(m) => m,
            Self::Unary(m) => m,
            Self::BinaryOp(m) => m,
            Self::Binary(m) => m,
            Self::Let(m) => m,
            Self::PatError(m) => m,
            Self::Wildcard(m) => m,
            Self::LiteralPat(m) => m,
            Self::IdentPat(m) => m,
            Self::PropertyPat(m) => m,
            Self::RecordPat(m) => m,
            Self::Pat(m) => m,
            Self::Branch(m) => m,
            Self::Case(m) => m,
            Self::If(m) => m,
            Self::Func(m) => m,
            Self::Call(m) => m,
            Self::ExprError(m) => m,
            Self::Expr(m) => m,
        }
    }
}

// pub trait FromAttached<P(m) => m, T>
// where
//     P: Phase,
//     T: Attached<P>,
// {
//     fn from_attached(value: T::Meta) -> Self;
// }

// pub trait TryAsAttachedRef<T, P>
// where
//     P: Phase,
//     T: Attached<P>,
// {
//     fn try_as_attached_ref(&self) -> Option<&T::Meta>;
// }

// pub trait TryAsAttachedMut<T, P>
// where
//     P: Phase,
//     T: Attached<P>,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut T::Meta>;
// }

// impl<P> FromAttached<P, super::Name> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::Name as Attached<P>>::Meta) -> Self {
//         Self::Name(value)
//     }
// }

// impl<P> FromAttached<P, super::Ident> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::Ident as Attached<P>>::Meta) -> Self {
//         Self::Ident(value)
//     }
// }

// impl<P> FromAttached<P, super::Literal> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::Literal as Attached<P>>::Meta) -> Self {
//         Self::Literal(value)
//     }
// }

// impl<P> FromAttached<P, super::List> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::List as Attached<P>>::Meta) -> Self {
//         Self::List(value)
//     }
// }

// impl<P> FromAttached<P, super::Property> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::Property as Attached<P>>::Meta) -> Self {
//         Self::Property(value)
//     }
// }

// impl<P> FromAttached<P, super::Record> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::Record as Attached<P>>::Meta) -> Self {
//         Self::Record(value)
//     }
// }

// impl<P> FromAttached<P, super::RecordSelect> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::RecordSelect as Attached<P>>::Meta) -> Self {
//         Self::RecordSelect(value)
//     }
// }

// impl<P> FromAttached<P, super::RecordExtend> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::RecordExtend as Attached<P>>::Meta) -> Self {
//         Self::RecordExtend(value)
//     }
// }

// impl<P> FromAttached<P, super::RecordRestrict> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::RecordRestrict as Attached<P>>::Meta) -> Self {
//         Self::RecordRestrict(value)
//     }
// }

// impl<P> FromAttached<P, super::RecordUpdate> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::RecordUpdate as Attached<P>>::Meta) -> Self {
//         Self::RecordUpdate(value)
//     }
// }

// impl<P> FromAttached<P, super::UnaryOp> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::UnaryOp as Attached<P>>::Meta) -> Self {
//         Self::UnaryOp(value)
//     }
// }

// impl<P> FromAttached<P, super::Unary> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::Unary as Attached<P>>::Meta) -> Self {
//         Self::Unary(value)
//     }
// }

// impl<P> FromAttached<P, super::BinaryOp> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::BinaryOp as Attached<P>>::Meta) -> Self {
//         Self::BinaryOp(value)
//     }
// }

// impl<P> FromAttached<P, super::Binary> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::Binary as Attached<P>>::Meta) -> Self {
//         Self::Binary(value)
//     }
// }

// impl<P> FromAttached<P, super::Let> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::Let as Attached<P>>::Meta) -> Self {
//         Self::Let(value)
//     }
// }

// impl<P> FromAttached<P, super::PatError> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::PatError as Attached<P>>::Meta) -> Self {
//         Self::PatError(value)
//     }
// }

// impl<P> FromAttached<P, super::Wildcard> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::Wildcard as Attached<P>>::Meta) -> Self {
//         Self::Wildcard(value)
//     }
// }

// impl<P> FromAttached<P, super::LiteralPat> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::LiteralPat as Attached<P>>::Meta) -> Self {
//         Self::LiteralPat(value)
//     }
// }

// impl<P> FromAttached<P, super::IdentPat> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::IdentPat as Attached<P>>::Meta) -> Self {
//         Self::IdentPat(value)
//     }
// }

// impl<P> FromAttached<P, super::PropertyPat> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::PropertyPat as Attached<P>>::Meta) -> Self {
//         Self::PropertyPat(value)
//     }
// }

// impl<P> FromAttached<P, super::RecordPat> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::RecordPat as Attached<P>>::Meta) -> Self {
//         Self::RecordPat(value)
//     }
// }

// impl<P> FromAttached<P, super::Pat> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::Pat as Attached<P>>::Meta) -> Self {
//         Self::Pat(value)
//     }
// }

// impl<P> FromAttached<P, super::Branch> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::Branch as Attached<P>>::Meta) -> Self {
//         Self::Branch(value)
//     }
// }

// impl<P> FromAttached<P, super::Case> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::Case as Attached<P>>::Meta) -> Self {
//         Self::Case(value)
//     }
// }

// impl<P> FromAttached<P, super::If> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::If as Attached<P>>::Meta) -> Self {
//         Self::If(value)
//     }
// }

// impl<P> FromAttached<P, super::Func> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::Func as Attached<P>>::Meta) -> Self {
//         Self::Func(value)
//     }
// }

// impl<P> FromAttached<P, super::Call> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::Call as Attached<P>>::Meta) -> Self {
//         Self::Call(value)
//     }
// }

// impl<P> FromAttached<P, super::ExprError> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::ExprError as Attached<P>>::Meta) -> Self {
//         Self::ExprError(value)
//     }
// }

// impl<P> FromAttached<P, super::Expr> for Meta<P>
// where
//     P: Phase,
// {
//     fn from_attached(value: <super::Expr as Attached<P>>::Meta) -> Self {
//         Self::Expr(value)
//     }
// }

// impl<P> TryAsAttachedRef<super::Name, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::Name as Attached<P>>::Meta> {
//         match self {
//             Self::Name(n) => Some(n),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::Ident, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::Ident as Attached<P>>::Meta> {
//         match self {
//             Self::Ident(i) => Some(i),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::Literal, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::Literal as Attached<P>>::Meta> {
//         match self {
//             Self::Literal(l) => Some(l),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::List, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::List as Attached<P>>::Meta> {
//         match self {
//             Self::List(l) => Some(l),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::Property, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::Property as Attached<P>>::Meta> {
//         match self {
//             Self::Property(p) => Some(p),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::Record, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::Record as Attached<P>>::Meta> {
//         match self {
//             Self::Record(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::RecordSelect, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::RecordSelect as Attached<P>>::Meta> {
//         match self {
//             Self::RecordSelect(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::RecordExtend, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::RecordExtend as Attached<P>>::Meta> {
//         match self {
//             Self::RecordExtend(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::RecordRestrict, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::RecordRestrict as Attached<P>>::Meta> {
//         match self {
//             Self::RecordRestrict(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::RecordUpdate, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::RecordUpdate as Attached<P>>::Meta> {
//         match self {
//             Self::RecordUpdate(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::UnaryOp, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::UnaryOp as Attached<P>>::Meta> {
//         match self {
//             Self::UnaryOp(u) => Some(u),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::Unary, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::Unary as Attached<P>>::Meta> {
//         match self {
//             Self::Unary(u) => Some(u),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::BinaryOp, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::BinaryOp as Attached<P>>::Meta> {
//         match self {
//             Self::BinaryOp(b) => Some(b),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::Binary, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::Binary as Attached<P>>::Meta> {
//         match self {
//             Self::Binary(b) => Some(b),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::Let, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::Let as Attached<P>>::Meta> {
//         match self {
//             Self::Let(l) => Some(l),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::PatError, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::PatError as Attached<P>>::Meta> {
//         match self {
//             Self::PatError(p) => Some(p),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::Wildcard, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::Wildcard as Attached<P>>::Meta> {
//         match self {
//             Self::Wildcard(w) => Some(w),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::LiteralPat, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::LiteralPat as Attached<P>>::Meta> {
//         match self {
//             Self::LiteralPat(l) => Some(l),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::IdentPat, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::IdentPat as Attached<P>>::Meta> {
//         match self {
//             Self::IdentPat(i) => Some(i),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::PropertyPat, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::PropertyPat as Attached<P>>::Meta> {
//         match self {
//             Self::PropertyPat(p) => Some(p),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::RecordPat, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::RecordPat as Attached<P>>::Meta> {
//         match self {
//             Self::RecordPat(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::Pat, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::Pat as Attached<P>>::Meta> {
//         match self {
//             Self::Pat(p) => Some(p),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::Branch, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::Branch as Attached<P>>::Meta> {
//         match self {
//             Self::Branch(b) => Some(b),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::Case, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::Case as Attached<P>>::Meta> {
//         match self {
//             Self::Case(c) => Some(c),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::If, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::If as Attached<P>>::Meta> {
//         match self {
//             Self::If(i) => Some(i),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::Func, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::Func as Attached<P>>::Meta> {
//         match self {
//             Self::Func(f) => Some(f),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::Call, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::Call as Attached<P>>::Meta> {
//         match self {
//             Self::Call(c) => Some(c),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::ExprError, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::ExprError as Attached<P>>::Meta> {
//         match self {
//             Self::ExprError(e) => Some(e),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedRef<super::Expr, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_ref(&self) -> Option<&<super::Expr as Attached<P>>::Meta> {
//         match self {
//             Self::Expr(e) => Some(e),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::Name, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::Name as Attached<P>>::Meta> {
//         match self {
//             Self::Name(n) => Some(n),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::Ident, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::Ident as Attached<P>>::Meta> {
//         match self {
//             Self::Ident(i) => Some(i),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::Literal, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::Literal as Attached<P>>::Meta> {
//         match self {
//             Self::Literal(l) => Some(l),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::List, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::List as Attached<P>>::Meta> {
//         match self {
//             Self::List(l) => Some(l),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::Property, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::Property as Attached<P>>::Meta> {
//         match self {
//             Self::Property(p) => Some(p),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::Record, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::Record as Attached<P>>::Meta> {
//         match self {
//             Self::Record(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::RecordSelect, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::RecordSelect as Attached<P>>::Meta> {
//         match self {
//             Self::RecordSelect(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::RecordExtend, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::RecordExtend as Attached<P>>::Meta> {
//         match self {
//             Self::RecordExtend(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::RecordRestrict, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::RecordRestrict as Attached<P>>::Meta> {
//         match self {
//             Self::RecordRestrict(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::RecordUpdate, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::RecordUpdate as Attached<P>>::Meta> {
//         match self {
//             Self::RecordUpdate(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::UnaryOp, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::UnaryOp as Attached<P>>::Meta> {
//         match self {
//             Self::UnaryOp(u) => Some(u),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::Unary, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::Unary as Attached<P>>::Meta> {
//         match self {
//             Self::Unary(u) => Some(u),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::BinaryOp, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::BinaryOp as Attached<P>>::Meta> {
//         match self {
//             Self::BinaryOp(b) => Some(b),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::Binary, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::Binary as Attached<P>>::Meta> {
//         match self {
//             Self::Binary(b) => Some(b),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::Let, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::Let as Attached<P>>::Meta> {
//         match self {
//             Self::Let(l) => Some(l),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::PatError, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::PatError as Attached<P>>::Meta> {
//         match self {
//             Self::PatError(p) => Some(p),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::Wildcard, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::Wildcard as Attached<P>>::Meta> {
//         match self {
//             Self::Wildcard(w) => Some(w),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::LiteralPat, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::LiteralPat as Attached<P>>::Meta> {
//         match self {
//             Self::LiteralPat(l) => Some(l),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::IdentPat, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::IdentPat as Attached<P>>::Meta> {
//         match self {
//             Self::IdentPat(i) => Some(i),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::PropertyPat, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::PropertyPat as Attached<P>>::Meta> {
//         match self {
//             Self::PropertyPat(p) => Some(p),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::RecordPat, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::RecordPat as Attached<P>>::Meta> {
//         match self {
//             Self::RecordPat(r) => Some(r),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::Pat, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::Pat as Attached<P>>::Meta> {
//         match self {
//             Self::Pat(p) => Some(p),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::Branch, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::Branch as Attached<P>>::Meta> {
//         match self {
//             Self::Branch(b) => Some(b),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::Case, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::Case as Attached<P>>::Meta> {
//         match self {
//             Self::Case(c) => Some(c),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::If, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::If as Attached<P>>::Meta> {
//         match self {
//             Self::If(i) => Some(i),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::Func, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::Func as Attached<P>>::Meta> {
//         match self {
//             Self::Func(f) => Some(f),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::Call, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::Call as Attached<P>>::Meta> {
//         match self {
//             Self::Call(c) => Some(c),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::ExprError, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::ExprError as Attached<P>>::Meta> {
//         match self {
//             Self::ExprError(e) => Some(e),
//             _ => None,
//         }
//     }
// }

// impl<P> TryAsAttachedMut<super::Expr, P> for Meta<P>
// where
//     P: Phase,
// {
//     fn try_as_attached_mut(&mut self) -> Option<&mut <super::Expr as Attached<P>>::Meta> {
//         match self {
//             Self::Expr(e) => Some(e),
//             _ => None,
//         }
//     }
// }
