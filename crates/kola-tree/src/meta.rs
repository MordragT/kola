use std::{fmt::Debug, marker::PhantomData, ops::Deref, rc::Rc};

use crate::NodeId;

use super::Phase;

#[derive(Debug, Clone)]
pub struct Metadata<P, C = MetaVec<P>>
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
