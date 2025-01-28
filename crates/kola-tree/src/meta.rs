use std::{fmt::Debug, marker::PhantomData, ops::Deref, rc::Rc};

use crate::{Phase, id::NodeId, node};

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
    Name(<node::Name as Attached<P>>::Meta),
    Ident(<node::Ident as Attached<P>>::Meta),
    Literal(<node::Literal as Attached<P>>::Meta),
    List(<node::List as Attached<P>>::Meta),
    Property(<node::Property as Attached<P>>::Meta),
    Record(<node::Record as Attached<P>>::Meta),
    RecordSelect(<node::RecordSelect as Attached<P>>::Meta),
    RecordExtend(<node::RecordExtend as Attached<P>>::Meta),
    RecordRestrict(<node::RecordRestrict as Attached<P>>::Meta),
    RecordUpdate(<node::RecordUpdate as Attached<P>>::Meta),
    UnaryOp(<node::UnaryOp as Attached<P>>::Meta),
    Unary(<node::Unary as Attached<P>>::Meta),
    BinaryOp(<node::BinaryOp as Attached<P>>::Meta),
    Binary(<node::Binary as Attached<P>>::Meta),
    Let(<node::Let as Attached<P>>::Meta),
    PatError(<node::PatError as Attached<P>>::Meta),
    Wildcard(<node::Wildcard as Attached<P>>::Meta),
    LiteralPat(<node::LiteralPat as Attached<P>>::Meta),
    IdentPat(<node::IdentPat as Attached<P>>::Meta),
    PropertyPat(<node::PropertyPat as Attached<P>>::Meta),
    RecordPat(<node::RecordPat as Attached<P>>::Meta),
    Pat(<node::Pat as Attached<P>>::Meta),
    Branch(<node::Branch as Attached<P>>::Meta),
    Case(<node::Case as Attached<P>>::Meta),
    If(<node::If as Attached<P>>::Meta),
    Func(<node::Func as Attached<P>>::Meta),
    Call(<node::Call as Attached<P>>::Meta),
    ExprError(<node::ExprError as Attached<P>>::Meta),
    Expr(<node::Expr as Attached<P>>::Meta),
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
