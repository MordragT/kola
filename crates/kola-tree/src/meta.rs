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
        T: MetaCast<P>,
    {
        let meta = self.get(id);
        T::try_downcast_ref(meta).unwrap()
    }

    fn meta_mut<T>(&mut self, id: NodeId<T>) -> &mut T::Meta
    where
        T: MetaCast<P>,
    {
        let meta = self.get_mut(id);
        T::try_downcast_mut(meta).unwrap()
    }

    fn update_meta<T>(&mut self, id: NodeId<T>, meta: T::Meta) -> T::Meta
    where
        T: MetaCast<P>,
    {
        std::mem::replace(self.meta_mut(id), meta)
    }

    // TODO better implement From trait
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

pub trait MetaCast<P: Phase> {
    type Meta;

    fn upcast(attached: Self::Meta) -> Meta<P>;
    fn try_downcast_ref(meta: &Meta<P>) -> Option<&Self::Meta>;
    fn try_downcast_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta>;
}

macro_rules! impl_meta_cast {
    ($($node:ident),*) => {
        $(
            impl<P: Phase> MetaCast<P> for node::$node {
                type Meta = P::$node;

                fn upcast(this: Self::Meta) -> Meta<P> {
                    Meta::$node(this)
                }

                fn try_downcast_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
                    match meta {
                        Meta::$node(val) => Some(val),
                        _ => None,
                    }
                }

                fn try_downcast_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
                    match meta {
                        Meta::$node(val) => Some(val),
                        _ => None,
                    }
                }
            }
        )*
    };
}

impl_meta_cast!(
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
    TypeAlias
);

#[derive(Clone, Debug)]
pub enum Meta<P: Phase> {
    Name(<node::Name as MetaCast<P>>::Meta),
    Ident(<node::Ident as MetaCast<P>>::Meta),
    Literal(<node::Literal as MetaCast<P>>::Meta),
    List(<node::List as MetaCast<P>>::Meta),
    Property(<node::Property as MetaCast<P>>::Meta),
    Record(<node::Record as MetaCast<P>>::Meta),
    RecordSelect(<node::RecordSelect as MetaCast<P>>::Meta),
    RecordExtend(<node::RecordExtend as MetaCast<P>>::Meta),
    RecordRestrict(<node::RecordRestrict as MetaCast<P>>::Meta),
    RecordUpdate(<node::RecordUpdate as MetaCast<P>>::Meta),
    UnaryOp(<node::UnaryOp as MetaCast<P>>::Meta),
    Unary(<node::Unary as MetaCast<P>>::Meta),
    BinaryOp(<node::BinaryOp as MetaCast<P>>::Meta),
    Binary(<node::Binary as MetaCast<P>>::Meta),
    Let(<node::Let as MetaCast<P>>::Meta),
    PatError(<node::PatError as MetaCast<P>>::Meta),
    Wildcard(<node::Wildcard as MetaCast<P>>::Meta),
    LiteralPat(<node::LiteralPat as MetaCast<P>>::Meta),
    IdentPat(<node::IdentPat as MetaCast<P>>::Meta),
    PropertyPat(<node::PropertyPat as MetaCast<P>>::Meta),
    RecordPat(<node::RecordPat as MetaCast<P>>::Meta),
    Pat(<node::Pat as MetaCast<P>>::Meta),
    Branch(<node::Branch as MetaCast<P>>::Meta),
    Case(<node::Case as MetaCast<P>>::Meta),
    If(<node::If as MetaCast<P>>::Meta),
    Func(<node::Func as MetaCast<P>>::Meta),
    Call(<node::Call as MetaCast<P>>::Meta),
    ExprError(<node::ExprError as MetaCast<P>>::Meta),
    Expr(<node::Expr as MetaCast<P>>::Meta),
    TypeError(<node::TypeError as MetaCast<P>>::Meta),
    TypeIdent(<node::TypeIdent as MetaCast<P>>::Meta),
    PropertyType(<node::PropertyType as MetaCast<P>>::Meta),
    RecordType(<node::RecordType as MetaCast<P>>::Meta),
    FuncType(<node::FuncType as MetaCast<P>>::Meta),
    MonoType(<node::MonoType as MetaCast<P>>::Meta),
    PolyType(<node::PolyType as MetaCast<P>>::Meta),
    TypeAlias(<node::TypeAlias as MetaCast<P>>::Meta),
}

macro_rules! inner {
    ($enum: ident) => {
        match $enum {
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
            Self::TypeError(m) => m,
            Self::TypeIdent(m) => m,
            Self::PropertyType(m) => m,
            Self::RecordType(m) => m,
            Self::FuncType(m) => m,
            Self::MonoType(m) => m,
            Self::PolyType(m) => m,
            Self::TypeAlias(m) => m,
        }
    };
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
            TypeError = M,
            TypeIdent = M,
            PropertyType = M,
            RecordType = M,
            FuncType = M,
            MonoType = M,
            PolyType = M,
            TypeAlias = M,
        >,
{
    pub fn inner_ref(&self) -> &M {
        inner!(self)
    }

    pub fn inner_mut(&mut self) -> &mut M {
        inner!(self)
    }

    pub fn inner_copied(&self) -> M
    where
        M: Copy,
    {
        *inner!(self)
    }

    pub fn inner_cloned(&self) -> M
    where
        M: Clone,
    {
        inner!(self).clone()
    }

    pub fn into_inner(self) -> M {
        inner!(self)
    }
}
