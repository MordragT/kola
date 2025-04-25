use std::{fmt::Debug, marker::PhantomData, ops::Deref, rc::Rc};

use derive_more::Display;

use crate::{id::Id, node};

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
    fn get<T>(&self, id: Id<T>) -> &Meta<P>;

    fn get_mut<T>(&mut self, id: Id<T>) -> &mut Meta<P>;

    fn meta<T>(&self, id: Id<T>) -> &T::Meta
    where
        T: MetaCast<P>,
    {
        let meta = self.get(id);
        T::try_downcast_ref(meta).unwrap()
    }

    fn meta_mut<T>(&mut self, id: Id<T>) -> &mut T::Meta
    where
        T: MetaCast<P>,
    {
        let meta = self.get_mut(id);
        T::try_downcast_mut(meta).unwrap()
    }

    fn update_meta<T>(&mut self, id: Id<T>, meta: T::Meta) -> T::Meta
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
    fn get<T>(&self, id: Id<T>) -> &Meta<P> {
        &self[id.as_usize()]
    }

    fn get_mut<T>(&mut self, id: Id<T>) -> &mut Meta<P> {
        &mut self[id.as_usize()]
    }
}

pub trait MetaCast<P: Phase> {
    type Meta;

    fn upcast(attached: Self::Meta) -> Meta<P>;
    fn try_downcast_ref(meta: &Meta<P>) -> Option<&Self::Meta>;
    fn try_downcast_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Empty;

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Stub<T>(pub T);

macro_rules! define {
    ($($node:ident),*) => {
        pub trait Phase: 'static + Debug + Copy {
            $(type $node: Debug + Clone;)*
        }

        pub trait UniformPhase: 'static + Debug + Copy {
            type Meta: Debug + Clone;
        }

        impl<T: UniformPhase> Phase for T {
            $(type $node = T::Meta;)*
        }

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

        #[derive(Clone, Debug)]
        pub enum Meta<P: Phase> {
            $($node(<node::$node as MetaCast<P>>::Meta),)*
        }

        impl<P, M> Meta<P>
        where
            M: Clone + Debug,
            P: Phase<$($node = M,)*>,
        {
            pub fn inner_ref(&self) -> &M {
                match self {
                    $(Self::$node(m) => m,)*
                }
            }

            pub fn inner_mut(&mut self) -> &mut M {
                match self {
                    $(Self::$node(m) => m,)*
                }
            }

            pub fn inner_copied(&self) -> M
            where
                M: Copy,
            {
                match self {
                    $(Self::$node(m) => *m,)*
                }
            }

            pub fn inner_cloned(&self) -> M
            where
                M: Clone,
            {
                match self {
                    $(Self::$node(m) => m.clone(),)*
                }
            }

            pub fn into_inner(self) -> M {
                match self {
                    $(Self::$node(m) => m,)*
                }
            }
        }
    };
}

define!(
    Name,
    // Patterns
    AnyPat,
    LiteralPat,
    IdentPat,
    RecordFieldPat,
    RecordPat,
    VariantCasePat,
    VariantPat,
    PatError,
    Pat,
    // Expressions
    LiteralExpr,
    PathExpr,
    ListExpr,
    RecordField,
    RecordExpr,
    RecordFieldPath,
    RecordExtendExpr,
    RecordRestrictExpr,
    RecordUpdateOp,
    RecordUpdateExpr,
    UnaryOp,
    UnaryExpr,
    BinaryOp,
    BinaryExpr,
    LetExpr,
    CaseBranch,
    CaseExpr,
    IfExpr,
    LambdaExpr,
    CallExpr,
    ExprError,
    Expr,
    // Types
    TypePath,
    TypeVar,
    RecordFieldType,
    RecordType,
    VariantCaseType,
    VariantType,
    FuncType,
    TypeApplication,
    TypeExpr,
    TypeError,
    Type,
    // Modules
    ValueBind,
    TypeBind,
    OpaqueTypeBind,
    ModuleBind,
    ModuleTypeBind,
    Bind,
    Module,
    ModulePath,
    ModuleImport,
    ModuleExpr,
    ValueSpec,
    OpaqueTypeKind,
    OpaqueTypeSpec,
    ModuleSpec,
    Spec,
    ModuleType
);
