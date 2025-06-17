use crate::{id::Id, node};
use std::{collections::HashMap, fmt::Debug};

pub type MetaVec<P> = Vec<Meta<P>>;
pub type MetaMap<P> = HashMap<usize, Meta<P>>;

pub trait MetaMapExt<P: Phase> {
    fn insert_meta<T>(&mut self, id: Id<T>, meta: T::Meta)
    where
        T: MetaCast<P>;
}

impl<P: Phase> MetaMapExt<P> for MetaMap<P> {
    fn insert_meta<T>(&mut self, id: Id<T>, meta: T::Meta)
    where
        T: MetaCast<P>,
    {
        let meta = T::upcast(meta);
        self.insert(id.as_usize(), meta);
    }
}

pub trait MetaView<P: Phase>: Sized {
    fn get(&self, id: usize) -> &Meta<P>;
    fn get_mut(&mut self, id: usize) -> &mut Meta<P>;

    fn meta<T>(&self, id: Id<T>) -> &T::Meta
    where
        T: MetaCast<P>,
    {
        let meta = self.get(id.as_usize());
        T::try_downcast_ref(meta).unwrap()
    }

    fn meta_mut<T>(&mut self, id: Id<T>) -> &mut T::Meta
    where
        T: MetaCast<P>,
    {
        let meta = self.get_mut(id.as_usize());
        T::try_downcast_mut(meta).unwrap()
    }

    fn update_meta<T>(&mut self, id: Id<T>, meta: T::Meta) -> T::Meta
    where
        T: MetaCast<P>,
    {
        std::mem::replace(self.meta_mut(id), meta)
    }
}

impl<P: Phase> MetaView<P> for MetaVec<P> {
    fn get(&self, id: usize) -> &Meta<P> {
        &self[id]
    }

    fn get_mut(&mut self, id: usize) -> &mut Meta<P> {
        &mut self[id]
    }
}

impl<P: Phase> MetaView<P> for MetaMap<P> {
    fn get(&self, id: usize) -> &Meta<P> {
        self.get(&id).unwrap()
    }

    fn get_mut(&mut self, id: usize) -> &mut Meta<P> {
        self.get_mut(&id).unwrap()
    }
}

pub trait MetaCast<P: Phase> {
    type Meta;

    fn upcast(attached: Self::Meta) -> Meta<P>;
    fn try_downcast_ref(meta: &Meta<P>) -> Option<&Self::Meta>;
    fn try_downcast_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta>;
}

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

        impl<P: Phase> Meta<P> {
            pub fn kind(&self) -> node::NodeKind {
                match self {
                    $(
                        Self::$node(_) => node::NodeKind::$node,
                    )*
                }
            }
        }

        impl<P> Meta<P>
        where
            P: Phase<$($node: Default,)*>,
        {
            pub fn default_for(kind: node::NodeKind) -> Self {
                match kind {
                    $(
                        node::NodeKind::$node => Self::$node(Default::default()),
                    )*
                }
            }
        }

        impl<P, M> Meta<P>
        where
            M: Clone + Debug,
            P: Phase<$($node = M,)*>,
        {
            pub fn default_with( m: M, kind: node::NodeKind) -> Self {
                match kind {
                    $(
                        node::NodeKind::$node => Self::$node(m),
                    )*
                }
            }

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
    ModuleName,
    TypeName,
    ValueName,
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
    ListExpr,
    RecordField,
    RecordExpr,
    RecordExtendExpr,
    RecordRestrictExpr,
    RecordUpdateOp,
    RecordUpdateExpr,
    FieldPath,
    PathExpr,
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
    Vis,
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
