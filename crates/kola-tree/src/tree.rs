use pastey::paste;
use std::sync::Arc;

use kola_relations::id::Id;
use kola_relations::slice::{SliceId, SliceStorage};
use kola_relations::{Allocate, SliceRelation, SliceRelationMut, TotalRelation};
use kola_span::Loc;

use crate::node::*;

pub trait TotalSpan<K: 'static, V: 'static> {
    fn get_span(&self, id: Id<K>) -> &V;
}

pub struct LocatedIter<'a, T, M> {
    iter: std::slice::Iter<'a, (T, M)>,
}

impl<'a, T, M> LocatedIter<'a, T, M> {
    #[inline]
    pub fn new(iter: std::slice::Iter<'a, (T, M)>) -> Self {
        Self { iter }
    }
}

impl<'a, T, M> Iterator for LocatedIter<'a, T, M> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(val, _)| val)
    }
}

pub struct LocatedSpanIter<'a, T, M> {
    iter: std::slice::Iter<'a, (T, M)>,
}

impl<'a, T, M> LocatedSpanIter<'a, T, M> {
    #[inline]
    pub fn new(iter: std::slice::Iter<'a, (T, M)>) -> Self {
        Self { iter }
    }
}

impl<'a, T, M> Iterator for LocatedSpanIter<'a, T, M> {
    type Item = &'a M;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(_, loc)| loc)
    }
}

pub struct LocatedIterMut<'a, T, M> {
    iter: std::slice::IterMut<'a, (T, M)>,
}

impl<'a, T, M> LocatedIterMut<'a, T, M> {
    #[inline]
    pub fn new(iter: std::slice::IterMut<'a, (T, M)>) -> Self {
        Self { iter }
    }
}

impl<'a, T, M> Iterator for LocatedIterMut<'a, T, M> {
    type Item = &'a mut T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(val, _)| val)
    }
}

macro_rules! define_tree_and_relations {
    ( $( $Name:ident ),* $(,)? ) => {
        // --- Enums: NodeKind, Node, AnyId ---

        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(u8)]
        pub enum NodeKind {
            $(
                $Name,
            )*
        }

        #[derive(Debug, Clone, PartialEq, PartialOrd)]
        pub enum Node<'a> {
            $(
                $Name(&'a $Name),
            )*
        }

        $(
            impl<'a> From<&'a $Name> for Node<'a> {
                #[inline]
                fn from(node: &'a $Name) -> Self {
                    Node::$Name(node)
                }
            }
        )*

        impl<'a> Node<'a> {
            #[inline]
            pub fn kind(self) -> NodeKind {
                match self {
                    $(
                        Node::$Name(_) => NodeKind::$Name,
                    )*
                }
            }
        }

        #[derive(
            Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash,
            serde::Serialize, serde::Deserialize,
        )]
        pub enum AnyId {
            $(
                $Name(kola_relations::Id<$Name>),
            )*
        }

        $(
            impl From<kola_relations::Id<$Name>> for AnyId {
                #[inline]
                fn from(id: kola_relations::Id<$Name>) -> Self {
                    AnyId::$Name(id)
                }
            }
        )*

        paste! {
            // --- Tree & Checkpoint ---

            #[derive(Debug, Clone)]
            pub struct Tree<M = Loc> {
                pub slices: SliceStorage,
                $(pub [< $Name:snake:lower >]: Arc<Vec<([< $Name >], M)>>,)+
            }

            impl<M> Default for Tree<M> {
                #[inline]
                fn default() -> Self {
                    Self {
                        slices: SliceStorage::default(),
                        $([< $Name:snake:lower >]: Arc::new(Vec::new()),)+
                    }
                }
            }

            #[derive(Debug, Clone, Copy)]
            pub struct TreeCheckpoint {
                pub slices: usize,
                $(pub [< $Name:snake:lower >]: usize,)+
            }

            impl<M: Clone + 'static> Tree<M> {
                #[inline]
                pub fn checkpoint(&self) -> TreeCheckpoint {
                    TreeCheckpoint {
                        slices: self.slices.checkpoint(),
                        $([< $Name:snake:lower >]: self.[< $Name:snake:lower >].len(),)+
                    }
                }

                #[inline]
                pub fn restore(&mut self, cp: &TreeCheckpoint) {
                    self.slices.restore(cp.slices);
                    $(Arc::make_mut(&mut self.[< $Name:snake:lower >]).truncate(cp.[< $Name:snake:lower >]);)+
                }

                #[inline]
                pub fn get_any(&self, id: AnyId) -> Node<'_> {
                    match id {
                        $(
                            AnyId::$Name(id) => Node::$Name(self.get(id)),
                        )*
                    }
                }

                #[inline]
                pub fn get_any_span(&self, id: AnyId) -> &M {
                    match id {
                        $(
                            AnyId::$Name(id) => self.get_span(id),
                        )*
                    }
                }
            }

            // --- Relation Implementations ---

            $(
                impl<M: 'static> kola_relations::RelationIds<[< $Name >]> for Tree<M> {
                    type Ids<'a> = kola_relations::id::IdIter<[< $Name >]> where Self: 'a;

                    #[inline]
                    fn ids(&self) -> Self::Ids<'_> {
                        kola_relations::id::IdIter::new(0, self.[< $Name:snake:lower >].len() as u32)
                    }
                }

                // Node retrieval
                impl<M: 'static> kola_relations::TotalRelation<[< $Name >], [< $Name >]> for Tree<M> {
                    #[inline]
                    fn get(&self, id: Id<[< $Name >]>) -> &[< $Name >] {
                        &self.[< $Name:snake:lower >][id.as_usize()].0
                    }

                    type Rows<'a> = std::iter::Zip<
                        kola_relations::id::IdIter<[< $Name >]>,
                        LocatedIter<'a, [< $Name >], M>
                    > where Self: 'a, M: 'a;

                    #[inline]
                    fn rows(&self) -> Self::Rows<'_> {
                        kola_relations::RelationIds::<[< $Name >]>::ids(self)
                            .zip(LocatedIter::new(self.[< $Name:snake:lower >].iter()))
                    }
                }

                // Span retrieval
                impl<M: 'static> TotalSpan<[< $Name >], M> for Tree<M> {
                    #[inline]
                    fn get_span(&self, id: Id<[< $Name >]>) -> &M {
                        &self.[< $Name:snake:lower >][id.as_usize()].1
                    }
                }

                // Mutable Node retrieval
                impl<M: Clone + 'static> kola_relations::TotalRelationMut<[< $Name >], [< $Name >]> for Tree<M> {
                    #[inline]
                    fn get_mut(&mut self, id: Id<[< $Name >]>) -> &mut [< $Name >] {
                        &mut Arc::make_mut(&mut self.[< $Name:snake:lower >])[id.as_usize()].0
                    }

                    type RowsMut<'a> = std::iter::Zip<
                        kola_relations::id::IdIter<[< $Name >]>,
                        LocatedIterMut<'a, [< $Name >], M>
                    > where Self: 'a, M: 'a;

                    #[inline]
                    fn rows_mut(&mut self) -> Self::RowsMut<'_> {
                        let ids = kola_relations::RelationIds::<[< $Name >]>::ids(self);
                        let iter = LocatedIterMut::new(Arc::make_mut(&mut self.[< $Name:snake:lower >]).iter_mut());
                        ids.zip(iter)
                    }
                }

                // Single-pass located allocation
                impl<M: Clone + 'static> kola_relations::Allocate<([< $Name >], M)> for Tree<M> {
                    type Key = [< $Name >];

                    #[inline]
                    fn alloc(&mut self, value: ([< $Name >], M)) -> Id<[< $Name >]> {
                        let id = Id::new(self.[< $Name:snake:lower >].len() as u32);
                        Arc::make_mut(&mut self.[< $Name:snake:lower >]).push(value);
                        id
                    }
                }

                // Allocate raw T directly using M::default()
                impl<M: Clone + Default + 'static> kola_relations::Allocate<[< $Name >]> for Tree<M> {
                    type Key = [< $Name >];

                    #[inline]
                    fn alloc(&mut self, value: [< $Name >]) -> Id<[< $Name >]> {
                        kola_relations::Allocate::<([< $Name >], M)>::alloc(self, (value, M::default()))
                    }
                }
            )*
        }
    };
}

define_tree_and_relations! {
    FunctorName,
    ModuleTypeName,
    ModuleName,
    KindName,
    TypeName,
    ValueName,
    AnyPat,
    LiteralPat,
    BindPat,
    ListElPat,
    ListPat,
    RecordFieldPat,
    RecordPat,
    VariantTagPat,
    VariantPat,
    PatError,
    Pat,
    LiteralExpr,
    ListExpr,
    RecordField,
    RecordExpr,
    RecordExtendExpr,
    RecordRestrictExpr,
    RecordUpdateOp,
    RecordUpdateExpr,
    RecordMergeExpr,
    FieldPath,
    QualifiedExpr,
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
    HandlerClause,
    HandleExpr,
    DoExpr,
    TagExpr,
    TypeWitnessExpr,
    ExprError,
    Expr,
    EffectOpType,
    EffectType,
    QualifiedType,
    TypeVar,
    LabelOrVar,
    RecordFieldType,
    RecordType,
    TagType,
    VariantType,
    FuncType,
    TypeApplication,
    CompType,
    TypeExpr,
    TypeError,
    TypeVarBind,
    ForallBinder,
    TypeScheme,
    BindError,
    Vis,
    ValueBind,
    TypeBind,
    ModuleBind,
    ModuleTypeBind,
    FunctorParam,
    FunctorBind,
    Bind,
    ModuleError,
    ModuleBody,
    ModulePath,
    ModuleImport,
    FunctorArgs,
    FunctorApp,
    ModuleExpr,
    SpecError,
    ValueSpec,
    ModuleSpec,
    Spec,
    ConcreteModuleType,
    QualifiedModuleType,
    ModuleType,
}

impl<M: Clone + 'static> Tree<M> {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn alloc<T: 'static>(&mut self, value: T) -> Id<T>
    where
        Self: Allocate<T, Key = T>,
    {
        Allocate::<T>::alloc(self, value)
    }

    #[inline]
    pub fn alloc_located<T: 'static>(&mut self, value: (T, M)) -> Id<T>
    where
        Self: Allocate<(T, M), Key = T>,
    {
        Allocate::<(T, M)>::alloc(self, value)
    }

    #[inline]
    pub fn alloc_slice<T: 'static>(
        &mut self,
        values: impl IntoIterator<Item = Id<T>>,
    ) -> SliceId<T> {
        self.slices.alloc(values)
    }

    #[inline]
    pub fn get<T: 'static>(&self, id: Id<T>) -> &T
    where
        Self: TotalRelation<T, T>,
    {
        TotalRelation::<T, T>::get(self, id)
    }

    #[inline]
    pub fn get_span<T: 'static>(&self, id: Id<T>) -> &M
    where
        Self: TotalSpan<T, M>,
    {
        TotalSpan::<T, M>::get_span(self, id)
    }

    #[inline]
    pub fn get_slice_mut<T>(&mut self, slice_id: SliceId<T>) -> &mut [Id<T>] {
        self.slices.get_mut(slice_id)
    }

    #[inline]
    pub fn finish(self, _root: Id<ModuleBody>) -> Self {
        self
    }

    #[inline]
    pub fn root_id(&self) -> Id<ModuleBody> {
        kola_relations::RelationIds::<ModuleBody>::ids(self)
            .next()
            .expect("Tree has no root node")
    }
}

impl<T: 'static, M: 'static> SliceRelation<T> for Tree<M> {
    #[inline]
    fn get_slice(&self, slice_id: SliceId<T>) -> &[Id<T>] {
        self.slices.get(slice_id)
    }
}

impl<T: 'static, M: 'static> SliceRelationMut<T> for Tree<M> {
    #[inline]
    fn get_slice_mut(&mut self, slice_id: SliceId<T>) -> &mut [Id<T>] {
        self.slices.get_mut(slice_id)
    }
}
