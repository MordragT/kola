mod expr;
mod module;
mod namespace;
mod pat;
mod ty;

pub use expr::*;
pub use module::*;
pub use namespace::*;
pub use pat::*;
pub use ty::*;

use pastey::paste;

use crate::{
    id::{Id, IdIter},
    query::{Col, Get},
};

macro_rules! repeat_ty {
    ($_name:ident $ty:ty) => {
        $ty
    };
}

macro_rules! define_node_family {
    ( $( $Name:ident ),* $(,)? ) => {
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
                fn from(node: &'a $Name) -> Self {
                    Node::$Name(node)
                }
            }
        )*

        impl<'a> Node<'a> {
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
                $Name(Id<$Name>),
            )*
        }

        $(
            impl From<Id<$Name>> for AnyId {
                fn from(id: Id<$Name>) -> Self {
                    AnyId::$Name(id)
                }
            }
        )*

        paste! {
            #[derive(Debug, Clone)]
            pub struct Storage< $([< $Name T >],)+ > {
                $(pub [< $Name:snake:lower >]: Vec<[< $Name T >]>,)+
            }

            impl< $([< $Name T >],)+ > Default for Storage< $([< $Name T >],)+ > {
                fn default() -> Self {
                    Self {
                        $([< $Name:snake:lower >]: Vec::new(),)+
                    }
                }
            }

            #[derive(Debug, Clone, Copy)]
            pub struct StorageCheckpoint {
                $(pub [< $Name:snake:lower >]: usize,)+
            }

            $(
                impl Col<$Name> for StorageCheckpoint {
                    type Column = usize;
                    type Ids<'a> = IdIter<$Name>
                    where Self: 'a;

                    fn col(&self) -> &usize { &self.[< $Name:snake:lower >] }
                    fn col_mut(&mut self) -> &mut usize { &mut self.[< $Name:snake:lower >] }

                    fn ids<'a>(&'a self) -> IdIter<$Name> {
                        let len = self.[< $Name:snake:lower >] as u32;
                        IdIter::new(0, len)
                    }
                }
            )*

            impl< $([< $Name T>],)+ > Storage< $([< $Name T >],)+ > {
                pub fn checkpoint(&self) -> StorageCheckpoint {
                    StorageCheckpoint {
                        $([< $Name:snake:lower >]: self.[< $Name:snake:lower >].len(),)+
                    }
                }

                pub fn restore(&mut self, cp: &StorageCheckpoint) {
                    $(self.[< $Name:snake:lower >].truncate(cp.[< $Name:snake:lower >]);)+
                }
            }


            pub type NodeStorage = Storage<
                $( $Name, )+
            >;

            $(
                impl Get<$Name> for NodeStorage {
                    type Item = $Name;
                    fn get(&self, id: Id<$Name>) -> &$Name { &self.[< $Name:snake:lower >][id.as_usize()] }
                    fn get_mut(&mut self, id: Id<$Name>) -> &mut $Name { &mut self.[< $Name:snake:lower >][id.as_usize()] }
                }

                impl Col<$Name> for NodeStorage {
                    type Column = Vec<$Name>;
                    type Ids<'a> = IdIter<$Name>
                    where Self: 'a;

                    fn col(&self) -> &Vec<$Name> { &self.[< $Name:snake:lower >] }
                    fn col_mut(&mut self) -> &mut Vec<$Name> { &mut self.[< $Name:snake:lower >] }

                    fn ids<'a>(&'a self) -> IdIter<$Name> {
                        let len = self.[< $Name:snake:lower >].len() as u32;
                        IdIter::new(0, len)
                    }
                }
            )+

            impl NodeStorage {
                pub fn get_any(&self, id: AnyId) -> Node<'_> {
                    match id {
                        $(
                            AnyId::$Name(id) => Node::$Name(&self.[< $Name:snake:lower >][id.as_usize()]),
                        )*
                    }
                }

                pub fn get<T>(&self, id: Id<T>) -> &T
                where
                    NodeStorage: Get<T, Item = T>,
                {
                    <NodeStorage as Get<T>>::get(self, id)
                }

                pub fn get_mut<T>(&mut self, id: Id<T>) -> &mut T
                where
                    NodeStorage: Get<T, Item = T>,
                {
                    <NodeStorage as Get<T>>::get_mut(self, id)
                }

                pub fn alloc<T>(&mut self, val: T) -> Id<T>
                where
                    NodeStorage: Col<T, Column = Vec<T>>,
                {
                    let id = self.col().len() as u32;
                    self.col_mut().push(val);
                    Id::new(id)
                }
            }


            pub type UniversalStorage<M> = Storage<
                $( repeat_ty!($Name M), )*
            >;

            $(
                impl<M> Get<$Name> for UniversalStorage<M> {
                    type Item = M;
                    fn get(&self, id: Id<$Name>) -> &M { &self.[< $Name:snake:lower >][id.as_usize()] }
                    fn get_mut(&mut self, id: Id<$Name>) -> &mut M { &mut self.[< $Name:snake:lower >][id.as_usize()] }
                }


                impl<M> Col<$Name> for UniversalStorage<M> {
                    type Column = Vec<M>;
                    type Ids<'a> = IdIter<$Name>
                    where Self: 'a;

                    fn col(&self) -> &Vec<M> { &self.[< $Name:snake:lower >] }
                    fn col_mut(&mut self) -> &mut Vec<M> { &mut self.[< $Name:snake:lower >] }

                    fn ids<'a>(&'a self) -> IdIter<$Name> {
                        let len = self.[< $Name:snake:lower >].len() as u32;
                        IdIter::new(0, len)
                    }
                }
            )+

            impl<M> UniversalStorage<M> {
                pub fn from_checkpoint(cp: StorageCheckpoint) -> Self
                where M: Default + Clone {
                    Self {
                        $([< $Name:snake:lower >]: vec![M::default(); cp.[< $Name:snake:lower >]],)+
                    }
                }

                pub fn from_checkpoint_with(cp: StorageCheckpoint, value: M) -> Self
                where
                    M: Clone,
                {
                    Self {
                        $([< $Name:snake:lower >]: vec![value.clone(); cp.[< $Name:snake:lower >]],)+
                    }
                }

                pub fn get_any(&self, id: AnyId) -> &M {
                    match id {
                        $(
                            AnyId::$Name(id) => &self.[< $Name:snake:lower >][id.as_usize()],
                        )*
                    }
                }
            }
        }
    };
}

define_node_family! {
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
