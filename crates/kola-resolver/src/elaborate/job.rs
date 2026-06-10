use std::collections::{HashMap, VecDeque};

use kola_span::Loc;
use kola_tree::{
    id::Id,
    node::{self, FunctorName, ModuleName},
};

use crate::symbol::{AnySym, ModuleSym, Substitute, merge2, merge4};

pub type ElabJobs = VecDeque<ElabJob>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ElabJob {
    Functor {
        id: Id<node::FunctorApp>,
        parent: ModuleSym,
        bind: ModuleSym,
        path: Option<ModuleSym>,
        loc: Loc,
        functor: FunctorName,
        args: Vec<ModuleSym>,
    },
    Path {
        id: Id<node::ModulePath>,
        parent: ModuleSym,
        bind: ModuleSym,
        loc: Loc,
        path: Vec<ModuleName>,
    },
}

impl ElabJob {
    pub fn functor(
        id: Id<node::FunctorApp>,
        parent: ModuleSym,
        bind: ModuleSym,
        path: Option<ModuleSym>,
        loc: Loc,
        functor: FunctorName,
        args: Vec<ModuleSym>,
    ) -> Self {
        Self::Functor {
            id,
            parent,
            bind,
            path,
            loc,
            functor,
            args,
        }
    }

    pub fn path(
        id: Id<node::ModulePath>,
        parent: ModuleSym,
        bind: ModuleSym,
        loc: Loc,
        path: Vec<ModuleName>,
    ) -> Self {
        Self::Path {
            id,
            parent,
            bind,
            loc,
            path,
        }
    }

    pub fn bind(&self) -> ModuleSym {
        match self {
            Self::Functor { bind, .. } => *bind,
            Self::Path { bind, .. } => *bind,
        }
    }

    pub fn loc(&self) -> Loc {
        match self {
            Self::Functor { loc, .. } => *loc,
            Self::Path { loc, .. } => *loc,
        }
    }

    pub fn parent(&self) -> ModuleSym {
        match self {
            Self::Functor { parent, .. } => *parent,
            Self::Path { parent, .. } => *parent,
        }
    }
}

impl Substitute for ElabJob {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        match self {
            Self::Functor {
                id,
                parent,
                bind,
                path,
                loc,
                functor,
                args,
            } => {
                let parent_opt = parent.try_subst(s);
                let bind_opt = bind.try_subst(s);
                let path_opt = path.try_subst(s);
                let args_opt = args.try_subst(s);

                merge4(
                    parent_opt,
                    || *parent,
                    bind_opt,
                    || *bind,
                    path_opt,
                    || *path,
                    args_opt,
                    || args.clone(),
                )
                .map(|(parent, bind, path, args)| {
                    Self::functor(*id, parent, bind, path, *loc, *functor, args)
                })
            }
            Self::Path {
                id,
                parent,
                bind,
                loc,
                path,
            } => {
                let parent_opt = parent.try_subst(s);
                let bind_opt = bind.try_subst(s);

                merge2(parent_opt, || *parent, bind_opt, || *bind)
                    .map(|(parent, bind)| Self::path(*id, parent, bind, *loc, path.clone()))
            }
        }
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>) {
        match self {
            Self::Functor {
                parent, bind, args, ..
            } => {
                parent.subst_mut(s);
                bind.subst_mut(s);
                args.subst_mut(s);
            }
            Self::Path { parent, bind, .. } => {
                parent.subst_mut(s);
                bind.subst_mut(s);
            }
        }
    }
}
