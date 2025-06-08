use kola_span::Loc;
use kola_syntax::loc::{LocPhase, Locations};
use kola_tree::{
    id::Id,
    meta::{MetaCast, MetaView},
};
use kola_utils::interner::PathKey;
use std::{collections::HashMap, ops::Index, rc::Rc};

use crate::QualId;

#[derive(Debug, Clone, Default)]
pub struct Topography(HashMap<PathKey, Rc<Locations>>);

impl Topography {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn insert(&mut self, path: PathKey, loc: Locations) {
        self.0.insert(path, Rc::new(loc));
    }

    pub fn get(&self, path: &PathKey) -> Option<&Rc<Locations>> {
        self.0.get(path)
    }

    pub fn span<T>(&self, id: QualId<T>) -> Loc
    where
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        let topography = &self.0[&id.0];
        *topography.meta(id.1)
    }
}

impl Index<PathKey> for Topography {
    type Output = Rc<Locations>;

    fn index(&self, path: PathKey) -> &Self::Output {
        self.0.get(&path).unwrap()
    }
}
