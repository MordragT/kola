use kola_span::{Loc, SourceId};
use kola_syntax::loc::{LocPhase, Locations};
use kola_tree::meta::{MetaCast, MetaView};
use std::{collections::HashMap, ops::Index, rc::Rc};

use crate::GlobalId;

#[derive(Debug, Clone, Default)]
pub struct Topography(HashMap<SourceId, Rc<Locations>>);

impl Topography {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn insert(&mut self, source: SourceId, loc: Locations) {
        self.0.insert(source, Rc::new(loc));
    }

    pub fn get(&self, source: SourceId) -> Option<&Rc<Locations>> {
        self.0.get(&source)
    }

    pub fn span<T>(&self, id: GlobalId<T>) -> Loc
    where
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        let topography = &self.0[&id.source];
        *topography.meta(id.id)
    }
}

impl Index<SourceId> for Topography {
    type Output = Rc<Locations>;

    fn index(&self, source: SourceId) -> &Self::Output {
        self.0.get(&source).unwrap()
    }
}
