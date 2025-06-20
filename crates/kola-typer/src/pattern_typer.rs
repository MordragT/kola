use kola_tree::prelude::*;
use std::ops::ControlFlow;

use crate::{constraints::Constraints, env::LocalTypeEnv, types::MonoType};

pub struct PatternTyper<'a> {
    env: &'a mut LocalTypeEnv,
    cons: &'a mut Constraints,
    source: MonoType,
}

impl<'a> PatternTyper<'a> {
    pub fn new(env: &'a mut LocalTypeEnv, cons: &'a mut Constraints, source: MonoType) -> Self {
        Self { env, cons, source }
    }

    pub fn run<T, N>(mut self, id: Id<N>, tree: &T)
    where
        T: TreeView,
        Id<N>: Visitable<T>,
    {
        match id.visit_by(&mut self, tree) {
            ControlFlow::Break(_) => (),
            ControlFlow::Continue(_) => (),
        }
    }
}

impl<'a, T> Visitor<T> for PatternTyper<'a>
where
    T: TreeView,
{
    type BreakValue = !;

    fn visit_pat_error(
        &mut self,
        _id: Id<node::PatError>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    fn visit_any_pat(&mut self, _id: Id<node::AnyPat>, _tree: &T) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    fn visit_literal_pat(
        &mut self,
        _id: Id<node::LiteralPat>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    fn visit_bind_pat(
        &mut self,
        _id: Id<node::BindPat>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    fn visit_list_pat(
        &mut self,
        _id: Id<node::ListPat>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    fn visit_record_pat(
        &mut self,
        _id: Id<node::RecordPat>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    fn visit_variant_pat(
        &mut self,
        _id: Id<node::VariantPat>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    fn visit_pat(&mut self, id: Id<node::Pat>, tree: &T) -> ControlFlow<Self::BreakValue> {
        match *id.get(tree) {
            node::Pat::Error(error_id) => self.visit_pat_error(error_id, tree),
            node::Pat::Any(any_id) => self.visit_any_pat(any_id, tree),
            node::Pat::Literal(literal_id) => self.visit_literal_pat(literal_id, tree),
            node::Pat::Bind(bind_id) => self.visit_bind_pat(bind_id, tree),
            node::Pat::List(list_id) => self.visit_list_pat(list_id, tree),
            node::Pat::Record(record_id) => self.visit_record_pat(record_id, tree),
            node::Pat::Variant(variant_id) => self.visit_variant_pat(variant_id, tree),
        }
    }
}
