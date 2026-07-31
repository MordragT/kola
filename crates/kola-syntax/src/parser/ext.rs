use std::fmt::Debug;

use kola_span::{Collection, Loc, combinator::Combinator};
use kola_tree::prelude::*;

use super::ParseInput;
use super::state::State;

pub struct Slice<T>(SliceBuilder<T>);

impl<T> Clone for Slice<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> Debug for Slice<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Slice({:?})", self.0)
    }
}

impl<'t, T> Collection<ParseInput<'t>, Id<T>> for Slice<T> {
    type Output = SliceId<T>;

    fn new_with(_input: &mut ParseInput) -> Self {
        let builder = SliceStorage::builder();
        Self(builder)
    }

    fn push_with(&mut self, item: Id<T>, _input: &mut ParseInput) {
        self.0.push(item);
    }

    fn finish_with(self, input: &mut ParseInput<'t>) -> Self::Output {
        self.0.finish(&mut input.state.builder.slices)
    }
}

pub const trait KolaCombinator<'t, T: Debug>: const Combinator<ParseInput<'t>, T> {
    fn to_node(self) -> impl const Combinator<ParseInput<'t>, Id<T>>
    where
        NodeStorage: Col<T, Column = Vec<T>>,
        UniversalStorage<Loc>: Col<T, Column = Vec<Loc>>,
    {
        self.map_with(|node, loc, input| {
            let state: &mut State = input.state();
            state.insert(node, loc)
        })
    }

    fn map_to_node<F, U>(self, f: F) -> impl const Combinator<ParseInput<'t>, Id<U>>
    where
        U: Debug,
        F: Fn(T) -> U + Copy,
        NodeStorage: Col<U, Column = Vec<U>>,
        UniversalStorage<Loc>: Col<U, Column = Vec<Loc>>,
    {
        self.map(f).to_node()
    }

    fn to_expr(self) -> impl const Combinator<ParseInput<'t>, Id<node::Expr>>
    where
        node::Expr: From<T>,
    {
        self.map(node::Expr::from).to_node()
    }

    fn to_pat(self) -> impl const Combinator<ParseInput<'t>, Id<node::Pat>>
    where
        node::Pat: From<T>,
    {
        self.map(node::Pat::from).to_node()
    }

    fn to_type(self) -> impl const Combinator<ParseInput<'t>, Id<node::TypeExpr>>
    where
        node::TypeExpr: From<T>,
    {
        self.map(node::TypeExpr::from).to_node()
    }

    fn to_module_expr(self) -> impl const Combinator<ParseInput<'t>, Id<node::ModuleExpr>>
    where
        node::ModuleExpr: From<T>,
    {
        self.map(node::ModuleExpr::from).to_node()
    }

    fn to_bind(self) -> impl const Combinator<ParseInput<'t>, Id<node::Bind>>
    where
        node::Bind: From<T>,
    {
        self.map(node::Bind::from).to_node()
    }

    fn to_spec(self) -> impl const Combinator<ParseInput<'t>, Id<node::Spec>>
    where
        node::Spec: From<T>,
    {
        self.map(node::Spec::from).to_node()
    }

    fn to_module_type(self) -> impl const Combinator<ParseInput<'t>, Id<node::ModuleType>>
    where
        node::ModuleType: From<T>,
    {
        self.map(node::ModuleType::from).to_node()
    }
}

const impl<'t, T, P> KolaCombinator<'t, T> for P
where
    T: Debug,
    P: const Combinator<ParseInput<'t>, T>,
{
}
