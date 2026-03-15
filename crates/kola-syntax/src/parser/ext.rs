use kola_span::Loc;
use kola_span::combinator::Combinator;
use kola_span::input::Input;
use kola_tree::prelude::*;

use super::ParseInput;
use super::state::State;
use crate::loc::LocPhase;

pub const trait KolaCombinator<'t, T>: const Combinator<ParseInput<'t>, T> {
    fn to_node(self) -> impl const Combinator<ParseInput<'t>, Id<T>>
    where
        Node: From<T>,
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        self.map_with(|node, loc, input| {
            let state: &mut State = input.state();
            state.insert(node, loc)
        })
    }

    fn map_to_node<F, U>(self, f: F) -> impl const Combinator<ParseInput<'t>, Id<U>>
    where
        F: Fn(T) -> U + Copy,
        U: MetaCast<LocPhase, Meta = Loc>,
        Node: From<U>,
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

    fn to_type(self) -> impl const Combinator<ParseInput<'t>, Id<node::Type>>
    where
        node::Type: From<T>,
    {
        self.map(node::Type::from).to_node()
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

impl<'t, T, P> const KolaCombinator<'t, T> for P where P: const Combinator<ParseInput<'t>, T> {}
