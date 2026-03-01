use chumsky::prelude::*;

use kola_span::Loc;
use kola_tree::prelude::*;

use super::{Extra, ParseInput, State};
use crate::loc::LocPhase;

/// A wrapper for parsers that HAVE side-effects (like semantic tokens).
/// This type does NOT implement Parser, forcing the user to acknowledge the
/// side-effect by calling `to`, `ignore_then`, or `then_ignore`.
#[must_use = "Captured parsers must be consumed to ensure side-effects run"]
pub struct Captured<'t, T, P> {
    parser: P,
    _phantom: std::marker::PhantomData<&'t T>,
}

impl<'t, T, P> Captured<'t, T, P>
where
    P: Parser<'t, ParseInput<'t>, T, Extra<'t>>,
{
    pub fn new(parser: P) -> Self {
        Self {
            parser,
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn into_inner(self) -> P {
        self.parser
    }

    /// Replace the output with a fixed value, ensuring the side effect runs.
    /// By using `map`, we ensure the optimizer cannot skip the underlying parser.
    pub fn to<U: Clone + 't>(self, value: U) -> impl Parser<'t, ParseInput<'t>, U, Extra<'t>> {
        self.parser.map(move |_| value.clone())
    }

    /// Like Parser::ignore_then, but ensures this captured parser's side effect runs.
    pub fn ignore_then<U, P2>(self, other: P2) -> impl Parser<'t, ParseInput<'t>, U, Extra<'t>>
    where
        P2: Parser<'t, ParseInput<'t>, U, Extra<'t>>,
    {
        // Using .then() and .map() forces the optimizer to run the first parser
        // to produce the first element of the tuple.
        self.parser.then(other).map(|(_, u)| u)
    }

    /// Like Parser::then_ignore, but ensures this captured parser's side effect runs.
    pub fn then_ignore<U, P2>(self, other: P2) -> impl Parser<'t, ParseInput<'t>, T, Extra<'t>>
    where
        P2: Parser<'t, ParseInput<'t>, U, Extra<'t>>,
    {
        // Using .then() and .map() forces the optimizer to run both parsers.
        self.parser.then(other).map(|(t, _)| t)
    }

    /// Ignore the output, but ensure the side effect runs.
    pub fn ignored(self) -> impl Parser<'t, ParseInput<'t>, (), Extra<'t>> {
        self.parser.map(|_| ())
    }
}

pub trait KolaParser<'t, T>: Parser<'t, ParseInput<'t>, T, Extra<'t>> + Sized {
    #[inline]
    fn spanned(self) -> impl Parser<'t, ParseInput<'t>, (T, Loc), Extra<'t>> {
        self.map_with(|node, e| {
            let span = e.span();
            (node, span)
        })
    }

    #[inline]
    fn to_node(self) -> impl Parser<'t, ParseInput<'t>, Id<T>, Extra<'t>>
    where
        Node: From<T>,
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        self.map_with(|node, e| {
            let span = e.span();
            let state: &mut State = e.state();
            state.insert(node, span)
        })
    }

    #[inline]
    fn map_to_node<F, U>(self, f: F) -> impl Parser<'t, ParseInput<'t>, Id<U>, Extra<'t>>
    where
        F: Fn(T) -> U,
        U: MetaCast<LocPhase, Meta = Loc>,
        Node: From<U>,
    {
        self.map(f).to_node()
    }

    #[inline]
    fn to_expr(self) -> impl Parser<'t, ParseInput<'t>, Id<node::Expr>, Extra<'t>>
    where
        node::Expr: From<T>,
    {
        self.map(node::Expr::from).to_node()
    }

    #[inline]
    fn to_pat(self) -> impl Parser<'t, ParseInput<'t>, Id<node::Pat>, Extra<'t>>
    where
        node::Pat: From<T>,
    {
        self.map(node::Pat::from).to_node()
    }

    #[inline]
    fn to_type(self) -> impl Parser<'t, ParseInput<'t>, Id<node::Type>, Extra<'t>>
    where
        node::Type: From<T>,
    {
        self.map(node::Type::from).to_node()
    }

    #[inline]
    fn to_module_expr(self) -> impl Parser<'t, ParseInput<'t>, Id<node::ModuleExpr>, Extra<'t>>
    where
        node::ModuleExpr: From<T>,
    {
        self.map(node::ModuleExpr::from).to_node()
    }

    #[inline]
    fn to_bind(self) -> impl Parser<'t, ParseInput<'t>, Id<node::Bind>, Extra<'t>>
    where
        node::Bind: From<T>,
    {
        self.map(node::Bind::from).to_node()
    }

    #[inline]
    fn to_spec(self) -> impl Parser<'t, ParseInput<'t>, Id<node::Spec>, Extra<'t>>
    where
        node::Spec: From<T>,
    {
        self.map(node::Spec::from).to_node()
    }

    #[inline]
    fn to_module_type(self) -> impl Parser<'t, ParseInput<'t>, Id<node::ModuleType>, Extra<'t>>
    where
        node::ModuleType: From<T>,
    {
        self.map(node::ModuleType::from).to_node()
    }
}

impl<'t, T, P> KolaParser<'t, T> for P where P: Parser<'t, ParseInput<'t>, T, Extra<'t>> + Sized {}
