use chumsky::{Parser, extra::SimpleState, input::Input, span::SimpleSpan};
use kola_tree::{
    Attached, Meta, MetaContainer, MetaVec, Metadata, NodeId, Phase, Tree, TreeBuilder,
};
use parser::StateRepr;

use self::{
    error::{SyntaxError, SyntaxErrors},
    token::Tokens,
};
use crate::source::Source;

pub mod error;
pub mod lexer;
pub mod parser;
pub mod token;
// pub mod visit;

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Clone, Copy, Debug)]
pub struct SyntaxPhase;

impl Phase for SyntaxPhase {
    type Name = Span;
    type Ident = Span;
    type Literal = Span;
    type List = Span;
    type Property = Span;
    type Record = Span;
    type RecordSelect = Span;
    type RecordExtend = Span;
    type RecordRestrict = Span;
    type RecordUpdate = Span;
    type UnaryOp = Span;
    type Unary = Span;
    type BinaryOp = Span;
    type Binary = Span;
    type Let = Span;
    type PatError = Span;
    type Wildcard = Span;
    type LiteralPat = Span;
    type IdentPat = Span;
    type PropertyPat = Span;
    type RecordPat = Span;
    type Pat = Span;
    type Branch = Span;
    type Case = Span;
    type If = Span;
    type Func = Span;
    type Call = Span;
    type ExprError = Span;
    type Expr = Span;
}

pub struct TokenizeResult<'a> {
    pub tokens: Option<Tokens<'a>>,
    pub errors: SyntaxErrors,
}

pub fn tokenize(input: &str) -> TokenizeResult<'_> {
    let lexer = lexer::lexer();
    let (tokens, errors) = lexer.parse(input).into_output_errors();

    let errors = errors
        .into_iter()
        .map(SyntaxError::from)
        .collect::<SyntaxErrors>();
    TokenizeResult { tokens, errors }
}

pub type SpanMetadata = Metadata<SyntaxPhase, MetaVec<SyntaxPhase>>;

pub struct ParseResult {
    pub tree: Option<Tree>,
    pub spans: SpanMetadata,
    pub errors: SyntaxErrors,
}

pub fn parse(tokens: Tokens<'_>, eoi: Span) -> ParseResult {
    let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));
    let parser = parser::expr_parser();

    let tree = StateRepr::new();
    let mut state = SimpleState(tree);

    let (root, errors) = parser
        .parse_with_state(input, &mut state)
        .into_output_errors();

    let errors = errors
        .into_iter()
        .map(SyntaxError::from)
        .collect::<SyntaxErrors>();

    let StateRepr { builder, meta } = state.0;
    let tree = root.map(|root| builder.finish(root));

    ParseResult {
        tree,
        spans: meta.into_metadata(),
        errors,
    }
}
