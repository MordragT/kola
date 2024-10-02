use chumsky::prelude::*;

use super::{
    ast::{Binding, Expr, Literal, Module, Pattern, RecordExpr},
    token::{Delimiter, Op, Token},
    Span,
};

pub type ParserInput<'tokens, 'src> =
    chumsky::input::SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;
pub type Extra<'tokens, 'src> = extra::Err<Rich<'tokens, Token<'src>, Span>>;

pub fn parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Module<'src>, Extra<'tokens, 'src>> {
    todo()
}

pub fn binding_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Binding<'src>, Extra<'tokens, 'src>> {
    todo()
}

pub fn pattern_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Pattern<'src>, Extra<'tokens, 'src>> {
    todo()
}

pub fn expr_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expr<'src>, Extra<'tokens, 'src>> + Clone {
    recursive(|expr| {
        let literal = select! {
            Token::Num(n) => Literal::Num(n),
            Token::Bool(b) => Literal::Bool(b),
            Token::Char(c) => Literal::Char(c),
            Token::Str(s) => Literal::Str(s)
        }
        .map_with(|l, e| Expr::Literal(l, e.span()));

        let ident = select! { Token::Ident(id) => id };

        let list = nested_parser(
            expr.clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .map_with(|l, e| Expr::List(l, e.span())),
            Delimiter::Bracket,
            |span| Expr::Error(span),
        );

        // let func = todo();

        let record = ident
            .then_ignore(just(Token::Op(Op::Eq)))
            .then(expr.clone())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .map_with(|r, e| Expr::Record(r, e.span()));

        let extension = just(Token::Op(Op::Plus))
            .ignore_then(ident)
            .then_ignore(just(Token::Op(Op::Eq)))
            .then(expr.clone())
            .then_ignore(just(Token::Pipe))
            .then(expr.clone())
            .map_with(|((field, value), source), e| {
                Expr::RecordExpr(
                    Box::new(RecordExpr::Extension(source, field, value)),
                    e.span(),
                )
            });

        let restriction = just(Token::Op(Op::Minus))
            .ignore_then(ident)
            .then_ignore(just(Token::Pipe))
            .then(expr.clone())
            .map_with(|(field, source), e| {
                Expr::RecordExpr(Box::new(RecordExpr::Restriction(source, field)), e.span())
            });

        let update = ident
            .then_ignore(just(Token::Op(Op::Eq)))
            .then(expr.clone())
            .then_ignore(just(Token::Pipe))
            .then(expr.clone())
            .map_with(|((field, value), source), e| {
                Expr::RecordExpr(Box::new(RecordExpr::Update(source, field, value)), e.span())
            });

        let selection = expr
            .clone()
            .then_ignore(just(Token::Dot))
            .then(ident)
            .map_with(|(source, field), e| {
                Expr::RecordExpr(Box::new(RecordExpr::Selection(source, field)), e.span())
            });

        let record_expr = nested_parser(
            choice((extension, restriction, update, record)),
            Delimiter::Brace,
            |span| Expr::Error(span),
        )
        .or(selection);

        choice((
            literal,
            list,
            // func,
            ident.map_with(|i, e| Expr::Ident(i, e.span())),
            record_expr,
        ))
    })
    .boxed()
}

pub fn nested_parser<'tokens, 'src: 'tokens, T: 'src>(
    parser: impl Parser<'tokens, ParserInput<'tokens, 'src>, T, Extra<'tokens, 'src>> + 'tokens,
    delim: Delimiter,
    fallback: impl Fn(Span) -> T + Clone + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, T, Extra<'tokens, 'src>> + Clone {
    parser
        .delimited_by(just(Token::Open(delim)), just(Token::Close(delim)))
        .recover_with(via_parser(nested_delimiters(
            Token::Open(delim),
            Token::Close(delim),
            [
                (
                    Token::Open(Delimiter::Paren),
                    Token::Close(Delimiter::Paren),
                ),
                (
                    Token::Open(Delimiter::Bracket),
                    Token::Close(Delimiter::Bracket),
                ),
                (
                    Token::Open(Delimiter::Brace),
                    Token::Close(Delimiter::Brace),
                ),
            ],
            fallback,
        )))
        .boxed()
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use chumsky::{input::Input, Parser};

    use crate::syntax::{
        ast::{Expr, Literal, RecordExpr},
        lexer::lexer,
    };

    use super::expr_parser;

    #[test]
    fn record_extension() {
        let src = "{ +x = 10 | y }";
        let tokens = lexer().parse(src).into_output().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_output().unwrap();

        dbg!(&expr);

        let (expr, _) = expr.into_record_expr().unwrap();

        assert_matches!(
            expr,
            RecordExpr::Extension(
                Expr::Ident("y", _),
                "x",
                Expr::Literal(Literal::Num(10.0), _)
            )
        );
    }

    #[test]
    fn record() {
        let src = "{ x = 10, y = 20 }";
        let tokens = lexer().parse(src).into_output().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let (mut expr, _) = expr_parser()
            .parse(input)
            .into_output()
            .unwrap()
            .into_record()
            .unwrap();

        assert_matches!(
            expr.remove("x").unwrap(),
            Expr::Literal(Literal::Num(10.0), _)
        );
        assert_matches!(
            expr.remove("y").unwrap(),
            Expr::Literal(Literal::Num(20.0), _)
        );

        assert!(expr.is_empty())
    }
}
