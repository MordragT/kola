use chumsky::prelude::*;
use ecow::EcoString;

use crate::syntax::ast::{
    CaseExpr, Ident, LetExpr, List, RecordExtend, RecordRestrict, RecordSelect, RecordUpdate,
};

use super::{
    ast::{
        BinaryExpr, BinaryOp, Expr, FnExpr, IfExpr, Literal, Pat, Record, RecordPat, UnaryExpr,
        UnaryOp,
    },
    token::{Delimiter, Op, Token},
    Span,
};

pub type ParserInput<'tokens, 'src> =
    chumsky::input::SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;
pub type Extra<'tokens, 'src> = extra::Err<Rich<'tokens, Token<'src>, Span>>;

// pub fn parser<'tokens, 'src: 'tokens>(
// ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Module<'src>, Extra<'tokens, 'src>> {
//     todo()
// }

// pub fn binding_parser<'tokens, 'src: 'tokens>(
// ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Binding<'src>, Extra<'tokens, 'src>> {
//     todo()
// }

pub fn literal_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Literal, Extra<'tokens, 'src>> {
    select! {
        Token::Num(n) => Literal::Num(n),
        Token::Bool(b) => Literal::Bool(b),
        Token::Char(c) => Literal::Char(c),
        Token::Str(s) => Literal::Str(EcoString::from(s))
    }
}

pub fn pat_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Pat, Extra<'tokens, 'src>> + Clone {
    recursive(|pat| {
        let ident = select! { Token::Ident(id) => EcoString::from(id) };

        let wildcard = just(Token::Wildcard).map_with(|_, e| Pat::Wildcard(e.span()));
        let literal = literal_parser().map_with(|l, e| Pat::Literal(l, e.span()));

        let record = nested_parser(
            ident
                .then(just(Token::Colon).ignore_then(pat.clone()).or_not())
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .map_with(|fields, e| Pat::Record(RecordPat { fields }, e.span())),
            Delimiter::Brace,
            Pat::Error,
        );

        choice((
            ident.map_with(|i, e| Pat::Ident(i, e.span())),
            wildcard,
            literal,
            record,
        ))
        .boxed()
    })
    .boxed()
}

pub fn expr_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expr, Extra<'tokens, 'src>> + Clone {
    recursive(|expr| {
        let ident = select! { Token::Ident(id) => EcoString::from(id) };

        let literal = literal_parser().map_with(|l, e| Expr::Literal(l, e.span()));

        let list = nested_parser(
            expr.clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .map_with(|values, e| Expr::List(List { values }, e.span())),
            Delimiter::Bracket,
            Expr::Error,
        );

        // record operations

        let instantiate = ident
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .map_with(|fields, e| Expr::Record(Record { fields }, e.span()))
            .boxed();

        enum RecordOp {
            Extend(Ident, Box<Expr>),
            Restrict(Ident),
            Update(Ident, Box<Expr>),
        }

        let extend = just(Token::Op(Op::Add))
            .ignore_then(ident)
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map(|(field, value)| RecordOp::Extend(field, Box::new(value)))
            .boxed();

        let restrict = just(Token::Op(Op::Sub))
            .ignore_then(ident)
            .map(RecordOp::Restrict)
            .boxed();

        let update = ident
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map(|(field, value)| RecordOp::Update(field, Box::new(value)))
            .boxed();

        let inner_op = just(Token::Pipe)
            .ignore_then(choice((extend, restrict, update)))
            .repeated()
            .at_least(1);
        let record_op = ident
            .map_with(|i, e| Expr::Ident(i, e.span()))
            .foldl_with(inner_op, |source, op, e| {
                let source = Box::new(source);
                match op {
                    RecordOp::Extend(field, value) => Expr::RecordExtend(
                        RecordExtend {
                            source,
                            field,
                            value,
                        },
                        e.span(),
                    ),
                    RecordOp::Restrict(field) => {
                        Expr::RecordRestrict(RecordRestrict { source, field }, e.span())
                    }
                    RecordOp::Update(field, value) => Expr::RecordUpdate(
                        RecordUpdate {
                            source,
                            field,
                            value,
                        },
                        e.span(),
                    ),
                }
            })
            .boxed();

        let record_expr = nested_parser(record_op.or(instantiate), Delimiter::Brace, Expr::Error);

        let let_ = just(Token::Let)
            .ignore_then(ident)
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map_with(|((name, value), inside), e| {
                Expr::Let(
                    LetExpr {
                        name,
                        value: Box::new(value),
                        inside: Box::new(inside),
                    },
                    e.span(),
                )
            });

        let if_ = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .then_ignore(just(Token::Else))
            .then(expr.clone())
            .map_with(|((predicate, then), or), e| {
                Expr::If(
                    IfExpr {
                        predicate: Box::new(predicate),
                        then: Box::new(then),
                        or: Box::new(or),
                    },
                    e.span(),
                )
            })
            .boxed();

        let branches = pat_parser()
            .then_ignore(just(Token::DoubleArrow))
            .then(expr.clone())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .at_least(1)
            .collect();
        let case = just(Token::Case)
            .ignore_then(ident)
            .then_ignore(just(Token::Of))
            .then(branches)
            .map_with(|(source, branches), e| Expr::Case(CaseExpr { source, branches }, e.span()))
            .boxed();

        let fn_ = just(Token::Backslash)
            .ignore_then(ident)
            .then_ignore(just(Token::DoubleArrow))
            .then(expr.clone())
            .map_with(|(param, body), e| {
                Expr::Fn(
                    FnExpr {
                        param: EcoString::from(param),
                        body: Box::new(body),
                    },
                    e.span(),
                )
            })
            .boxed();

        let atom = choice((
            ident.map_with(|i, e| Expr::Ident(i, e.span())),
            nested_parser(expr.clone(), Delimiter::Paren, Expr::Error), // ( <expr> )
            literal,
            list,
            record_expr,
            let_,
            if_,
            case,
            fn_,
        ))
        .boxed();

        let select = atom
            .foldl_with(
                just(Token::Dot).ignore_then(ident).repeated(),
                |source, field, e| {
                    Expr::RecordSelect(
                        RecordSelect {
                            source: Box::new(source),
                            field,
                        },
                        e.span(),
                    )
                },
            )
            .boxed();

        let unary = just(Token::Op(Op::Sub))
            .to(UnaryOp::Neq)
            .or(just(Token::Op(Op::Not)).to(UnaryOp::Neq))
            .repeated()
            .foldr_with(select, |op, target, e| {
                Expr::Unary(
                    UnaryExpr {
                        op,
                        target: Box::new(target),
                    },
                    e.span(),
                )
            })
            .boxed();

        let op = choice((
            just(Token::Op(Op::Mul)).to(BinaryOp::Mul),
            just(Token::Op(Op::Div)).to(BinaryOp::Div),
            just(Token::Op(Op::Rem)).to(BinaryOp::Rem),
        ));
        let product = unary
            .clone()
            .foldl_with(op.then(unary).repeated(), |left, (op, right), e| {
                Expr::Binary(
                    BinaryExpr {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    e.span(),
                )
            })
            .boxed();

        let op = just(Token::Op(Op::Add))
            .to(BinaryOp::Add)
            .or(just(Token::Op(Op::Sub)).to(BinaryOp::Sub));
        let sum = product
            .clone()
            .foldl_with(op.then(product).repeated(), |left, (op, right), e| {
                Expr::Binary(
                    BinaryExpr {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    e.span(),
                )
            })
            .boxed();

        let op = choice((
            just(Token::Op(Op::Less)).to(BinaryOp::Less),
            just(Token::Op(Op::LessEq)).to(BinaryOp::LessEq),
            just(Token::Op(Op::Greater)).to(BinaryOp::Greater),
            just(Token::Op(Op::GreaterEq)).to(BinaryOp::GreaterEq),
            just(Token::Op(Op::Eq)).to(BinaryOp::Eq),
            just(Token::Op(Op::NotEq)).to(BinaryOp::NotEq),
        ));
        let comparison = sum
            .clone()
            .foldl_with(op.then(sum).repeated(), |left, (op, right), e| {
                Expr::Binary(
                    BinaryExpr {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    e.span(),
                )
            })
            .boxed();

        let op = choice((
            just(Token::Op(Op::And)).to(BinaryOp::And),
            just(Token::Op(Op::Or)).to(BinaryOp::Or),
            just(Token::Op(Op::Xor)).to(BinaryOp::Xor),
        ));
        let logical = comparison
            .clone()
            .foldl_with(op.then(comparison).repeated(), |left, (op, right), e| {
                Expr::Binary(
                    BinaryExpr {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    e.span(),
                )
            })
            .boxed();

        logical
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
        ast::{BinaryOp, CaseExpr, Expr, FnExpr, IfExpr, Literal, Pat, RecordExtend, RecordSelect},
        lexer::lexer,
    };

    use super::{expr_parser, pat_parser};

    #[test]
    fn pat() {
        let src = "{ a: x, b: { y }, c: _, d }";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let pat = pat_parser().parse(input).into_result().unwrap();

        let mut record = pat.into_record().unwrap().0.fields;

        let a = record.remove("a").unwrap().unwrap().into_ident().unwrap().0;
        assert_eq!(a, "x");

        let b = record
            .remove("b")
            .unwrap()
            .unwrap()
            .into_record()
            .unwrap()
            .0
            .fields;
        assert!(b.contains_key("y"));

        let c = record.remove("c").unwrap();
        assert_matches!(c.unwrap(), Pat::Wildcard(_));

        record.remove("d").unwrap();

        assert!(record.is_empty());
    }

    #[test]
    fn case_expr() {
        let src = "case x of 1 => true, _ => false";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        let CaseExpr {
            source,
            mut branches,
        } = expr.into_case().unwrap().0;

        assert_eq!(source, "x");

        let (wildcard, rhs) = branches.pop().unwrap();
        let rhs = rhs.into_literal().unwrap().0;
        assert_matches!(wildcard, Pat::Wildcard(_));
        assert_eq!(rhs, Literal::Bool(false));

        let (first, rhs) = branches.pop().unwrap();
        let rhs = rhs.into_literal().unwrap().0;
        assert_matches!(first, Pat::Literal(Literal::Num(1.0), _));
        assert_eq!(rhs, Literal::Bool(true));

        assert!(branches.is_empty());
    }

    #[test]
    fn fn_expr() {
        let src = "\\name => \"Hello\" + name";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        let FnExpr { param, body } = expr.into_fn().unwrap().0;

        assert_eq!(param, "name");

        let body = body.into_binary().unwrap().0;

        assert_eq!(body.op, BinaryOp::Add);
    }

    #[test]
    fn arithmetic_expr() {
        // ((-4 * 10) + (40 / 4)) + 30 = 0
        let src = "-4 * 10 + 40 / 4 + 30 == 0";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        // _ = 0
        let eq = expr.into_binary().unwrap().0;

        assert_eq!(eq.op, BinaryOp::Eq);

        // _ + 30
        let sum = eq.left.into_binary().unwrap().0;
        assert_eq!(sum.op, BinaryOp::Add);

        // (_) + (_)
        let sum = sum.left.into_binary().unwrap().0;
        assert_eq!(sum.op, BinaryOp::Add);

        // (-4 * 10)
        let mul = sum.left.into_binary().unwrap().0;
        assert_eq!(mul.op, BinaryOp::Mul);

        // (40 / 4)
        let div = sum.right.into_binary().unwrap().0;
        assert_eq!(div.op, BinaryOp::Div);
    }

    #[test]
    fn if_expr() {
        let src = "if y then { x = 10 }.x else 0";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        let IfExpr {
            predicate,
            then,
            or,
        } = expr.into_if().unwrap().0;

        assert_eq!(predicate.into_ident().unwrap().0, "y");

        let RecordSelect { source, field } = then.into_record_select().unwrap().0;

        assert_eq!(field, "x");

        let mut record = source.into_record().unwrap().0.fields;

        assert_eq!(
            record.remove("x").unwrap().into_literal().unwrap().0,
            Literal::Num(10.0)
        );

        assert_eq!(or.into_literal().unwrap().0, Literal::Num(0.0));
    }

    #[test]
    fn record_select() {
        let src = "x.y.z";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        let RecordSelect { source, field } = expr.into_record_select().unwrap().0;
        assert_eq!(field, "z");

        let RecordSelect { source, field } = source.into_record_select().unwrap().0;
        assert_eq!(field, "y");

        let ident = source.into_ident().unwrap().0;
        assert_eq!(ident, "x");
    }

    #[test]
    fn record_extension() {
        let src = "{ y | +x = 10 }";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        let RecordExtend {
            source,
            field,
            value,
        } = expr.into_record_extend().unwrap().0;

        let source = source.into_ident().unwrap().0;
        assert_eq!(source, "y");
        assert_eq!(field, "x");

        let value = value.into_literal().unwrap().0;
        assert_eq!(value, Literal::Num(10.0));
    }

    #[test]
    fn record() {
        let src = "{ x = 10, y = 20 }";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        let mut record = expr.into_record().unwrap().0.fields;

        assert_matches!(
            record.remove("x").unwrap(),
            Expr::Literal(Literal::Num(10.0), _)
        );
        assert_matches!(
            record.remove("y").unwrap(),
            Expr::Literal(Literal::Num(20.0), _)
        );

        assert!(record.is_empty())
    }
}
