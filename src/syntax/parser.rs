use chumsky::prelude::*;
use ecow::EcoString;

use crate::syntax::{
    ast::{
        CallExpr, CaseExpr, Ident, LetExpr, List, RecordExtend, RecordRestrict, RecordSelect,
        RecordUpdate,
    },
    node::SyntaxNode,
};

use super::{
    ast::{
        BinaryExpr, BinaryOp, Expr, FnExpr, IfExpr, Literal, Pat, Record, RecordPat, UnaryExpr,
        UnaryOp,
    },
    node::NodeParser,
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
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Literal, Extra<'tokens, 'src>> + Sized {
    select! {
        Token::Num(n) => Literal::Num(n),
        Token::Bool(b) => Literal::Bool(b),
        Token::Char(c) => Literal::Char(c),
        Token::Str(s) => Literal::Str(EcoString::from(s))
    }
}

pub fn pat_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Pat<Span>, Extra<'tokens, 'src>> + Clone {
    recursive(|pat| {
        let ident = select! { Token::Ident(id) => EcoString::from(id) };

        let wildcard = just(Token::Wildcard).map_with(|_, e| Pat::Wildcard(e.span()));
        let literal = literal_parser().to_pat();

        let record = nested_parser(
            ident
                .then(just(Token::Colon).ignore_then(pat.clone()).or_not())
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .map(|fields| RecordPat { fields })
                .to_pat(),
            Delimiter::Brace,
            Pat::Error,
        );

        choice((ident.to_pat(), wildcard, literal, record)).boxed()
    })
    .boxed()
}

pub fn expr_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expr<Span>, Extra<'tokens, 'src>> + Clone {
    recursive(|expr| {
        let ident = select! { Token::Ident(id) => EcoString::from(id) };

        let literal = literal_parser().to_expr();

        let list = nested_parser(
            expr.clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .map(|values| List { values })
                .to_expr(),
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
            .map(|fields| Record { fields })
            .to_expr()
            .boxed();

        enum RecordOp {
            Extend(Ident, Box<Expr<Span>>),
            Restrict(Ident),
            Update(Ident, Box<Expr<Span>>),
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
            .to_expr()
            .foldl_with(inner_op, |source, op, e| {
                let source = Box::new(source);
                match op {
                    RecordOp::Extend(field, value) => SyntaxNode::new(
                        RecordExtend {
                            source,
                            field,
                            value,
                        },
                        e.span(),
                    )
                    .into(),
                    RecordOp::Restrict(field) => {
                        SyntaxNode::new(RecordRestrict { source, field }, e.span()).into()
                    }
                    RecordOp::Update(field, value) => SyntaxNode::new(
                        RecordUpdate {
                            source,
                            field,
                            value,
                        },
                        e.span(),
                    )
                    .into(),
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
            .map(|((name, value), inside)| LetExpr {
                name,
                value: Box::new(value),
                inside: Box::new(inside),
            })
            .to_expr();

        let if_ = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .then_ignore(just(Token::Else))
            .then(expr.clone())
            .map(|((predicate, then), or)| IfExpr {
                predicate: Box::new(predicate),
                then: Box::new(then),
                or: Box::new(or),
            })
            .to_expr()
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
            .map(|(source, branches)| CaseExpr { source, branches })
            .to_expr()
            .boxed();

        let fn_ = just(Token::Backslash)
            .ignore_then(ident)
            .then_ignore(just(Token::DoubleArrow))
            .then(expr.clone())
            .map_with(|(param, body), e| FnExpr {
                param: EcoString::from(param),
                body: Box::new(body),
            })
            .to_expr()
            .boxed();

        let call = nested_parser(
            ident
                .to_node()
                .then(expr.clone())
                .map(|(func, arg)| CallExpr {
                    func,
                    arg: Box::new(arg),
                })
                .to_expr(),
            Delimiter::Paren,
            Expr::Error,
        )
        .boxed();

        let atom = choice((
            ident.to_expr(),
            literal,
            list,
            record_expr,
            let_,
            if_,
            case,
            fn_,
            call,
        ))
        .boxed();

        let select = atom
            .foldl_with(
                just(Token::Dot).ignore_then(ident).repeated(),
                |source, field, e| {
                    SyntaxNode::new(
                        RecordSelect {
                            source: Box::new(source),
                            field,
                        },
                        e.span(),
                    )
                    .into()
                },
            )
            .boxed();

        let unary = just(Token::Op(Op::Sub))
            .to(UnaryOp::Neg)
            .or(just(Token::Op(Op::Not)).to(UnaryOp::Neg))
            .repeated()
            .foldr_with(select, |op, target, e| {
                SyntaxNode::new(
                    UnaryExpr {
                        op,
                        target: Box::new(target),
                    },
                    e.span(),
                )
                .into()
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
                SyntaxNode::new(
                    BinaryExpr {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    e.span(),
                )
                .into()
            })
            .boxed();

        let op = just(Token::Op(Op::Add))
            .to(BinaryOp::Add)
            .or(just(Token::Op(Op::Sub)).to(BinaryOp::Sub));
        let sum = product
            .clone()
            .foldl_with(op.then(product).repeated(), |left, (op, right), e| {
                SyntaxNode::new(
                    BinaryExpr {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    e.span(),
                )
                .into()
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
                SyntaxNode::new(
                    BinaryExpr {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    e.span(),
                )
                .into()
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
                SyntaxNode::new(
                    BinaryExpr {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    e.span(),
                )
                .into()
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
        node::SyntaxNode,
    };

    use super::{expr_parser, pat_parser};

    #[test]
    fn pat() {
        let src = "{ a: x, b: { y }, c: _, d }";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let pat = pat_parser().parse(input).into_result().unwrap();

        let mut record = pat.into_record().unwrap().inner.fields;

        let a = record
            .remove("a")
            .unwrap()
            .unwrap()
            .into_ident()
            .unwrap()
            .inner;
        assert_eq!(a, "x");

        let b = record
            .remove("b")
            .unwrap()
            .unwrap()
            .into_record()
            .unwrap()
            .inner
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
        } = expr.into_case().unwrap().inner;

        assert_eq!(source, "x");

        let (wildcard, rhs) = branches.pop().unwrap();
        let rhs = rhs.into_literal().unwrap().inner;
        assert_matches!(wildcard, Pat::Wildcard(_));
        assert_eq!(rhs, Literal::Bool(false));

        let (first, rhs) = branches.pop().unwrap();
        let rhs = rhs.into_literal().unwrap().inner;
        assert_matches!(
            first,
            Pat::Literal(SyntaxNode {
                inner: Literal::Num(1.0),
                meta: _
            })
        );
        assert_eq!(rhs, Literal::Bool(true));

        assert!(branches.is_empty());
    }

    #[test]
    fn fn_expr() {
        let src = "\\name => \"Hello\" + name";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        let FnExpr { param, body } = expr.into_fn().unwrap().inner;

        assert_eq!(param, "name");

        let body = body.into_binary().unwrap().inner;

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
        let eq = expr.into_binary().unwrap().inner;

        assert_eq!(eq.op, BinaryOp::Eq);

        // _ + 30
        let sum = eq.left.into_binary().unwrap().inner;
        assert_eq!(sum.op, BinaryOp::Add);

        // (_) + (_)
        let sum = sum.left.into_binary().unwrap().inner;
        assert_eq!(sum.op, BinaryOp::Add);

        // (-4 * 10)
        let mul = sum.left.into_binary().unwrap().inner;
        assert_eq!(mul.op, BinaryOp::Mul);

        // (40 / 4)
        let div = sum.right.into_binary().unwrap().inner;
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
        } = expr.into_if().unwrap().inner;

        assert_eq!(predicate.into_ident().unwrap().inner, "y");

        let RecordSelect { source, field } = then.into_record_select().unwrap().inner;

        assert_eq!(field, "x");

        let mut record = source.into_record().unwrap().inner.fields;

        assert_eq!(
            record.remove("x").unwrap().into_literal().unwrap().inner,
            Literal::Num(10.0)
        );

        assert_eq!(or.into_literal().unwrap().inner, Literal::Num(0.0));
    }

    #[test]
    fn record_select() {
        let src = "x.y.z";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        let RecordSelect { source, field } = expr.into_record_select().unwrap().inner;
        assert_eq!(field, "z");

        let RecordSelect { source, field } = source.into_record_select().unwrap().inner;
        assert_eq!(field, "y");

        let ident = source.into_ident().unwrap().inner;
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
        } = expr.into_record_extend().unwrap().inner;

        let source = source.into_ident().unwrap().inner;
        assert_eq!(source, "y");
        assert_eq!(field, "x");

        let value = value.into_literal().unwrap().inner;
        assert_eq!(value, Literal::Num(10.0));
    }

    #[test]
    fn record() {
        let src = "{ x = 10, y = 20 }";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        let mut record = expr.into_record().unwrap().inner.fields;

        assert_eq!(
            record
                .remove("x")
                .unwrap()
                .into_literal()
                .map(SyntaxNode::into_inner),
            Some(Literal::Num(10.0))
        );
        assert_eq!(
            record
                .remove("y")
                .unwrap()
                .into_literal()
                .map(SyntaxNode::into_inner),
            Some(Literal::Num(20.0))
        );

        assert!(record.is_empty())
    }
}
