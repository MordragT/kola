use chumsky::prelude::*;

use crate::semantic::types::MonoType;

use super::{
    ast,
    node::Node,
    token::{Delimiter, Op, Token},
    Span,
};

pub trait ParserExt<'tokens, 'src: 'tokens, T>:
    Parser<'tokens, ParserInput<'tokens, 'src>, T, Extra<'tokens, 'src>> + Sized
{
    #[inline]
    fn to_node(
        self,
    ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Node<T>, Extra<'tokens, 'src>> {
        self.map_with(|inner, e| Node::new(inner, e.span()))
    }

    #[inline]
    fn to_expr(
        self,
    ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, ast::Expr, Extra<'tokens, 'src>>
    where
        T: Into<ast::Expr>,
    {
        self.map(Into::into)
    }

    #[inline]
    fn to_pat(
        self,
    ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, ast::Pat, Extra<'tokens, 'src>>
    where
        T: Into<ast::Pat>,
    {
        self.map(Into::into)
    }

    #[inline]
    fn to_expr_node(
        self,
    ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, ast::Expr, Extra<'tokens, 'src>>
    where
        Node<T>: Into<ast::Expr>,
    {
        self.to_node().map(Into::into)
    }

    #[inline]
    fn to_pat_node(
        self,
    ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, ast::Pat, Extra<'tokens, 'src>>
    where
        Node<T>: Into<ast::Pat>,
    {
        self.to_node().map(Into::into)
    }
}

impl<
        'tokens,
        'src: 'tokens,
        T,
        P: Parser<'tokens, ParserInput<'tokens, 'src>, T, Extra<'tokens, 'src>> + Sized,
    > ParserExt<'tokens, 'src, T> for P
{
}

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

pub fn name_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, ast::Name, Extra<'tokens, 'src>> + Clone {
    select! { Token::Symbol(s) => ast::Symbol::from(s) }.map_with(|name, e| ast::Name {
        name,
        span: e.span(),
    })
}

pub fn ident_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, ast::IdentExpr, Extra<'tokens, 'src>> + Clone
{
    select! { Token::Symbol(s) => ast::Symbol::from(s) }
        .to_node()
        .boxed()
}

pub fn literal_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, ast::LiteralExpr, Extra<'tokens, 'src>> + Sized
{
    select! {
        Token::Num(n) => ast::Literal::Num(n),
        Token::Bool(b) => ast::Literal::Bool(b),
        Token::Char(c) => ast::Literal::Char(c),
        Token::Str(s) => ast::Literal::Str(ast::Symbol::from(s))
    }
    .to_node()
}

pub fn pat_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, ast::Pat, Extra<'tokens, 'src>> + Clone {
    recursive(|pat| {
        let wildcard = just(Token::Wildcard)
            .map_with(|_, e| ast::Wildcard {
                span: e.span(),
                ty: MonoType::variable(),
            })
            .to_pat();

        let property = name_parser()
            .then(just(Token::Colon).ignore_then(pat.clone()).or_not())
            .map_with(|(key, value), e| ast::PropertyPat {
                key,
                value,
                span: e.span(),
            });
        let record = nested_parser(
            property
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .map(|fields| ast::RecordPatRepr { fields })
                .to_pat_node(),
            Delimiter::Brace,
            |span| ast::Pat::Error(ast::PatError { span }),
        );

        choice((
            ident_parser().to_pat(),
            wildcard,
            literal_parser().to_pat(),
            record,
        ))
        .boxed()
    })
    .boxed()
}

pub fn expr_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, ast::Expr, Extra<'tokens, 'src>> + Clone {
    recursive(|expr| {
        let name = name_parser();
        let ident = ident_parser();
        let literal = literal_parser()
            .to_expr()
            .labelled("LiteralExpr")
            .as_context();

        let list = nested_parser(
            expr.clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .map(|values| ast::List { values })
                .to_expr_node(),
            Delimiter::Bracket,
            |span| ast::Expr::Error(ast::ExprError { span }),
        )
        .labelled("ListExpr")
        .as_context();

        // record operations

        let property = name
            .clone()
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map_with(|(key, value), e| ast::Property {
                key,
                value,
                span: e.span(),
            });
        let instantiate = property
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .map(|fields| ast::Record { fields })
            .to_expr_node()
            .boxed();

        enum RecordOp {
            Extend(ast::Name, ast::Expr),
            Restrict(ast::Name),
            Update(ast::Name, ast::Expr),
        }

        let extend = just(Token::Op(Op::Add))
            .ignore_then(name.clone())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map(|(field, value)| RecordOp::Extend(field, value))
            .boxed();

        let restrict = just(Token::Op(Op::Sub))
            .ignore_then(name.clone())
            .map(RecordOp::Restrict)
            .boxed();

        let update = name
            .clone()
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map(|(field, value)| RecordOp::Update(field, value))
            .boxed();

        let inner_op = just(Token::Pipe)
            .ignore_then(choice((extend, restrict, update)))
            .repeated()
            .at_least(1);
        let record_op = ident
            .clone()
            .to_expr()
            .foldl_with(inner_op, |source, op, e| match op {
                RecordOp::Extend(field, value) => Node::new(
                    ast::RecordExtend {
                        source,
                        field,
                        value,
                    },
                    e.span(),
                )
                .into(),
                RecordOp::Restrict(field) => {
                    Node::new(ast::RecordRestrict { source, field }, e.span()).into()
                }
                RecordOp::Update(field, value) => Node::new(
                    ast::RecordUpdate {
                        source,
                        field,
                        value,
                    },
                    e.span(),
                )
                .into(),
            })
            .boxed();

        let record_expr = nested_parser(record_op.or(instantiate), Delimiter::Brace, |span| {
            ast::Expr::Error(ast::ExprError { span })
        })
        .labelled("RecordExpr")
        .as_context();

        let let_ = just(Token::Let)
            .ignore_then(name.clone())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((name, value), inside)| ast::Let {
                name,
                value,
                inside,
            })
            .to_expr_node()
            .labelled("LetExpr")
            .as_context()
            .boxed();

        let if_ = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .then_ignore(just(Token::Else))
            .then(expr.clone())
            .map(|((predicate, then), or)| ast::If {
                predicate,
                then,
                or,
            })
            .to_expr_node()
            .labelled("IfExpr")
            .as_context()
            .boxed();

        let branch = pat_parser()
            .then_ignore(just(Token::DoubleArrow))
            .then(expr.clone())
            .map_with(|(pat, matches), e| ast::Branch {
                pat,
                matches,
                span: e.span(),
            });
        let branches = branch
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .at_least(1)
            .collect();
        let case = just(Token::Case)
            .ignore_then(ident.clone())
            .then_ignore(just(Token::Of))
            .then(branches)
            .map(|(source, branches)| ast::Case { source, branches })
            .to_expr_node()
            .labelled("CaseExpr")
            .as_context()
            .boxed();

        let func = just(Token::Backslash)
            .ignore_then(ident.clone())
            .then_ignore(just(Token::DoubleArrow))
            .then(expr.clone())
            .map(|(param, body)| ast::Func { param, body })
            .to_expr_node()
            .labelled("FuncExpr")
            .as_context()
            .boxed();

        let call = nested_parser(
            ident
                .clone()
                .then(expr.clone())
                .map(|(func, arg)| ast::Call { func, arg })
                .to_expr_node(),
            Delimiter::Paren,
            |span| ast::Expr::Error(ast::ExprError { span }),
        )
        .labelled("CallExpr")
        .as_context()
        .boxed();

        let atom = choice((
            ident.clone().to_expr().labelled("IdentExpr").as_context(),
            literal,
            list,
            record_expr,
            let_,
            if_,
            case,
            func,
            call,
        ))
        .boxed();

        let select = atom
            .foldl_with(
                just(Token::Dot).ignore_then(name.clone()).repeated(),
                |source, field, e| Node::new(ast::RecordSelect { source, field }, e.span()).into(),
            )
            .boxed();

        let unary_op = just(Token::Op(Op::Sub))
            .to(ast::UnaryOpKind::Neg)
            .or(just(Token::Op(Op::Not)).to(ast::UnaryOpKind::Neg))
            .to_node();
        let unary = unary_op
            .repeated()
            .foldr_with(select, |op, target, e| {
                Node::new(ast::Unary { op, target }, e.span()).into()
            })
            .boxed();

        let op = choice((
            just(Token::Op(Op::Mul)).to(ast::BinaryOpKind::Mul),
            just(Token::Op(Op::Div)).to(ast::BinaryOpKind::Div),
            just(Token::Op(Op::Rem)).to(ast::BinaryOpKind::Rem),
        ))
        .to_node();
        let product = unary
            .clone()
            .foldl_with(op.then(unary).repeated(), |left, (op, right), e| {
                Node::new(ast::Binary { op, left, right }, e.span()).into()
            })
            .boxed();

        let op = just(Token::Op(Op::Add))
            .to(ast::BinaryOpKind::Add)
            .or(just(Token::Op(Op::Sub)).to(ast::BinaryOpKind::Sub))
            .to_node();
        let sum = product
            .clone()
            .foldl_with(op.then(product).repeated(), |left, (op, right), e| {
                Node::new(ast::Binary { op, left, right }, e.span()).into()
            })
            .boxed();

        let op = choice((
            just(Token::Op(Op::Less)).to(ast::BinaryOpKind::Less),
            just(Token::Op(Op::LessEq)).to(ast::BinaryOpKind::LessEq),
            just(Token::Op(Op::Greater)).to(ast::BinaryOpKind::Greater),
            just(Token::Op(Op::GreaterEq)).to(ast::BinaryOpKind::GreaterEq),
            just(Token::Op(Op::Eq)).to(ast::BinaryOpKind::Eq),
            just(Token::Op(Op::NotEq)).to(ast::BinaryOpKind::NotEq),
        ))
        .to_node();
        let comparison = sum
            .clone()
            .foldl_with(op.then(sum).repeated(), |left, (op, right), e| {
                Node::new(ast::Binary { op, left, right }, e.span()).into()
            })
            .boxed();

        let op = choice((
            just(Token::Op(Op::And)).to(ast::BinaryOpKind::And),
            just(Token::Op(Op::Or)).to(ast::BinaryOpKind::Or),
            just(Token::Op(Op::Xor)).to(ast::BinaryOpKind::Xor),
        ))
        .to_node();
        let logical = comparison
            .clone()
            .foldl_with(op.then(comparison).repeated(), |left, (op, right), e| {
                Node::new(ast::Binary { op, left, right }, e.span()).into()
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
    use chumsky::{input::Input, Parser};

    use crate::syntax::{ast, lexer::lexer};

    use super::{expr_parser, pat_parser};

    #[test]
    fn pat() {
        let src = "{ a: x, b: { y }, c: _, d }";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let pat = pat_parser().parse(input).into_result().unwrap();

        let record = pat.into_record().unwrap();

        let a = record.get("a").unwrap();
        assert!(a.value().is_some());
        assert_eq!(a.value().unwrap().as_ident().unwrap().inner(), "x");

        let b = record.get("b").unwrap();
        assert!(b.value().is_some());
        let b = b.value().unwrap().as_record().unwrap();
        assert!(b.get("y").unwrap().value.is_none());

        let c = record.get("c").unwrap();
        assert!(c.value().unwrap().is_wildcard());

        let d = record.get("d").unwrap();
        assert_eq!(d.value(), None);
    }

    #[test]
    fn case_expr() {
        let src = "case x of 1 => true, _ => false";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        let ast::Case {
            source,
            mut branches,
        } = expr.into_case().unwrap().into_inner();

        assert_eq!(source.inner(), "x");

        let branch = branches.pop().unwrap();
        assert!(branch.pat.is_wildcard());
        let matches = branch.matches.into_literal().unwrap().into_inner();
        assert_eq!(matches, ast::Literal::Bool(false));

        let branch = branches.pop().unwrap();
        let pat = branch.pat.into_literal().unwrap().into_inner();
        assert_eq!(pat, ast::Literal::Num(1.0));
        let matches = branch.matches.into_literal().unwrap().into_inner();
        assert_eq!(matches, ast::Literal::Bool(true));

        assert!(branches.is_empty());
    }

    #[test]
    fn func_expr() {
        let src = "\\name => \"Hello\" + name";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        let ast::Func { param, body } = expr.into_func().unwrap().into_inner();

        assert_eq!(param.inner(), "name");

        let body = body.into_binary().unwrap();
        assert_eq!(body.kind(), ast::BinaryOpKind::Add);
    }

    #[test]
    fn arithmetic_expr() {
        // ((-4 * 10) + (40 / 4)) + 30 = 0
        let src = "-4 * 10 + 40 / 4 + 30 == 0";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        // _ = 0
        let eq = expr.into_binary().unwrap().into_inner();

        assert_eq!(eq.kind(), ast::BinaryOpKind::Eq);

        // _ + 30
        let sum = eq.left.into_binary().unwrap().into_inner();
        assert_eq!(sum.kind(), ast::BinaryOpKind::Add);

        // (_) + (_)
        let sum = sum.left.into_binary().unwrap().into_inner();
        assert_eq!(sum.kind(), ast::BinaryOpKind::Add);

        // (-4 * 10)
        let mul = sum.left.into_binary().unwrap().into_inner();
        assert_eq!(mul.kind(), ast::BinaryOpKind::Mul);

        // (40 / 4)
        let div = sum.right.into_binary().unwrap().into_inner();
        assert_eq!(div.kind(), ast::BinaryOpKind::Div);
    }

    #[test]
    fn if_expr() {
        let src = "if y then { x = 10 }.x else 0";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        let ast::If {
            predicate,
            then,
            or,
        } = expr.into_if().unwrap().into_inner();

        assert_eq!(predicate.into_ident().unwrap().inner(), "y");

        let ast::RecordSelect { source, field } = then.into_record_select().unwrap().into_inner();

        assert_eq!(field.name, "x");

        let record = source.into_record().unwrap();
        let x = record.get("x").unwrap();
        assert_eq!(
            x.value().as_literal().unwrap().inner(),
            &ast::Literal::Num(10.0)
        );

        assert_eq!(
            or.into_literal().unwrap().into_inner(),
            ast::Literal::Num(0.0)
        );
    }

    #[test]
    fn record_select() {
        let src = "x.y.z";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        let ast::RecordSelect { source, field } = expr.into_record_select().unwrap().into_inner();
        assert_eq!(field.name, "z");

        let ast::RecordSelect { source, field } = source.into_record_select().unwrap().into_inner();
        assert_eq!(field.name, "y");

        let ident = source.into_ident().unwrap();
        assert_eq!(ident.inner(), "x");
    }

    #[test]
    fn record_extension() {
        let src = "{ y | +x = 10 }";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        let ast::RecordExtend {
            source,
            field,
            value,
        } = expr.into_record_extend().unwrap().into_inner();
        assert_eq!(field.name, "x");

        let source = source.into_ident().unwrap();
        assert_eq!(source.inner(), "y");

        let value = value.into_literal().unwrap().into_inner();
        assert_eq!(value, ast::Literal::Num(10.0));
    }

    #[test]
    fn record() {
        let src = "{ x = 10, y = 20 }";
        let tokens = lexer().parse(src).into_result().unwrap();

        let input = tokens.as_slice().spanned((src.len()..src.len()).into());
        let expr = expr_parser().parse(input).into_result().unwrap();

        let record = expr.into_record().unwrap();

        let x = record.get("x").unwrap();
        assert_eq!(
            x.value().as_literal().unwrap().inner(),
            &ast::Literal::Num(10.0)
        );

        let y = record.get("y").unwrap();
        assert_eq!(
            y.value().as_literal().unwrap().inner(),
            &ast::Literal::Num(20.0)
        );
    }
}
