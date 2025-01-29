use chumsky::{input::ValueInput, prelude::*};
use kola_tree::prelude::*;

use super::{Extra, ParserExt, State};
use crate::{
    Span, SyntaxPhase,
    token::{Delimiter, Op, Token},
};

/*
%token symbol bool str num char
%% /* LL(1) */
LiteralExpr := bool
    | str
    | num
    | char
ListExpr := '[' (Expr (',' Expr)*)? ']' // TODO trailing comma

Property := Symbol '=' Expr
Instantiate := '=' Expr (',' Property)* // TODO trailing comma
Extend := '+' Symbol = Expr
Restrict := '-' Symbol
Update := Symbol = Expr
RecordOp = '|' (Extend | Restrict | Update)+
RecordExpr = '{' Symbol (Instantiate | RecordOp) '}' // TODO empty record

LetExpr := 'let' Symbol '=' Expr 'in' Expr
IfExpr := 'if' Expr 'then' Expr 'else' Expr

PropertyPat := Symbol ':' Pat
Pat := '_'
    | '{' PropertyPat (',' PropertyPat)* '}' // TODO empty RecordPat
    | LiteralExpr
    | Symbol
Branch := Pat '=>' Expr
CaseExpr := 'case' Symbol 'of' Branch (',' Branch)*

FuncExpr := '\' Symbol '=>' Expr // TODO Symbol should be pattern

Callable := Symbol
    | FuncExpr
    | CallExpr
CallExpr := '(' Callable Expr ')'
*/

pub fn name_parser<'src, I>() -> impl Parser<'src, I, NodeId<node::Name>, Extra<'src>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    select! { Token::Symbol(s) => Symbol::from(s) }
        .map(node::Name)
        .to_node()
        .boxed()
}

pub fn ident_parser<'src, I>() -> impl Parser<'src, I, node::Ident, Extra<'src>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    select! { Token::Symbol(s) => Symbol::from(s) }.map(node::Ident)
}

pub fn literal_parser<'src, I>() -> impl Parser<'src, I, node::Literal, Extra<'src>> + Sized
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    select! {
        Token::Num(n) => node::Literal::Num(n),
        Token::Bool(b) => node::Literal::Bool(b),
        Token::Char(c) => node::Literal::Char(c),
        Token::Str(s) => node::Literal::Str(Symbol::from(s))
    }
}

pub fn pat_parser<'src, I>() -> impl Parser<'src, I, NodeId<node::Pat>, Extra<'src>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    recursive(|pat| {
        let ident = select! { Token::Symbol(s) => Symbol::from(s) }
            .map(node::IdentPat)
            .to_pat()
            .to_node();
        let wildcard = just(Token::Wildcard).to(node::Wildcard).to_pat().to_node();
        let literal = literal_parser().map(node::LiteralPat).to_pat().to_node();

        let property = name_parser()
            .then(just(Token::Colon).ignore_then(pat.clone()).or_not())
            .map(|(key, value)| node::PropertyPat { key, value })
            .to_node();

        let record = nested_parser(
            property
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .map(|fields| node::RecordPat { fields })
                .to_pat()
                .to_node(),
            Delimiter::Brace,
            |span| node::Pat::Error(node::PatError),
        );

        choice((ident, wildcard, literal, record)).boxed()
    })
    .boxed()
}

pub fn expr_parser<'src, I>() -> impl Parser<'src, I, NodeId<node::Expr>, Extra<'src>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
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
                .map(|values| node::List { values })
                .to_expr(),
            Delimiter::Bracket,
            |span| node::Expr::Error(node::ExprError),
        )
        .labelled("ListExpr")
        .as_context();

        // record operations

        let property = name
            .clone()
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map(|(key, value)| node::Property { key, value })
            .to_node();
        let instantiate = property
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .map(|fields| node::Record { fields })
            .to_expr()
            .boxed();

        enum RecordOp {
            Extend(NodeId<node::Name>, NodeId<node::Expr>),
            Restrict(NodeId<node::Name>),
            Update(NodeId<node::Name>, NodeId<node::Expr>),
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
            .foldl_with(inner_op, |source, op, e| {
                let span = e.span();
                let tree: &mut State = e.state();

                match op {
                    RecordOp::Extend(field, value) => tree.insert_expr(
                        node::RecordExtend {
                            source,
                            field,
                            value,
                        },
                        span,
                    ),
                    RecordOp::Restrict(field) => {
                        tree.insert_expr(node::RecordRestrict { source, field }, span)
                    }
                    RecordOp::Update(field, value) => tree.insert_expr(
                        node::RecordUpdate {
                            source,
                            field,
                            value,
                        },
                        span,
                    ),
                }
            })
            .boxed();

        let record_expr = nested_parser(record_op.or(instantiate), Delimiter::Brace, |span| {
            node::Expr::Error(node::ExprError)
        })
        .labelled("RecordExpr")
        .as_context();

        let let_ = just(Token::Let)
            .ignore_then(name.clone())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((name, value), inside)| node::Let {
                name,
                value,
                inside,
            })
            .to_expr()
            .labelled("LetExpr")
            .as_context()
            .boxed();

        let if_ = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .then_ignore(just(Token::Else))
            .then(expr.clone())
            .map(|((predicate, then), or)| node::If {
                predicate,
                then,
                or,
            })
            .to_expr()
            .labelled("IfExpr")
            .as_context()
            .boxed();

        let branch = pat_parser()
            .then_ignore(just(Token::DoubleArrow))
            .then(expr.clone())
            .map(|(pat, matches)| node::Branch { pat, matches })
            .to_node();
        let branches = branch
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .at_least(1)
            .collect();
        let case = just(Token::Case)
            .ignore_then(ident.clone().to_node())
            .then_ignore(just(Token::Of))
            .then(branches)
            .map(|(source, branches)| node::Case { source, branches })
            .to_expr()
            .labelled("CaseExpr")
            .as_context()
            .boxed();

        let func = just(Token::Backslash)
            .ignore_then(ident.clone().to_node())
            .then_ignore(just(Token::DoubleArrow))
            .then(expr.clone())
            .map(|(param, body)| node::Func { param, body })
            .to_expr()
            .labelled("FuncExpr")
            .as_context()
            .boxed();

        // TODO allow "recursive" (a (b c)) and maybe also syntactic sugar (a b c)
        let call = recursive(|call| {
            let callable = choice((
                ident.clone().to_expr().labelled("IdentExpr").as_context(),
                func.clone(),
                call,
            ));

            nested_parser(
                callable
                    .then(expr.clone())
                    .map(|(func, arg)| node::Call { func, arg })
                    .to_expr(),
                Delimiter::Paren,
                |span| node::Expr::Error(node::ExprError),
            )
            .boxed()
        })
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
                |source, field, e| {
                    let span = e.span();
                    let tree: &mut State = e.state();
                    tree.insert_expr(node::RecordSelect { source, field }, span)
                },
            )
            .boxed();

        let unary_op = just(Token::Op(Op::Sub))
            .to(node::UnaryOp::Neg)
            .or(just(Token::Op(Op::Not)).to(node::UnaryOp::Neg))
            .to_node();
        let unary = unary_op
            .repeated()
            .foldr_with(select, |op, target, e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_expr(node::Unary { op, target }, span)
            })
            .boxed();

        let op = choice((
            just(Token::Op(Op::Mul)).to(node::BinaryOp::Mul),
            just(Token::Op(Op::Div)).to(node::BinaryOp::Div),
            just(Token::Op(Op::Rem)).to(node::BinaryOp::Rem),
        ))
        .to_node();
        let product = unary
            .clone()
            .foldl_with(op.then(unary).repeated(), |left, (op, right), e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_expr(node::Binary { op, left, right }, span)
            })
            .boxed();

        let op = just(Token::Op(Op::Add))
            .to(node::BinaryOp::Add)
            .or(just(Token::Op(Op::Sub)).to(node::BinaryOp::Sub))
            .to_node();
        let sum = product
            .clone()
            .foldl_with(op.then(product).repeated(), |left, (op, right), e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_expr(node::Binary { op, left, right }, span)
            })
            .boxed();

        let op = choice((
            just(Token::Op(Op::Less)).to(node::BinaryOp::Less),
            just(Token::Op(Op::LessEq)).to(node::BinaryOp::LessEq),
            just(Token::Op(Op::Greater)).to(node::BinaryOp::Greater),
            just(Token::Op(Op::GreaterEq)).to(node::BinaryOp::GreaterEq),
            just(Token::Op(Op::Eq)).to(node::BinaryOp::Eq),
            just(Token::Op(Op::NotEq)).to(node::BinaryOp::NotEq),
        ))
        .to_node();
        let comparison = sum
            .clone()
            .foldl_with(op.then(sum).repeated(), |left, (op, right), e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_expr(node::Binary { op, left, right }, span)
            })
            .boxed();

        let op = choice((
            just(Token::Op(Op::And)).to(node::BinaryOp::And),
            just(Token::Op(Op::Or)).to(node::BinaryOp::Or),
            just(Token::Op(Op::Xor)).to(node::BinaryOp::Xor),
        ))
        .to_node();
        let logical = comparison
            .clone()
            .foldl_with(op.then(comparison).repeated(), |left, (op, right), e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_expr(node::Binary { op, left, right }, span)
            })
            .boxed();

        logical
    })
    .boxed()
}

pub fn nested_parser<'src, I, T>(
    parser: impl Parser<'src, I, NodeId<T>, Extra<'src>> + 'src,
    delim: Delimiter,
    fallback: impl Fn(Span) -> T + Clone + 'src,
) -> impl Parser<'src, I, NodeId<T>, Extra<'src>> + Clone
where
    T: InnerNode + Attached<SyntaxPhase, Meta = Span> + 'src,
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    parser
        .delimited_by(just(Token::Open(delim)), just(Token::Close(delim)))
        .recover_with(via_parser(
            nested_delimiters(
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
            )
            .to_node(),
        ))
        .boxed()
}

#[cfg(test)]
mod tests {
    use chumsky::prelude::*;

    use kola_tree::prelude::*;

    use super::{expr_parser, pat_parser};
    use crate::{lexer::lexer, parser::try_parse_with};

    #[test]
    fn pat() {
        let src = "{ a: x, b: { y }, c: _, d }";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (pat, tree, _spans) = try_parse_with(input, pat_parser()).unwrap();

        let record = pat.get(&tree).as_record().unwrap();

        let a = record.get("a", &tree).unwrap();
        assert!(a.value.is_some());
        assert_eq!(a.value(&tree).unwrap().as_ident().unwrap(), "x");

        let b = record.get("b", &tree).unwrap();
        assert!(b.value.is_some());
        let b = b.value(&tree).unwrap().as_record().unwrap();
        assert!(b.get("y", &tree).unwrap().value.is_none());

        let c = record.get("c", &tree).unwrap();
        assert!(c.value(&tree).unwrap().is_wildcard());

        let d = record.get("d", &tree).unwrap();
        assert_eq!(d.value(&tree), None);
    }

    #[test]
    fn case_expr() {
        let src = "case x of 1 => true, _ => false";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (expr, tree, _spans) = try_parse_with(input, expr_parser()).unwrap();

        let node::Case { source, branches } = expr.get(&tree).as_case().unwrap().get(&tree);

        assert_eq!(source.get(&tree), "x");

        let mut branches = branches.iter().rev();

        let branch = branches.next().unwrap().get(&tree);
        assert!(branch.pat(&tree).is_wildcard());
        let matches = branch.matches(&tree).as_literal().unwrap().get(&tree);
        assert_eq!(matches, &node::Literal::Bool(false));

        let branch = branches.next().unwrap().get(&tree);
        let pat = &branch.pat(&tree).as_literal().unwrap().0;
        assert_eq!(pat, &node::Literal::Num(1.0));
        let matches = branch.matches(&tree).as_literal().unwrap().get(&tree);
        assert_eq!(matches, &node::Literal::Bool(true));

        assert_eq!(branches.len(), 0);
    }

    #[test]
    fn func_expr() {
        let src = "\\name => \"Hello\" + name";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (expr, tree, _spans) = try_parse_with(input, expr_parser()).unwrap();

        let node::Func { param, body } = expr.get(&tree).as_func().unwrap().get(&tree);

        assert_eq!(param.get(&tree), "name");

        let body = body.get(&tree).as_binary().unwrap().get(&tree);
        assert_eq!(body.op(&tree), node::BinaryOp::Add);
    }

    #[test]
    fn arithmetic_expr() {
        // ((-4 * 10) + (40 / 4)) + 30 = 0
        let src = "-4 * 10 + 40 / 4 + 30 == 0";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (expr, tree, _spans) = try_parse_with(input, expr_parser()).unwrap();

        // _ = 0
        let eq = expr.get(&tree).as_binary().unwrap().get(&tree);

        assert_eq!(eq.op(&tree), node::BinaryOp::Eq);

        // _ + 30
        let sum = eq.left(&tree).as_binary().unwrap().get(&tree);
        assert_eq!(sum.op(&tree), node::BinaryOp::Add);

        // (_) + (_)
        let sum = sum.left(&tree).as_binary().unwrap().get(&tree);
        assert_eq!(sum.op(&tree), node::BinaryOp::Add);

        // (-4 * 10)
        let mul = sum.left(&tree).as_binary().unwrap().get(&tree);
        assert_eq!(mul.op(&tree), node::BinaryOp::Mul);

        // (40 / 4)
        let div = sum.right(&tree).as_binary().unwrap().get(&tree);
        assert_eq!(div.op(&tree), node::BinaryOp::Div);
    }

    #[test]
    fn if_expr() {
        let src = "if y then { x = 10 }.x else 0";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (expr, tree, _spans) = try_parse_with(input, expr_parser()).unwrap();

        let node::If {
            predicate,
            then,
            or,
        } = expr.get(&tree).as_if().unwrap().get(&tree);

        assert_eq!(predicate.get(&tree).as_ident().unwrap().get(&tree), "y");

        let node::RecordSelect { source, field } =
            then.get(&tree).as_record_select().unwrap().get(&tree);

        assert_eq!(field.get(&tree), "x");

        let record = source.get(&tree).as_record().unwrap().get(&tree);
        let x = record.get("x", &tree).unwrap();
        assert_eq!(
            x.value(&tree).as_literal().unwrap().get(&tree),
            &node::Literal::Num(10.0)
        );

        assert_eq!(
            or.get(&tree).as_literal().unwrap().get(&tree),
            &node::Literal::Num(0.0)
        );
    }

    #[test]
    fn record_select() {
        let src = "x.y.z";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (expr, tree, _spans) = try_parse_with(input, expr_parser()).unwrap();

        let node::RecordSelect { source, field } =
            expr.get(&tree).as_record_select().unwrap().get(&tree);
        assert_eq!(field.get(&tree), "z");

        let node::RecordSelect { source, field } =
            source.get(&tree).as_record_select().unwrap().get(&tree);
        assert_eq!(field.get(&tree), "y");

        let ident = source.get(&tree).as_ident().unwrap().get(&tree);
        assert_eq!(ident, "x");
    }

    #[test]
    fn record_extension() {
        let src = "{ y | +x = 10 }";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (expr, tree, _spans) = try_parse_with(input, expr_parser()).unwrap();

        let node::RecordExtend {
            source,
            field,
            value,
        } = expr.get(&tree).as_record_extend().unwrap().get(&tree);
        assert_eq!(field.get(&tree), "x");

        let source = source.get(&tree).as_ident().unwrap().get(&tree);
        assert_eq!(source, "y");

        let value = value.get(&tree).as_literal().unwrap().get(&tree);
        assert_eq!(value, &node::Literal::Num(10.0));
    }

    #[test]
    fn record() {
        let src = "{ x = 10, y = 20 }";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (expr, tree, _spans) = try_parse_with(input, expr_parser()).unwrap();

        let record = expr.get(&tree).as_record().unwrap().get(&tree);

        let x = record.get("x", &tree).unwrap();
        assert_eq!(
            x.value(&tree).as_literal().unwrap().get(&tree),
            &node::Literal::Num(10.0)
        );

        let y = record.get("y", &tree).unwrap();
        assert_eq!(
            y.value(&tree).as_literal().unwrap().get(&tree),
            &node::Literal::Num(20.0)
        );
    }
}
