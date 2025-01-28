use chumsky::{input::ValueInput, prelude::*};
use kola_tree::{self as tree, Attached, InnerNode, Meta, Node, NodeId, TreeBuilder};

use super::{
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

pub struct StateRepr {
    pub builder: TreeBuilder,
    pub meta: Vec<Meta<SyntaxPhase>>,
}

impl StateRepr {
    pub fn new() -> Self {
        Self {
            builder: TreeBuilder::new(),
            meta: Vec::new(),
        }
    }

    pub fn insert<T>(&mut self, node: T, meta: Span) -> NodeId<T>
    where
        T: Into<Node> + Attached<SyntaxPhase, Meta = Span>,
    {
        let id = self.builder.insert::<SyntaxPhase, _>(node);
        self.meta.push(T::into_meta(meta));

        id
    }

    pub fn insert_expr<T>(&mut self, node: T, meta: Span) -> NodeId<tree::Expr>
    where
        T: Into<Node> + Attached<SyntaxPhase, Meta = Span>,
        tree::Expr: From<NodeId<T>>,
    {
        let id = self.insert(node, meta.clone());
        let expr = tree::Expr::from(id);
        self.insert(expr, meta)
    }
}

pub type State = extra::SimpleState<StateRepr>;

pub type Extra<'src> = extra::Full<Rich<'src, Token<'src>, Span>, State, ()>;

pub trait ParserExt<'src, I, T>: Parser<'src, I, T, Extra<'src>> + Sized
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    #[inline]
    fn to_node(self) -> impl Parser<'src, I, NodeId<T>, Extra<'src>>
    where
        T: InnerNode + Attached<SyntaxPhase, Meta = Span>,
    {
        self.map_with(|node, e| {
            let span = e.span();
            let tree: &mut State = e.state();
            tree.insert(node, span)
        })
    }

    #[inline]
    fn to_expr(self) -> impl Parser<'src, I, NodeId<tree::Expr>, Extra<'src>>
    where
        T: InnerNode + Attached<SyntaxPhase, Meta = Span>,
        tree::Expr: From<NodeId<T>>,
    {
        self.to_node().map(tree::Expr::from).to_node()
    }

    #[inline]
    fn to_pat(self) -> impl Parser<'src, I, tree::Pat, Extra<'src>>
    where
        T: Into<tree::Pat>,
    {
        self.map(Into::into)
    }
}

impl<
    'src,
    T,
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
    P: Parser<'src, I, T, Extra<'src>> + Sized,
> ParserExt<'src, I, T> for P
{
}

// pub type ParserInput<'tokens, 'src> =
//     chumsky::input::MappedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

// pub fn parser<'tokens, 'src: 'tokens>(
// ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Module<'src>, Extra<'tokens, 'src>> {
//     todo()
// }

// pub fn binding_parser<'tokens, 'src: 'tokens>(
// ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Binding<'src>, Extra<'tokens, 'src>> {
//     todo()
// }

pub fn name_parser<'src, I>() -> impl Parser<'src, I, NodeId<tree::Name>, Extra<'src>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    select! { Token::Symbol(s) => tree::Symbol::from(s) }
        .map(tree::Name)
        .to_node()
        .boxed()
}

pub fn ident_parser<'src, I>() -> impl Parser<'src, I, tree::Ident, Extra<'src>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    select! { Token::Symbol(s) => tree::Symbol::from(s) }.map(tree::Ident)
}

pub fn literal_parser<'src, I>() -> impl Parser<'src, I, tree::Literal, Extra<'src>> + Sized
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    select! {
        Token::Num(n) => tree::Literal::Num(n),
        Token::Bool(b) => tree::Literal::Bool(b),
        Token::Char(c) => tree::Literal::Char(c),
        Token::Str(s) => tree::Literal::Str(tree::Symbol::from(s))
    }
}

pub fn pat_parser<'src, I>() -> impl Parser<'src, I, NodeId<tree::Pat>, Extra<'src>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    recursive(|pat| {
        let ident = select! { Token::Symbol(s) => tree::Symbol::from(s) }
            .map(tree::IdentPat)
            .to_pat()
            .to_node();
        let wildcard = just(Token::Wildcard).to(tree::Wildcard).to_pat().to_node();
        let literal = literal_parser().map(tree::LiteralPat).to_pat().to_node();

        let property = name_parser()
            .then(just(Token::Colon).ignore_then(pat.clone()).or_not())
            .map(|(key, value)| tree::PropertyPat { key, value })
            .to_node();

        let record = nested_parser(
            property
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .map(|fields| tree::RecordPat { fields })
                .to_pat()
                .to_node(),
            Delimiter::Brace,
            |span| tree::Pat::Error(tree::PatError),
        );

        choice((ident, wildcard, literal, record)).boxed()
    })
    .boxed()
}

pub fn expr_parser<'src, I>() -> impl Parser<'src, I, NodeId<tree::Expr>, Extra<'src>> + Clone
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
                .map(|values| tree::List { values })
                .to_expr(),
            Delimiter::Bracket,
            |span| tree::Expr::Error(tree::ExprError),
        )
        .labelled("ListExpr")
        .as_context();

        // record operations

        let property = name
            .clone()
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map(|(key, value)| tree::Property { key, value })
            .to_node();
        let instantiate = property
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .map(|fields| tree::Record { fields })
            .to_expr()
            .boxed();

        enum RecordOp {
            Extend(NodeId<tree::Name>, NodeId<tree::Expr>),
            Restrict(NodeId<tree::Name>),
            Update(NodeId<tree::Name>, NodeId<tree::Expr>),
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
                        tree::RecordExtend {
                            source,
                            field,
                            value,
                        },
                        span,
                    ),
                    RecordOp::Restrict(field) => {
                        tree.insert_expr(tree::RecordRestrict { source, field }, span)
                    }
                    RecordOp::Update(field, value) => tree.insert_expr(
                        tree::RecordUpdate {
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
            tree::Expr::Error(tree::ExprError)
        })
        .labelled("RecordExpr")
        .as_context();

        let let_ = just(Token::Let)
            .ignore_then(name.clone())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((name, value), inside)| tree::Let {
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
            .map(|((predicate, then), or)| tree::If {
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
            .map(|(pat, matches)| tree::Branch { pat, matches })
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
            .map(|(source, branches)| tree::Case { source, branches })
            .to_expr()
            .labelled("CaseExpr")
            .as_context()
            .boxed();

        let func = just(Token::Backslash)
            .ignore_then(ident.clone().to_node())
            .then_ignore(just(Token::DoubleArrow))
            .then(expr.clone())
            .map(|(param, body)| tree::Func { param, body })
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
                    .map(|(func, arg)| tree::Call { func, arg })
                    .to_expr(),
                Delimiter::Paren,
                |span| tree::Expr::Error(tree::ExprError),
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
                    tree.insert_expr(tree::RecordSelect { source, field }, span)
                },
            )
            .boxed();

        let unary_op = just(Token::Op(Op::Sub))
            .to(tree::UnaryOp::Neg)
            .or(just(Token::Op(Op::Not)).to(tree::UnaryOp::Neg))
            .to_node();
        let unary = unary_op
            .repeated()
            .foldr_with(select, |op, target, e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_expr(tree::Unary { op, target }, span)
            })
            .boxed();

        let op = choice((
            just(Token::Op(Op::Mul)).to(tree::BinaryOp::Mul),
            just(Token::Op(Op::Div)).to(tree::BinaryOp::Div),
            just(Token::Op(Op::Rem)).to(tree::BinaryOp::Rem),
        ))
        .to_node();
        let product = unary
            .clone()
            .foldl_with(op.then(unary).repeated(), |left, (op, right), e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_expr(tree::Binary { op, left, right }, span)
            })
            .boxed();

        let op = just(Token::Op(Op::Add))
            .to(tree::BinaryOp::Add)
            .or(just(Token::Op(Op::Sub)).to(tree::BinaryOp::Sub))
            .to_node();
        let sum = product
            .clone()
            .foldl_with(op.then(product).repeated(), |left, (op, right), e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_expr(tree::Binary { op, left, right }, span)
            })
            .boxed();

        let op = choice((
            just(Token::Op(Op::Less)).to(tree::BinaryOp::Less),
            just(Token::Op(Op::LessEq)).to(tree::BinaryOp::LessEq),
            just(Token::Op(Op::Greater)).to(tree::BinaryOp::Greater),
            just(Token::Op(Op::GreaterEq)).to(tree::BinaryOp::GreaterEq),
            just(Token::Op(Op::Eq)).to(tree::BinaryOp::Eq),
            just(Token::Op(Op::NotEq)).to(tree::BinaryOp::NotEq),
        ))
        .to_node();
        let comparison = sum
            .clone()
            .foldl_with(op.then(sum).repeated(), |left, (op, right), e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_expr(tree::Binary { op, left, right }, span)
            })
            .boxed();

        let op = choice((
            just(Token::Op(Op::And)).to(tree::BinaryOp::And),
            just(Token::Op(Op::Or)).to(tree::BinaryOp::Or),
            just(Token::Op(Op::Xor)).to(tree::BinaryOp::Xor),
        ))
        .to_node();
        let logical = comparison
            .clone()
            .foldl_with(op.then(comparison).repeated(), |left, (op, right), e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_expr(tree::Binary { op, left, right }, span)
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

// #[cfg(test)]
// mod tests {
//     use chumsky::{input::Input, Parser};

//     use crate::syntax::{lexer::lexer, tree};

//     use super::{expr_parser, pat_parser};

//     #[test]
//     fn pat() {
//         let src = "{ a: x, b: { y }, c: _, d }";
//         let tokens = lexer().parse(src).into_result().unwrap();

//         let input = tokens.as_slice().spanned((src.len()..src.len()).into());
//         let pat = pat_parser().parse(input).into_result().unwrap();

//         let record = pat.into_record().unwrap();

//         let a = record.get("a").unwrap();
//         assert!(a.value().is_some());
//         assert_eq!(a.value().unwrap().as_ident().unwrap().inner(), "x");

//         let b = record.get("b").unwrap();
//         assert!(b.value().is_some());
//         let b = b.value().unwrap().as_record().unwrap();
//         assert!(b.get("y").unwrap().value.is_none());

//         let c = record.get("c").unwrap();
//         assert!(c.value().unwrap().is_wildcard());

//         let d = record.get("d").unwrap();
//         assert_eq!(d.value(), None);
//     }

//     #[test]
//     fn case_expr() {
//         let src = "case x of 1 => true, _ => false";
//         let tokens = lexer().parse(src).into_result().unwrap();

//         let input = tokens.as_slice().spanned((src.len()..src.len()).into());
//         let expr = expr_parser().parse(input).into_result().unwrap();

//         let tree::Case {
//             source,
//             mut branches,
//         } = expr.into_case().unwrap().into_inner();

//         assert_eq!(source.inner(), "x");

//         let branch = branches.pop().unwrap();
//         assert!(branch.pat.is_wildcard());
//         let matches = branch.matches.into_literal().unwrap().into_inner();
//         assert_eq!(matches, tree::Literal::Bool(false));

//         let branch = branches.pop().unwrap();
//         let pat = branch.pat.into_literal().unwrap().into_inner();
//         assert_eq!(pat, tree::Literal::Num(1.0));
//         let matches = branch.matches.into_literal().unwrap().into_inner();
//         assert_eq!(matches, tree::Literal::Bool(true));

//         assert!(branches.is_empty());
//     }

//     #[test]
//     fn func_expr() {
//         let src = "\\name => \"Hello\" + name";
//         let tokens = lexer().parse(src).into_result().unwrap();

//         let input = tokens.as_slice().spanned((src.len()..src.len()).into());
//         let expr = expr_parser().parse(input).into_result().unwrap();

//         let tree::Func { param, body } = expr.into_func().unwrap().into_inner();

//         assert_eq!(param.inner(), "name");

//         let body = body.into_binary().unwrap();
//         assert_eq!(body.kind(), tree::BinaryOpKind::Add);
//     }

//     #[test]
//     fn arithmetic_expr() {
//         // ((-4 * 10) + (40 / 4)) + 30 = 0
//         let src = "-4 * 10 + 40 / 4 + 30 == 0";
//         let tokens = lexer().parse(src).into_result().unwrap();

//         let input = tokens.as_slice().spanned((src.len()..src.len()).into());
//         let expr = expr_parser().parse(input).into_result().unwrap();

//         // _ = 0
//         let eq = expr.into_binary().unwrap().into_inner();

//         assert_eq!(eq.kind(), tree::BinaryOpKind::Eq);

//         // _ + 30
//         let sum = eq.left.into_binary().unwrap().into_inner();
//         assert_eq!(sum.kind(), tree::BinaryOpKind::Add);

//         // (_) + (_)
//         let sum = sum.left.into_binary().unwrap().into_inner();
//         assert_eq!(sum.kind(), tree::BinaryOpKind::Add);

//         // (-4 * 10)
//         let mul = sum.left.into_binary().unwrap().into_inner();
//         assert_eq!(mul.kind(), tree::BinaryOpKind::Mul);

//         // (40 / 4)
//         let div = sum.right.into_binary().unwrap().into_inner();
//         assert_eq!(div.kind(), tree::BinaryOpKind::Div);
//     }

//     #[test]
//     fn if_expr() {
//         let src = "if y then { x = 10 }.x else 0";
//         let tokens = lexer().parse(src).into_result().unwrap();

//         let input = tokens.as_slice().spanned((src.len()..src.len()).into());
//         let expr = expr_parser().parse(input).into_result().unwrap();

//         let tree::If {
//             predicate,
//             then,
//             or,
//         } = expr.into_if().unwrap().into_inner();

//         assert_eq!(predicate.into_ident().unwrap().inner(), "y");

//         let tree::RecordSelect { source, field } = then.into_record_select().unwrap().into_inner();

//         assert_eq!(field.name, "x");

//         let record = source.into_record().unwrap();
//         let x = record.get("x").unwrap();
//         assert_eq!(
//             x.value().as_literal().unwrap().inner(),
//             &tree::Literal::Num(10.0)
//         );

//         assert_eq!(
//             or.into_literal().unwrap().into_inner(),
//             tree::Literal::Num(0.0)
//         );
//     }

//     #[test]
//     fn record_select() {
//         let src = "x.y.z";
//         let tokens = lexer().parse(src).into_result().unwrap();

//         let input = tokens.as_slice().spanned((src.len()..src.len()).into());
//         let expr = expr_parser().parse(input).into_result().unwrap();

//         let tree::RecordSelect { source, field } = expr.into_record_select().unwrap().into_inner();
//         assert_eq!(field.name, "z");

//         let tree::RecordSelect { source, field } =
//             source.into_record_select().unwrap().into_inner();
//         assert_eq!(field.name, "y");

//         let ident = source.into_ident().unwrap();
//         assert_eq!(ident.inner(), "x");
//     }

//     #[test]
//     fn record_extension() {
//         let src = "{ y | +x = 10 }";
//         let tokens = lexer().parse(src).into_result().unwrap();

//         let input = tokens.as_slice().spanned((src.len()..src.len()).into());
//         let expr = expr_parser().parse(input).into_result().unwrap();

//         let tree::RecordExtend {
//             source,
//             field,
//             value,
//         } = expr.into_record_extend().unwrap().into_inner();
//         assert_eq!(field.name, "x");

//         let source = source.into_ident().unwrap();
//         assert_eq!(source.inner(), "y");

//         let value = value.into_literal().unwrap().into_inner();
//         assert_eq!(value, tree::Literal::Num(10.0));
//     }

//     #[test]
//     fn record() {
//         let src = "{ x = 10, y = 20 }";
//         let tokens = lexer().parse(src).into_result().unwrap();

//         let input = tokens.as_slice().spanned((src.len()..src.len()).into());
//         let expr = expr_parser().parse(input).into_result().unwrap();

//         let record = expr.into_record().unwrap();

//         let x = record.get("x").unwrap();
//         assert_eq!(
//             x.value().as_literal().unwrap().inner(),
//             &tree::Literal::Num(10.0)
//         );

//         let y = record.get("y").unwrap();
//         assert_eq!(
//             y.value().as_literal().unwrap().inner(),
//             &tree::Literal::Num(20.0)
//         );
//     }
// }
