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

pub fn module_parser<'src, I>() -> impl Parser<'src, I, NodeId<node::Module>, Extra<'src>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    todo()
}

pub fn name_parser<'src, I>() -> impl Parser<'src, I, NodeId<node::Name>, Extra<'src>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    select! { Token::Symbol(s) => node::Symbol::from(s) }
        .map(node::Name)
        .to_node()
        .boxed()
}

/// Parser for literal expressions in the language.
///
/// Grammar:
/// ```bnf
/// literal_expr ::= num
///                | bool
///                | char
///                | str
/// ```
pub fn literal_parser<'src, I>() -> impl Parser<'src, I, node::LiteralExpr, Extra<'src>> + Sized
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    select! {
        Token::Num(n) => node::LiteralExpr::Num(n),
        Token::Bool(b) => node::LiteralExpr::Bool(b),
        Token::Char(c) => node::LiteralExpr::Char(c),
        Token::Str(s) => node::LiteralExpr::Str(node::Symbol::from(s))
    }
}

/*
Some more rambling about syntax:

While I like this more generic solution for lists, lists are very often typed for configurations.
Also tuples are very often typed.
This means that they should probably have a first class syntax which is good on the eyes and the most
used syntax for list and tuples are sadly : [] and ()
These are the symbols I am not yet using: ; ` @ $ % ^ \

So maybe I could also use [] for Variants, but use it not in value position.
Then the [] becomes free for lists (while I still kind of like [] in pattern matching for variants)
Also <> is kind of free in patterns and also in type position:

type Color = < Red, Green, Blue, Other : str >
let color = Other "cyan"
case color of <Other cyan, Green> => ... # looks kind of funny I don't like it

so maybe:

type Color = [Red, Green, Blue, Other : str]
let color = Other "cyan"
case color of Other "cyan" or|, Green => # well seems no good to me

Also combinations are possible like:

type Color = [|Red, Green, Blue, Other : str|]
type Color = |Red, Green, Blue, Other : str|*

type Test = { name : Str } | { peter : [Red | Blue]}

type ColorOpen = [| Red | Green | Blue | Other : str |*]

type Color = [| Red, Green, Blue |]
type Person = {| name : Str, age : Num |}

type OpenPerson = {| name : Str, age : Num |*}
*/

/*
TODO
- list/array patterns (Idea: Implement lists like functions)
- tuples as special function which creates Types of { 0 : a, 1 : b }
- guards
- potentially as-patterns
- test case of Variants

\1 2 3 4 5\
\head ...tail\ => ...

let xs : List Num = (List 1 2 3 4 5 6) in case xs of # I could also capitalize them to somehow say they are data constructors.
    (List head) => ...,
    (List head ...tail) => ...,
    (List a _ c) => ...,
    (List ...init last) => ...,
    (List a ...middle z) => ...,

Potential List syntax
(List 1 2 3) 11 keystrokes
[1, 2, 3] 8 keystrokes
(:1 2 3) 8 keystrokes
\1 2 3\ 7 keystrokes
{1 2 3} 7 keystrokes
(.1 2 3) 6 keystrokes
(,1 2 3) 6 keystrokes
(1 2 3) 5 keystrokes
[1 2 3] 4 keystrokes

type Color = [Red, Green, Blue, Other : Str] # Maybe also remove the colon here ?

Maybe also remove the `:` in pattern matchin to be consistent with Variant Constructors ?

let color = [Other "Cyan"] in case color of
    [Red] => "Red",
    [Green, Blue] => "GreenBlue",
    [Other str] => str
    vs.
    [Other : str] => str


Tuples:

module type Stack = {
   opaque type Stack : Type -> Type
   push : forall a . a -> Stack a -> Stack a
   pop : forall a . Stack a -> [Some : Tuple (Stack a) a, None]
}

module list_impl_stack : Stack = {
  opaque type Stack = List

  push = fn x => fn xs => (list x ...xs)

  pop = fn xs => case xs of
    (list) => [None],
    (list ...init tail) => [Some (tuple init tail)]
}

Data Types:

data type Machine = { ip : Str, cmd : Str }
let machine = (Machine { ip = "127.0.0.1", cmd = "echo /passwords" })

Schemes

data scheme Machine : { ip : Str, cmd : Str }
    = constructor { ip, cmd ? "ssh -p 8070" + ip} => { ip, cmd, } # field prunit? (what was the term) ip = ip
    ~ validator

*/

/// Parser for pattern expressions used in match statements.
///
/// Grammar:
/// ```bnf
/// pat           ::= ident_pat
///                 | wildcard_pat
///                 | literal_pat
///                 | record_pat
///                 | variant_pat
///
/// ident_pat     ::= name
/// wildcard_pat  ::= '_'
/// literal_pat   ::= num | bool | char | str
///
/// record_pat    ::= '{' (record_field_pat (',' record_field_pat)*)? '}'
/// record_field_pat ::= name (':' pat)?
///
/// variant_pat    ::= '<' (variant_case_pat (',' variant_case_pat)*)? '>'
/// variant_case_pat ::= name (':' pat)?
/// ```
pub fn pat_parser<'src, I>() -> impl Parser<'src, I, NodeId<node::Pat>, Extra<'src>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    recursive(|pat| {
        let ident = select! { Token::Symbol(s) => node::Symbol::from(s) }
            .map_to_node(node::IdentPat)
            .to_pat();
        let wildcard = just(Token::Underscore).to(node::AnyPat).to_node().to_pat();
        let literal = literal_parser().map_to_node(node::LiteralPat).to_pat();

        let field = name_parser()
            .then(just(Token::Colon).ignore_then(pat.clone()).or_not())
            .map_to_node(|(field, pat)| node::RecordFieldPat { field, pat });

        let record = nested_parser(
            field
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .map_to_node(node::RecordPat)
                .to_pat(),
            Delimiter::Brace,
            |_span| node::PatError,
        );

        let case = name_parser()
            .then(just(Token::Colon).ignore_then(pat.clone()).or_not())
            .map_to_node(|(case, pat)| node::VariantCasePat { case, pat });

        let variant = nested_parser(
            case.separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .map_to_node(node::VariantPat)
                .to_pat(),
            Delimiter::Brace,
            |_span| node::PatError,
        );

        choice((ident, wildcard, literal, record, variant)).boxed()
    })
    .boxed()
}

pub fn expr_parser<'src, I>() -> impl Parser<'src, I, NodeId<node::Expr>, Extra<'src>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    recursive(|expr| {
        let name = name_parser();

        // Path expression (a.b.c) for variable and module access
        let path = name
            .clone()
            .separated_by(just(Token::Dot))
            .collect()
            .map_to_node(node::PathExpr)
            .to_expr()
            .labelled("PathExpr")
            .as_context()
            .boxed();

        let literal = literal_parser()
            .to_node()
            .to_expr()
            .labelled("LiteralExpr")
            .as_context();

        // TODO use new syntax: let xs = (list 1 2 3) in ...
        let list = nested_parser(
            expr.clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .map_to_node(node::ListExpr)
                .to_expr(),
            Delimiter::Bracket,
            |_span| node::ExprError,
        )
        .labelled("ListExpr")
        .as_context();

        // record operations

        let field = name
            .clone()
            .then_ignore(just(Token::Op(Op::Assign)))
            .then(expr.clone())
            .map(|(field, value)| node::RecordField { field, value })
            .to_node();

        let instantiate = field
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .map_to_node(node::RecordExpr)
            .to_expr()
            .boxed();

        enum RecordOp {
            Extend(NodeId<node::Name>, NodeId<node::Expr>),
            Restrict(NodeId<node::Name>),
            Update(
                NodeId<node::Name>,
                NodeId<node::RecordUpdateOp>,
                NodeId<node::Expr>,
            ),
        }

        let extend = just(Token::Op(Op::Add))
            .ignore_then(name.clone())
            .then_ignore(just(Token::Op(Op::Assign)))
            .then(expr.clone())
            .map(|(field, value)| RecordOp::Extend(field, value))
            .boxed();

        let restrict = just(Token::Op(Op::Sub))
            .ignore_then(name.clone())
            .map(RecordOp::Restrict)
            .boxed();

        let update_op = choice((
            just(Token::Op(Op::Assign)).to(node::RecordUpdateOp::Assign),
            just(Token::Op(Op::AddAssign)).to(node::RecordUpdateOp::AddAssign),
            just(Token::Op(Op::SubAssign)).to(node::RecordUpdateOp::SubAssign),
            just(Token::Op(Op::MulAssign)).to(node::RecordUpdateOp::MulAssign),
            just(Token::Op(Op::DivAssign)).to(node::RecordUpdateOp::DivAssign),
            just(Token::Op(Op::RemAssign)).to(node::RecordUpdateOp::RemAssign),
        ))
        .to_node();

        let update = group((name.clone(), update_op, expr.clone()))
            .map(|(field, op, value)| RecordOp::Update(field, op, value))
            .boxed();

        let inner_op = just(Token::Pipe)
            .ignore_then(choice((extend, restrict, update)))
            .repeated()
            .at_least(1);
        let record_op = path
            .clone()
            .foldl_with(inner_op, |source, op, e| {
                let span = e.span();
                let tree: &mut State = e.state();

                match op {
                    RecordOp::Extend(field, value) => tree.insert_as::<node::Expr, _>(
                        node::RecordExtendExpr {
                            source,
                            field,
                            value,
                        },
                        span,
                    ),
                    RecordOp::Restrict(field) => tree.insert_as::<node::Expr, _>(
                        node::RecordRestrictExpr { source, field },
                        span,
                    ),
                    RecordOp::Update(field, op, value) => tree.insert_as::<node::Expr, _>(
                        node::RecordUpdateExpr {
                            source,
                            field,
                            op,
                            value,
                        },
                        span,
                    ),
                }
            })
            .boxed();

        let record_expr = nested_parser(record_op.or(instantiate), Delimiter::Brace, |_span| {
            node::ExprError
        })
        .labelled("RecordExpr")
        .as_context();

        // TODO allow type annotation
        let let_ = just(Token::Let)
            .ignore_then(name.clone())
            .then_ignore(just(Token::Op(Op::Assign)))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map_to_node(|((name, value), inside)| node::LetExpr {
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
            .map_to_node(|((predicate, then), or)| node::IfExpr {
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
            .map(|(pat, matches)| node::CaseBranch { pat, matches })
            .to_node();
        let branches = branch
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .at_least(1)
            .collect();
        let case = just(Token::Case)
            .ignore_then(path.clone())
            .then_ignore(just(Token::Of))
            .then(branches)
            .map_to_node(|(source, branches)| node::CaseExpr { source, branches })
            .to_expr()
            .labelled("CaseExpr")
            .as_context()
            .boxed();

        // Allow type annotation of ident
        let func = just(Token::Fn)
            .ignore_then(name.clone())
            .then_ignore(just(Token::DoubleArrow))
            .then(expr.clone())
            .map_to_node(|(param, body)| node::LambdaExpr { param, body })
            .to_expr()
            .labelled("FuncExpr")
            .as_context()
            .boxed();

        // TODO allow "recursive" (a (b c)) and maybe also syntactic sugar (a b c)
        let call = recursive(|call| {
            let callable = choice((
                path.clone().labelled("PathExpr").as_context(),
                func.clone(),
                call,
            ));

            nested_parser(
                callable
                    .then(expr.clone())
                    .map_to_node(|(func, arg)| node::CallExpr { func, arg })
                    .to_expr(),
                Delimiter::Paren,
                |_span| node::ExprError,
            )
            .boxed()
        })
        .labelled("CallExpr")
        .as_context()
        .boxed();

        let atom = choice((
            literal,
            list,
            record_expr,
            let_,
            if_,
            case,
            func,
            call,
            path,
        ))
        .boxed();

        let unary_op = just(Token::Op(Op::Sub))
            .to(node::UnaryOp::Neg)
            .or(just(Token::Op(Op::Not)).to(node::UnaryOp::Neg))
            .to_node();
        let unary = unary_op
            .repeated()
            .foldr_with(atom, |op, target, e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_as::<node::Expr, _>(
                    node::UnaryExpr {
                        op,
                        operand: target,
                    },
                    span,
                )
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
                tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, left, right }, span)
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
                tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, left, right }, span)
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
                tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, left, right }, span)
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
                tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, left, right }, span)
            })
            .boxed();

        logical
    })
    .boxed()
}

/// Parser for type expressions in the language.
///
/// Grammar:
/// ```bnf
/// type_expression ::= func_type
///
/// func_type       ::= type_application ('->' type_expression)*
///
/// type_application::= atom_type (atom_type)*  // Left-associative application
///
/// atom_type       ::= type_path
///                   | record_type
///                   | variant_type
///                   | '(' type_expression ')'
///
/// record_type     ::= '{' (record_field (',' record_field)*)? '}'
/// record_field    ::= name ':' type_expression
///
/// variant_type    ::= '[' (variant_case (',' variant_case)*)? ']'
/// variant_case    ::= name (':' type_expression)?
///
/// type_path       ::= name ('.' name)*  // Path to a type (like Num or std.List)
/// ```
pub fn type_expr_parser<'src, I>()
-> impl Parser<'src, I, NodeId<node::TypeExpr>, Extra<'src>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    recursive(|ty| {
        let name = name_parser();

        let path = name
            .clone()
            .separated_by(just(Token::Dot))
            .at_least(1)
            .collect()
            .map_to_node(node::TypePath)
            .to_type_expr()
            .boxed();

        let field = name
            .clone()
            .then_ignore(just(Token::Colon))
            .then(ty.clone())
            .map_to_node(|(name, ty)| node::RecordFieldType { name, ty });

        let record = nested_parser(
            field
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .map_to_node(node::RecordType)
                .to_type_expr(),
            Delimiter::Brace,
            |_span| node::TypeError,
        );

        let case = name
            .clone()
            .then(just(Token::Colon).ignore_then(ty.clone()).or_not())
            .map_to_node(|(name, ty)| node::VariantCaseType { name, ty });

        let variant = nested_parser(
            case.separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .map_to_node(node::VariantType)
                .to_type_expr(),
            Delimiter::Bracket,
            |_span| node::TypeError,
        );

        let atom = choice((
            path,
            record,
            variant,
            nested_parser(ty.clone(), Delimiter::Paren, |_| node::TypeError),
        ));

        let appl = atom
            .clone()
            .foldl_with(atom.clone().repeated(), |constructor, arg, e| {
                let span = e.span();
                let tree: &mut State = e.state();

                tree.insert_as::<node::TypeExpr, _>(
                    node::TypeApplication { constructor, arg },
                    span,
                )
            })
            .boxed();

        let func = appl
            .clone()
            .foldl_with(
                just(Token::Arrow).ignore_then(ty.clone()).repeated(),
                |input, output, e| {
                    let span = e.span();
                    let tree: &mut State = e.state();

                    tree.insert_as::<node::TypeExpr, _>(node::FuncType { input, output }, span)
                },
            )
            .boxed();

        func
    })
}

/// Parser for types in the language, which may include polymorphic type variables.
///
/// Grammar:
/// ```bnf
/// type      ::= 'forall' name+ '.' type_expression
///             | type_expression
/// ```
pub fn type_parser<'src, I>() -> impl Parser<'src, I, NodeId<node::Type>, Extra<'src>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    // hindley milner only allows standard polymorphism (top-level forall)
    // higher-rank polymorphism (nested forall) is undecidable for full type-inference
    just(Token::Forall)
        .ignore_then(name_parser().repeated().at_least(1).collect())
        .then_ignore(just(Token::Dot))
        .or_not()
        .then(type_expr_parser())
        .map_to_node(|(vars, ty)| node::Type {
            vars: vars.unwrap_or_default(),
            ty,
        })
        .boxed()
}

/// Parser for type bindings in the language.
///
/// Grammar:
/// ```bnf
/// type_bind ::= 'type' name '=' type
/// ```
pub fn type_bind_parser<'src, I>()
-> impl Parser<'src, I, NodeId<node::TypeBind>, Extra<'src>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    just(Token::Type)
        .ignore_then(name_parser())
        .then_ignore(just(Token::Op(Op::Assign)))
        .then(type_parser())
        .map_to_node(|(name, ty)| node::TypeBind { name, ty })
        .boxed()
}

pub fn nested_parser<'src, I, T, U>(
    parser: impl Parser<'src, I, NodeId<T>, Extra<'src>> + 'src,
    delim: Delimiter,
    fallback: impl Fn(Span) -> U + Clone + 'src,
) -> impl Parser<'src, I, NodeId<T>, Extra<'src>> + Clone
where
    Node: From<T> + From<U>,
    T: From<NodeId<U>> + MetaCast<SyntaxPhase, Meta = Span> + 'src,
    U: MetaCast<SyntaxPhase, Meta = Span> + 'src,
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
            .to_node()
            .map(T::from)
            .to_node(),
        ))
        .boxed()
}

// pub fn nested_in_parser<'src, I, T, U>(
//     left: Token,
//     parser: impl Parser<'src, I, NodeId<T>, Extra<'src>> + 'src,
//     right: Token,
//     fallback: impl Fn(Span) -> U + Clone + 'src,
// ) -> impl Parser<'src, I, NodeId<T>, Extra<'src>> + Clone
// where
//     Node: From<T> + From<U>,
//     T: From<NodeId<U>> + MetaCast<SyntaxPhase, Meta = Span> + 'src,
//     U: MetaCast<SyntaxPhase, Meta = Span> + 'src,
//     I: ValueInput<'src, Token = Token<'src>, Span = Span>,
// {
//     parser
//         .delimited_by(just(left_, just(Token::Close(delim)))
//         .recover_with(via_parser(
//             nested_delimiters(
//                 Token::Open(delim),
//                 Token::Close(delim),
//                 [
//                     (
//                         Token::Open(Delimiter::Paren),
//                         Token::Close(Delimiter::Paren),
//                     ),
//                     (
//                         Token::Open(Delimiter::Bracket),
//                         Token::Close(Delimiter::Bracket),
//                     ),
//                     (
//                         Token::Open(Delimiter::Brace),
//                         Token::Close(Delimiter::Brace),
//                     ),
//                 ],
//                 fallback,
//             )
//             .to_node()
//             .map(T::from)
//             .to_node(),
//         ))
//         .boxed()
// }

#[cfg(test)]
mod tests {
    use chumsky::prelude::*;

    use kola_tree::prelude::*;

    use super::{expr_parser, pat_parser, type_bind_parser, type_expr_parser, type_parser};
    use crate::{lexer::lexer, parser::try_parse_with};

    #[test]
    fn pat() {
        let src = "{ a: x, b: { y }, c: _, d }";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (pat, tree, _spans) = try_parse_with(input, pat_parser()).unwrap();

        let record = pat.get(&tree).to_record().unwrap().get(&tree);

        let a = record.get("a", &tree).unwrap();
        assert!(a.pat.is_some());
        assert_eq!(a.pat(&tree).unwrap().to_ident().unwrap().get(&tree), "x");

        let b = record.get("b", &tree).unwrap();
        assert!(b.pat.is_some());
        let b = b.pat(&tree).unwrap().to_record().unwrap().get(&tree);
        assert!(b.get("y", &tree).unwrap().pat.is_none());

        let c = record.get("c", &tree).unwrap();
        assert!(c.pat(&tree).unwrap().is_wildcard());

        let d = record.get("d", &tree).unwrap();
        assert_eq!(d.pat(&tree), None);
    }

    #[test]
    fn case_expr() {
        let src = "case x of 1 => true, _ => false";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (expr, tree, _spans) = try_parse_with(input, expr_parser()).unwrap();

        let node::CaseExpr { source, branches } = expr.get(&tree).to_case().unwrap().get(&tree);

        assert_eq!(
            source
                .get(&tree)
                .to_path()
                .unwrap()
                .get(&tree)
                .get(0, &tree),
            "x"
        );

        let mut branches = branches.iter().rev();

        let branch = branches.next().unwrap().get(&tree);
        assert!(branch.pat(&tree).is_wildcard());
        let matches = branch.matches(&tree).to_literal().unwrap().get(&tree);
        assert_eq!(matches, &node::LiteralExpr::Bool(false));

        let branch = branches.next().unwrap().get(&tree);
        let pat = &branch.pat(&tree).to_literal().unwrap().get(&tree).0;
        assert_eq!(pat, &node::LiteralExpr::Num(1.0));
        let matches = branch.matches(&tree).to_literal().unwrap().get(&tree);
        assert_eq!(matches, &node::LiteralExpr::Bool(true));

        assert_eq!(branches.len(), 0);
    }

    #[test]
    fn func_expr() {
        let src = "fn name => \"Hello\" + name";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (expr, tree, _spans) = try_parse_with(input, expr_parser()).unwrap();

        let node::LambdaExpr { param, body } = expr.get(&tree).to_lambda().unwrap().get(&tree);

        assert_eq!(param.get(&tree), "name");

        let body = body.get(&tree).to_binary().unwrap().get(&tree);
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
        let eq = expr.get(&tree).to_binary().unwrap().get(&tree);

        assert_eq!(eq.op(&tree), node::BinaryOp::Eq);

        // _ + 30
        let sum = eq.left(&tree).to_binary().unwrap().get(&tree);
        assert_eq!(sum.op(&tree), node::BinaryOp::Add);

        // (_) + (_)
        let sum = sum.left(&tree).to_binary().unwrap().get(&tree);
        assert_eq!(sum.op(&tree), node::BinaryOp::Add);

        // (-4 * 10)
        let mul = sum.left(&tree).to_binary().unwrap().get(&tree);
        assert_eq!(mul.op(&tree), node::BinaryOp::Mul);

        // (40 / 4)
        let div = sum.right(&tree).to_binary().unwrap().get(&tree);
        assert_eq!(div.op(&tree), node::BinaryOp::Div);
    }

    #[test]
    fn if_expr() {
        let src = "if y then x else 0";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (expr, tree, _spans) = try_parse_with(input, expr_parser()).unwrap();

        let node::IfExpr {
            predicate,
            then,
            or,
        } = expr.get(&tree).to_if().unwrap().get(&tree);

        assert_eq!(
            predicate
                .get(&tree)
                .to_path()
                .unwrap()
                .get(&tree)
                .get(0, &tree),
            "y"
        );

        assert_eq!(
            then.get(&tree).to_path().unwrap().get(&tree).get(0, &tree),
            "x"
        );

        assert_eq!(
            or.get(&tree).to_literal().unwrap().get(&tree),
            &node::LiteralExpr::Num(0.0)
        );
    }

    #[test]
    fn path_expr() {
        let src = "x.y.z";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (expr, tree, _spans) = try_parse_with(input, expr_parser()).unwrap();

        let path = expr.get(&tree).to_path().unwrap().get(&tree);
        assert_eq!(path.0.len(), 3);

        assert_eq!(path.get(0, &tree), "x");
        assert_eq!(path.get(1, &tree), "y");
        assert_eq!(path.get(2, &tree), "z");
    }

    #[test]
    fn record_extension() {
        let src = "{ y | +x = 10 }";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (expr, tree, _spans) = try_parse_with(input, expr_parser()).unwrap();

        let node::RecordExtendExpr {
            source,
            field,
            value,
        } = expr.get(&tree).to_record_extend().unwrap().get(&tree);
        assert_eq!(field.get(&tree), "x");

        let source = source.get(&tree).to_path().unwrap().get(&tree);
        assert_eq!(source.get(0, &tree), "y");

        let value = value.get(&tree).to_literal().unwrap().get(&tree);
        assert_eq!(value, &node::LiteralExpr::Num(10.0));
    }

    #[test]
    fn record() {
        let src = "{ x = 10, y = 20 }";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (expr, tree, _spans) = try_parse_with(input, expr_parser()).unwrap();

        let record = expr.get(&tree).to_record().unwrap().get(&tree);

        let x = record.get("x", &tree).unwrap();
        assert_eq!(
            x.value(&tree).to_literal().unwrap().get(&tree),
            &node::LiteralExpr::Num(10.0)
        );

        let y = record.get("y", &tree).unwrap();
        assert_eq!(
            y.value(&tree).to_literal().unwrap().get(&tree),
            &node::LiteralExpr::Num(20.0)
        );
    }

    #[test]
    fn type_expr() {
        let src = "Num -> { a : Num, b : Num -> Num } -> Str";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (ty, tree, _spans) = try_parse_with(input, type_expr_parser()).unwrap();

        // Num -> (...)
        let node::FuncType { input, output } = ty.get(&tree).to_func_type().unwrap().get(&tree);
        assert_eq!(
            input
                .get(&tree)
                .to_type_path()
                .unwrap()
                .get(&tree)
                .get(0, &tree),
            "Num"
        );

        // { a: Num, ... } -> ...
        let node::FuncType { input, output } = output.get(&tree).to_func_type().unwrap().get(&tree);

        let input = &input.get(&tree).to_record_type().unwrap().get(&tree).0;
        assert_eq!(input.len(), 2);

        let a = input[0].get(&tree);
        assert_eq!(a.name.get(&tree), "a");
        assert_eq!(
            a.ty.get(&tree)
                .to_type_path()
                .unwrap()
                .get(&tree)
                .get(0, &tree),
            "Num"
        );

        let b = input[1].get(&tree);
        assert_eq!(b.name.get(&tree), "b");
        assert!(b.ty.get(&tree).is_func_type());

        assert_eq!(
            output
                .get(&tree)
                .to_type_path()
                .unwrap()
                .get(&tree)
                .get(0, &tree),
            "Str"
        );
    }

    #[test]
    fn type_application() {
        let src = "Map (Num -> Str) (std.List Str)";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (ty, tree, _spans) = try_parse_with(input, type_expr_parser()).unwrap();

        // Map (Num -> Str) @@ (std.List Str)
        let node::TypeApplication { constructor, arg } =
            ty.get(&tree).to_type_application().unwrap().get(&tree);

        {
            // Map @@ Num -> Str
            let node::TypeApplication { constructor, arg } = constructor
                .get(&tree)
                .to_type_application()
                .unwrap()
                .get(&tree);

            let map = constructor.get(&tree).to_type_path().unwrap().get(&tree);
            assert_eq!(map.get(0, &tree), "Map");

            let node::FuncType { input, output } =
                arg.get(&tree).to_func_type().unwrap().get(&tree);
            assert!(input.get(&tree).is_type_path()); // Num
            assert!(output.get(&tree).is_type_path()); // Str
        }

        // std.List @@ Str
        let node::TypeApplication { constructor, arg } =
            arg.get(&tree).to_type_application().unwrap().get(&tree);
        assert!(constructor.get(&tree).is_type_path()); // std.List
        assert!(arg.get(&tree).is_type_path()); // Str
    }

    #[test]
    fn type_() {
        let src = "forall a b . { left : a, right : Num -> b }";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (pty, tree, _spans) = try_parse_with(input, type_parser()).unwrap();

        let node::Type { vars, ty } = pty.get(&tree);

        assert_eq!(vars.len(), 2);
        assert_eq!(vars[0].get(&tree), "a");
        assert_eq!(vars[1].get(&tree), "b");

        let record_type = ty.get(&tree).to_record_type().unwrap().get(&tree);
        assert_eq!(record_type.0.len(), 2);

        let left = record_type.get(0, &tree);
        assert_eq!(left.name.get(&tree), "left");
        assert_eq!(
            left.ty
                .get(&tree)
                .to_type_path()
                .unwrap()
                .get(&tree)
                .get(0, &tree),
            "a"
        );

        let right = record_type.get(1, &tree);
        assert_eq!(right.name.get(&tree), "right");

        let node::FuncType { input, output } =
            right.ty.get(&tree).to_func_type().unwrap().get(&tree);
        assert_eq!(
            input
                .get(&tree)
                .to_type_path()
                .unwrap()
                .get(&tree)
                .get(0, &tree),
            "Num"
        );
        assert_eq!(
            output
                .get(&tree)
                .to_type_path()
                .unwrap()
                .get(&tree)
                .get(0, &tree),
            "b"
        );
    }

    #[test]
    fn type_bind() {
        let src = "type Person = forall a . { id : a, name : Str, age : Num }";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (alias, tree, _spans) = try_parse_with(input, type_bind_parser()).unwrap();

        let node::TypeBind { name, ty } = alias.get(&tree);
        assert_eq!(name.get(&tree), "Person");

        let node::Type { vars, ty } = ty.get(&tree);
        assert_eq!(vars.len(), 1);
        assert_eq!(vars[0].get(&tree), "a");

        let record_type = &ty.get(&tree).to_record_type().unwrap().get(&tree).0;
        assert_eq!(record_type.len(), 3);
    }

    #[test]
    fn variant_type_bind() {
        let src = "type Option = forall a . [ Some : a, None ]";
        let tokens = lexer().parse(src).into_result().unwrap();
        let eoi = (src.len()..src.len()).into();
        let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));

        let (bind, tree, _spans) = try_parse_with(input, type_bind_parser()).unwrap();

        let node::TypeBind { name, ty } = bind.get(&tree);
        assert_eq!(name.get(&tree), "Option");

        let node::Type { vars, ty } = ty.get(&tree);
        assert_eq!(vars.len(), 1);
        assert_eq!(vars[0].get(&tree), "a");

        let variant = ty.get(&tree).to_variant_type().unwrap().get(&tree);
        assert_eq!(variant.0.len(), 2);

        let some = variant.get(0, &tree);
        assert_eq!(some.name.get(&tree), "Some");
        assert_eq!(
            some.ty
                .unwrap()
                .get(&tree)
                .to_type_path()
                .unwrap()
                .get(&tree)
                .get(0, &tree),
            "a"
        );

        let none = variant.get(1, &tree);
        assert_eq!(none.name.get(&tree), "None");
        assert_eq!(none.ty, None);
    }
}
