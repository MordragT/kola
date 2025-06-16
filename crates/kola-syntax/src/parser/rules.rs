use chumsky::prelude::*;

use kola_span::Loc;
use kola_tree::prelude::*;

use super::{KolaParser, State, primitives::*};
use crate::{
    loc::LocPhase,
    token::{CloseT, CtrlT, Delim, KwT, LiteralT, OpT, OpenT},
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

// TODO case expr also end in a ',' which is ambiguos to binds being separated by ','
// Therefore replace the "case x of 10 => ..., 5 => ...," with something different.

// pub fn root_parser<'t>() -> impl KolaParser<'t, Id<node::ModuleBind>> + Clone {
//     module_parser().map_with(|module, e| {
//         let span = e.span();
//         let tree: &mut State = e.state();

//         let vis = tree.insert(node::Vis::Export, span);
//         let key = tree.intern("root");
//         let name = tree.insert(node::Name(key), span);
//         let value = tree.insert(node::ModuleExpr::Module(module), span);

//         tree.insert(
//             node::ModuleBind {
//                 vis,
//                 name,
//                 ty: None,
//                 value,
//             },
//             span,
//         )
//     })
// }

pub fn module_name_parser<'t>() -> impl KolaParser<'t, Id<node::ModuleName>> + Clone {
    symbol().map(node::ModuleName::new).to_node().boxed()
}

pub fn type_name_parser<'t>() -> impl KolaParser<'t, Id<node::TypeName>> + Clone {
    symbol().map(node::TypeName::new).to_node().boxed()
}

pub fn value_name_parser<'t>() -> impl KolaParser<'t, Id<node::ValueName>> + Clone {
    symbol().map(node::ValueName::new).to_node().boxed()
}
pub fn module_parser<'t>() -> impl KolaParser<'t, Id<node::Module>> + Clone {
    let module_type = module_type_parser();

    recursive(|module| {
        let module_import = kw(KwT::IMPORT)
            .ignore_then(module_name_parser())
            .map_to_node(node::ModuleImport)
            .to_module_expr();

        let module_path = module_name_parser()
            .separated_by(ctrl(CtrlT::DOUBLE_COLON))
            .collect()
            .map_to_node(node::ModulePath)
            .to_module_expr();

        let module_expr = choice((module.clone().to_module_expr(), module_import, module_path));

        let vis = kw(KwT::EXPORT)
            .to(node::Vis::Export)
            .or_not()
            .map_to_node(|vis| vis.unwrap_or(node::Vis::None))
            .boxed();

        let value_bind = vis
            .clone()
            .then(value_name_parser())
            .then(ctrl(CtrlT::COLON).ignore_then(type_parser()).or_not())
            .then_ignore(op(OpT::ASSIGN))
            .then(expr_parser())
            .map_to_node(|(((vis, name), ty), value)| node::ValueBind {
                vis,
                name,
                ty,
                value,
            })
            .to_bind();

        let type_bind = type_bind_parser().to_bind();

        // TODO opaque type bind

        let module_bind = vis
            .then_ignore(kw(KwT::MODULE))
            .then(module_name_parser())
            .then(ctrl(CtrlT::COLON).ignore_then(module_type.clone()).or_not())
            .then_ignore(op(OpT::ASSIGN))
            .then(module_expr)
            .map_to_node(|(((vis, name), ty), value)| node::ModuleBind {
                vis,
                name,
                ty,
                value,
            })
            .to_bind();

        let module_type_bind = kw(KwT::MODULE)
            .ignore_then(kw(KwT::TYPE))
            .ignore_then(module_name_parser())
            .then_ignore(op(OpT::ASSIGN))
            .then(module_type)
            .map_to_node(|(name, ty)| node::ModuleTypeBind { name, ty })
            .to_bind();

        let bind = choice((module_type_bind, module_bind, type_bind, value_bind)).boxed();

        bind.separated_by(ctrl(CtrlT::COMMA))
            .allow_trailing()
            .collect()
            .map_to_node(node::Module)
            .delimited_by(open_delim(OpenT::BRACE), close_delim(CloseT::BRACE))
            .boxed()
    })
}

pub fn module_type_parser<'t>() -> impl KolaParser<'t, Id<node::ModuleType>> + Clone {
    recursive(|module_type| {
        let value_spec = value_name_parser()
            .then_ignore(ctrl(CtrlT::COLON))
            .then(type_parser())
            .map_to_node(|(name, ty)| node::ValueSpec { name, ty })
            .to_spec();

        let type_bind = type_bind_parser().to_spec();

        // TODO opaque type spec

        let module_spec = kw(KwT::MODULE)
            .ignore_then(module_name_parser())
            .then_ignore(ctrl(CtrlT::COLON))
            .then(module_type)
            .map_to_node(|(name, ty)| node::ModuleSpec { name, ty })
            .to_spec();

        let spec = choice((value_spec, type_bind, module_spec)).boxed();

        spec.separated_by(ctrl(CtrlT::COMMA))
            .allow_trailing()
            .collect()
            .map_to_node(node::ModuleType)
            .delimited_by(open_delim(OpenT::BRACE), close_delim(CloseT::BRACE))
            .boxed()
    })
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
pub fn literal_parser<'t>() -> impl KolaParser<'t, node::LiteralExpr> + Sized {
    literal().map_with(|l, e| {
        let state: &mut State = e.state();
        match l {
            LiteralT::Num(n) => node::LiteralExpr::Num(n),
            LiteralT::Bool(b) => node::LiteralExpr::Bool(b),
            LiteralT::Char(c) => node::LiteralExpr::Char(c),
            LiteralT::Str(s) => node::LiteralExpr::Str(state.intern(s)),
        }
    })
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
pub fn pat_parser<'t>() -> impl KolaParser<'t, Id<node::Pat>> + Clone {
    recursive(|pat| {
        let ident = symbol().map_to_node(node::IdentPat).to_pat();
        let wildcard = ctrl(CtrlT::UNDERSCORE).to(node::AnyPat).to_node().to_pat();
        let literal = literal_parser().map_to_node(node::LiteralPat).to_pat();

        let field = value_name_parser()
            .then(ctrl(CtrlT::COLON).ignore_then(pat.clone()).or_not())
            .map_to_node(|(field, pat)| node::RecordFieldPat { field, pat });

        let record = nested_parser(
            field
                .separated_by(ctrl(CtrlT::COMMA))
                .allow_trailing()
                .collect()
                .map_to_node(node::RecordPat)
                .to_pat(),
            Delim::Brace,
            |_span| node::PatError,
        );

        let case = value_name_parser()
            .then(ctrl(CtrlT::COLON).ignore_then(pat.clone()).or_not())
            .map_to_node(|(case, pat)| node::VariantCasePat { case, pat });

        let variant = nested_parser(
            case.separated_by(ctrl(CtrlT::COMMA))
                .allow_trailing()
                .collect()
                .map_to_node(node::VariantPat)
                .to_pat(),
            Delim::Angle,
            |_span| node::PatError,
        );

        choice((ident, wildcard, literal, record, variant)).boxed()
    })
    .boxed()
}

pub fn expr_parser<'t>() -> impl KolaParser<'t, Id<node::Expr>> + Clone {
    recursive(|expr| {
        let name = value_name_parser();

        // Path expression (module::record.field) for variable and module access
        let path = group((
            symbol().spanned(),
            ctrl(CtrlT::DOUBLE_COLON)
                .ignore_then(symbol().spanned())
                .repeated()
                .collect::<Vec<_>>(),
            ctrl(CtrlT::DOT)
                .ignore_then(value_name_parser())
                .repeated()
                .collect::<Vec<_>>(),
        ))
        .map_with(|(mut binding, mut path, select), e| {
            let loc = e.span();
            let tree: &mut State = e.state();

            let path = if path.is_empty() {
                None
            } else {
                // a :: b ...
                // a is currently the binding and must be inserted as module_path
                // b is currently in path and must be extracted from it
                binding = std::mem::replace(&mut path[0], binding);

                let path_loc = loc.map_span(|span| span.after(&binding.1.span).unwrap());
                let path = path
                    .into_iter()
                    .map(|(key, span)| tree.insert(node::ModuleName::new(key), span))
                    .collect();

                Some(tree.insert(node::ModulePath(path), path_loc))
            };

            let binding = tree.insert(node::ValueName::new(binding.0), binding.1);

            node::PathExpr {
                path,
                binding,
                select,
            }
        })
        .to_node()
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
                .separated_by(ctrl(CtrlT::COMMA))
                .allow_trailing()
                .collect()
                .map_to_node(node::ListExpr)
                .to_expr(),
            Delim::Bracket,
            |_span| node::ExprError,
        )
        .labelled("ListExpr")
        .as_context();

        // record operations

        let field = name
            .clone()
            .then_ignore(op(OpT::ASSIGN))
            .then(expr.clone())
            .map(|(field, value)| node::RecordField { field, value })
            .to_node();

        let instantiate = field
            .separated_by(ctrl(CtrlT::COMMA))
            .allow_trailing()
            .collect()
            .map_to_node(node::RecordExpr)
            .to_expr()
            .boxed();

        enum RecordOp {
            Extend(Vec<Id<node::ValueName>>, Id<node::Expr>),
            Restrict(Vec<Id<node::ValueName>>),
            Update(
                Vec<Id<node::ValueName>>,
                Id<node::RecordUpdateOp>,
                Id<node::Expr>,
            ),
        }

        let field_path = name
            .clone()
            .separated_by(ctrl(CtrlT::DOT))
            .collect()
            .boxed();

        let extend = op(OpT::ADD)
            .ignore_then(field_path.clone())
            .then_ignore(op(OpT::ASSIGN))
            .then(expr.clone())
            .map(|(field, value)| RecordOp::Extend(field, value))
            .boxed();

        let restrict = op(OpT::SUB)
            .ignore_then(field_path.clone())
            .map(RecordOp::Restrict)
            .boxed();

        let update_op = choice((
            op(OpT::ASSIGN).to(node::RecordUpdateOp::Assign),
            op(OpT::ADD_ASSIGN).to(node::RecordUpdateOp::AddAssign),
            op(OpT::SUB_ASSIGN).to(node::RecordUpdateOp::SubAssign),
            op(OpT::MUL_ASSIGN).to(node::RecordUpdateOp::MulAssign),
            op(OpT::DIV_ASSIGN).to(node::RecordUpdateOp::DivAssign),
            op(OpT::REM_ASSIGN).to(node::RecordUpdateOp::RemAssign),
        ))
        .to_node();

        let update = group((field_path.clone(), update_op, expr.clone()))
            .map(|(field, op, value)| RecordOp::Update(field, op, value))
            .boxed();

        let inner_op = ctrl(CtrlT::PIPE)
            .ignore_then(choice((extend, restrict, update)))
            .repeated()
            .at_least(1);
        let record_op = path
            .clone()
            .foldl_with(inner_op, |source, op, e| {
                let span = e.span();
                let tree: &mut State = e.state();

                let mut source_path = source
                    .get(&tree.builder)
                    .to_path()
                    .unwrap()
                    .get(&tree.builder)
                    .clone();

                match op {
                    RecordOp::Extend(mut field_path, mut value) => {
                        source_path.select.append(&mut field_path);

                        let field = source_path.select.pop().unwrap();
                        let source = tree.insert_as::<node::Expr, _>(source_path.clone(), span);
                        value = tree.insert_as::<node::Expr, _>(
                            node::RecordExtendExpr {
                                source,
                                field,
                                value,
                            },
                            span,
                        );
                        while source_path.select.len() > 1 {
                            let field = source_path.select.pop().unwrap();
                            let op = tree.insert(node::RecordUpdateOp::Assign, span);
                            let source = tree.insert_as::<node::Expr, _>(source_path.clone(), span);
                            value = tree.insert_as::<node::Expr, _>(
                                node::RecordUpdateExpr {
                                    source,
                                    field,
                                    op,
                                    value,
                                },
                                span,
                            );
                        }

                        value
                    }
                    RecordOp::Restrict(mut field_path) => {
                        source_path.select.append(&mut field_path);

                        let field = source_path.select.pop().unwrap();
                        let source = tree.insert_as::<node::Expr, _>(source_path.clone(), span);
                        let mut value = tree.insert_as::<node::Expr, _>(
                            node::RecordRestrictExpr { source, field },
                            span,
                        );

                        while source_path.select.len() > 1 {
                            let field = source_path.select.pop().unwrap();
                            let op = tree.insert(node::RecordUpdateOp::Assign, span);
                            let source = tree.insert_as::<node::Expr, _>(source_path.clone(), span);
                            value = tree.insert_as::<node::Expr, _>(
                                node::RecordUpdateExpr {
                                    source,
                                    field,
                                    op,
                                    value,
                                },
                                span,
                            );
                        }

                        value
                    }

                    RecordOp::Update(mut field_path, op, mut value) => {
                        source_path.select.append(&mut field_path);

                        let field = source_path.select.pop().unwrap();
                        let source = tree.insert_as::<node::Expr, _>(source_path.clone(), span);
                        value = tree.insert_as::<node::Expr, _>(
                            node::RecordUpdateExpr {
                                source,
                                field,
                                op,
                                value,
                            },
                            span,
                        );

                        while source_path.select.len() > 1 {
                            let field = source_path.select.pop().unwrap();
                            let op = tree.insert(node::RecordUpdateOp::Assign, span);
                            let source = tree.insert_as::<node::Expr, _>(source_path.clone(), span);
                            value = tree.insert_as::<node::Expr, _>(
                                node::RecordUpdateExpr {
                                    source,
                                    field,
                                    op,
                                    value,
                                },
                                span,
                            );
                        }

                        value
                    }
                }
            })
            .boxed();

        let record_expr = nested_parser(record_op.or(instantiate), Delim::Brace, |_span| {
            node::ExprError
        })
        .labelled("RecordExpr")
        .as_context();

        // TODO allow type annotation
        let let_ = kw(KwT::LET)
            .ignore_then(name.clone())
            .then_ignore(op(OpT::ASSIGN))
            .then(expr.clone())
            .then_ignore(kw(KwT::IN))
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

        let if_ = kw(KwT::IF)
            .ignore_then(expr.clone())
            .then_ignore(kw(KwT::THEN))
            .then(expr.clone())
            .then_ignore(kw(KwT::ELSE))
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
            .then_ignore(ctrl(CtrlT::DOUBLE_ARROW))
            .then(expr.clone())
            .map(|(pat, matches)| node::CaseBranch { pat, matches })
            .to_node();
        let branches = branch
            .separated_by(ctrl(CtrlT::COMMA))
            .allow_trailing()
            .at_least(1)
            .collect();
        let case = kw(KwT::CASE)
            .ignore_then(path.clone())
            .then_ignore(kw(KwT::OF))
            .then(branches)
            .map_to_node(|(source, branches)| node::CaseExpr { source, branches })
            .to_expr()
            .labelled("CaseExpr")
            .as_context()
            .boxed();

        // Allow type annotation of ident
        let func = kw(KwT::FN)
            .ignore_then(name.clone())
            .then_ignore(ctrl(CtrlT::DOUBLE_ARROW))
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
                Delim::Paren,
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

        let unary_op = op(OpT::SUB)
            .to(node::UnaryOp::Neg)
            .or(op(OpT::NOT).to(node::UnaryOp::Neg))
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

        let product_op = choice((
            op(OpT::MUL).to(node::BinaryOp::Mul),
            op(OpT::DIV).to(node::BinaryOp::Div),
            op(OpT::REM).to(node::BinaryOp::Rem),
        ))
        .to_node();
        let product = unary
            .clone()
            .foldl_with(product_op.then(unary).repeated(), |left, (op, right), e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, left, right }, span)
            })
            .boxed();

        let sum_op = op(OpT::ADD)
            .to(node::BinaryOp::Add)
            .or(op(OpT::SUB).to(node::BinaryOp::Sub))
            .to_node();
        let sum = product
            .clone()
            .foldl_with(sum_op.then(product).repeated(), |left, (op, right), e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, left, right }, span)
            })
            .boxed();

        let comparison_op = choice((
            op(OpT::LESS).to(node::BinaryOp::Less),
            op(OpT::LESS_EQ).to(node::BinaryOp::LessEq),
            op(OpT::GREATER).to(node::BinaryOp::Greater),
            op(OpT::GREATER_EQ).to(node::BinaryOp::GreaterEq),
            op(OpT::EQ).to(node::BinaryOp::Eq),
            op(OpT::NOT_EQ).to(node::BinaryOp::NotEq),
        ))
        .to_node();
        let comparison = sum
            .clone()
            .foldl_with(
                comparison_op.then(sum).repeated(),
                |left, (op, right), e| {
                    let span = e.span();
                    let tree: &mut State = e.state();
                    tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, left, right }, span)
                },
            )
            .boxed();

        let logical_op = choice((
            op(OpT::AND).to(node::BinaryOp::And),
            op(OpT::OR).to(node::BinaryOp::Or),
            op(OpT::XOR).to(node::BinaryOp::Xor),
        ))
        .to_node();
        let logical = comparison
            .clone()
            .foldl_with(
                logical_op.then(comparison).repeated(),
                |left, (op, right), e| {
                    let span = e.span();
                    let tree: &mut State = e.state();
                    tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, left, right }, span)
                },
            )
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
pub fn type_expr_parser<'t>() -> impl KolaParser<'t, Id<node::TypeExpr>> + Clone {
    recursive(|ty| {
        let path = symbol()
            .spanned()
            .separated_by(ctrl(CtrlT::DOUBLE_COLON))
            .at_least(1)
            .collect::<Vec<_>>()
            .map_with(|mut path, e| {
                let loc = e.span();
                let tree: &mut State = e.state();

                let (ty_name, ty_loc) = path.pop().unwrap();
                let ty = tree.insert(node::TypeName::new(ty_name), ty_loc);

                let path = if !path.is_empty() {
                    let module_loc = loc.map_span(|span| span.before(&ty_loc.span).unwrap());
                    let module_path = path
                        .into_iter()
                        .map(|(name, span)| tree.insert(node::ModuleName::new(name), span))
                        .collect::<Vec<_>>();

                    Some(tree.insert(node::ModulePath(module_path), module_loc))
                } else {
                    None
                };

                node::TypePath { path, ty }
            })
            .to_node()
            .to_type_expr()
            .boxed();

        let row_var = ctrl(CtrlT::PIPE)
            .ignore_then(type_name_parser())
            .or_not()
            .boxed();

        let field = value_name_parser()
            .then_ignore(ctrl(CtrlT::COLON))
            .then(ty.clone())
            .map_to_node(|(name, ty)| node::RecordFieldType { name, ty });

        let record = nested_parser(
            field
                .separated_by(ctrl(CtrlT::COMMA))
                .at_least(1)
                .allow_trailing()
                .collect()
                .then(row_var.clone())
                .map_to_node(|(fields, extension)| node::RecordType { fields, extension })
                .to_type_expr(),
            Delim::Brace,
            |_span| node::TypeError,
        );

        let case = value_name_parser()
            .then(ctrl(CtrlT::COLON).ignore_then(ty.clone()).or_not())
            .map_to_node(|(name, ty)| node::VariantCaseType { name, ty });

        let variant = nested_parser(
            case.separated_by(ctrl(CtrlT::COMMA))
                .at_least(1)
                .allow_trailing()
                .collect()
                .then(row_var)
                .map_to_node(|(cases, extension)| node::VariantType { cases, extension })
                .to_type_expr(),
            Delim::Angle,
            |_span| node::TypeError,
        );

        let atom = choice((
            path,
            record,
            variant,
            nested_parser(ty.clone(), Delim::Paren, |_| node::TypeError),
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
                ctrl(CtrlT::ARROW).ignore_then(ty.clone()).repeated(),
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
pub fn type_parser<'t>() -> impl KolaParser<'t, Id<node::Type>> + Clone {
    // hindley milner only allows standard polymorphism (top-level forall)
    // higher-rank polymorphism (nested forall) is undecidable for full type-inference
    kw(KwT::FORALL)
        .ignore_then(
            symbol()
                .map_to_node(node::TypeVar)
                .repeated()
                .at_least(1)
                .collect(),
        )
        .then_ignore(ctrl(CtrlT::DOT))
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
pub fn type_bind_parser<'t>() -> impl KolaParser<'t, Id<node::TypeBind>> + Clone {
    kw(KwT::TYPE)
        .ignore_then(type_name_parser())
        .then_ignore(op(OpT::ASSIGN))
        .then(type_parser())
        .map_to_node(|(name, ty)| node::TypeBind { name, ty })
        .boxed()
}

pub fn nested_parser<'t, T, U>(
    parser: impl KolaParser<'t, Id<T>> + 't,
    delim: Delim,
    fallback: impl Fn(Loc) -> U + Clone + 't,
) -> impl KolaParser<'t, Id<T>> + Clone
where
    Node: From<T> + From<U>,
    T: From<Id<U>> + MetaCast<LocPhase, Meta = Loc> + 't,
    U: MetaCast<LocPhase, Meta = Loc> + 't,
{
    let (open, close) = match delim {
        Delim::Paren => (OpenT::PAREN, CloseT::PAREN),
        Delim::Bracket => (OpenT::BRACKET, CloseT::BRACKET),
        Delim::Brace => (OpenT::BRACE, CloseT::BRACE),
        Delim::Angle => (OpenT::ANGLE, CloseT::ANGLE),
    };

    nested_in_parser(open, close, parser, fallback)
}

pub fn nested_in_parser<'t, T, U>(
    open: OpenT<'t>,
    close: CloseT<'t>,
    parser: impl KolaParser<'t, Id<T>> + 't,
    fallback: impl Fn(Loc) -> U + Clone + 't,
) -> impl KolaParser<'t, Id<T>> + Clone
where
    Node: From<T> + From<U>,
    T: From<Id<U>> + MetaCast<LocPhase, Meta = Loc> + 't,
    U: MetaCast<LocPhase, Meta = Loc> + 't,
{
    parser
        .delimited_by(open_delim(open), close_delim(close))
        .recover_with(via_parser(
            nested_delimiters(
                open.0,
                close.0,
                [
                    (OpenT::PAREN.0, CloseT::PAREN.0),
                    (OpenT::BRACKET.0, CloseT::BRACKET.0),
                    (OpenT::BRACE.0, CloseT::BRACE.0),
                ],
                fallback,
            )
            .to_node()
            .map(T::from)
            .to_node(),
        ))
        .boxed()
}
#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;

    use kola_span::SourceId;
    use kola_tree::{inspector::NodeInspector, prelude::*};
    use kola_utils::interner::{PathInterner, StrInterner};

    use super::{
        expr_parser, module_parser, module_type_parser, pat_parser, type_bind_parser,
        type_expr_parser, type_parser,
    };
    use crate::{
        lexer::{LexInput, try_tokenize},
        parser::{KolaParser, ParseInput, ParseResult, try_parse_with},
    };

    fn mocked_source() -> SourceId {
        let mut interner = PathInterner::new();
        interner.intern(Utf8PathBuf::from("test"))
    }

    fn try_parse_str_with<'t, T>(
        text: &'t str,
        parser: impl KolaParser<'t, T> + 't,
        interner: &'t mut StrInterner,
    ) -> ParseResult<T> {
        let source = mocked_source();
        let input = LexInput { source, text };
        let tokens = try_tokenize(input).unwrap();
        let input = ParseInput::new(source, tokens, text.len());

        try_parse_with(input, parser, interner).unwrap()
    }

    #[test]
    fn pat() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("{ a: x, b: { y }, c: _, d }", pat_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);

        let record = inspector.as_record().unwrap();

        record
            .field_named("a")
            .unwrap()
            .pattern()
            .unwrap()
            .as_ident()
            .unwrap()
            .has_name("x");

        assert!(
            record
                .field_named("b")
                .unwrap()
                .pattern()
                .unwrap()
                .as_record()
                .unwrap()
                .field_named("y")
                .unwrap()
                .pattern()
                .is_none()
        );

        record
            .field_named("c")
            .unwrap()
            .pattern()
            .unwrap()
            .as_any()
            .unwrap()
            .is_any();

        assert!(record.field_named("d").unwrap().pattern().is_none());
    }

    #[test]
    fn case_expr() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } = try_parse_str_with(
            "case x of 1 => true, _ => false",
            expr_parser(),
            &mut interner,
        );

        let inspector = NodeInspector::new(node, &builder, &interner);

        let case = inspector.as_case().unwrap();

        case.source().as_path().unwrap().has_binding_name("x");

        case.has_branches(2);

        case.branch_at(0).pat().as_literal().unwrap().is_num(1.0);

        case.branch_at(0)
            .matches()
            .as_literal()
            .unwrap()
            .is_bool(true);

        case.branch_at(1).pat().as_any().unwrap().is_any();

        case.branch_at(1)
            .matches()
            .as_literal()
            .unwrap()
            .is_bool(false);
    }

    #[test]
    fn func_expr() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("fn name => \"Hello\" + name", expr_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);

        inspector
            .as_lambda()
            .unwrap()
            .has_param("name")
            .body()
            .as_binary()
            .unwrap()
            .has_op(node::BinaryOp::Add);
    }

    #[test]
    fn arithmetic_expr() {
        let mut interner = StrInterner::new();

        // ((-4 * 10) + (40 / 4)) + 30 = 0
        let ParseResult { node, builder, .. } =
            try_parse_str_with("-4 * 10 + 40 / 4 + 30 == 0", expr_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);

        // _ == 0
        let eq = inspector.as_binary().unwrap();
        eq.has_op(node::BinaryOp::Eq);

        // Right side is 0
        eq.right().as_literal().unwrap().is_num(0.0);

        // Left side: _ + 30
        let sum = eq.left().as_binary().unwrap();
        sum.has_op(node::BinaryOp::Add);

        // Right side of sum is 30
        sum.right().as_literal().unwrap().is_num(30.0);

        // Left side of sum: (_) + (_)
        let sum2 = sum.left().as_binary().unwrap();
        sum2.has_op(node::BinaryOp::Add);

        // Left side of sum2: (-4 * 10)
        let mul = sum2.left().as_binary().unwrap();
        mul.has_op(node::BinaryOp::Mul);

        // Right side of sum2: (40 / 4)
        let div = sum2.right().as_binary().unwrap();
        div.has_op(node::BinaryOp::Div);
    }

    #[test]
    fn if_expr() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("if y then x else 0", expr_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);

        let if_expr = inspector.as_if().unwrap();

        if_expr.predicate().as_path().unwrap().has_binding_name("y");
        if_expr.then().as_path().unwrap().has_binding_name("x");
        if_expr.or().as_literal().unwrap().is_num(0.0);
    }

    #[test]
    fn path_expr() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("x.y.z", expr_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);

        inspector
            .as_path()
            .unwrap()
            .has_binding_name("x")
            .has_segments(2)
            .segment_at_is(0, "y")
            .segment_at_is(1, "z");
    }

    #[test]
    fn record_extension() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("{ y | +x = 10 }", expr_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);

        let extend = inspector.as_record_extend().unwrap();

        extend.field();

        extend.source().as_path().unwrap().has_binding_name("y");
        extend.value().as_literal().unwrap().is_num(10.0);
    }

    #[test]
    fn record() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("{ x = 10, y = 20 }", expr_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);

        let record = inspector.as_record().unwrap();
        record.has_fields(2);

        record
            .field_named("x")
            .unwrap()
            .has_field_name("x")
            .value()
            .as_literal()
            .unwrap()
            .is_num(10.0);

        record
            .field_named("y")
            .unwrap()
            .has_field_name("y")
            .value()
            .as_literal()
            .unwrap()
            .is_num(20.0);
    }

    #[test]
    fn type_expr() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } = try_parse_str_with(
            "Num -> { a : Num, b : Num -> Num } -> Str",
            type_expr_parser(),
            &mut interner,
        );

        let inspector = NodeInspector::new(node, &builder, &interner);

        // Num -> (...)
        let func = inspector.as_function().unwrap();

        func.input().as_path().unwrap().has_type_name("Num");

        // { a: Num, ... } -> ...
        let func2 = func.output().as_function().unwrap();

        let record = func2.input().as_record().unwrap();
        record.has_fields(2);

        record
            .field_at(0)
            .has_field_name("a")
            .type_expr()
            .as_path()
            .unwrap()
            .has_type_name("Num");

        record
            .field_at(1)
            .has_field_name("b")
            .type_expr()
            .as_function()
            .unwrap();

        func2.output().as_path().unwrap().has_type_name("Str");
    }

    #[test]
    fn type_application() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } = try_parse_str_with(
            "Map (Num -> Str) (std.List Str)",
            type_expr_parser(),
            &mut interner,
        );

        let inspector = NodeInspector::new(node, &builder, &interner);

        // Map (Num -> Str) @@ (std.List Str)
        let app = inspector.as_application().unwrap();

        // Map @@ Num -> Str
        let inner_app = app.constructor().as_application().unwrap();

        inner_app
            .constructor()
            .as_path()
            .unwrap()
            .has_type_name("Map");

        inner_app.arg().as_function().unwrap();

        // std.List @@ Str
        let list_app = app.arg().as_application().unwrap();

        list_app.constructor().as_path().unwrap();

        list_app.arg().as_path().unwrap();
    }

    #[test]
    fn type_() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } = try_parse_str_with(
            "forall a b . { left : a, right : Num -> b }",
            type_parser(),
            &mut interner,
        );

        let inspector = NodeInspector::new(node, &builder, &interner);

        inspector
            .has_type_vars(2)
            .type_var_at(0)
            .inspect(|name, tree| {
                assert_eq!(interner.get(name.get(tree).0).unwrap(), "a");
            });

        inspector.type_var_at(1).inspect(|name, tree| {
            assert_eq!(interner.get(name.get(tree).0).unwrap(), "b");
        });

        let record = inspector.type_expr().as_record().unwrap();
        record.has_fields(2);

        record
            .field_at(0)
            .has_field_name("left")
            .type_expr()
            .as_path()
            .unwrap()
            .has_type_name("a");

        let right_field = record.field_at(1);
        right_field.has_field_name("right");

        let fn_type = right_field.type_expr().as_function().unwrap();

        fn_type.input().as_path().unwrap().has_type_name("Num");
        fn_type.output().as_path().unwrap().has_type_name("b");
    }

    #[test]
    fn type_bind() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } = try_parse_str_with(
            "type Person = forall a . { id : a, name : Str, age : Num }",
            type_bind_parser(),
            &mut interner,
        );

        let inspector = NodeInspector::new(node, &builder, &interner);

        inspector.has_name("Person");

        let type_node = inspector.type_node();
        type_node.has_type_vars(1);

        let record = type_node.type_expr().as_record().unwrap();
        record.has_fields(3);
    }

    #[test]
    fn variant_type_bind() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } = try_parse_str_with(
            "type Option = forall a b . < Some : a, None | b >",
            type_bind_parser(),
            &mut interner,
        );

        let inspector = NodeInspector::new(node, &builder, &interner);

        inspector.has_name("Option");

        let type_node = inspector.type_node();
        type_node
            .has_type_vars(2)
            .type_var_at(0)
            .inspect(|name, tree| {
                assert_eq!(interner.get(name.get(tree).0).unwrap(), "a");
            });

        let variant = type_node.type_expr().as_variant().unwrap();
        variant.has_cases(2);

        variant
            .case_at(0)
            .has_case_name("Some")
            .type_expr()
            .unwrap()
            .as_path()
            .unwrap()
            .has_type_name("a");

        assert!(
            variant
                .case_at(1)
                .has_case_name("None")
                .type_expr()
                .is_none()
        );

        variant.extension().unwrap().has_name("b");
    }

    #[test]
    fn module() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("{ x = 10, type T = Num }", module_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);

        inspector.has_binds(2);

        inspector
            .bind_at(0)
            .as_value()
            .unwrap()
            .has_name("x")
            .value()
            .as_literal()
            .unwrap()
            .is_num(10.0);

        inspector
            .bind_at(1)
            .as_type()
            .unwrap()
            .has_name("T")
            .type_node()
            .type_expr()
            .as_path()
            .unwrap()
            .has_type_name("Num");
    }

    #[test]
    fn module_type() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } = try_parse_str_with(
            "{ x : Num, type T = Str }",
            module_type_parser(),
            &mut interner,
        );

        let inspector = NodeInspector::new(node, &builder, &interner);

        inspector.has_specs(2);

        inspector
            .spec_at(0)
            .as_value()
            .unwrap()
            .has_name("x")
            .type_node()
            .type_expr()
            .as_path()
            .unwrap()
            .has_type_name("Num");

        inspector
            .spec_at(1)
            .as_type_bind()
            .unwrap()
            .has_name("T")
            .type_node()
            .type_expr()
            .as_path()
            .unwrap()
            .has_type_name("Str");
    }

    #[test]
    fn nested_module() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("{ module M = { x = 10 } }", module_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);

        inspector.has_binds(1);

        let module_bind = inspector.bind_at(0).as_module().unwrap().has_name("M");

        assert!(module_bind.module_type().is_none());

        module_bind
            .value()
            .as_module()
            .unwrap()
            .has_binds(1)
            .bind_at(0)
            .as_value()
            .unwrap()
            .has_name("x")
            .value()
            .as_literal()
            .unwrap()
            .is_num(10.0);
    }

    #[test]
    fn nested_module_with_type() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } = try_parse_str_with(
            "{ module M : { x : Num } = { x = 10 } }",
            module_parser(),
            &mut interner,
        );

        let inspector = NodeInspector::new(node, &builder, &interner);

        inspector.has_binds(1);

        let module_bind = inspector.bind_at(0).as_module().unwrap().has_name("M");

        // Check interface
        let module_type = module_bind.module_type().unwrap();
        module_type.has_specs(1);

        module_type
            .spec_at(0)
            .as_value()
            .unwrap()
            .has_name("x")
            .type_node()
            .type_expr()
            .as_path()
            .unwrap()
            .has_type_name("Num");

        // Check implementation
        module_bind
            .value()
            .as_module()
            .unwrap()
            .has_binds(1)
            .bind_at(0)
            .as_value()
            .unwrap()
            .has_name("x")
            .value()
            .as_literal()
            .unwrap()
            .is_num(10.0);
    }
}
