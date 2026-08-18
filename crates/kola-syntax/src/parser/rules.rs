use kola_combinators::input::Input;
use kola_combinators::parser::Parser;
use kola_combinators::primitive::{Lazy, OpaqueFn, choice, group, lazy};
use kola_combinators::select;
use kola_combinators::{
    combinator::{Combinator, IterCombinator},
    skip::{skip_delimiters, skip_until},
};
use kola_span::Located;
use kola_tree::{node::ValueName, prelude::*};

use super::ParseInput;
use super::ext::{KolaCombinator, Slice};
use super::state::State;
use crate::token::{CloseT, CtrlT, Delim, KwT, LiteralT, OpT, OpenT, Symbol, Token};

use super::primitives::*;

// ---------------------------------------------------------------------------
// Name parsers
// ---------------------------------------------------------------------------

pub const fn functor_name_parser<'t>() -> impl const KolaCombinator<'t, Id<node::FunctorName>> {
    // TODO SCREAMING_CASE ?
    lower_symbol(Symbol::Functor)
        .map(node::FunctorName::new)
        .to_node()
}

pub const fn module_type_name_parser<'t>() -> impl const KolaCombinator<'t, Id<node::ModuleTypeName>>
{
    upper_symbol(Symbol::ModuleType)
        .map(node::ModuleTypeName::new)
        .to_node()
}

pub const fn module_name_parser<'t>() -> impl const KolaCombinator<'t, Id<node::ModuleName>> {
    lower_symbol(Symbol::Module)
        .map(node::ModuleName::new)
        .to_node()
}

pub const fn kind_name_parser<'t>() -> impl const KolaCombinator<'t, Id<node::KindName>> {
    upper_symbol(Symbol::Kind)
        .map(node::KindName::new)
        .to_node()
}

pub const fn type_name_parser<'t>() -> impl const KolaCombinator<'t, Id<node::TypeName>> {
    symbol(Symbol::Type).map(node::TypeName::new).to_node()
}

pub const fn value_name_parser<'t>() -> impl const KolaCombinator<'t, Id<node::ValueName>> {
    lower_symbol(Symbol::Value)
        .map(node::ValueName::new)
        .to_node()
}

pub const fn tag_name_parser<'t>() -> impl const KolaCombinator<'t, Id<node::ValueName>> {
    upper_symbol(Symbol::Tag)
        .map(node::ValueName::new)
        .to_node()
}

// ---------------------------------------------------------------------------
// Nested delimiters (error recovery)
// ---------------------------------------------------------------------------

const DELIM_PAIRS: [(Token, Token); 4] = [
    (OpenT::PAREN.0, CloseT::PAREN.0),
    (OpenT::BRACKET.0, CloseT::BRACKET.0),
    (OpenT::BRACE.0, CloseT::BRACE.0),
    (OpenT::ANGLE.0, CloseT::ANGLE.0),
];

pub const fn nested_parser<'t, T, U>(
    parser: impl Parser<ParseInput<'t>, Id<T>> + Copy,
    delim: Delim,
    fallback: U,
) -> impl const KolaCombinator<'t, Id<T>>
where
    Tree: Allocate<Located<T>, Key = T> + Allocate<Located<U>, Key = U>,
    T: From<Id<U>> + 'static,
    U: Copy + 'static,
{
    let index = match delim {
        Delim::Paren => 0,
        Delim::Bracket => 1,
        Delim::Brace => 2,
        Delim::Angle => 3,
    };

    parser
        .delimited_by(
            open_delim(OpenT(DELIM_PAIRS[index].0)),
            close_delim(CloseT(DELIM_PAIRS[index].1)),
        )
        .catch(
            skip_delimiters(index, DELIM_PAIRS),
            move |loc, report, input| {
                let state: &mut State = input.state();
                state.recovered.append(report);
                state.insert_as::<T, _>(fallback, loc)
            },
        )
}

pub const fn module_parser<'t>() -> OpaqueFn<ParseInput<'t>, Id<node::ModuleBody>> {
    lazy::<ParseInput<'t>, Id<node::ModuleBody>, ModuleCombinator>()
}

#[derive(Debug, Clone, Copy)]
pub struct ModuleCombinator;

impl<'t> Lazy<ParseInput<'t>, Id<node::ModuleBody>> for ModuleCombinator {
    type Combinator = impl const KolaCombinator<'t, Id<node::ModuleBody>>;
    const COMBINATOR: Self::Combinator = {
        let module = module_parser();

        let module_import = select! { Token::Import(id) => id }
            .map_to_node(node::ModuleImport)
            .to_module_expr();

        let functor_args = module_path_parser()
            .separated_by(ctrl(CtrlT::COMMA))
            .at_least(1)
            .collect::<Slice<_>>()
            .map_to_node(node::FunctorArgs);

        let module_path = module_name_parser()
            .then_ignore(ctrl(CtrlT::DOUBLE_COLON).rewind())
            .then_ignore(ctrl(CtrlT::DOUBLE_COLON))
            .repeated()
            .at_least(1)
            .collect::<Slice<_>>()
            .map_to_node(node::ModulePath)
            .or_not();

        let functor_app = nested_parser(
            module_path
                .then(functor_name_parser())
                .then(functor_args)
                .map(|((path, func), args)| node::FunctorApp { path, func, args })
                .to_node()
                .to_module_expr(),
            Delim::Paren,
            node::ModuleError,
        );

        let module_expr = choice((
            functor_app,
            module.to_module_expr(),
            module_import,
            module_path_parser().to_module_expr(),
        ));

        let value_bind = group((
            vis_parser(),
            value_name_parser(),
            ctrl(CtrlT::COLON)
                .ignore_then(type_scheme_parser().throw("Expected type annotation after ':'"))
                .or_not(),
            op(OpT::ASSIGN).ignore_then(expr_parser().throw("Expected expression after '='")),
        ))
        .map_to_node(|(vis, name, ty_scheme, value)| node::ValueBind {
            vis,
            name,
            ty_scheme,
            value,
        })
        .to_bind();

        let type_bind = type_bind_parser().to_bind();

        let module_bind = group((
            vis_parser(),
            kw(KwT::MODULE).ignore_then(module_name_parser()),
            ctrl(CtrlT::COLON)
                .ignore_then(module_type_parser().throw("Expected module type after ':'"))
                .or_not(),
            op(OpT::ASSIGN).ignore_then(module_expr.throw("Expected module expression after '='")),
        ))
        .map_to_node(|(vis, name, ty, value)| node::ModuleBind {
            vis,
            name,
            ty,
            value,
        })
        .to_bind();

        let module_type_bind = group((
            vis_parser(),
            kw(KwT::MODULE).ignore_then(kw(KwT::TYPE).ignore_then(
                module_type_name_parser().throw("Expected module type name after 'module type'"),
            )),
            op(OpT::ASSIGN)
                .ignore_then(module_type_parser().throw("Expected module type after '='")),
        ))
        .map_to_node(|(vis, name, ty)| node::ModuleTypeBind { vis, name, ty })
        .to_bind();

        let functor_param = module_name_parser()
            .then(ctrl(CtrlT::COLON).ignore_then(module_type_parser()))
            .map_to_node(|(name, ty)| node::FunctorParam { name, ty })
            .delimited_by(open_delim(OpenT::PAREN), close_delim(CloseT::PAREN));

        let functor_bind = group((
            vis_parser(),
            kw(KwT::MODULE).ignore_then(kw(KwT::FUNCTOR).ignore_then(
                functor_name_parser().throw("Expected functor name after 'module functor'"),
            )),
            functor_param.repeated().at_least(1).collect::<Slice<_>>(),
            ctrl(CtrlT::DOUBLE_ARROW).ignore_then(module.throw("Expected functor body after '=>'")),
        ))
        .map_to_node(|(vis, name, params, body)| node::FunctorBind {
            vis,
            name,
            params,
            body,
        })
        .to_bind();

        let bind = choice((
            functor_bind,
            module_type_bind,
            module_bind,
            type_bind,
            value_bind,
        ));

        bind.separated_by(ctrl(CtrlT::COMMA))
            .allow_trailing()
            .catch(
                skip_until(|t| *t == CtrlT::COMMA.0),
                |loc, report, input| {
                    let state: &mut State = input.state();
                    state.recovered.append(report);
                    state.insert_as::<node::Bind, _>(node::BindError, loc)
                },
            )
            .collect::<Slice<_>>()
            .map_to_node(node::ModuleBody)
            .delimited_by(open_delim(OpenT::BRACE), close_delim(CloseT::BRACE))
    };
}

pub const fn module_type_parser<'t>() -> OpaqueFn<ParseInput<'t>, Id<node::ModuleType>> {
    lazy::<ParseInput<'t>, Id<node::ModuleType>, ModuleTypeCombinator>()
}

#[derive(Debug, Clone, Copy)]
pub struct ModuleTypeCombinator;

impl<'t> Lazy<ParseInput<'t>, Id<node::ModuleType>> for ModuleTypeCombinator {
    type Combinator = impl const KolaCombinator<'t, Id<node::ModuleType>>;
    const COMBINATOR: Self::Combinator = {
        let module_type = module_type_parser();

        let value_spec = value_name_parser()
            .then(
                ctrl(CtrlT::COLON)
                    .ignore_then(type_scheme_parser().throw("Expected type annotation after ':'")),
            )
            .map_to_node(|(name, ty)| node::ValueSpec { name, ty })
            .to_spec();

        let module_spec = kw(KwT::MODULE)
            .ignore_then(module_name_parser().throw("Expected module name after 'module'"))
            .then(ctrl(CtrlT::COLON).ignore_then(module_type))
            .map_to_node(|(name, ty)| node::ModuleSpec { name, ty })
            .to_spec();

        let spec = choice((value_spec, module_spec));

        let concrete = spec
            .separated_by(ctrl(CtrlT::COMMA))
            .allow_trailing()
            .catch(
                skip_until(|t| *t == CtrlT::COMMA.0),
                |loc, report, input| {
                    let state: &mut State = input.state();
                    state.recovered.append(report);
                    state.insert_as::<node::Spec, _>(node::SpecError, loc)
                },
            )
            .collect::<Slice<_>>()
            .map_to_node(node::ConcreteModuleType)
            .delimited_by(open_delim(OpenT::BRACE), close_delim(CloseT::BRACE))
            .to_module_type();

        let qualified = module_name_parser()
            .then_ignore(ctrl(CtrlT::DOUBLE_COLON))
            .repeated()
            .at_least(1)
            .collect::<Slice<_>>()
            .map_to_node(node::ModulePath)
            .or_not()
            .then(module_type_name_parser())
            .map_to_node(|(path, ty)| node::QualifiedModuleType { path, ty })
            .to_module_type();

        concrete.or(qualified)
    };
}

/// Parser for pattern expressions used in match statements.
///
/// Grammar:
/// ```bnf
/// pat           ::= bind_pat
///                 | wildcard_pat
///                 | literal_pat
///                 | list_pat
///                 | record_pat
///                 | variant_pat
///
/// bind_pat      ::= name
/// wildcard_pat  ::= '_'
/// literal_pat   ::= '(' ')' | num | bool | char | str
///
/// list_pat      ::= '[' (list_element_pat (',' list_element_pat)*)? ']'
/// list_element_pat ::= pat | '...' name?
///
/// record_pat    ::= '{' (record_field_pat (',' record_field_pat)* (',' spread_pat)?)? '}'
/// record_field_pat ::= name (':' pat)?
/// spread_pat    ::= '...' name?
///
/// variant_pat   ::= '<' (variant_case_pat (',' variant_case_pat)*)? '>'
/// variant_case_pat ::= name (':' pat)?
/// ```
pub const fn pat_parser<'t>() -> OpaqueFn<ParseInput<'t>, Id<node::Pat>> {
    lazy::<ParseInput<'t>, Id<node::Pat>, PatCombinator>()
}

#[derive(Debug, Clone, Copy)]
pub struct PatCombinator;

impl<'t> Lazy<ParseInput<'t>, Id<node::Pat>> for PatCombinator {
    type Combinator = impl const KolaCombinator<'t, Id<node::Pat>>;
    const COMBINATOR: Self::Combinator = {
        let pat = pat_parser();

        let bind = value_name_parser().map_to_node(node::BindPat).to_pat();
        let wildcard = ctrl(CtrlT::UNDERSCORE).to(node::AnyPat).to_node().to_pat();
        let literal = literal_parser()
            .map_to_node(node::LiteralPat::from)
            .to_pat();

        // List element pattern: either a pattern or a spread
        let list_element = pat
            .map(node::ListElPat::Pat)
            .or(spread_parser().map(node::ListElPat::Spread))
            .to_node();

        let list = nested_parser(
            list_element
                .separated_by(ctrl(CtrlT::COMMA))
                .allow_trailing()
                .collect::<Slice<_>>()
                .throw("Expected pattern or spread in list pattern")
                .map_to_node(node::ListPat)
                .to_pat(),
            Delim::Bracket,
            node::PatError,
        );

        // Record field pattern
        let field = value_name_parser()
            .then(ctrl(CtrlT::COLON).ignore_then(pat).or_not())
            .map_to_node(|(field, pat)| node::RecordFieldPat { field, pat });

        let record = nested_parser(
            field
                .separated_by(ctrl(CtrlT::COMMA))
                .collect::<Slice<_>>()
                .throw("Expected field pattern in record pattern")
                .then(ctrl(CtrlT::COMMA).then(ctrl(CtrlT::TRIPLE_DOT)).or_not())
                .map_to_node(|(fields, spread)| node::RecordPat {
                    fields,
                    polymorph: spread.is_some(),
                })
                .to_pat(),
            Delim::Brace,
            node::PatError,
        );

        // Variant case pattern
        let none_case = kw(KwT::NONE)
            .map_with(|_, _loc, input: &mut ParseInput<'t>| {
                let span = input.prev_loc();
                let tree: &mut State = input.state();

                let name = ValueName::new(tree.interner.intern("None"));
                let tag = tree.insert(name, span);

                node::VariantTagPat { tag, pat: None }
            })
            .to_node();

        let case = tag_name_parser()
            .then(
                ctrl(CtrlT::COLON)
                    .ignore_then(pat.throw("Expected pattern after tag"))
                    .or_not(),
            )
            .map_to_node(|(tag, pat)| node::VariantTagPat { tag, pat });

        let variant = nested_parser(
            none_case
                .or(case)
                .separated_by(ctrl(CtrlT::COMMA))
                .allow_trailing()
                .collect::<Slice<_>>()
                .throw("Expected variant case pattern")
                .map_to_node(node::VariantPat)
                .to_pat(),
            Delim::Angle,
            node::PatError,
        );

        choice((bind, wildcard, literal, list, record, variant))
    };
}

#[derive(Debug, Clone, Copy)]
pub struct ExprAtomCombinator;

impl<'t> Lazy<ParseInput<'t>, Id<node::Expr>> for ExprAtomCombinator {
    type Combinator = impl const KolaCombinator<'t, Id<node::Expr>>;
    const COMBINATOR: Self::Combinator = {
        let expr = expr_parser();

        #[derive(Debug)]
        enum RecordOp {
            Extend(
                Id<node::FieldPath>,
                Option<Id<node::TypeExpr>>,
                Id<node::Expr>,
            ),
            Restrict(Id<node::FieldPath>, Option<Id<node::TypeExpr>>),
            Update(
                Id<node::FieldPath>,
                Option<Id<node::TypeExpr>>,
                Id<node::RecordUpdateOp>,
                Id<node::Expr>,
            ),
        }

        let extend = group((
            op(OpT::ADD).ignore_then(field_path_parser()),
            ctrl(CtrlT::COLON).ignore_then(type_parser()).or_not(),
            op(OpT::ASSIGN).ignore_then(expr),
        ))
        .map(|(select, type_, value)| RecordOp::Extend(select, type_, value));

        let restrict = op(OpT::SUB)
            .ignore_then(field_path_parser())
            .then(ctrl(CtrlT::COLON).ignore_then(type_parser()).or_not())
            .map(|(select, type_)| RecordOp::Restrict(select, type_));

        let update_op = choice((
            op(OpT::ASSIGN).to(node::RecordUpdateOp::Assign),
            op(OpT::ADD_ASSIGN).to(node::RecordUpdateOp::AddAssign),
            op(OpT::SUB_ASSIGN).to(node::RecordUpdateOp::SubAssign),
            op(OpT::MUL_ASSIGN).to(node::RecordUpdateOp::MulAssign),
            op(OpT::DIV_ASSIGN).to(node::RecordUpdateOp::DivAssign),
            op(OpT::REM_ASSIGN).to(node::RecordUpdateOp::RemAssign),
        ))
        .to_node();

        let update = group((
            field_path_parser(),
            ctrl(CtrlT::COLON).ignore_then(type_parser()).or_not(),
            update_op,
            expr,
        ))
        .map(|(field, type_, op, value)| RecordOp::Update(field, type_, op, value));

        let inner_op = ctrl(CtrlT::PIPE)
            .ignore_then(choice((extend, restrict, update)))
            .repeated()
            .at_least(1);

        let record_op = qualified_parser()
            .then(ctrl(CtrlT::COLON).ignore_then(type_parser()).or_not())
            .foldl_with(
                inner_op,
                |(source, source_type), op, span, input: &mut ParseInput<'t>| {
                    let tree: &mut State = input.state();

                    let expr = match op {
                        RecordOp::Extend(field_path, value_type, value) => tree
                            .insert_as::<node::Expr, _>(
                                node::RecordExtendExpr {
                                    source,
                                    source_type,
                                    field_path,
                                    value,
                                    value_type,
                                },
                                span,
                            ),
                        RecordOp::Restrict(field_path, value_type) => tree
                            .insert_as::<node::Expr, _>(
                                node::RecordRestrictExpr {
                                    source,
                                    source_type,
                                    field_path,
                                    value_type,
                                },
                                span,
                            ),
                        RecordOp::Update(field_path, value_type, op, value) => tree
                            .insert_as::<node::Expr, _>(
                                node::RecordUpdateExpr {
                                    source,
                                    source_type,
                                    field_path,
                                    op,
                                    value,
                                    value_type,
                                },
                                span,
                            ),
                    };

                    (expr, None)
                },
            )
            .map(|(expr, _)| expr);

        let none = kw(KwT::NONE)
            .map_with(|_, _loc, input: &mut ParseInput<'t>| {
                let span = input.prev_loc();
                let tree: &mut State = input.state();

                let name = ValueName::new(tree.interner.intern("None"));
                let name_id = tree.insert(name, span);
                let tag = tree.insert_as::<node::Expr, _>(node::TagExpr(name_id), span);
                let arg = tree.insert_as::<node::Expr, _>(node::LiteralExpr::Unit, span);

                node::CallExpr { func: tag, arg }
            })
            .to_node()
            .to_expr();

        let tag = tag_name_parser().map_to_node(node::TagExpr).to_expr();

        let module_path = module_name_parser()
            .then_ignore(ctrl(CtrlT::DOUBLE_COLON).rewind())
            .then_ignore(ctrl(CtrlT::DOUBLE_COLON))
            .repeated()
            .at_least(1)
            .collect::<Slice<_>>()
            .map_to_node(node::ModulePath)
            .or_not();

        let type_wit = ctrl(CtrlT::AT)
            .ignore_then(
                module_path
                    .then(type_name_parser())
                    .throw("Expected type name after '@'"),
            )
            .map(|(path, ty)| node::QualifiedType { path, ty })
            .to_node()
            .map_to_node(node::TypeWitnessExpr::Qualified)
            .or(ctrl(CtrlT::TICK)
                .ignore_then(value_name_parser())
                .map_to_node(node::TypeWitnessExpr::Label))
            .to_expr();

        let literal = literal_parser().to_node().to_expr();

        let list = nested_parser(
            expr.separated_by(ctrl(CtrlT::COMMA))
                .allow_trailing()
                .collect::<Slice<_>>()
                .throw("Expected expression in list literal")
                .map_to_node(node::ListExpr)
                .to_expr(),
            Delim::Bracket,
            node::ExprError,
        );

        let field = group((
            value_name_parser(),
            ctrl(CtrlT::COLON).ignore_then(type_parser()).or_not(),
            op(OpT::ASSIGN).ignore_then(expr),
        ))
        .map(|(label, ty, value)| node::RecordField { label, ty, value })
        .to_node();

        let instantiate = field
            .separated_by(ctrl(CtrlT::COMMA))
            .allow_trailing()
            .collect::<Slice<_>>()
            .map_to_node(node::RecordExpr)
            .to_expr();

        let record_expr = nested_parser(
            record_op
                .or(instantiate)
                .throw("Expected record expression"),
            Delim::Brace,
            node::ExprError,
        );

        let let_ = group((
            kw(KwT::LET)
                .ignore_then(value_name_parser().throw("Expected variable name after 'let'")),
            ctrl(CtrlT::COLON)
                .ignore_then(type_parser().throw("Expected type after ':'"))
                .or_not(),
            op(OpT::ASSIGN).ignore_then(expr.throw("Expected expression after '='")),
            kw(KwT::IN).ignore_then(expr.throw("Expected expression after 'in'")),
        ))
        .map_to_node(|(name, value_type, value, body)| node::LetExpr {
            name,
            value_type,
            value,
            body,
        })
        .to_expr();

        let if_ = group((
            kw(KwT::IF).ignore_then(expr.throw("Expected condition after 'if'")),
            kw(KwT::THEN).ignore_then(expr.throw("Expected expression after 'then'")),
            kw(KwT::ELSE).ignore_then(expr.throw("Expected expression after 'else'")),
        ))
        .map_to_node(|(pred, then, or_else)| node::IfExpr {
            pred,
            then,
            or_else,
        })
        .to_expr();

        let branch = pat_parser()
            .then(ctrl(CtrlT::DOUBLE_ARROW).ignore_then(expr))
            .map(|(pat, body)| node::CaseBranch { pat, body })
            .to_node();

        let branches = ctrl(CtrlT::PIPE)
            .ignore_then(branch)
            .repeated()
            .at_least(1)
            .collect::<Slice<_>>();

        let case = kw(KwT::CASE)
            .ignore_then(qualified_parser().throw("Expected qualified name after 'case'"))
            .then(branches.throw("Expected case branches after 'case'"))
            .map_to_node(|(source, branches)| node::CaseExpr { source, branches })
            .to_expr();

        let do_expr = kw(KwT::DO)
            .ignore_then(value_name_parser().throw("Expected operation name after 'do'"))
            .then(expr.throw("Expected argument expression after operation name"))
            .map_to_node(|(op, arg)| node::DoExpr { op, arg })
            .to_expr();

        let handle_call_expr = nested_parser(
            expr.repeated()
                .at_least(2)
                .collect::<Vec<_>>()
                .map_with(|exprs, span, input: &mut ParseInput<'t>| {
                    let tree: &mut State = input.state();

                    let mut iter = exprs.into_iter();
                    let first = iter.next().unwrap();

                    iter.fold(first, |func, arg| {
                        tree.insert_as::<node::Expr, _>(node::CallExpr { func, arg }, span)
                    })
                })
                .throw("Expected a call expression"),
            Delim::Paren,
            node::ExprError,
        );

        let clause = group((
            value_name_parser(),
            value_name_parser(),
            ctrl(CtrlT::DOUBLE_ARROW).ignore_then(expr),
        ))
        .map_to_node(|(op, param, body)| node::HandlerClause { op, param, body });

        let clauses = ctrl(CtrlT::PIPE)
            .ignore_then(clause)
            .repeated()
            .at_least(1)
            .collect::<Slice<_>>();

        let handle = kw(KwT::HANDLE)
            .ignore_then(
                do_expr
                    .or(handle_call_expr)
                    .throw("Expected a 'do' expression or a call expression after 'handle'"),
            )
            .then(clauses.throw("Expected handler clauses after 'handle <expr>'"))
            .map_to_node(|(source, clauses)| node::HandleExpr { source, clauses })
            .to_expr();

        let func = group((
            kw(KwT::FN)
                .ignore_then(value_name_parser().throw("Expected parameter name after 'fn'")),
            ctrl(CtrlT::COLON)
                .ignore_then(type_parser().throw("Expected parameter type after ':'"))
                .or_not(),
            ctrl(CtrlT::DOUBLE_ARROW).ignore_then(expr.throw("Expected function body after '=>'")),
        ))
        .map_to_node(|(param, param_type, body)| node::LambdaExpr {
            param,
            param_type,
            body,
        })
        .to_expr();

        let paren_expr = nested_parser(
            expr.repeated().at_least(1).collect::<Vec<_>>().map_with(
                |exprs, span, input: &mut ParseInput<'t>| {
                    let tree: &mut State = input.state();

                    let len = exprs.len();
                    let mut iter = exprs.into_iter();
                    let first = iter.next().unwrap();

                    if len == 1 {
                        first
                    } else {
                        iter.fold(first, |acc, arg| {
                            tree.insert_as::<node::Expr, _>(node::CallExpr { func: acc, arg }, span)
                        })
                    }
                },
            ),
            Delim::Paren,
            node::ExprError,
        );

        choice((
            literal,
            list,
            record_expr,
            let_,
            if_,
            case,
            handle,
            do_expr,
            func,
            paren_expr,
            qualified_parser(),
            none,
            tag,
            type_wit,
        ))
    };
}

pub const fn expr_parser<'t>() -> OpaqueFn<ParseInput<'t>, Id<node::Expr>> {
    lazy::<ParseInput<'t>, Id<node::Expr>, ExprCombinator>()
}

#[derive(Debug, Clone, Copy)]
pub struct ExprCombinator;

impl<'t> Lazy<ParseInput<'t>, Id<node::Expr>> for ExprCombinator {
    type Combinator = impl const KolaCombinator<'t, Id<node::Expr>>;
    const COMBINATOR: Self::Combinator = {
        use kola_combinators::pratt::PrattNil;

        let atom = lazy::<ParseInput<'t>, Id<node::Expr>, ExprAtomCombinator>();

        let ops = PrattNil
            // Pipe forward (|>) - left associative, lower precedence
            // Level 10
            .infix(
                ctrl(CtrlT::PIPE_FORWARD),
                10,
                11,
                |lhs, _op, rhs, loc: kola_span::Loc, input: &mut ParseInput<'t>| {
                    let tree: &mut State = input.state();
                    tree.insert_as::<node::Expr, _>(
                        node::CallExpr {
                            func: rhs,
                            arg: lhs,
                        },
                        loc,
                    )
                },
            )
            // Pipe backward (<|) - right associative, higher precedence
            // Level 20
            .infix(
                ctrl(CtrlT::PIPE_BACKWARD),
                20,
                20,
                |func, _op, arg, loc: kola_span::Loc, input: &mut ParseInput<'t>| {
                    let tree: &mut State = input.state();
                    tree.insert_as::<node::Expr, _>(node::CallExpr { func, arg }, loc)
                },
            )
            // Logical (||, &&) - left associative
            // Level 30
            .infix(
                choice((
                    op(OpT::AND).to(node::BinaryOp::And),
                    op(OpT::OR).to(node::BinaryOp::Or),
                ))
                .to_node(),
                30,
                31,
                |lhs, op, rhs, loc: kola_span::Loc, input: &mut ParseInput<'t>| {
                    let tree: &mut State = input.state();
                    tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, lhs, rhs }, loc)
                },
            )
            // Comparison (<, <=, >, >=, ==, !=) - left associative
            // Level 40
            .infix(
                choice((
                    op(OpT::LESS).to(node::BinaryOp::Less),
                    op(OpT::LESS_EQ).to(node::BinaryOp::LessEq),
                    op(OpT::GREATER).to(node::BinaryOp::Greater),
                    op(OpT::GREATER_EQ).to(node::BinaryOp::GreaterEq),
                    op(OpT::EQ).to(node::BinaryOp::Eq),
                    op(OpT::NOT_EQ).to(node::BinaryOp::NotEq),
                ))
                .to_node(),
                40,
                41,
                |lhs, op, rhs, loc: kola_span::Loc, input: &mut ParseInput<'t>| {
                    let tree: &mut State = input.state();
                    tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, lhs, rhs }, loc)
                },
            )
            // Merge - left associative
            // Level 50
            .infix(
                op(OpT::MERGE),
                50,
                51,
                |lhs, _op, rhs, loc: kola_span::Loc, input: &mut ParseInput<'t>| {
                    let tree: &mut State = input.state();
                    tree.insert_as::<node::Expr, _>(node::RecordMergeExpr { lhs, rhs }, loc)
                },
            )
            // Concat - left associative
            // Level 60
            .infix(
                op(OpT::CONCAT).to(node::BinaryOp::Concat).to_node(),
                60,
                61,
                |lhs, op, rhs, loc: kola_span::Loc, input: &mut ParseInput<'t>| {
                    let tree: &mut State = input.state();
                    tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, lhs, rhs }, loc)
                },
            )
            // Sum (+, -) - left associative
            // Level 70
            .infix(
                choice((
                    op(OpT::ADD).to(node::BinaryOp::Add),
                    op(OpT::SUB).to(node::BinaryOp::Sub),
                ))
                .to_node(),
                70,
                71,
                |lhs, op, rhs, loc: kola_span::Loc, input: &mut ParseInput<'t>| {
                    let tree: &mut State = input.state();
                    tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, lhs, rhs }, loc)
                },
            )
            // Product (*, /, %) - left associative
            // Level 80
            .infix(
                choice((
                    op(OpT::MUL).to(node::BinaryOp::Mul),
                    op(OpT::DIV).to(node::BinaryOp::Div),
                    op(OpT::REM).to(node::BinaryOp::Rem),
                ))
                .to_node(),
                80,
                81,
                |lhs, op, rhs, loc: kola_span::Loc, input: &mut ParseInput<'t>| {
                    let tree: &mut State = input.state();
                    tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, lhs, rhs }, loc)
                },
            )
            // Unary (-, !) - right associative
            // Level 90
            .prefix(
                choice((
                    op(OpT::SUB).to(node::UnaryOp::Neg),
                    op(OpT::NOT).to(node::UnaryOp::Not),
                ))
                .to_node(),
                90,
                |op, operand, loc: kola_span::Loc, input: &mut ParseInput<'t>| {
                    let tree: &mut State = input.state();
                    tree.insert_as::<node::Expr, _>(node::UnaryExpr { op, operand }, loc)
                },
            );

        atom.pratt(ops)
    };
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
pub const fn type_parser<'t>() -> OpaqueFn<ParseInput<'t>, Id<node::TypeExpr>> {
    lazy::<ParseInput<'t>, Id<node::TypeExpr>, TypeCombinator>()
}

#[derive(Debug, Clone, Copy)]
pub struct TypeCombinator;

impl<'t> Lazy<ParseInput<'t>, Id<node::TypeExpr>> for TypeCombinator {
    type Combinator = impl const KolaCombinator<'t, Id<node::TypeExpr>>;
    const COMBINATOR: Self::Combinator = {
        let ty = type_parser();

        let module_path = module_name_parser()
            .then_ignore(ctrl(CtrlT::DOUBLE_COLON).rewind())
            .then_ignore(ctrl(CtrlT::DOUBLE_COLON))
            .repeated()
            .at_least(1)
            .collect::<Slice<_>>()
            .map_to_node(node::ModulePath)
            .or_not();

        // TODO this also includes type variables which is a bit surprising
        let qual_ty = module_path
            .then(type_name_parser())
            .map(|(path, ty)| node::QualifiedType { path, ty })
            .to_node()
            .to_type();

        let label = value_name_parser()
            .map_to_node(node::LabelOrVar::Label)
            .or(ctrl(CtrlT::AT)
                .ignore_then(symbol(Symbol::TypeVar))
                .map_to_node(node::TypeVar)
                .map_to_node(node::LabelOrVar::Var));

        let field = label
            .then(ctrl(CtrlT::COLON).ignore_then(ty))
            .map_to_node(|(label_or_var, ty)| node::RecordFieldType { label_or_var, ty });

        let record = nested_parser(
            field
                .separated_by(ctrl(CtrlT::COMMA))
                .allow_trailing()
                .collect::<Slice<_>>()
                .throw("Expected field in record type")
                .then(row_var_parser())
                .map_to_node(|(fields, extension)| node::RecordType { fields, extension })
                .to_type(),
            Delim::Brace,
            node::TypeError,
        );

        let none_tag = kw(KwT::NONE)
            .map_with(|_, _loc, input: &mut ParseInput<'t>| {
                let span = input.prev_loc();
                let tree: &mut State = input.state();

                let name = ValueName::new(tree.interner.intern("None"));
                let tag = tree.insert(name, span);

                node::TagType {
                    name: tag,
                    ty: None,
                }
            })
            .to_node();

        let tag = tag_name_parser()
            .then(
                ctrl(CtrlT::COLON)
                    .ignore_then(ty.throw("Expected type annotation after ':'"))
                    .or_not(),
            )
            .map_to_node(|(name, ty)| node::TagType { name, ty });

        let variant = nested_parser(
            none_tag
                .or(tag)
                .separated_by(ctrl(CtrlT::COMMA))
                .allow_trailing()
                .collect::<Slice<_>>()
                .throw("Expected tag in variant type")
                .then(row_var_parser())
                .map_to_node(|(tags, extension)| node::VariantType { tags, extension })
                .to_type(),
            Delim::Angle,
            node::TypeError,
        );

        let atom = choice((
            qual_ty,
            record,
            variant,
            nested_parser(ty, Delim::Paren, node::TypeError),
        ));

        let appl = atom.foldl_with(
            atom.repeated(),
            |constructor, arg, span, input: &mut ParseInput<'t>| {
                let tree: &mut State = input.state();
                tree.insert_as::<node::TypeExpr, _>(
                    node::TypeApplication { constructor, arg },
                    span,
                )
            },
        );

        let effect_op = value_name_parser()
            .then(ctrl(CtrlT::COLON).ignore_then(ty.throw("Expected type annotation after ':'")))
            .map_to_node(|(name, ty)| node::EffectOpType { name, ty });

        let effect = effect_op
            .separated_by(ctrl(CtrlT::COMMA))
            .allow_trailing()
            .collect::<Slice<_>>()
            .delimited_by(open_delim(OpenT::BRACE), close_delim(CloseT::BRACE))
            .map_to_node(node::EffectType);

        let computation = ty
            .then(ctrl(CtrlT::TILDE).ignore_then(effect).or_not())
            .map_to_node(|(ty, effect)| node::CompType { ty, effect });

        let func = appl.foldl_with(
            ctrl(CtrlT::ARROW).ignore_then(computation).repeated(),
            |input_ty, output, span, input: &mut ParseInput<'t>| {
                let tree: &mut State = input.state();

                tree.insert_as::<node::TypeExpr, _>(
                    node::FuncType {
                        input: input_ty,
                        output,
                    },
                    span,
                )
            },
        );

        func
    };
}

// ---------------------------------------------------------------------------
// Shared sub-parser helpers (nested const fn)
//
// Each call produces a fresh value — no Clone, no borrow, and the `impl`
// return type acts as a type-size firewall for the compiler.
// ---------------------------------------------------------------------------

const fn vis_parser<'t>() -> impl const KolaCombinator<'t, Id<node::Vis>> {
    kw(KwT::EXPORT)
        .to(node::Vis::Export)
        .or_not()
        .map_to_node(|vis| vis.unwrap_or(node::Vis::None))
}

const fn module_path_parser<'t>() -> impl const KolaCombinator<'t, Id<node::ModulePath>> {
    module_name_parser()
        .separated_by(ctrl(CtrlT::DOUBLE_COLON))
        .collect::<Slice<_>>()
        .map_to_node(node::ModulePath)
}

const fn field_path_parser<'t>() -> impl const KolaCombinator<'t, Id<node::FieldPath>> {
    value_name_parser()
        .separated_by(ctrl(CtrlT::DOT))
        .collect::<Slice<_>>()
        .map_to_node(node::FieldPath)
}

/// Parser for qualified expressions: `module::record.field`
const fn qualified_parser<'t>() -> impl const KolaCombinator<'t, Id<node::Expr>> {
    let module_path = module_name_parser()
        .then_ignore(ctrl(CtrlT::DOUBLE_COLON).rewind())
        .then_ignore(ctrl(CtrlT::DOUBLE_COLON))
        .repeated()
        .at_least(1)
        .collect::<Slice<_>>()
        .map_to_node(node::ModulePath)
        .or_not();

    let field_path = ctrl(CtrlT::DOT)
        .ignore_then(value_name_parser())
        .repeated()
        .at_least(1)
        .collect::<Slice<_>>()
        .map_to_node(node::FieldPath)
        .or_not();

    group((module_path, value_name_parser(), field_path))
        .map(|(module_path, source, field_path)| node::QualifiedExpr {
            module_path,
            source,
            field_path,
        })
        .to_node()
        .to_expr()
}

const fn spread_parser<'t>() -> impl const KolaCombinator<'t, Option<Id<node::ValueName>>> {
    ctrl(CtrlT::TRIPLE_DOT).ignore_then(value_name_parser().or_not())
}

const fn row_var_parser<'t>() -> impl const KolaCombinator<'t, Option<Id<node::TypeName>>> {
    ctrl(CtrlT::PIPE).ignore_then(type_name_parser()).or_not()
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
pub const fn literal_parser<'t>() -> impl const KolaCombinator<'t, node::LiteralExpr> {
    literal().map_with(|l, _loc, input: &mut ParseInput<'t>| {
        let state: &mut State = input.state();
        match l {
            LiteralT::Unit => node::LiteralExpr::Unit,
            LiteralT::Num(n) => node::LiteralExpr::Num(n),
            LiteralT::Bool(b) => node::LiteralExpr::Bool(b),
            LiteralT::Str(s) => {
                let s = unescaper::unescape(s).unwrap(); // TODO maybe handle this ?
                node::LiteralExpr::Str(state.intern(s))
            }
        }
    })
}

/// Parser for types in the language, which may include polymorphic type variables.
///
/// Grammar:
/// ```bnf
/// type      ::= 'forall' name+ '.' type_expression
///             | type_expression
/// ```
pub const fn type_scheme_parser<'t>() -> impl const KolaCombinator<'t, Id<node::TypeScheme>> {
    const fn type_var_bind<'t>() -> impl const KolaCombinator<'t, Id<node::TypeVarBind>> {
        let var = symbol(Symbol::TypeVar).map_to_node(node::TypeVar);

        let kinded = symbol(Symbol::TypeVar)
            .map_to_node(node::TypeVar)
            .then(
                ctrl(CtrlT::COLON)
                    .ignore_then(kind_name_parser().throw("Expected kind annotation after ':'")),
            )
            .delimited_by(open_delim(OpenT::PAREN), close_delim(CloseT::PAREN))
            .map_to_node(|(var, kind)| node::TypeVarBind {
                var,
                kind: Some(kind),
            });

        var.map_to_node(|var| node::TypeVarBind { var, kind: None })
            .or(kinded)
    }

    // hindley milner only allows standard polymorphism (top-level forall)
    // higher-rank polymorphism (nested forall) is undecidable for full type-inference
    let forall = kw(KwT::FORALL)
        .ignore_then(
            type_var_bind()
                .repeated()
                .at_least(1)
                .collect::<Slice<_>>()
                .throw("Expected at least one type variable after 'forall'"),
        )
        .then_ignore(ctrl(CtrlT::DOT))
        .map_to_node(node::ForallBinder);

    forall
        .or_not()
        .then(type_parser())
        .map_to_node(|(forall, ty)| node::TypeScheme { forall, ty })
}

/// Parser for type bindings in the language.
///
/// Grammar:
/// ```bnf
/// type_bind ::= 'type' name '=' type
/// ```
pub const fn type_bind_parser<'t>() -> impl const KolaCombinator<'t, Id<node::TypeBind>> {
    group((
        vis_parser(),
        kw(KwT::TYPE).ignore_then(type_name_parser()),
        op(OpT::ASSIGN).ignore_then(type_scheme_parser()),
    ))
    .map_to_node(|(vis, name, ty_scheme)| node::TypeBind {
        vis,
        name,
        ty_scheme,
    })
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;
    use std::sync::Arc;

    use camino::Utf8PathBuf;

    use kola_combinators::parser::Parser;
    use kola_interner::StrInterner;
    use kola_span::SourceManager;
    use kola_span::io::MockFileSystem;
    use kola_tree::prelude::*;

    use super::{
        expr_parser, module_parser, module_type_parser, pat_parser, type_bind_parser, type_parser,
        type_scheme_parser,
    };
    use crate::{
        lexer::{TokenOutput, try_tokenize},
        parser::{ParseInput, ParseResult, try_parse_with},
    };

    fn try_parse_str_with<'t, T, P>(
        text: &'t str,
        parser: P,
        interner: &'t mut StrInterner,
    ) -> ParseResult<T>
    where
        T: Debug,
        P: Parser<ParseInput<'t>, T>,
    {
        let path = Utf8PathBuf::from("test.kola");
        let mut fs = MockFileSystem::new();
        fs.add_file(path.clone(), text.to_owned());

        let source_manager = Box::new(SourceManager::new(Arc::new(fs)));
        let source_manager = Box::leak(source_manager);

        let TokenOutput { root_id, token_map } =
            try_tokenize(source_manager, path.as_path()).unwrap();

        let input = ParseInput::new(root_id, token_map[&root_id].clone(), interner);

        try_parse_with(input, parser).unwrap()
    }

    #[test]
    fn pat() {
        let mut interner = StrInterner::default();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("{ a: x, b: { y }, c: _, d }", pat_parser(), &mut interner);

        // 1. Eagerly retrieve the fields SliceId with zero verbosity
        let fields = node
            .query()
            .cast::<node::RecordPat>()
            .project_slice(|r| r.fields)
            .get_slice(&builder);

        // 2. Query the slice eagerly and cleanly without nested closures

        // Field 0: "a: x"
        query()
            .get_at(0)
            .get_slice_items(&builder, fields)
            .for_each_value(&builder, |f| {
                f.field.assert_name("a", &builder, &interner);
            });

        query_slice(fields)
            .get_at(0)
            .project_opt(|f| f.pat)
            .cast::<node::BindPat>()
            .for_each_value(&builder, |b| {
                b.0.assert_name("x", &builder, &interner);
            });

        // Field 1: "b: { y }"
        query_slice(fields).get_at(1).for_each_value(&builder, |f| {
            f.field.assert_name("b", &builder, &interner);
        });

        query_slice(fields)
            .get_at(1)
            .project_opt(|f| f.pat)
            .cast::<node::RecordPat>()
            .project_slice(|r| r.fields)
            .get_at(0)
            .for_each_value(&builder, |f| {
                f.field.assert_name("y", &builder, &interner);
                assert!(f.pat.is_none());
            });

        // Field 2: "c: _"
        query_slice(fields).get_at(2).for_each_value(&builder, |f| {
            f.field.assert_name("c", &builder, &interner);
        });

        // Retrieve AnyPat to assert its presence
        let _any = query_slice(fields)
            .get_at(2)
            .project_opt(|f| f.pat)
            .cast::<node::AnyPat>()
            .get_one(&builder);

        // Field 3: "d"
        query_slice(fields).get_at(3).for_each_value(&builder, |f| {
            f.field.assert_name("d", &builder, &interner);
            assert!(f.pat.is_none());
        });
    }
    //     #[test]
    //     fn case_expr() {
    //         let mut interner = StrInterner::default();

    //         let ParseResult { node, builder, .. } = try_parse_str_with(
    //             "case x | 1 => true | _ => false",
    //             expr_parser(),
    //             &mut interner,
    //         );

    //         query_one::<node::Expr>(node)
    //             .cast::<node::CaseExpr>()
    //             .branch(&builder, |case| {
    //                 // Assert on source "x"
    //                 case.project(|case| case.source)
    //                     .cast::<node::QualifiedExpr>()
    //                     .inspect(|_, source| source.source.assert_name("x", &builder, &interner))
    //                     .execute(&builder)
    //                     .next();

    //                 // Assert on branches
    //                 case.project_slice(|case| case.branches)
    //                     .branch(&builder, |branches| {
    //                         // Branch 0: "1 => true"
    //                         branches
    //                             .get_at(0)
    //                             .inspect(|_, b| {
    //                                 let pat_0 = b.pat.cast::<node::LiteralPat>(&builder);
    //                                 pat_0.assert_eq(&node::LiteralPat::Num(1.0));
    //                                 let expr_0 = b.body.cast::<node::LiteralExpr>(&builder);
    //                                 expr_0.assert_eq(&node::LiteralExpr::Bool(true));
    //                             })
    //                             .execute(&builder)
    //                             .next();

    //                         // Branch 1: "_ => false"
    //                         branches
    //                             .get_at(1)
    //                             .inspect(|_, b| {
    //                                 b.pat.cast::<node::AnyPat>(&builder);
    //                                 let expr_1 = b.body.cast::<node::LiteralExpr>(&builder);
    //                                 expr_1.assert_eq(&node::LiteralExpr::Bool(false));
    //                             })
    //                             .execute(&builder)
    //                             .next();
    //                     });
    //             });
    //     }

    //     #[test]
    //     fn complex_case_expr() {
    //         let mut interner = StrInterner::default();

    //         let test_case = r#"
    //             case data
    //             | () => "unit"
    //             | true => "bool true"
    //             | 42 => "number"
    //             | "hello" => "string"
    //             | x => "bind pattern"
    //             | _ => "wildcard"
    //             | [a, b, ...rest] => "list with spread"
    //             | [head, ...] => "list anonymous spread"
    //             | [] => "empty list"
    //             | { name, age: years, ... } => "record with spread"
    //             | { x, y } => "simple record"
    //             | {} => "empty record"
    //             | < Some : value > => "variant some"
    //             | < None > => "variant none"
    //             | < Ok : result, Err : error > => "multiple variants"
    //         "#;

    //         let ParseResult { node, builder, .. } =
    //             try_parse_str_with(test_case, expr_parser(), &mut interner);

    //         query_one::<node::Expr>(node)
    //             .cast::<node::CaseExpr>()
    //             .branch(&builder, |case| {
    //                 case.project(|case| case.source)
    //                     .cast::<node::QualifiedExpr>()
    //                     .inspect(|_, s| s.source.assert_name("data", &builder, &interner))
    //                     .execute(&builder)
    //                     .next();

    //                 case.project_slice(|case| case.branches)
    //                     .branch(&builder, |branches| {
    //                         // Branch 0: "() => "unit""
    //                         branches
    //                             .get_at(0)
    //                             .inspect(|_, b| {
    //                                 b.pat
    //                                     .cast::<node::LiteralPat>(&builder)
    //                                     .assert_eq(&node::LiteralPat::Unit);
    //                                 b.body
    //                                     .cast::<node::LiteralExpr>(&builder)
    //                                     .assert_eq(&node::LiteralExpr::Str(interner["unit"]));
    //                             })
    //                             .execute(&builder)
    //                             .next();

    //                         // Branch 1: "true => "bool true""
    //                         branches
    //                             .get_at(1)
    //                             .inspect(|_, b| {
    //                                 b.pat
    //                                     .cast::<node::LiteralPat>(&builder)
    //                                     .assert_eq(&node::LiteralPat::Bool(true));
    //                                 b.body
    //                                     .cast::<node::LiteralExpr>(&builder)
    //                                     .assert_eq(&node::LiteralExpr::Str(interner["bool true"]));
    //                             })
    //                             .execute(&builder)
    //                             .next();

    //                         // Branch 2: "42 => "number""
    //                         branches
    //                             .get_at(2)
    //                             .inspect(|_, b| {
    //                                 b.pat
    //                                     .cast::<node::LiteralPat>(&builder)
    //                                     .assert_eq(&node::LiteralPat::Num(42.0));
    //                                 b.body
    //                                     .cast::<node::LiteralExpr>(&builder)
    //                                     .assert_eq(&node::LiteralExpr::Str(interner["number"]));
    //                             })
    //                             .execute(&builder)
    //                             .next();

    //                         // Branch 3: ""hello" => "string""
    //                         branches
    //                             .get_at(3)
    //                             .inspect(|_, b| {
    //                                 b.pat
    //                                     .cast::<node::LiteralPat>(&builder)
    //                                     .assert_eq(&node::LiteralPat::Str(interner["hello"]));
    //                                 b.body
    //                                     .cast::<node::LiteralExpr>(&builder)
    //                                     .assert_eq(&node::LiteralExpr::Str(interner["string"]));
    //                             })
    //                             .execute(&builder)
    //                             .next();

    //                         // Branch 4: "x => "bind pattern""
    //                         branches
    //                             .get_at(4)
    //                             .inspect(|_, b| {
    //                                 b.pat
    //                                     .cast::<node::BindPat>(&builder)
    //                                     .0
    //                                     .assert_name("x", &builder, &interner);
    //                                 b.body
    //                                     .cast::<node::LiteralExpr>(&builder)
    //                                     .assert_eq(&node::LiteralExpr::Str(interner["bind pattern"]));
    //                             })
    //                             .execute(&builder)
    //                             .next();

    //                         // Branch 5: "_ => "wildcard""
    //                         branches
    //                             .get_at(5)
    //                             .inspect(|_, b| {
    //                                 b.pat.cast::<node::AnyPat>(&builder);
    //                                 b.body
    //                                     .cast::<node::LiteralExpr>(&builder)
    //                                     .assert_eq(&node::LiteralExpr::Str(interner["wildcard"]));
    //                             })
    //                             .execute(&builder)
    //                             .next();

    //                         // Branch 6: "[a, b, ...rest] => "list with spread""
    //                         branches
    //                             .get_at(6)
    //                             .inspect(|_, b| {
    //                                 let list = b.pat.cast::<node::ListPat>(&builder);
    //                                 let el = list.0;
    //                                 assert_eq!(el.get(&builder).len(), 3);
    //                                 el.get_at(0, &builder)
    //                                     .cast::<node::Pat>(&builder)
    //                                     .cast::<node::BindPat>(&builder)
    //                                     .0
    //                                     .assert_name("a", &builder, &interner);
    //                                 el.get_at(1, &builder)
    //                                     .cast::<node::Pat>(&builder)
    //                                     .cast::<node::BindPat>(&builder)
    //                                     .0
    //                                     .assert_name("b", &builder, &interner);
    //                                 let node::ListElPat::Spread(some_name) = el.get_at(2, &builder)
    //                                 else {
    //                                     panic!("expected Spread")
    //                                 };
    //                                 some_name.unwrap().assert_name("rest", &builder, &interner);
    //                                 b.body.cast::<node::LiteralExpr>(&builder).assert_eq(
    //                                     &node::LiteralExpr::Str(interner["list with spread"]),
    //                                 );
    //                             })
    //                             .execute(&builder)
    //                             .next();

    //                         // Branch 7: "[head, ...] => "list anonymous spread""
    //                         branches
    //                             .get_at(7)
    //                             .inspect(|_, b| {
    //                                 let list = b.pat.cast::<node::ListPat>(&builder);
    //                                 let el = list.0;
    //                                 assert_eq!(el.get(&builder).len(), 2);
    //                                 el.get_at(0, &builder)
    //                                     .cast::<node::Pat>(&builder)
    //                                     .cast::<node::BindPat>(&builder)
    //                                     .0
    //                                     .assert_name("head", &builder, &interner);
    //                                 assert!(matches!(
    //                                     el.get_at(1, &builder),
    //                                     node::ListElPat::Spread(None)
    //                                 ));
    //                                 b.body.cast::<node::LiteralExpr>(&builder).assert_eq(
    //                                     &node::LiteralExpr::Str(interner["list anonymous spread"]),
    //                                 );
    //                             })
    //                             .execute(&builder)
    //                             .next();

    //                         // Branch 8: "[] => "empty list""
    //                         branches
    //                             .get_at(8)
    //                             .inspect(|_, b| {
    //                                 let list = b.pat.cast::<node::ListPat>(&builder);
    //                                 assert_eq!(list.0.get(&builder).len(), 0);
    //                                 b.body
    //                                     .cast::<node::LiteralExpr>(&builder)
    //                                     .assert_eq(&node::LiteralExpr::Str(interner["empty list"]));
    //                             })
    //                             .execute(&builder)
    //                             .next();

    //                         // Branch 9: "{ name, age: years, ... } => "record with spread""
    //                         branches
    //                             .get_at(9)
    //                             .inspect(|_, b| {
    //                                 let record = b.pat.cast::<node::RecordPat>(&builder);
    //                                 let f = record.fields;
    //                                 assert_eq!(f.get(&builder).len(), 2);
    //                                 f.get_at(0, &builder)
    //                                     .field
    //                                     .assert_name("name", &builder, &interner);
    //                                 assert!(f.get_at(0, &builder).pat.is_none());

    //                                 f.get_at(1, &builder)
    //                                     .field
    //                                     .assert_name("age", &builder, &interner);
    //                                 f.get_at(1, &builder)
    //                                     .pat
    //                                     .unwrap()
    //                                     .cast::<node::BindPat>(&builder)
    //                                     .0
    //                                     .assert_name("years", &builder, &interner);

    //                                 assert!(record.polymorph);
    //                                 b.body.cast::<node::LiteralExpr>(&builder).assert_eq(
    //                                     &node::LiteralExpr::Str(interner["record with spread"]),
    //                                 );
    //                             })
    //                             .execute(&builder)
    //                             .next();

    //                         // Branch 10: "{ x, y } => "simple record""
    //                         branches
    //                             .get_at(10)
    //                             .inspect(|_, b| {
    //                                 let record = b.pat.cast::<node::RecordPat>(&builder);
    //                                 let f = record.fields;
    //                                 assert_eq!(f.get(&builder).len(), 2);
    //                                 f.get_at(0, &builder)
    //                                     .field
    //                                     .assert_name("x", &builder, &interner);
    //                                 f.get_at(1, &builder)
    //                                     .field
    //                                     .assert_name("y", &builder, &interner);
    //                                 b.body
    //                                     .cast::<node::LiteralExpr>(&builder)
    //                                     .assert_eq(&node::LiteralExpr::Str(interner["simple record"]));
    //                             })
    //                             .execute(&builder)
    //                             .next();

    //                         // Branch 11: "{} => "empty record""
    //                         branches
    //                             .get_at(11)
    //                             .inspect(|_, b| {
    //                                 let record = b.pat.cast::<node::RecordPat>(&builder);
    //                                 assert_eq!(record.fields.get(&builder).len(), 0);
    //                                 b.body
    //                                     .cast::<node::LiteralExpr>(&builder)
    //                                     .assert_eq(&node::LiteralExpr::Str(interner["empty record"]));
    //                             })
    //                             .execute(&builder)
    //                             .next();

    //                         // Branch 12: "< Some : value > => "variant some""
    //                         branches
    //                             .get_at(12)
    //                             .inspect(|_, b| {
    //                                 let variant = b.pat.cast::<node::VariantPat>(&builder);
    //                                 let tags = variant.0;
    //                                 assert_eq!(tags.get(&builder).len(), 1);
    //                                 tags.get_at(0, &builder)
    //                                     .tag
    //                                     .assert_name("Some", &builder, &interner);
    //                                 tags.get_at(0, &builder)
    //                                     .pat
    //                                     .unwrap()
    //                                     .cast::<node::BindPat>(&builder)
    //                                     .0
    //                                     .assert_name("value", &builder, &interner);
    //                                 b.body
    //                                     .cast::<node::LiteralExpr>(&builder)
    //                                     .assert_eq(&node::LiteralExpr::Str(interner["variant some"]));
    //                             })
    //                             .execute(&builder)
    //                             .next();

    //                         // Branch 13: "< None > => "variant none""
    //                         branches
    //                             .get_at(13)
    //                             .inspect(|_, b| {
    //                                 let variant = b.pat.cast::<node::VariantPat>(&builder);
    //                                 let tags = variant.0;
    //                                 assert_eq!(tags.get(&builder).len(), 1);
    //                                 tags.get_at(0, &builder)
    //                                     .tag
    //                                     .assert_name("None", &builder, &interner);
    //                                 assert!(tags.get_at(0, &builder).pat.is_none());
    //                                 b.body
    //                                     .cast::<node::LiteralExpr>(&builder)
    //                                     .assert_eq(&node::LiteralExpr::Str(interner["variant none"]));
    //                             })
    //                             .execute(&builder)
    //                             .next();

    //                         // Branch 14: "< Ok : result, Err : error > => "multiple variants""
    //                         branches
    //                             .get_at(14)
    //                             .inspect(|_, b| {
    //                                 let variant = b.pat.cast::<node::VariantPat>(&builder);
    //                                 let tags = variant.0;
    //                                 assert_eq!(tags.get(&builder).len(), 2);
    //                                 tags.get_at(0, &builder)
    //                                     .tag
    //                                     .assert_name("Ok", &builder, &interner);
    //                                 tags.get_at(0, &builder)
    //                                     .pat
    //                                     .unwrap()
    //                                     .cast::<node::BindPat>(&builder)
    //                                     .0
    //                                     .assert_name("result", &builder, &interner);
    //                                 tags.get_at(1, &builder)
    //                                     .tag
    //                                     .assert_name("Err", &builder, &interner);
    //                                 tags.get_at(1, &builder)
    //                                     .pat
    //                                     .unwrap()
    //                                     .cast::<node::BindPat>(&builder)
    //                                     .0
    //                                     .assert_name("error", &builder, &interner);
    //                                 b.body.cast::<node::LiteralExpr>(&builder).assert_eq(
    //                                     &node::LiteralExpr::Str(interner["multiple variants"]),
    //                                 );
    //                             })
    //                             .execute(&builder)
    //                             .next();
    //                     });
    //             });
    //     }

    //     #[test]
    //     fn func_expr() {
    //         let mut interner = StrInterner::default();

    //         let ParseResult { node, builder, .. } =
    //             try_parse_str_with("fn name => \"Hello\" + name", expr_parser(), &mut interner);

    //         query_one::<node::Expr>(node)
    //             .cast::<node::LambdaExpr>()
    //             .inspect(|_, lambda| {
    //                 lambda.param.assert_name("name", &builder, &interner);
    //                 let binary = lambda.body.cast::<node::BinaryExpr>(&builder);
    //                 binary.op.assert_eq(&node::BinaryOp::Add);
    //             })
    //             .execute(&builder)
    //             .next();
    //     }

    //     #[test]
    //     fn arithmetic_expr() {
    //         let mut interner = StrInterner::default();

    //         let ParseResult { node, builder, .. } =
    //             try_parse_str_with("-4 * 10 + 40 / 4 + 30 == 0", expr_parser(), &mut interner);

    //         query_one::<node::Expr>(node)
    //             .cast::<node::BinaryExpr>()
    //             .inspect(|_, eq| {
    //                 eq.op.assert_eq(&node::BinaryOp::Eq);
    //                 eq.rhs
    //                     .cast::<node::LiteralExpr>(&builder)
    //                     .assert_eq(&node::LiteralExpr::Num(0.0));

    //                 let sum = eq.lhs.cast::<node::BinaryExpr>(&builder);
    //                 sum.op.assert_eq(&node::BinaryOp::Add);
    //                 sum.rhs
    //                     .cast::<node::LiteralExpr>(&builder)
    //                     .assert_eq(&node::LiteralExpr::Num(30.0));

    //                 let sum2 = sum.lhs.cast::<node::BinaryExpr>(&builder);
    //                 sum2.op.assert_eq(&node::BinaryOp::Add);
    //                 sum2.lhs
    //                     .cast::<node::BinaryExpr>(&builder)
    //                     .op
    //                     .assert_eq(&node::BinaryOp::Mul);
    //                 sum2.rhs
    //                     .cast::<node::BinaryExpr>(&builder)
    //                     .op
    //                     .assert_eq(&node::BinaryOp::Div);
    //             })
    //             .execute(&builder)
    //             .next();
    //     }

    //     #[test]
    //     fn if_expr() {
    //         let mut interner = StrInterner::default();

    //         let ParseResult { node, builder, .. } =
    //             try_parse_str_with("if y then x else 0", expr_parser(), &mut interner);

    //         query_one::<node::Expr>(node)
    //             .cast::<node::IfExpr>()
    //             .inspect(|_, if_expr| {
    //                 if_expr
    //                     .pred
    //                     .cast::<node::QualifiedExpr>(&builder)
    //                     .source
    //                     .assert_name("y", &builder, &interner);
    //                 if_expr
    //                     .then
    //                     .cast::<node::QualifiedExpr>(&builder)
    //                     .source
    //                     .assert_name("x", &builder, &interner);
    //                 if_expr
    //                     .or_else
    //                     .cast::<node::LiteralExpr>(&builder)
    //                     .assert_eq(&node::LiteralExpr::Num(0.0));
    //             })
    //             .execute(&builder)
    //             .next();
    //     }

    //     #[test]
    //     fn qualified_expr() {
    //         let mut interner = StrInterner::default();

    //         // Test simple field access
    //         let ParseResult { node, builder, .. } =
    //             try_parse_str_with("x.y.z", expr_parser(), &mut interner);

    //         query_one::<node::Expr>(node)
    //             .cast::<node::QualifiedExpr>()
    //             .inspect(|_, q| {
    //                 q.source.assert_name("x", &builder, &interner);
    //                 let path = q.field_path.unwrap().get(&builder);
    //                 path.0
    //                     .get_at(0, &builder)
    //                     .assert_name("y", &builder, &interner);
    //                 path.0
    //                     .get_at(1, &builder)
    //                     .assert_name("z", &builder, &interner);
    //             })
    //             .execute(&builder)
    //             .next();

    //         // Test module path with field access
    //         let ParseResult { node, builder, .. } = try_parse_str_with(
    //             "mod1::mod2::value.field1.field2",
    //             expr_parser(),
    //             &mut interner,
    //         );

    //         query_one::<node::Expr>(node)
    //             .cast::<node::QualifiedExpr>()
    //             .inspect(|_, q| {
    //                 let m_path = q.module_path.unwrap().get(&builder);
    //                 m_path
    //                     .0
    //                     .get_at(0, &builder)
    //                     .assert_name("mod1", &builder, &interner);
    //                 m_path
    //                     .0
    //                     .get_at(1, &builder)
    //                     .assert_name("mod2", &builder, &interner);

    //                 q.source.assert_name("value", &builder, &interner);

    //                 let path = q.field_path.unwrap().get(&builder);
    //                 path.0
    //                     .get_at(0, &builder)
    //                     .assert_name("field1", &builder, &interner);
    //                 path.0
    //                     .get_at(1, &builder)
    //                     .assert_name("field2", &builder, &interner);
    //             })
    //             .execute(&builder)
    //             .next();
    //     }

    //     #[test]
    //     fn record_extension() {
    //         let mut interner = StrInterner::default();

    //         let ParseResult { node, builder, .. } =
    //             try_parse_str_with("{ y | +x = 10 }", expr_parser(), &mut interner);

    //         query_one::<node::Expr>(node)
    //             .cast::<node::RecordExtendExpr>()
    //             .inspect(|_, extend| {
    //                 extend
    //                     .source
    //                     .cast::<node::QualifiedExpr>(&builder)
    //                     .source
    //                     .assert_name("y", &builder, &interner);
    //                 extend
    //                     .field_path
    //                     .get(&builder)
    //                     .0
    //                     .get_at(0, &builder)
    //                     .assert_name("x", &builder, &interner);
    //                 extend
    //                     .value
    //                     .cast::<node::LiteralExpr>(&builder)
    //                     .assert_eq(&node::LiteralExpr::Num(10.0));
    //             })
    //             .execute(&builder)
    //             .next();
    //     }

    //     #[test]
    //     fn record() {
    //         let mut interner = StrInterner::default();

    //         let ParseResult { node, builder, .. } =
    //             try_parse_str_with("{ x = 10, y = 20 }", expr_parser(), &mut interner);

    //         query_one::<node::Expr>(node)
    //             .cast::<node::RecordExpr>()
    //             .inspect(|_, record| {
    //                 let fields = record.0;
    //                 assert_eq!(fields.get(&builder).len(), 2);
    //                 fields
    //                     .get_at(0, &builder)
    //                     .label
    //                     .assert_name("x", &builder, &interner);
    //                 fields
    //                     .get_at(0, &builder)
    //                     .value
    //                     .cast::<node::LiteralExpr>(&builder)
    //                     .assert_eq(&node::LiteralExpr::Num(10.0));
    //                 fields
    //                     .get_at(1, &builder)
    //                     .label
    //                     .assert_name("y", &builder, &interner);
    //                 fields
    //                     .get_at(1, &builder)
    //                     .value
    //                     .cast::<node::LiteralExpr>(&builder)
    //                     .assert_eq(&node::LiteralExpr::Num(20.0));
    //             })
    //             .execute(&builder)
    //             .next();
    //     }

    //     #[test]
    //     fn type_expr() {
    //         let mut interner = StrInterner::default();

    //         let ParseResult { node, builder, .. } = try_parse_str_with(
    //             "Num -> { a : Num, b : Num -> Num } -> Str",
    //             type_parser(),
    //             &mut interner,
    //         );

    //         query_one::<node::TypeExpr>(node)
    //             .cast::<node::FuncType>()
    //             .inspect(|_, func| {
    //                 func.input
    //                     .cast::<node::QualifiedType>(&builder)
    //                     .ty
    //                     .assert_name("Num", &builder, &interner);

    //                 let func2 = func.output.ty.cast::<node::FuncType>(&builder);
    //                 let record = func2.input.cast::<node::RecordType>(&builder);
    //                 let f = record.0;
    //                 assert_eq!(f.get(&builder).len(), 2);

    //                 let node::LabelOrVar::Label(label_a) = f.get_at(0, &builder).label_or_var else {
    //                     panic!("expected label")
    //                 };
    //                 label_a.assert_name("a", &builder, &interner);
    //                 f.get_at(0, &builder)
    //                     .ty
    //                     .cast::<node::QualifiedType>(&builder)
    //                     .ty
    //                     .assert_name("Num", &builder, &interner);

    //                 let node::LabelOrVar::Label(label_b) = f.get_at(1, &builder).label_or_var else {
    //                     panic!("expected label")
    //                 };
    //                 label_b.assert_name("b", &builder, &interner);
    //                 f.get_at(1, &builder).ty.cast::<node::FuncType>(&builder);

    //                 func2
    //                     .output
    //                     .ty
    //                     .cast::<node::QualifiedType>(&builder)
    //                     .ty
    //                     .assert_name("Str", &builder, &interner);
    //             })
    //             .execute(&builder)
    //             .next();
    //     }

    //     #[test]
    //     fn type_application() {
    //         let mut interner = StrInterner::default();

    //         let ParseResult { node, builder, .. } = try_parse_str_with(
    //             "Map (Num -> Str) (std::List Str)",
    //             type_parser(),
    //             &mut interner,
    //         );

    //         query_one::<node::TypeExpr>(node)
    //             .cast::<node::TypeApplication>()
    //             .inspect(|_, app| {
    //                 let inner_app = app.constructor.cast::<node::TypeApplication>(&builder);
    //                 inner_app
    //                     .constructor
    //                     .cast::<node::QualifiedType>(&builder)
    //                     .ty
    //                     .assert_name("Map", &builder, &interner);
    //                 inner_app.arg.cast::<node::FuncType>(&builder);

    //                 let list_app = app.arg.cast::<node::TypeApplication>(&builder);
    //                 list_app.constructor.cast::<node::QualifiedType>(&builder);
    //                 list_app.arg.cast::<node::QualifiedType>(&builder);
    //             })
    //             .execute(&builder)
    //             .next();
    //     }

    //     #[test]
    //     fn type_() {
    //         let mut interner = StrInterner::default();

    //         let ParseResult { node, builder, .. } = try_parse_str_with(
    //             "forall a b . { left : a, right : Num -> b }",
    //             type_scheme_parser(),
    //             &mut interner,
    //         );

    //         let scheme = node.get(&builder);
    //         let binders = scheme
    //             .forall
    //             .unwrap()
    //             .get(&builder)
    //             .0
    //             .get_elements(&builder);
    //         let binder_names: Vec<_> = binders
    //             .map(|b| interner.get(b.var.get(&builder).0).unwrap())
    //             .collect();
    //         assert_eq!(binder_names, vec!["a", "b"]);

    //         let record = scheme.ty.cast::<node::RecordType>(&builder);
    //         let f = record.0;
    //         assert_eq!(f.get(&builder).len(), 2);

    //         let node::LabelOrVar::Label(label_l) = f.get_at(0, &builder).label_or_var else {
    //             panic!("expected label")
    //         };
    //         label_l.assert_name("left", &builder, &interner);
    //         f.get_at(0, &builder)
    //             .ty
    //             .cast::<node::QualifiedType>(&builder)
    //             .ty
    //             .assert_name("a", &builder, &interner);

    //         let node::LabelOrVar::Label(label_r) = f.get_at(1, &builder).label_or_var else {
    //             panic!("expected label")
    //         };
    //         label_r.assert_name("right", &builder, &interner);
    //         let f_right = f.get_at(1, &builder).ty.cast::<node::FuncType>(&builder);
    //         f_right
    //             .input
    //             .cast::<node::QualifiedType>(&builder)
    //             .ty
    //             .assert_name("Num", &builder, &interner);
    //         f_right
    //             .output
    //             .ty
    //             .cast::<node::QualifiedType>(&builder)
    //             .ty
    //             .assert_name("b", &builder, &interner);
    //     }

    //     #[test]
    //     fn type_bind() {
    //         let mut interner = StrInterner::default();

    //         let ParseResult { node, builder, .. } = try_parse_str_with(
    //             "type Person = forall a . { id : a, name : Str, age : Num }",
    //             type_bind_parser(),
    //             &mut interner,
    //         );

    //         let bind = node.get(&builder);
    //         bind.name.assert_name("Person", &builder, &interner);

    //         let scheme = bind.ty_scheme.get(&builder);
    //         assert!(scheme.forall.is_some());
    //         let record = scheme.ty.cast::<node::RecordType>(&builder);
    //         assert_eq!(record.0.get(&builder).len(), 3);
    //     }

    //     #[test]
    //     fn variant_type_bind() {
    //         let mut interner = StrInterner::default();

    //         let ParseResult { node, builder, .. } = try_parse_str_with(
    //             "type Option = forall a b . < Some : a, None | b >",
    //             type_bind_parser(),
    //             &mut interner,
    //         );

    //         let bind = node.get(&builder);
    //         bind.name.assert_name("Option", &builder, &interner);

    //         let scheme = bind.ty_scheme.get(&builder);
    //         let binder = scheme.forall.unwrap().get(&builder).0;
    //         binder
    //             .get_at(0, &builder)
    //             .var
    //             .assert_name("a", &builder, &interner);

    //         let variant = scheme.ty.cast::<node::VariantType>(&builder);
    //         assert_eq!(variant.tags.get(&builder).len(), 2);
    //         variant
    //             .tags
    //             .get_at(0, &builder)
    //             .name
    //             .assert_name("Some", &builder, &interner);
    //         variant
    //             .tags
    //             .get_at(0, &builder)
    //             .ty
    //             .unwrap()
    //             .cast::<node::QualifiedType>(&builder)
    //             .ty
    //             .assert_name("a", &builder, &interner);

    //         variant
    //             .tags
    //             .get_at(1, &builder)
    //             .name
    //             .assert_name("None", &builder, &interner);
    //         assert!(variant.tags.get_at(1, &builder).ty.is_none());

    //         variant
    //             .extension
    //             .unwrap()
    //             .assert_name("b", &builder, &interner);
    //     }

    //     #[test]
    //     fn module() {
    //         let mut interner = StrInterner::default();

    //         let ParseResult { node, builder, .. } =
    //             try_parse_str_with("{ x = 10, type T = Num }", module_parser(), &mut interner);

    //         let body = node.get(&builder);
    //         assert_eq!(body.0.get(&builder).len(), 2);

    //         let bind_0 = body.0.get_at(0, &builder).cast::<node::ValueBind>(&builder);
    //         bind_0.name.assert_name("x", &builder, &interner);
    //         bind_0
    //             .value
    //             .cast::<node::LiteralExpr>(&builder)
    //             .assert_eq(&node::LiteralExpr::Num(10.0));

    //         let bind_1 = body.0.get_at(1, &builder).cast::<node::TypeBind>(&builder);
    //         bind_1.name.assert_name("T", &builder, &interner);
    //         bind_1
    //             .ty_scheme
    //             .get(&builder)
    //             .ty
    //             .cast::<node::QualifiedType>(&builder)
    //             .ty
    //             .assert_name("Num", &builder, &interner);
    //     }

    //     #[test]
    //     fn module_type() {
    //         let mut interner = StrInterner::default();

    //         let ParseResult { node, builder, .. } =
    //             try_parse_str_with("{ x : Num }", module_type_parser(), &mut interner);

    //         let concrete = node.cast::<node::ConcreteModuleType>(&builder);
    //         assert_eq!(concrete.0.get(&builder).len(), 1);

    //         let val_spec = concrete
    //             .0
    //             .get_at(0, &builder)
    //             .cast::<node::ValueSpec>(&builder);
    //         val_spec.name.assert_name("x", &builder, &interner);
    //         val_spec
    //             .ty
    //             .ty
    //             .cast::<node::QualifiedType>(&builder)
    //             .ty
    //             .assert_name("Num", &builder, &interner);
    //     }

    //     #[test]
    //     fn nested_module() {
    //         let mut interner = StrInterner::default();

    //         let ParseResult { node, builder, .. } =
    //             try_parse_str_with("{ module m = { x = 10 } }", module_parser(), &mut interner);

    //         let body = node.get(&builder);
    //         assert_eq!(body.0.get(&builder).len(), 1);

    //         let m_bind = body
    //             .0
    //             .get_at(0, &builder)
    //             .cast::<node::ModuleBind>(&builder);
    //         m_bind.name.assert_name("m", &builder, &interner);
    //         assert!(m_bind.ty.is_none());

    //         let m_body = m_bind.value.cast::<node::ModuleBody>(&builder);
    //         assert_eq!(m_body.0.get(&builder).len(), 1);

    //         let val_bind = m_body
    //             .0
    //             .get_at(0, &builder)
    //             .cast::<node::ValueBind>(&builder);
    //         val_bind.name.assert_name("x", &builder, &interner);
    //         val_bind
    //             .value
    //             .cast::<node::LiteralExpr>(&builder)
    //             .assert_eq(&node::LiteralExpr::Num(10.0));
    //     }

    //     #[test]
    //     fn nested_module_with_type() {
    //         let mut interner = StrInterner::default();

    //         let ParseResult { node, builder, .. } = try_parse_str_with(
    //             "{ module m : { x : Num } = { x = 10 } }",
    //             module_parser(),
    //             &mut interner,
    //         );

    //         let body = node.get(&builder);
    //         assert_eq!(body.0.get(&builder).len(), 1);

    //         let m_bind = body
    //             .0
    //             .get_at(0, &builder)
    //             .cast::<node::ModuleBind>(&builder);
    //         m_bind.name.assert_name("m", &builder, &interner);

    //         // Check interface spec
    //         let concrete_ty = m_bind
    //             .ty
    //             .as_ref()
    //             .unwrap()
    //             .cast::<node::ConcreteModuleType>(&builder);
    //         assert_eq!(concrete_ty.0.get(&builder).len(), 1);
    //         let val_spec = concrete_ty
    //             .0
    //             .get_at(0, &builder)
    //             .cast::<node::ValueSpec>(&builder);
    //         val_spec.name.assert_name("x", &builder, &interner);
    //         val_spec
    //             .ty
    //             .ty
    //             .cast::<node::QualifiedType>(&builder)
    //             .ty
    //             .assert_name("Num", &builder, &interner);

    //         // Check implementation body
    //         let m_body = m_bind.value.cast::<node::ModuleBody>(&builder);
    //         assert_eq!(m_body.0.get(&builder).len(), 1);
    //         let val_bind = m_body
    //             .0
    //             .get_at(0, &builder)
    //             .cast::<node::ValueBind>(&builder);
    //         val_bind.name.assert_name("x", &builder, &interner);
    //         val_bind
    //             .value
    //             .cast::<node::LiteralExpr>(&builder)
    //             .assert_eq(&node::LiteralExpr::Num(10.0));
    //     }

    //     #[test]
    //     fn comments_are_transparent_to_parser() {
    //         let mut interner = StrInterner::default();

    //         let ParseResult { node, builder, .. } = try_parse_str_with(
    //             r#"{
    //                 # A regular comment
    //                 x = 10,
    //                 ## A doc comment
    //                 y = 20
    //             }"#,
    //             module_parser(),
    //             &mut interner,
    //         );

    //         let body = node.get(&builder);
    //         assert_eq!(body.0.get(&builder).len(), 2);

    //         let bind_0 = body.0.get_at(0, &builder).cast::<node::ValueBind>(&builder);
    //         bind_0.name.assert_name("x", &builder, &interner);
    //         bind_0
    //             .value
    //             .cast::<node::LiteralExpr>(&builder)
    //             .assert_eq(&node::LiteralExpr::Num(10.0));

    //         let bind_1 = body.0.get_at(1, &builder).cast::<node::ValueBind>(&builder);
    //         bind_1.name.assert_name("y", &builder, &interner);
    //         bind_1
    //             .value
    //             .cast::<node::LiteralExpr>(&builder)
    //             .assert_eq(&node::LiteralExpr::Num(20.0));
    //     }
}
