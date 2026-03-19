use kola_span::combinator::{Combinator, IterCombinator};
use kola_span::input::Input;
use kola_span::parser::Parser;
use kola_span::primitive::{Lazy, OpaqueFn, choice, group, lazy};
use kola_tree::{node::ValueName, prelude::*};

use super::ParseInput;
use super::ext::KolaCombinator;
use super::state::State;
use crate::token::{CloseT, CtrlT, Delim, KwT, LiteralT, OpT, OpenT, Symbol};

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

pub const fn effect_name_parser<'t>() -> impl const KolaCombinator<'t, Id<node::EffectName>> {
    upper_symbol(Symbol::Effect)
        .map(node::EffectName::new)
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

pub const fn nested_parser<'t, T>(
    parser: impl Parser<ParseInput<'t>, Id<T>> + Copy,
    delim: Delim,
) -> impl const KolaCombinator<'t, Id<T>> {
    let (open, close) = match delim {
        Delim::Paren => (OpenT::PAREN, CloseT::PAREN),
        Delim::Bracket => (OpenT::BRACKET, CloseT::BRACKET),
        Delim::Brace => (OpenT::BRACE, CloseT::BRACE),
        Delim::Angle => (OpenT::ANGLE, CloseT::ANGLE),
    };

    nested_in_parser(open, close, parser)
}

pub const fn nested_in_parser<'t, T>(
    open: OpenT<'t>,
    close: CloseT<'t>,
    parser: impl Parser<ParseInput<'t>, Id<T>> + Copy,
) -> impl const KolaCombinator<'t, Id<T>> {
    // Happy path only for now; recovery is TODO
    // TODO: re-add error recovery using skip_nested_delimiters + recover combinator
    parser.delimited_by(open_delim(open), close_delim(close))
}

pub const fn module_parser<'t>() -> OpaqueFn<ParseInput<'t>, Id<node::Module>> {
    lazy::<ParseInput<'t>, Id<node::Module>, ModuleCombinator>()
}

#[derive(Debug, Clone, Copy)]
pub struct ModuleCombinator;

impl<'t> Lazy<ParseInput<'t>, Id<node::Module>> for ModuleCombinator {
    type Combinator = impl const KolaCombinator<'t, Id<node::Module>>;
    const COMBINATOR: Self::Combinator = {
        let module = module_parser();

        let module_import = kw(KwT::IMPORT)
            .ignore_then(module_name_parser())
            .map_to_node(node::ModuleImport)
            .to_module_expr()
            .with_note("ModuleImport");

        let functor_args = module_path_parser()
            .repeated()
            .at_least(1)
            .separated_by(ctrl(CtrlT::COMMA))
            .collect()
            .map_to_node(node::FunctorArgs);

        let module_path = module_name_parser()
            .then_ignore(ctrl(CtrlT::DOUBLE_COLON).rewind())
            .then_ignore(ctrl(CtrlT::DOUBLE_COLON))
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
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
                .ignore_then(type_scheme_parser())
                .or_not(),
            op(OpT::ASSIGN).ignore_then(expr_parser()),
        ))
        .map_to_node(|(vis, name, ty_scheme, value)| node::ValueBind {
            vis,
            name,
            ty_scheme,
            value,
        })
        .to_bind();

        let type_bind = type_bind_parser().to_bind();

        // TODO opaque type bind

        let effect_op = value_name_parser()
            .then(ctrl(CtrlT::COLON).ignore_then(type_parser()))
            .map_to_node(|(name, ty)| node::EffectOpType { name, ty });

        let effect_row = effect_op
            .repeated()
            .separated_by(ctrl(CtrlT::COMMA))
            .allow_trailing()
            .collect()
            .delimited_by(open_delim(OpenT::BRACE), close_delim(CloseT::BRACE))
            .map_to_node(node::EffectRowType);

        let effect_type_bind = group((
            vis_parser(),
            kw(KwT::EFFECT).ignore_then(kw(KwT::TYPE).ignore_then(effect_name_parser())),
            op(OpT::ASSIGN).ignore_then(effect_row),
        ))
        .map_to_node(|(vis, name, ty)| node::EffectTypeBind { vis, name, ty })
        .to_bind();

        let module_bind = group((
            vis_parser(),
            kw(KwT::MODULE).ignore_then(module_name_parser()),
            ctrl(CtrlT::COLON)
                .ignore_then(module_type_parser())
                .or_not(),
            op(OpT::ASSIGN).ignore_then(module_expr),
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
            kw(KwT::MODULE).ignore_then(kw(KwT::TYPE).ignore_then(module_type_name_parser())),
            op(OpT::ASSIGN).ignore_then(module_type_parser()),
        ))
        .map_to_node(|(vis, name, ty)| node::ModuleTypeBind { vis, name, ty })
        .to_bind();

        let functor_param = module_name_parser()
            .then(ctrl(CtrlT::COLON).ignore_then(module_type_parser()))
            .map_to_node(|(name, ty)| node::FunctorParam { name, ty })
            .delimited_by(open_delim(OpenT::PAREN), close_delim(CloseT::PAREN));

        let functor_bind = group((
            vis_parser(),
            kw(KwT::MODULE).ignore_then(kw(KwT::FUNCTOR).ignore_then(functor_name_parser())),
            functor_param.repeated().at_least(1).collect(),
            ctrl(CtrlT::DOUBLE_ARROW).ignore_then(module),
        ))
        .map_to_node(|(vis, name, params, body)| node::FunctorBind {
            vis,
            name,
            params,
            body,
        })
        .to_bind()
        .with_note("Functor");

        let bind = choice((
            functor_bind,
            module_type_bind,
            module_bind,
            effect_type_bind,
            type_bind,
            value_bind,
        ));

        bind.repeated()
            .separated_by(ctrl(CtrlT::COMMA))
            .allow_trailing()
            .collect()
            .map_to_node(node::Module)
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
            .then(ctrl(CtrlT::COLON).ignore_then(type_scheme_parser()))
            .map_to_node(|(name, ty)| node::ValueSpec { name, ty })
            .to_spec();

        let type_spec = kw(KwT::TYPE)
            .ignore_then(type_name_parser())
            .map_to_node(|name| node::OpaqueTypeSpec { name })
            .to_spec();

        let module_spec = kw(KwT::MODULE)
            .ignore_then(module_name_parser())
            .then(ctrl(CtrlT::COLON).ignore_then(module_type))
            .map_to_node(|(name, ty)| node::ModuleSpec { name, ty })
            .to_spec();

        let spec = choice((value_spec, type_spec, module_spec));

        let concrete = spec
            .repeated()
            .separated_by(ctrl(CtrlT::COMMA))
            .allow_trailing()
            .collect()
            .map_to_node(node::ConcreteModuleType)
            .delimited_by(open_delim(OpenT::BRACE), close_delim(CloseT::BRACE))
            .to_module_type();

        let qualified = module_name_parser()
            .then_ignore(ctrl(CtrlT::DOUBLE_COLON))
            .repeated()
            .at_least(1)
            .collect()
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
                .repeated()
                .separated_by(ctrl(CtrlT::COMMA))
                .allow_trailing()
                .collect()
                .map_to_node(node::ListPat)
                .to_pat(),
            Delim::Bracket,
        );

        // Record field pattern
        let field = value_name_parser()
            .then(ctrl(CtrlT::COLON).ignore_then(pat).or_not())
            .map_to_node(|(field, pat)| node::RecordFieldPat { field, pat });

        let record = nested_parser(
            field
                .repeated()
                .separated_by(ctrl(CtrlT::COMMA))
                .collect()
                .then(ctrl(CtrlT::COMMA).then(ctrl(CtrlT::TRIPLE_DOT)).or_not())
                .map_to_node(|(fields, spread)| node::RecordPat {
                    fields,
                    polymorph: spread.is_some(),
                })
                .to_pat(),
            Delim::Brace,
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
            .then(ctrl(CtrlT::COLON).ignore_then(pat).or_not())
            .map_to_node(|(tag, pat)| node::VariantTagPat { tag, pat });

        let variant = nested_parser(
            none_case
                .or(case)
                .repeated()
                .separated_by(ctrl(CtrlT::COMMA))
                .allow_trailing()
                .collect()
                .map_to_node(node::VariantPat)
                .to_pat(),
            Delim::Angle,
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

        enum RecordOp {
            Extend(Id<node::FieldPath>, Option<Id<node::Type>>, Id<node::Expr>),
            Restrict(Id<node::FieldPath>, Option<Id<node::Type>>),
            Update(
                Id<node::FieldPath>,
                Option<Id<node::Type>>,
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
            .to_expr()
            .with_note("NoneExpr");

        let tag = tag_name_parser()
            .map_to_node(node::TagExpr)
            .to_expr()
            .with_note("TagExpr");

        let module_path = module_name_parser()
            .then_ignore(ctrl(CtrlT::DOUBLE_COLON).rewind())
            .then_ignore(ctrl(CtrlT::DOUBLE_COLON))
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .map_to_node(node::ModulePath)
            .or_not();

        let type_wit = ctrl(CtrlT::AT)
            .ignore_then(module_path.then(type_name_parser()))
            .map(|(path, ty)| node::QualifiedType { path, ty })
            .to_node()
            .map_to_node(node::TypeWitnessExpr::Qualified)
            .or(ctrl(CtrlT::TICK)
                .ignore_then(value_name_parser())
                .map_to_node(node::TypeWitnessExpr::Label))
            .to_expr()
            .with_note("TagWitnessExpr");

        let literal = literal_parser()
            .to_node()
            .to_expr()
            .with_note("LiteralExpr");

        let list = nested_parser(
            expr.repeated()
                .separated_by(ctrl(CtrlT::COMMA))
                .allow_trailing()
                .collect()
                .map_to_node(node::ListExpr)
                .to_expr(),
            Delim::Bracket,
        )
        .with_note("ListExpr");

        let field = group((
            value_name_parser(),
            ctrl(CtrlT::COLON).ignore_then(type_parser()).or_not(),
            op(OpT::ASSIGN).ignore_then(expr),
        ))
        .map(|(label, ty, value)| node::RecordField { label, ty, value })
        .to_node();

        let instantiate = field
            .repeated()
            .separated_by(ctrl(CtrlT::COMMA))
            .allow_trailing()
            .collect()
            .map_to_node(node::RecordExpr)
            .to_expr();

        let record_expr =
            nested_parser(record_op.or(instantiate), Delim::Brace).with_note("RecordExpr");

        let let_ = group((
            kw(KwT::LET).ignore_then(value_name_parser()),
            ctrl(CtrlT::COLON).ignore_then(type_parser()).or_not(),
            op(OpT::ASSIGN).ignore_then(expr),
            kw(KwT::IN).ignore_then(expr),
        ))
        .map_to_node(|(name, value_type, value, body)| node::LetExpr {
            name,
            value_type,
            value,
            body,
        })
        .to_expr()
        .with_note("LetExpr");

        let if_ = group((
            kw(KwT::IF).ignore_then(expr),
            kw(KwT::THEN).ignore_then(expr),
            kw(KwT::ELSE).ignore_then(expr),
        ))
        .map_to_node(|(pred, then, or_else)| node::IfExpr {
            pred,
            then,
            or_else,
        })
        .to_expr()
        .with_note("IfExpr");

        let branch = pat_parser()
            .then(ctrl(CtrlT::DOUBLE_ARROW).ignore_then(expr))
            .map(|(pat, body)| node::CaseBranch { pat, body })
            .to_node();

        let branches = ctrl(CtrlT::PIPE)
            .ignore_then(branch)
            .repeated()
            .at_least(1)
            .collect();

        let case = kw(KwT::CASE)
            .ignore_then(qualified_parser())
            .then(branches)
            .map_to_node(|(source, branches)| node::CaseExpr { source, branches })
            .to_expr()
            .with_note("CaseExpr");

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
            .collect();

        let handle = kw(KwT::HANDLE)
            .ignore_then(qualified_parser())
            .then(clauses)
            .map_to_node(|(source, clauses)| node::HandleExpr { source, clauses })
            .to_expr()
            .with_note("HandleExpr");

        let do_expr = kw(KwT::DO)
            .ignore_then(value_name_parser())
            .then(expr)
            .map_to_node(|(op, arg)| node::DoExpr { op, arg })
            .to_expr()
            .with_note("DoExpr");

        let func = group((
            kw(KwT::FN).ignore_then(value_name_parser()),
            ctrl(CtrlT::COLON).ignore_then(type_parser()).or_not(),
            ctrl(CtrlT::DOUBLE_ARROW).ignore_then(expr),
        ))
        .map_to_node(|(param, param_type, body)| node::LambdaExpr {
            param,
            param_type,
            body,
        })
        .to_expr()
        .with_note("FuncExpr");

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
        )
        .with_note("ParenExpr");

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
        use kola_span::pratt::PrattNil;

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
pub const fn type_parser<'t>() -> OpaqueFn<ParseInput<'t>, Id<node::Type>> {
    lazy::<ParseInput<'t>, Id<node::Type>, TypeCombinator>()
}

#[derive(Debug, Clone, Copy)]
pub struct TypeCombinator;

impl<'t> Lazy<ParseInput<'t>, Id<node::Type>> for TypeCombinator {
    type Combinator = impl const KolaCombinator<'t, Id<node::Type>>;
    const COMBINATOR: Self::Combinator = {
        let ty = type_parser();

        let module_path = module_name_parser()
            .then_ignore(ctrl(CtrlT::DOUBLE_COLON).rewind())
            .then_ignore(ctrl(CtrlT::DOUBLE_COLON))
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
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
                .repeated()
                .separated_by(ctrl(CtrlT::COMMA))
                .allow_trailing()
                .collect()
                .then(row_var_parser())
                .map_to_node(|(fields, extension)| node::RecordType { fields, extension })
                .to_type(),
            Delim::Brace,
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
            .then(ctrl(CtrlT::COLON).ignore_then(ty).or_not())
            .map_to_node(|(name, ty)| node::TagType { name, ty });

        let variant = nested_parser(
            none_tag
                .or(tag)
                .repeated()
                .separated_by(ctrl(CtrlT::COMMA))
                .allow_trailing()
                .collect()
                .then(row_var_parser())
                .map_to_node(|(tags, extension)| node::VariantType { tags, extension })
                .to_type(),
            Delim::Angle,
        );

        let atom = choice((qual_ty, record, variant, nested_parser(ty, Delim::Paren)));

        let appl = atom.foldl_with(
            atom.repeated(),
            |constructor, arg, span, input: &mut ParseInput<'t>| {
                let tree: &mut State = input.state();
                tree.insert_as::<node::Type, _>(node::TypeApplication { constructor, arg }, span)
            },
        );

        let effect_op = value_name_parser()
            .then(ctrl(CtrlT::COLON).ignore_then(ty))
            .map_to_node(|(name, ty)| node::EffectOpType { name, ty });

        let effect_row = effect_op
            .repeated()
            .separated_by(ctrl(CtrlT::COMMA))
            .allow_trailing()
            .collect()
            .delimited_by(open_delim(OpenT::BRACE), close_delim(CloseT::BRACE))
            .map_to_node(node::EffectRowType)
            .map_to_node(node::EffectType::Row);

        let qual_effect = module_path
            .then(effect_name_parser())
            .map(|(path, ty)| node::QualifiedEffectType { path, ty })
            .to_node()
            .map_to_node(node::EffectType::Qualified);

        let computation = ty
            .then(
                ctrl(CtrlT::TILDE)
                    .ignore_then(effect_row.or(qual_effect))
                    .or_not(),
            )
            .map_to_node(|(ty, effect)| node::CompType { ty, effect });

        let func = appl.foldl_with(
            ctrl(CtrlT::ARROW).ignore_then(computation).repeated(),
            |input_ty, output, span, input: &mut ParseInput<'t>| {
                let tree: &mut State = input.state();

                tree.insert_as::<node::Type, _>(
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
        .repeated()
        .separated_by(ctrl(CtrlT::DOUBLE_COLON))
        .collect()
        .map_to_node(node::ModulePath)
}

const fn field_path_parser<'t>() -> impl const KolaCombinator<'t, Id<node::FieldPath>> {
    value_name_parser()
        .repeated()
        .separated_by(ctrl(CtrlT::DOT))
        .collect()
        .map_to_node(node::FieldPath)
}

/// Parser for qualified expressions: `module::record.field`
const fn qualified_parser<'t>() -> impl const KolaCombinator<'t, Id<node::Expr>> {
    let module_path = module_name_parser()
        .then_ignore(ctrl(CtrlT::DOUBLE_COLON).rewind())
        .then_ignore(ctrl(CtrlT::DOUBLE_COLON))
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .map_to_node(node::ModulePath)
        .or_not();

    let field_path = ctrl(CtrlT::DOT)
        .ignore_then(value_name_parser())
        .repeated()
        .at_least(1)
        .collect()
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
        .with_note("QualifiedExpr")
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
            .then(ctrl(CtrlT::COLON).ignore_then(kind_name_parser()))
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
        .ignore_then(type_var_bind().repeated().at_least(1).collect())
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
    use camino::Utf8PathBuf;

    use kola_span::SourceId;
    use kola_span::parser::Parser;
    use kola_tree::{inspector::NodeInspector, prelude::*};
    use kola_utils::interner::{PathInterner, StrInterner};

    use super::{
        expr_parser, module_parser, module_type_parser, pat_parser, type_bind_parser, type_parser,
        type_scheme_parser,
    };
    use crate::{
        lexer::{LexInput, try_tokenize},
        parser::{ParseInput, ParseResult, try_parse_with},
    };

    fn mocked_source() -> SourceId {
        let mut interner = PathInterner::new();
        interner.intern(Utf8PathBuf::from("test"))
    }

    fn try_parse_str_with<'t, T>(
        text: &'t str,
        parser: impl Parser<ParseInput<'t>, T>,
        interner: &'t mut StrInterner,
    ) -> ParseResult<T> {
        let source = mocked_source();
        let input = LexInput { source, text };
        let tokens = try_tokenize(input).unwrap();
        let input = ParseInput::new(source, tokens, interner);

        try_parse_with(input, parser).unwrap()
    }

    #[test]
    fn pat() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("{ a: x, b: { y }, c: _, d }", pat_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);

        let record = inspector.to_record();
        record.fields_at(0).pat().to_bind().inner().has_name("x");
        record
            .fields_at(1)
            .pat()
            .to_record()
            .fields_at(0)
            .has_none_pat();
        record.fields_at(2).pat().to_any();
        record.fields_at(3).has_none_pat();
    }

    #[test]
    fn case_expr() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } = try_parse_str_with(
            "case x | 1 => true | _ => false",
            expr_parser(),
            &mut interner,
        );

        let inspector = NodeInspector::new(node, &builder, &interner);

        let case = inspector.to_case();
        case.source().to_qualified().source().has_name("x");
        case.has_branches_count(2)
            .branches_at(0)
            .pat()
            .to_literal()
            .assert_eq(&node::LiteralPat::Num(1.0));
        case.branches_at(0)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Bool(true));
        case.branches_at(1).pat().to_any();
        case.branches_at(1)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Bool(false));
    }

    #[test]
    fn complex_case_expr() {
        let mut interner = StrInterner::new();

        // Test comprehensive pattern matching with all pattern types
        let test_case = r#"
            case data
            | () => "unit"
            | true => "bool true"
            | 42 => "number"
            | "hello" => "string"
            | x => "bind pattern"
            | _ => "wildcard"
            | [a, b, ...rest] => "list with spread"
            | [head, ...] => "list anonymous spread"
            | [] => "empty list"
            | { name, age: years, ... } => "record with spread"
            | { x, y } => "simple record"
            | {} => "empty record"
            | < Some : value > => "variant some"
            | < None > => "variant none"
            | < Ok : result, Err : error > => "multiple variants"
        "#;

        let ParseResult { node, builder, .. } =
            try_parse_str_with(test_case, expr_parser(), &mut interner);

        dbg!(&builder);

        let inspector = NodeInspector::new(node, &builder, &interner);
        let case = inspector.to_case();
        case.source().to_qualified().source().has_name("data");
        case.has_branches_count(15);

        // Test literal patterns
        case.branches_at(0)
            .pat()
            .to_literal()
            .assert_eq(&node::LiteralPat::Unit);
        case.branches_at(0)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["unit"]));
        case.branches_at(1)
            .pat()
            .to_literal()
            .assert_eq(&node::LiteralPat::Bool(true));
        case.branches_at(1)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["bool true"]));
        case.branches_at(2)
            .pat()
            .to_literal()
            .assert_eq(&node::LiteralPat::Num(42.0));
        case.branches_at(2)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["number"]));
        case.branches_at(3)
            .pat()
            .to_literal()
            .assert_eq(&node::LiteralPat::Str(interner["hello"]));
        case.branches_at(3)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["string"]));

        // Test bind and wildcard patterns
        case.branches_at(4).pat().to_bind().inner().has_name("x");
        case.branches_at(4)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["bind pattern"]));
        case.branches_at(5).pat().to_any();
        case.branches_at(5)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["wildcard"]));

        // Test list patterns
        case.branches_at(6)
            .pat()
            .to_list()
            .has_inner_count(3)
            .inner_at(0)
            .to_pat()
            .to_bind()
            .inner()
            .has_name("a");
        case.branches_at(6)
            .pat()
            .to_list()
            .inner_at(1)
            .to_pat()
            .to_bind()
            .inner()
            .has_name("b");
        case.branches_at(6)
            .pat()
            .to_list()
            .inner_at(2)
            .to_some_spread()
            .has_name("rest");
        case.branches_at(6)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["list with spread"]));

        case.branches_at(7)
            .pat()
            .to_list()
            .has_inner_count(2)
            .inner_at(0)
            .to_pat()
            .to_bind()
            .inner()
            .has_name("head");
        case.branches_at(7)
            .pat()
            .to_list()
            .inner_at(1)
            .is_none_spread();
        case.branches_at(7)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["list anonymous spread"]));

        case.branches_at(8).pat().to_list().has_inner_count(0);
        case.branches_at(8)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["empty list"]));

        // Test record patterns
        case.branches_at(9)
            .pat()
            .to_record()
            .has_fields_count(2)
            .fields_at(0)
            .field()
            .has_name("name");
        case.branches_at(9)
            .pat()
            .to_record()
            .fields_at(0)
            .has_none_pat();
        case.branches_at(9)
            .pat()
            .to_record()
            .fields_at(1)
            .field()
            .has_name("age");
        case.branches_at(9)
            .pat()
            .to_record()
            .fields_at(1)
            .pat()
            .to_bind()
            .inner()
            .has_name("years");
        assert!(case.branches_at(9).pat().to_record().get().polymorph);
        case.branches_at(9)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["record with spread"]));

        case.branches_at(10)
            .pat()
            .to_record()
            .has_fields_count(2)
            .fields_at(0)
            .field()
            .has_name("x");
        case.branches_at(10)
            .pat()
            .to_record()
            .fields_at(1)
            .field()
            .has_name("y");
        case.branches_at(10)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["simple record"]));

        case.branches_at(11).pat().to_record().has_fields_count(0);
        case.branches_at(11)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["empty record"]));

        // Test variant patterns
        case.branches_at(12)
            .pat()
            .to_variant()
            .has_inner_count(1)
            .inner_at(0)
            .tag()
            .has_name("Some");
        case.branches_at(12)
            .pat()
            .to_variant()
            .inner_at(0)
            .pat()
            .to_bind()
            .inner()
            .has_name("value");
        case.branches_at(12)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["variant some"]));

        case.branches_at(13)
            .pat()
            .to_variant()
            .has_inner_count(1)
            .inner_at(0)
            .tag()
            .has_name("None");
        case.branches_at(13)
            .pat()
            .to_variant()
            .inner_at(0)
            .has_none_pat();
        case.branches_at(13)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["variant none"]));

        case.branches_at(14)
            .pat()
            .to_variant()
            .has_inner_count(2)
            .inner_at(0)
            .tag()
            .has_name("Ok");
        case.branches_at(14)
            .pat()
            .to_variant()
            .inner_at(0)
            .pat()
            .to_bind()
            .inner()
            .has_name("result");
        case.branches_at(14)
            .pat()
            .to_variant()
            .inner_at(1)
            .tag()
            .has_name("Err");
        case.branches_at(14)
            .pat()
            .to_variant()
            .inner_at(1)
            .pat()
            .to_bind()
            .inner()
            .has_name("error");
        case.branches_at(14)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["multiple variants"]));
    }

    #[test]
    fn func_expr() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("fn name => \"Hello\" + name", expr_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);

        let lambda = inspector.to_lambda();
        lambda.param().has_name("name");
        lambda
            .body()
            .to_binary()
            .op()
            .assert_eq(&node::BinaryOp::Add);
    }

    #[test]
    fn arithmetic_expr() {
        let mut interner = StrInterner::new();

        // ((-4 * 10) + (40 / 4)) + 30 = 0
        let ParseResult { node, builder, .. } =
            try_parse_str_with("-4 * 10 + 40 / 4 + 30 == 0", expr_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);

        let eq = inspector.to_binary();
        eq.op().assert_eq(&node::BinaryOp::Eq);
        eq.rhs()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Num(0.0));

        let sum = eq.lhs().to_binary();
        sum.op().assert_eq(&node::BinaryOp::Add);
        sum.rhs()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Num(30.0));

        let sum2 = sum.lhs().to_binary();
        sum2.op().assert_eq(&node::BinaryOp::Add);
        sum2.lhs().to_binary().op().assert_eq(&node::BinaryOp::Mul);
        sum2.rhs().to_binary().op().assert_eq(&node::BinaryOp::Div);
    }

    #[test]
    fn if_expr() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("if y then x else 0", expr_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);

        let if_expr = inspector.to_if();
        if_expr.pred().to_qualified().source().has_name("y");
        if_expr.then().to_qualified().source().has_name("x");
        if_expr
            .or_else()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Num(0.0));
    }

    #[test]
    fn qualified_expr() {
        let mut interner = StrInterner::new();

        // Test simple field access
        let ParseResult { node, builder, .. } =
            try_parse_str_with("x.y.z", expr_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);

        let qualified_expr = inspector.to_qualified();
        qualified_expr.source().has_name("x");
        qualified_expr.field_path().inner_at(0).has_name("y");
        qualified_expr.field_path().inner_at(1).has_name("z");

        // Test module path with field access
        let ParseResult { node, builder, .. } = try_parse_str_with(
            "mod1::mod2::value.field1.field2",
            expr_parser(),
            &mut interner,
        );

        let qualified_expr = NodeInspector::new(node, &builder, &interner).to_qualified();
        qualified_expr.module_path().inner_at(0).has_name("mod1");
        qualified_expr.module_path().inner_at(1).has_name("mod2");
        qualified_expr.source().has_name("value");
        qualified_expr.field_path().inner_at(0).has_name("field1");
        qualified_expr.field_path().inner_at(1).has_name("field2");
    }

    #[test]
    fn record_extension() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("{ y | +x = 10 }", expr_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);

        let extend = inspector.to_record_extend();
        extend.source().to_qualified().source().has_name("y");
        extend.field_path().inner_at(0).has_name("x");
        extend
            .value()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Num(10.0));
    }

    #[test]
    fn record() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("{ x = 10, y = 20 }", expr_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);

        let record = inspector.to_record();
        record.has_inner_count(2);
        record.inner_at(0).label().has_name("x");
        record
            .inner_at(0)
            .value()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Num(10.0));
        record.inner_at(1).label().has_name("y");
        record
            .inner_at(1)
            .value()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Num(20.0));
    }

    #[test]
    fn type_expr() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } = try_parse_str_with(
            "Num -> { a : Num, b : Num -> Num } -> Str",
            type_parser(),
            &mut interner,
        );

        let inspector = NodeInspector::new(node, &builder, &interner);

        let func = inspector.to_func();
        func.input().to_qualified().ty().has_name("Num");

        let func2 = func.output().ty().to_func();
        let record = func2.input().to_record();
        record.has_fields_count(2);
        record.fields_at(0).label_or_var().to_label().has_name("a");
        record.fields_at(0).ty().to_qualified().ty().has_name("Num");
        record.fields_at(1).label_or_var().to_label().has_name("b");
        record.fields_at(1).ty().to_func();
        func2.output().ty().to_qualified().ty().has_name("Str");
    }

    #[test]
    fn type_application() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } = try_parse_str_with(
            "Map (Num -> Str) (std::List Str)",
            type_parser(),
            &mut interner,
        );

        let inspector = NodeInspector::new(node, &builder, &interner);

        let app = inspector.to_application();
        let inner_app = app.constructor().to_application();
        inner_app.constructor().to_qualified().ty().has_name("Map");
        inner_app.arg().to_func();

        let list_app = app.arg().to_application();
        list_app.constructor().to_qualified();
        list_app.arg().to_qualified();
    }

    #[test]
    fn type_() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } = try_parse_str_with(
            "forall a b . { left : a, right : Num -> b }",
            type_scheme_parser(),
            &mut interner,
        );

        let inspector = NodeInspector::new(node, &builder, &interner);

        inspector
            .forall()
            .has_inner_count(2)
            .inner_at(0)
            .var()
            .inspect(|name, tree| {
                assert_eq!(interner.get(name.get(tree).0).unwrap(), "a");
            });
        inspector.forall().inner_at(1).var().inspect(|name, tree| {
            assert_eq!(interner.get(name.get(tree).0).unwrap(), "b");
        });

        let record = inspector.ty().to_record();
        record.has_fields_count(2);
        record
            .fields_at(0)
            .label_or_var()
            .to_label()
            .has_name("left");
        record.fields_at(0).ty().to_qualified().ty().has_name("a");
        record
            .fields_at(1)
            .label_or_var()
            .to_label()
            .has_name("right");
        record
            .fields_at(1)
            .ty()
            .to_func()
            .input()
            .to_qualified()
            .ty()
            .has_name("Num");
        record
            .fields_at(1)
            .ty()
            .to_func()
            .output()
            .ty()
            .to_qualified()
            .ty()
            .has_name("b");
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
        inspector.name().has_name("Person");
        inspector.ty_scheme().forall().has_inner_count(1);
        inspector.ty_scheme().ty().to_record().has_fields_count(3);
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
        inspector.name().has_name("Option");
        inspector
            .ty_scheme()
            .forall()
            .has_inner_count(2)
            .inner_at(0)
            .var()
            .inspect(|name, tree| {
                assert_eq!(interner.get(name.get(tree).0).unwrap(), "a");
            });

        let variant = inspector.ty_scheme().ty().to_variant();
        variant.has_tags_count(2);
        variant.tags_at(0).name().has_name("Some");
        variant.tags_at(0).ty().to_qualified().ty().has_name("a");
        variant.tags_at(1).name().has_name("None");
        variant.tags_at(1).has_none_ty();
        variant.extension().has_name("b");
    }

    #[test]
    fn module() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("{ x = 10, type T = Num }", module_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);
        inspector
            .has_inner_count(2)
            .inner_at(0)
            .to_value()
            .name()
            .has_name("x");
        inspector
            .inner_at(0)
            .to_value()
            .value()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Num(10.0));
        inspector.inner_at(1).to_type().name().has_name("T");
        inspector
            .inner_at(1)
            .to_type()
            .ty_scheme()
            .ty()
            .to_qualified()
            .ty()
            .has_name("Num");
    }

    #[test]
    fn module_type() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("{ x : Num, type T }", module_type_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);
        inspector
            .to_concrete()
            .has_inner_count(2)
            .inner_at(0)
            .to_value()
            .name()
            .has_name("x");
        inspector
            .to_concrete()
            .inner_at(0)
            .to_value()
            .ty()
            .ty()
            .to_qualified()
            .ty()
            .has_name("Num");
        inspector
            .to_concrete()
            .inner_at(1)
            .to_opaque_type()
            .name()
            .has_name("T");
    }

    #[test]
    fn nested_module() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } =
            try_parse_str_with("{ module m = { x = 10 } }", module_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);
        inspector
            .has_inner_count(1)
            .inner_at(0)
            .to_module()
            .name()
            .has_name("m");
        inspector.inner_at(0).to_module().has_none_ty();
        inspector
            .inner_at(0)
            .to_module()
            .value()
            .to_module()
            .has_inner_count(1)
            .inner_at(0)
            .to_value()
            .name()
            .has_name("x");
        inspector
            .inner_at(0)
            .to_module()
            .value()
            .to_module()
            .inner_at(0)
            .to_value()
            .value()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Num(10.0));
    }

    #[test]
    fn nested_module_with_type() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } = try_parse_str_with(
            "{ module m : { x : Num } = { x = 10 } }",
            module_parser(),
            &mut interner,
        );

        let inspector = NodeInspector::new(node, &builder, &interner);
        inspector
            .has_inner_count(1)
            .inner_at(0)
            .to_module()
            .name()
            .has_name("m");

        // Check interface
        inspector
            .inner_at(0)
            .to_module()
            .ty()
            .to_concrete()
            .has_inner_count(1)
            .inner_at(0)
            .to_value()
            .name()
            .has_name("x");
        inspector
            .inner_at(0)
            .to_module()
            .ty()
            .to_concrete()
            .inner_at(0)
            .to_value()
            .ty()
            .ty()
            .to_qualified()
            .ty()
            .has_name("Num");

        // Check implementation
        inspector
            .inner_at(0)
            .to_module()
            .value()
            .to_module()
            .has_inner_count(1)
            .inner_at(0)
            .to_value()
            .name()
            .has_name("x");
        inspector
            .inner_at(0)
            .to_module()
            .value()
            .to_module()
            .inner_at(0)
            .to_value()
            .value()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Num(10.0));
    }

    #[test]
    fn comments_are_transparent_to_parser() {
        let mut interner = StrInterner::new();

        let ParseResult { node, builder, .. } = try_parse_str_with(
            r#"{
                # A regular comment
                x = 10,
                ## A doc comment
                y = 20
            }"#,
            module_parser(),
            &mut interner,
        );

        let inspector = NodeInspector::new(node, &builder, &interner);
        inspector.has_inner_count(2);
        inspector.inner_at(0).to_value().name().has_name("x");
        inspector
            .inner_at(0)
            .to_value()
            .value()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Num(10.0));
        inspector.inner_at(1).to_value().name().has_name("y");
        inspector
            .inner_at(1)
            .to_value()
            .value()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Num(20.0));
    }
}
