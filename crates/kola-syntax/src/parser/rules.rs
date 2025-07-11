use chumsky::prelude::*;

use kola_span::Loc;
use kola_tree::{node::ValueName, prelude::*};

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

pub fn functor_name_parser<'t>() -> impl KolaParser<'t, Id<node::FunctorName>> + Clone {
    // TODO SCREAMING_CASE ?
    lower_symbol().map(node::FunctorName::new).to_node().boxed()
}

pub fn module_type_name_parser<'t>() -> impl KolaParser<'t, Id<node::ModuleTypeName>> + Clone {
    upper_symbol()
        .map(node::ModuleTypeName::new)
        .to_node()
        .boxed()
}

pub fn module_name_parser<'t>() -> impl KolaParser<'t, Id<node::ModuleName>> + Clone {
    lower_symbol().map(node::ModuleName::new).to_node().boxed()
}

pub fn effect_name_parser<'t>() -> impl KolaParser<'t, Id<node::EffectName>> + Clone {
    upper_symbol().map(node::EffectName::new).to_node().boxed()
}

pub fn type_name_parser<'t>() -> impl KolaParser<'t, Id<node::TypeName>> + Clone {
    symbol().map(node::TypeName::new).to_node().boxed()
}

pub fn lower_value_name_parser<'t>() -> impl KolaParser<'t, Id<node::ValueName>> + Clone {
    lower_symbol().map(node::ValueName::new).to_node().boxed()
}

pub fn upper_value_name_parser<'t>() -> impl KolaParser<'t, Id<node::ValueName>> + Clone {
    upper_symbol().map(node::ValueName::new).to_node().boxed()
}

pub fn module_parser<'t>() -> impl KolaParser<'t, Id<node::Module>> + Clone {
    let module_type = module_type_parser();

    recursive(|module| {
        let module_expr = recursive(|module_expr| {
            let module_import = kw(KwT::IMPORT)
                .ignore_then(module_name_parser())
                .map_to_node(node::ModuleImport)
                .to_module_expr()
                .labelled("ModuleImport")
                .as_context()
                .boxed();

            let module_path = module_name_parser()
                .separated_by(ctrl(CtrlT::DOUBLE_COLON))
                .collect()
                .map_to_node(node::ModulePath)
                .to_module_expr()
                .labelled("ModulePath")
                .as_context()
                .boxed();

            let functor_app = nested_parser(
                functor_name_parser()
                    .then(module_expr.clone())
                    .map_to_node(|(func, arg)| node::FunctorApp { func, arg })
                    .to_module_expr(),
                Delim::Paren,
                |_span| node::ModuleError,
            )
            .boxed();

            choice((
                functor_app,
                module.clone().to_module_expr(),
                module_import,
                module_path,
            ))
            .boxed()
        });

        let vis = kw(KwT::EXPORT)
            .to(node::Vis::Export)
            .or_not()
            .map_to_node(|vis| vis.unwrap_or(node::Vis::None))
            .boxed();

        let value_bind = group((
            vis.clone(),
            lower_value_name_parser(),
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

        let effect_op = lower_value_name_parser()
            .then_ignore(ctrl(CtrlT::COLON))
            .then(type_parser())
            .map_to_node(|(name, ty)| node::EffectOpType { name, ty });

        let effect_row = effect_op
            .separated_by(ctrl(CtrlT::COMMA))
            .allow_trailing()
            .collect()
            .delimited_by(open_delim(OpenT::BRACE), close_delim(CloseT::BRACE))
            .map_to_node(node::EffectRowType);

        let effect_type_bind = group((
            vis.clone(),
            kw(KwT::EFFECT)
                .ignore_then(kw(KwT::TYPE))
                .ignore_then(effect_name_parser()),
            op(OpT::ASSIGN).ignore_then(effect_row),
        ))
        .map_to_node(|(vis, name, ty)| node::EffectTypeBind { vis, name, ty })
        .to_bind();

        let module_bind = group((
            vis.clone(),
            kw(KwT::MODULE).ignore_then(module_name_parser()),
            ctrl(CtrlT::COLON).ignore_then(module_type.clone()).or_not(),
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
            vis.clone(),
            kw(KwT::MODULE)
                .ignore_then(kw(KwT::TYPE))
                .ignore_then(module_type_name_parser()),
            op(OpT::ASSIGN).ignore_then(module_type.clone()),
        ))
        .map_to_node(|(vis, name, ty)| node::ModuleTypeBind { vis, name, ty })
        .to_bind();

        let functor_bind = group((
            vis,
            kw(KwT::MODULE)
                .ignore_then(kw(KwT::FUNCTOR))
                .ignore_then(functor_name_parser()),
            module_name_parser(),
            ctrl(CtrlT::COLON).ignore_then(module_type),
            ctrl(CtrlT::DOUBLE_ARROW).ignore_then(module.clone()),
        ))
        .map_to_node(|(vis, name, param, param_ty, body)| node::FunctorBind {
            vis,
            name,
            param,
            param_ty,
            body,
        })
        .to_bind()
        .labelled("Functor")
        .as_context()
        .boxed();

        let bind = choice((
            functor_bind,
            module_type_bind,
            module_bind,
            effect_type_bind,
            type_bind,
            value_bind,
        ))
        .boxed();

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
        let value_spec = lower_value_name_parser()
            .then_ignore(ctrl(CtrlT::COLON))
            .then(type_scheme_parser())
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

        let concrete = spec
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

        concrete.or(qualified).boxed()
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
            LiteralT::Unit => node::LiteralExpr::Unit,
            LiteralT::Num(n) => node::LiteralExpr::Num(n),
            LiteralT::Bool(b) => node::LiteralExpr::Bool(b),
            LiteralT::Char(c) => node::LiteralExpr::Char(c),
            LiteralT::Str(s) => {
                let s = unescaper::unescape(s).unwrap(); // TODO maybe handle this ?
                node::LiteralExpr::Str(state.intern(s))
            }
        }
    })
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
pub fn pat_parser<'t>() -> impl KolaParser<'t, Id<node::Pat>> + Clone {
    recursive(|pat| {
        let bind = lower_value_name_parser()
            .map_to_node(node::BindPat)
            .to_pat();
        let wildcard = ctrl(CtrlT::UNDERSCORE).to(node::AnyPat).to_node().to_pat();
        let literal = literal_parser()
            .map_to_node(node::LiteralPat::from)
            .to_pat();

        let spread = ctrl(CtrlT::TRIPLE_DOT).ignore_then(lower_value_name_parser().or_not());

        // List element pattern: either a pattern or a spread
        let list_element = pat
            .clone()
            .map(node::ListElPat::Pat)
            .or(spread.clone().map(node::ListElPat::Spread))
            .to_node();

        let list = nested_parser(
            list_element
                .separated_by(ctrl(CtrlT::COMMA))
                .allow_trailing()
                .collect()
                .map_to_node(node::ListPat)
                .to_pat(),
            Delim::Bracket,
            |_span| node::PatError,
        );

        // Record field pattern
        let field = lower_value_name_parser()
            .then(ctrl(CtrlT::COLON).ignore_then(pat.clone()).or_not())
            .map_to_node(|(field, pat)| node::RecordFieldPat { field, pat });

        let record = nested_parser(
            field
                .separated_by(ctrl(CtrlT::COMMA))
                .collect()
                .then(ctrl(CtrlT::COMMA).then(ctrl(CtrlT::TRIPLE_DOT)).or_not())
                .map_to_node(|(fields, spread)| node::RecordPat {
                    fields,
                    polymorph: spread.is_some(),
                })
                .to_pat(),
            Delim::Brace,
            |_span| node::PatError,
        );

        // Variant case pattern
        let none_case = kw(KwT::NONE)
            .map_with(|_, e| {
                let span = e.span();
                let tree: &mut State = e.state();

                let name = ValueName::new(tree.interner.intern("None"));
                let tag = tree.insert(name, span);

                node::VariantTagPat { tag, pat: None }
            })
            .to_node();

        let case = upper_value_name_parser()
            .then(ctrl(CtrlT::COLON).ignore_then(pat.clone()).or_not())
            .map_to_node(|(tag, pat)| node::VariantTagPat { tag, pat });

        let variant = nested_parser(
            none_case
                .or(case)
                .separated_by(ctrl(CtrlT::COMMA))
                .allow_trailing()
                .collect()
                .map_to_node(node::VariantPat)
                .to_pat(),
            Delim::Angle,
            |_span| node::PatError,
        );

        choice((bind, wildcard, literal, list, record, variant)).boxed()
    })
    .boxed()
}

pub fn expr_parser<'t>() -> impl KolaParser<'t, Id<node::Expr>> + Clone {
    recursive(|expr| {
        let none = kw(KwT::NONE)
            .map_with(|_, e| {
                let span = e.span();
                let tree: &mut State = e.state();

                let name = ValueName::new(tree.interner.intern("None"));
                let name_id = tree.insert(name, span);
                let tag = tree.insert_as::<node::Expr, _>(node::TagExpr(name_id), span);
                let arg = tree.insert_as::<node::Expr, _>(node::LiteralExpr::Unit, span);

                node::CallExpr { func: tag, arg }
            })
            .to_node()
            .to_expr()
            .labelled("NoneExpr")
            .as_context()
            .boxed();

        let tag = upper_value_name_parser()
            .map_to_node(node::TagExpr)
            .to_expr()
            .labelled("TagExpr")
            .as_context()
            .boxed();

        let type_rep_expr = ctrl(CtrlT::AT)
            .ignore_then(
                symbol()
                    .spanned()
                    .separated_by(ctrl(CtrlT::DOUBLE_COLON))
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .map_with(|mut path, e| {
                let tree: &mut State = e.state();

                let (ty_name, ty_loc) = path.pop().unwrap();
                let ty = tree.insert(node::TypeName::new(ty_name), ty_loc);

                let path = if !path.is_empty() {
                    let module_loc = Loc::covering_located(&path).unwrap(); // Safety: Path is not empty
                    let module_path = path
                        .into_iter()
                        .map(|(name, span)| tree.insert(node::ModuleName::new(name), span))
                        .collect::<Vec<_>>();

                    Some(tree.insert(node::ModulePath(module_path), module_loc))
                } else {
                    None
                };

                node::TypeRepExpr { path, ty }
            })
            .to_node()
            .to_expr()
            .boxed();

        // Qualified expression (module::record.field) for variable and module access
        let qualified = group((
            lower_symbol().spanned(),
            ctrl(CtrlT::DOUBLE_COLON)
                .ignore_then(lower_symbol().spanned())
                .repeated()
                .collect::<Vec<_>>(),
            ctrl(CtrlT::DOT)
                .ignore_then(lower_value_name_parser())
                .repeated()
                .at_least(1)
                .collect()
                .map_to_node(node::FieldPath)
                .or_not(),
        ))
        .map_with(|(mut source, mut path, field_path), e| {
            let tree: &mut State = e.state();

            let module_path = if path.is_empty() {
                None
            } else {
                // For a::b::c.field1.field2:
                // - Original: source=a, path=[b, c]
                // - After transform: source=c, path=[a, b]
                // - Result: ModulePath=[a, b], SelectExpr={source: c, fields: [field1, field2]}
                let new_source = path.pop().unwrap();
                path.insert(0, source);
                source = new_source;

                let path_loc = Loc::covering_located(&path).unwrap(); // Safety: Path is not empty

                let path = path
                    .into_iter()
                    .map(|(key, span)| tree.insert(node::ModuleName::new(key), span))
                    .collect();
                let path = tree.insert(node::ModulePath(path), path_loc);

                Some(path)
            };

            let source = tree.insert(node::ValueName::new(source.0), source.1);

            node::QualifiedExpr {
                module_path,
                source,
                field_path,
            }
        })
        .to_node()
        .to_expr()
        .labelled("QualifiedExpr")
        .as_context()
        .boxed();

        let literal = literal_parser()
            .to_node()
            .to_expr()
            .labelled("LiteralExpr")
            .as_context();

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

        let field = group((
            lower_value_name_parser(),
            ctrl(CtrlT::COLON).ignore_then(type_parser()).or_not(),
            op(OpT::ASSIGN).ignore_then(expr.clone()),
        ))
        .map(|(label, ty, value)| node::RecordField { label, ty, value })
        .to_node();

        let instantiate = field
            .separated_by(ctrl(CtrlT::COMMA))
            .allow_trailing()
            .collect()
            .map_to_node(node::RecordExpr)
            .to_expr()
            .boxed();

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

        let field_path = lower_value_name_parser()
            .separated_by(ctrl(CtrlT::DOT))
            .collect()
            .map_to_node(node::FieldPath)
            .boxed();

        let extend = group((
            op(OpT::ADD).ignore_then(field_path.clone()),
            ctrl(CtrlT::COLON).ignore_then(type_parser()).or_not(),
            op(OpT::ASSIGN).ignore_then(expr.clone()),
        ))
        .map(|(select, type_, value)| RecordOp::Extend(select, type_, value))
        .boxed();

        let restrict = op(OpT::SUB)
            .ignore_then(field_path.clone())
            .then(ctrl(CtrlT::COLON).ignore_then(type_parser()).or_not())
            .map(|(select, type_)| RecordOp::Restrict(select, type_))
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

        let update = group((
            field_path.clone(),
            ctrl(CtrlT::COLON).ignore_then(type_parser()).or_not(),
            update_op,
            expr.clone(),
        ))
        .map(|(field, type_, op, value)| RecordOp::Update(field, type_, op, value))
        .boxed();

        let inner_op = ctrl(CtrlT::PIPE)
            .ignore_then(choice((extend, restrict, update)))
            .repeated()
            .at_least(1);

        let record_op = qualified
            .clone()
            .then(ctrl(CtrlT::COLON).ignore_then(type_parser()).or_not())
            .foldl_with(inner_op, |(source, source_type), op, e| {
                let span = e.span();
                let tree: &mut State = e.state();

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
                    RecordOp::Restrict(field_path, value_type) => tree.insert_as::<node::Expr, _>(
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

                (expr, None) // Set this to None so that subsequent record operations do not get a type missmatch due to the changed type
            })
            .map(|(expr, _)| expr)
            .boxed();

        let record_expr = nested_parser(record_op.or(instantiate), Delim::Brace, |_span| {
            node::ExprError
        })
        .labelled("RecordExpr")
        .as_context();

        // TODO allow type annotation
        let let_ = group((
            kw(KwT::LET).ignore_then(lower_value_name_parser()),
            ctrl(CtrlT::COLON).ignore_then(type_parser()).or_not(),
            op(OpT::ASSIGN).ignore_then(expr.clone()),
            kw(KwT::IN).ignore_then(expr.clone()),
        ))
        .map_to_node(|(name, value_type, value, body)| node::LetExpr {
            name,
            value_type,
            value,
            body,
        })
        .to_expr()
        .labelled("LetExpr")
        .as_context()
        .boxed();

        let if_ = group((
            kw(KwT::IF).ignore_then(expr.clone()),
            kw(KwT::THEN).ignore_then(expr.clone()),
            kw(KwT::ELSE).ignore_then(expr.clone()),
        ))
        .map_to_node(|(pred, then, or_else)| node::IfExpr {
            pred,
            then,
            or_else,
        })
        .to_expr()
        .labelled("IfExpr")
        .as_context()
        .boxed();

        let branch = pat_parser()
            .then_ignore(ctrl(CtrlT::DOUBLE_ARROW))
            .then(expr.clone())
            .map(|(pat, body)| node::CaseBranch { pat, body })
            .to_node();

        let branches = ctrl(CtrlT::PIPE)
            .ignore_then(branch)
            .repeated()
            .at_least(1)
            .collect();

        let case = kw(KwT::CASE)
            .ignore_then(qualified.clone())
            .then(branches)
            .map_to_node(|(source, branches)| node::CaseExpr { source, branches })
            .to_expr()
            .labelled("CaseExpr")
            .as_context()
            .boxed();

        let clause = group((
            lower_value_name_parser(),
            lower_value_name_parser(),
            ctrl(CtrlT::DOUBLE_ARROW).ignore_then(expr.clone()),
        ))
        .map_to_node(|(op, param, body)| node::HandlerClause { op, param, body });

        let clauses = ctrl(CtrlT::PIPE)
            .ignore_then(clause)
            .repeated()
            .at_least(1)
            .collect();

        let handle = kw(KwT::HANDLE)
            .ignore_then(qualified.clone())
            .then(clauses)
            .map_to_node(|(source, clauses)| node::HandleExpr { source, clauses })
            .to_expr()
            .labelled("HandleExpr")
            .as_context()
            .boxed();

        let do_expr = kw(KwT::DO)
            .ignore_then(lower_value_name_parser())
            .then(expr.clone())
            .map_to_node(|(op, arg)| node::DoExpr { op, arg })
            .to_expr()
            .labelled("DoExpr")
            .as_context()
            .boxed();

        let func = group((
            kw(KwT::FN).ignore_then(lower_value_name_parser()),
            ctrl(CtrlT::COLON).ignore_then(type_parser()).or_not(),
            ctrl(CtrlT::DOUBLE_ARROW).ignore_then(expr.clone()),
        ))
        .map_to_node(|(param, param_type, body)| node::LambdaExpr {
            param,
            param_type,
            body,
        })
        .to_expr()
        .labelled("FuncExpr")
        .as_context()
        .boxed();

        // // TODO allow syntactic sugar (a b c)
        // let call = recursive(|call| {
        //     let callable = choice((qualified.clone(), tag.clone(), func.clone(), call));

        //     nested_parser(
        //         callable
        //             .then(expr.clone())
        //             .map_to_node(|(func, arg)| node::CallExpr { func, arg })
        //             .to_expr(),
        //         Delim::Paren,
        //         |_span| node::ExprError,
        //     )
        //     .boxed()
        // })
        // .labelled("CallExpr")
        // .as_context()
        // .boxed();

        let paren_expr = nested_parser(
            expr.clone()
                .repeated()
                .at_least(1) // At least 1 expression
                .collect::<Vec<_>>()
                .map_with(|exprs, e| {
                    let span = e.span();
                    let tree: &mut State = e.state();

                    let len = exprs.len();
                    let mut iter = exprs.into_iter();
                    let first = iter.next().unwrap(); // Safety: len >= 1

                    if len == 1 {
                        // Single expression - this is grouping, just return the inner expression
                        first
                    } else {
                        // Multiple expressions - this is a function call
                        iter.fold(first, |acc, arg| {
                            tree.insert_as::<node::Expr, _>(node::CallExpr { func: acc, arg }, span)
                        })
                    }
                }),
            Delim::Paren,
            |_span| node::ExprError,
        )
        .labelled("ParenExpr")
        .as_context()
        .boxed();

        let atom = choice((
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
            qualified,
            none,
            tag,
            type_rep_expr,
        ))
        .boxed();

        let unary_op = op(OpT::SUB)
            .to(node::UnaryOp::Neg)
            .or(op(OpT::NOT).to(node::UnaryOp::Not))
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
            .foldl_with(product_op.then(unary).repeated(), |lhs, (op, rhs), e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, lhs, rhs }, span)
            })
            .boxed();

        let sum_op = op(OpT::ADD)
            .to(node::BinaryOp::Add)
            .or(op(OpT::SUB).to(node::BinaryOp::Sub))
            .to_node();
        let sum = product
            .clone()
            .foldl_with(sum_op.then(product).repeated(), |lhs, (op, rhs), e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, lhs, rhs }, span)
            })
            .boxed();

        let concat = sum
            .clone()
            .foldl_with(
                op(OpT::CONCAT)
                    .to(node::BinaryOp::Concat)
                    .to_node()
                    .then(sum)
                    .repeated(),
                |lhs, (op, rhs), e| {
                    let span = e.span();
                    let tree: &mut State = e.state();
                    tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, lhs, rhs }, span)
                },
            )
            .boxed();

        let merge = concat
            .clone()
            .foldl_with(
                op(OpT::MERGE).ignore_then(concat).repeated(),
                |lhs, rhs, e| {
                    let span = e.span();
                    let tree: &mut State = e.state();
                    tree.insert_as::<node::Expr, _>(node::RecordMergeExpr { lhs, rhs }, span)
                },
            )
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
        let comparison = merge
            .clone()
            .foldl_with(comparison_op.then(merge).repeated(), |lhs, (op, rhs), e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, lhs, rhs }, span)
            })
            .boxed();

        let logical_op = choice((
            op(OpT::AND).to(node::BinaryOp::And),
            op(OpT::OR).to(node::BinaryOp::Or),
        ))
        .to_node();
        let logical = comparison
            .clone()
            .foldl_with(
                logical_op.then(comparison).repeated(),
                |lhs, (op, rhs), e| {
                    let span = e.span();
                    let tree: &mut State = e.state();
                    tree.insert_as::<node::Expr, _>(node::BinaryExpr { op, lhs, rhs }, span)
                },
            )
            .boxed();

        // Pipe backward (<|) - right associative, higher precedence
        // f <| g <| x should parse as f(g(x))
        let pipe_backward = logical
            .clone()
            .spanned()
            .then(
                ctrl(CtrlT::PIPE_BACKWARD)
                    .ignore_then(logical.clone().spanned())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map_with(|(first, mut rest), e| {
                let tree: &mut State = e.state();

                // The first element is actually the last, therefore we need to do some swapping
                rest.insert(0, first);
                let first = rest.pop().unwrap().0; // Safety: rest is not empty

                // Right-fold the rest to handle right associativity
                rest.into_iter().rfold(first, |acc, (func, span)| {
                    tree.insert_as::<node::Expr, _>(node::CallExpr { func, arg: acc }, span)
                })
            })
            .boxed();

        // Pipe forward (|>) - left associative, lower precedence
        // a |> f |> g should parse as g(f(a))
        let pipe_forward = pipe_backward.clone().foldl_with(
            ctrl(CtrlT::PIPE_FORWARD)
                .ignore_then(pipe_backward)
                .repeated(),
            |lhs, rhs, e| {
                let span = e.span();
                let tree: &mut State = e.state();
                tree.insert_as::<node::Expr, _>(
                    node::CallExpr {
                        func: rhs,
                        arg: lhs,
                    },
                    span,
                )
            },
        );

        pipe_forward
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
pub fn type_parser<'t>() -> impl KolaParser<'t, Id<node::Type>> + Clone {
    recursive(|ty| {
        let qual_ty = symbol()
            .spanned()
            .separated_by(ctrl(CtrlT::DOUBLE_COLON))
            .at_least(1)
            .collect::<Vec<_>>()
            .map_with(|mut path, e| {
                let tree: &mut State = e.state();

                let (ty_name, ty_loc) = path.pop().unwrap();
                let ty = tree.insert(node::TypeName::new(ty_name), ty_loc);

                let path = if !path.is_empty() {
                    let module_loc = Loc::covering_located(&path).unwrap(); // Safety: Path is not empty
                    let module_path = path
                        .into_iter()
                        .map(|(name, span)| tree.insert(node::ModuleName::new(name), span))
                        .collect::<Vec<_>>();

                    Some(tree.insert(node::ModulePath(module_path), module_loc))
                } else {
                    None
                };

                node::QualifiedType { path, ty }
            })
            .to_node()
            .to_type()
            .boxed();

        let row_var = ctrl(CtrlT::PIPE)
            .ignore_then(type_name_parser())
            .or_not()
            .boxed();

        let field = lower_value_name_parser()
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
                .to_type(),
            Delim::Brace,
            |_span| node::TypeError,
        );

        let none_tag = kw(KwT::NONE)
            .map_with(|_, e| {
                let span = e.span();
                let tree: &mut State = e.state();

                let name = ValueName::new(tree.interner.intern("None"));
                let tag = tree.insert(name, span);

                node::TagType {
                    name: tag,
                    ty: None,
                }
            })
            .to_node();

        let tag = upper_value_name_parser()
            .then(ctrl(CtrlT::COLON).ignore_then(ty.clone()).or_not())
            .map_to_node(|(name, ty)| node::TagType { name, ty });

        let variant = nested_parser(
            none_tag
                .or(tag)
                .separated_by(ctrl(CtrlT::COMMA))
                .at_least(1)
                .allow_trailing()
                .collect()
                .then(row_var)
                .map_to_node(|(tags, extension)| node::VariantType { tags, extension })
                .to_type(),
            Delim::Angle,
            |_span| node::TypeError,
        );

        let atom = choice((
            qual_ty,
            record,
            variant,
            nested_parser(ty.clone(), Delim::Paren, |_| node::TypeError),
        ));

        let appl = atom
            .clone()
            .foldl_with(atom.clone().repeated(), |constructor, arg, e| {
                let span = e.span();
                let tree: &mut State = e.state();

                tree.insert_as::<node::Type, _>(node::TypeApplication { constructor, arg }, span)
            })
            .boxed();

        let effect_op = lower_value_name_parser()
            .then_ignore(ctrl(CtrlT::COLON))
            .then(ty.clone())
            .map_to_node(|(name, ty)| node::EffectOpType { name, ty });

        let effect_row = effect_op
            .separated_by(ctrl(CtrlT::COMMA))
            .allow_trailing()
            .collect()
            .delimited_by(open_delim(OpenT::BRACE), close_delim(CloseT::BRACE))
            .map_to_node(node::EffectRowType)
            .map_to_node(node::EffectType::Row);

        let qual_effect = symbol()
            .spanned()
            .separated_by(ctrl(CtrlT::DOUBLE_COLON))
            .at_least(1)
            .collect::<Vec<_>>()
            .map_with(|mut path, e| {
                let tree: &mut State = e.state();

                let (eff_name, ty_loc) = path.pop().unwrap();
                let ty = tree.insert(node::EffectName::new(eff_name), ty_loc);

                let path = if !path.is_empty() {
                    let module_loc = Loc::covering_located(&path).unwrap(); // Safety: Path is not empty
                    let module_path = path
                        .into_iter()
                        .map(|(name, span)| tree.insert(node::ModuleName::new(name), span))
                        .collect::<Vec<_>>();

                    Some(tree.insert(node::ModulePath(module_path), module_loc))
                } else {
                    None
                };

                node::QualifiedEffectType { path, ty }
            })
            .to_node()
            .map_to_node(node::EffectType::Qualified)
            .boxed();

        let computation = ty
            .clone()
            .then(
                ctrl(CtrlT::TILDE)
                    .ignore_then(effect_row.or(qual_effect))
                    .or_not(),
            )
            .map_to_node(|(ty, effect)| node::CompType { ty, effect });

        let func = appl
            .clone()
            .foldl_with(
                ctrl(CtrlT::ARROW).ignore_then(computation).repeated(),
                |input, output, e| {
                    let span = e.span();
                    let tree: &mut State = e.state();

                    tree.insert_as::<node::Type, _>(node::FuncType { input, output }, span)
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
pub fn type_scheme_parser<'t>() -> impl KolaParser<'t, Id<node::TypeScheme>> + Clone {
    let var = symbol().map_to_node(node::TypeVar);

    let kind = choice((
        kw(KwT::RECORD).to(node::Kind::Record),
        kw(KwT::LABEL).to(node::Kind::Label),
    ))
    .to_node();

    let bind = kind
        .or_not()
        .then(var)
        .map_to_node(|(kind, var)| node::TypeVarBind { kind, var })
        .boxed();

    let with = kw(KwT::WITH)
        .ignore_then(bind.clone().repeated().at_least(1).collect())
        .then_ignore(ctrl(CtrlT::DOT))
        .map_to_node(node::WithBinder);

    // hindley milner only allows standard polymorphism (top-level forall)
    // higher-rank polymorphism (nested forall) is undecidable for full type-inference
    let forall = kw(KwT::FORALL)
        .ignore_then(bind.repeated().at_least(1).collect())
        .then_ignore(ctrl(CtrlT::DOT))
        .map_to_node(node::ForallBinder);

    group((with.or_not(), forall.or_not(), type_parser()))
        .map_to_node(|(with, forall, ty)| node::TypeScheme { with, forall, ty })
        .boxed()
}

/// Parser for type bindings in the language.
///
/// Grammar:
/// ```bnf
/// type_bind ::= 'type' name '=' type
/// ```
pub fn type_bind_parser<'t>() -> impl KolaParser<'t, Id<node::TypeBind>> + Clone {
    let vis = kw(KwT::EXPORT)
        .to(node::Vis::Export)
        .or_not()
        .map_to_node(|vis| vis.unwrap_or(node::Vis::None))
        .boxed();

    group((
        vis,
        kw(KwT::TYPE).ignore_then(type_name_parser()),
        op(OpT::ASSIGN).ignore_then(type_scheme_parser()),
    ))
    .map_to_node(|(vis, name, ty_scheme)| node::TypeBind {
        vis,
        name,
        ty_scheme,
    })
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
        expr_parser, module_parser, module_type_parser, pat_parser, type_bind_parser, type_parser,
        type_scheme_parser,
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
            "case x of 1 => true, _ => false",
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
            case data of
            () => "unit",
            true => "bool true",
            42 => "number",
            'x' => "char",
            "hello" => "string",
            x => "bind pattern",
            _ => "wildcard",
            [a, b, ...rest] => "list with spread",
            [head, ...] => "list anonymous spread",
            [] => "empty list",
            { name, age: years, ... } => "record with spread",
            { x, y } => "simple record",
            {} => "empty record",
            < Some : value > => "variant some",
            < None > => "variant none",
            < Ok : result, Err : error > => "multiple variants"
        "#;

        let ParseResult { node, builder, .. } =
            try_parse_str_with(test_case, expr_parser(), &mut interner);

        let inspector = NodeInspector::new(node, &builder, &interner);
        let case = inspector.to_case();
        case.source().to_qualified().source().has_name("data");
        case.has_branches_count(16);

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
            .assert_eq(&node::LiteralPat::Char('x'));
        case.branches_at(3)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["char"]));
        case.branches_at(4)
            .pat()
            .to_literal()
            .assert_eq(&node::LiteralPat::Str(interner["hello"]));
        case.branches_at(4)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["string"]));

        // Test bind and wildcard patterns
        case.branches_at(5).pat().to_bind().inner().has_name("x");
        case.branches_at(5)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["bind pattern"]));
        case.branches_at(6).pat().to_any();
        case.branches_at(6)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["wildcard"]));

        // Test list patterns
        case.branches_at(7)
            .pat()
            .to_list()
            .has_inner_count(3)
            .inner_at(0)
            .to_pat()
            .to_bind()
            .inner()
            .has_name("a");
        case.branches_at(7)
            .pat()
            .to_list()
            .inner_at(1)
            .to_pat()
            .to_bind()
            .inner()
            .has_name("b");
        case.branches_at(7)
            .pat()
            .to_list()
            .inner_at(2)
            .to_some_spread()
            .has_name("rest");
        case.branches_at(7)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["list with spread"]));

        case.branches_at(8)
            .pat()
            .to_list()
            .has_inner_count(2)
            .inner_at(0)
            .to_pat()
            .to_bind()
            .inner()
            .has_name("head");
        case.branches_at(8)
            .pat()
            .to_list()
            .inner_at(1)
            .is_none_spread();
        case.branches_at(8)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["list anonymous spread"]));

        case.branches_at(9).pat().to_list().has_inner_count(0);
        case.branches_at(9)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["empty list"]));

        // Test record patterns
        case.branches_at(10)
            .pat()
            .to_record()
            .has_fields_count(2)
            .fields_at(0)
            .field()
            .has_name("name");
        case.branches_at(10)
            .pat()
            .to_record()
            .fields_at(0)
            .has_none_pat();
        case.branches_at(10)
            .pat()
            .to_record()
            .fields_at(1)
            .field()
            .has_name("age");
        case.branches_at(10)
            .pat()
            .to_record()
            .fields_at(1)
            .pat()
            .to_bind()
            .inner()
            .has_name("years");
        assert!(case.branches_at(10).pat().to_record().get().polymorph);
        case.branches_at(10)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["record with spread"]));

        case.branches_at(11)
            .pat()
            .to_record()
            .has_fields_count(2)
            .fields_at(0)
            .field()
            .has_name("x");
        case.branches_at(11)
            .pat()
            .to_record()
            .fields_at(1)
            .field()
            .has_name("y");
        case.branches_at(11)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["simple record"]));

        case.branches_at(12).pat().to_record().has_fields_count(0);
        case.branches_at(12)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["empty record"]));

        // Test variant patterns
        case.branches_at(13)
            .pat()
            .to_variant()
            .has_inner_count(1)
            .inner_at(0)
            .tag()
            .has_name("Some");
        case.branches_at(13)
            .pat()
            .to_variant()
            .inner_at(0)
            .pat()
            .to_bind()
            .inner()
            .has_name("value");
        case.branches_at(13)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["variant some"]));

        case.branches_at(14)
            .pat()
            .to_variant()
            .has_inner_count(1)
            .inner_at(0)
            .tag()
            .has_name("None");
        case.branches_at(14)
            .pat()
            .to_variant()
            .inner_at(0)
            .has_none_pat();
        case.branches_at(14)
            .body()
            .to_literal()
            .assert_eq(&node::LiteralExpr::Str(interner["variant none"]));

        case.branches_at(15)
            .pat()
            .to_variant()
            .has_inner_count(2)
            .inner_at(0)
            .tag()
            .has_name("Ok");
        case.branches_at(15)
            .pat()
            .to_variant()
            .inner_at(0)
            .pat()
            .to_bind()
            .inner()
            .has_name("result");
        case.branches_at(15)
            .pat()
            .to_variant()
            .inner_at(1)
            .tag()
            .has_name("Err");
        case.branches_at(15)
            .pat()
            .to_variant()
            .inner_at(1)
            .pat()
            .to_bind()
            .inner()
            .has_name("error");
        case.branches_at(15)
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
        record.fields_at(0).name().has_name("a");
        record.fields_at(0).ty().to_qualified().ty().has_name("Num");
        record.fields_at(1).name().has_name("b");
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
        record.fields_at(0).name().has_name("left");
        record.fields_at(0).ty().to_qualified().ty().has_name("a");
        record.fields_at(1).name().has_name("right");
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

        let ParseResult { node, builder, .. } = try_parse_str_with(
            "{ x : Num, type T = Str }",
            module_type_parser(),
            &mut interner,
        );

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
            .to_type_bind()
            .name()
            .has_name("T");
        inspector
            .to_concrete()
            .inner_at(1)
            .to_type_bind()
            .ty_scheme()
            .ty()
            .to_qualified()
            .ty()
            .has_name("Str");
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
}
