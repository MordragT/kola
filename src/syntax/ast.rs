use std::fmt;

use ecow::EcoString;
use owo_colors::{OwoColorize, Style};
use serde::{Deserialize, Serialize};

use crate::semantic::types::MonoType;

use super::{node::Node, print::prelude::*, Span};

pub type Symbol = EcoString;

// TODO Label more descriptive ?
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Name {
    pub name: Symbol,
    pub span: Span,
}

impl Printable for Name {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self { name, span } = self;

        let head = span.notate(arena).then(arena.notate(":"), arena);

        let kind = format_args!("{}", "Name".yellow()).notate_in(arena);
        let name = format_args!("\"{}\"", name).notate_in(arena);

        let single = [
            arena.notate(" "),
            kind.clone(),
            arena.notate(" "),
            name.clone(),
        ]
        .join_in(arena);

        let multi = [arena.break_line(), kind, arena.break_line(), name]
            .join_in(arena)
            .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

pub type IdentExpr = Node<Symbol>;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Bool(bool),
    Num(f64),
    Char(char),
    Str(Symbol),
}

impl Printable for Literal {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let kind = format_args!("{}", "Literal".purple()).notate_in(arena);

        let lit = match self {
            Self::Bool(b) => format_args!("\"{b}\"").notate_in(arena),
            Self::Num(n) => format_args!("\"{n}\"").notate_in(arena),
            Self::Char(c) => format_args!("\"{c}\"").notate_in(arena),
            Self::Str(s) => format_args!("\"{s}\"").notate_in(arena),
        };

        let single = arena.notate(" ").then(lit.clone(), arena);
        let multi = arena.break_line().then(lit, arena);

        kind.then(single.or(multi, arena), arena)
    }
}

pub type LiteralExpr = Node<Literal>;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct List {
    pub values: Vec<Expr>,
}

impl Printable for List {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let kind = arena.notate_with("List", Style::new().blue());

        let values = self.values.transform_in(arena);

        let single = values
            .iter()
            .cloned()
            .map(|expr| arena.notate(" ").then(expr.flatten(arena), arena))
            .join_in(arena);
        let multi = values
            .iter()
            .cloned()
            .map(|expr| arena.break_line().then(expr, arena))
            .join_in(arena)
            .indent(arena);

        kind.then(single.or(multi, arena), arena)
    }
}

pub type ListExpr = Node<List>;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Property {
    pub key: Name,
    pub value: Expr,
    pub span: Span,
}

impl Printable for Property {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self { key, value, span } = self;

        let head = span.notate_in(arena).then(arena.notate(":"), arena);

        let kind = format_args!("{}", "Property".blue()).notate_in(arena);
        let key = key.notate(arena);
        let value = value.notate(arena);

        let single = [
            arena.notate(" "),
            kind.clone(),
            arena.notate(" key = "),
            key.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
        ]
        .join_in(arena);

        let multi = [
            arena.break_line(),
            kind,
            arena.break_line(),
            arena.notate("key = "),
            key,
            arena.break_line(),
            arena.notate("value = "),
            value,
        ]
        .join_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl Property {
    pub fn value(&self) -> &Expr {
        &self.value
    }
}

// { x = 10, y = 20 }
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Record {
    pub fields: Vec<Property>,
}

impl Printable for Record {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let head = format_args!("{}", "Record".blue()).notate_in(arena);

        let fields = self.fields.transform_in(arena);

        let single = fields
            .iter()
            .cloned()
            .map(|field| arena.notate(" ").then(field.flatten(arena), arena))
            .join_in(arena);
        let multi = fields
            .iter()
            .cloned()
            .map(|field| arena.break_line().then(field, arena))
            .join_in(arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl Record {
    pub fn get(&self, name: impl AsRef<str>) -> Option<&Property> {
        self.fields.iter().find(|p| &p.key.name == name.as_ref())
    }
}

pub type RecordExpr = Node<Record>;

// x.y.z
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RecordSelect {
    pub source: Expr,
    pub field: Name,
}

impl Printable for RecordSelect {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self { source, field } = self;

        let head = format_args!("{}", "RecordSelect".blue()).notate_in(arena);

        let source = source.notate(arena);
        let field = field.notate(arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", field = "),
            field.clone().flatten(arena),
        ]
        .join_in(arena);

        let multi = [
            arena.break_line(),
            arena.notate("source = "),
            source,
            arena.break_line(),
            arena.notate("field = "),
            field,
        ]
        .join_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

pub type RecordSelectExpr = Node<RecordSelect>;

// following record operations can be combined with syntactic sugar:
// { y | +x = 10 | x = 100 | -x } == y

// { y | +x = 10 }
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RecordExtend {
    pub source: Expr,
    pub field: Name,
    pub value: Expr,
}

impl Printable for RecordExtend {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self {
            source,
            field,
            value,
        } = self;

        let head = format_args!("{}", "RecordExtend".blue()).notate_in(arena);

        let source = source.notate(arena);
        let field = field.notate(arena);
        let value = value.notate(arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", field = "),
            field.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
        ]
        .join_in(arena);

        let multi = [
            arena.break_line(),
            arena.notate("source = "),
            source,
            arena.break_line(),
            arena.notate("field = "),
            field,
            arena.break_line(),
            arena.notate("value = "),
            value,
        ]
        .join_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

pub type RecordExtendExpr = Node<RecordExtend>;

// { y | -x }
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RecordRestrict {
    pub source: Expr,
    pub field: Name,
}

impl Printable for RecordRestrict {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self { source, field } = self;

        let head = format_args!("{}", "RecordRestrict".blue()).notate_in(arena);

        let source = source.notate(arena);
        let field = field.notate(arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", field = "),
            field.clone().flatten(arena),
        ]
        .join_in(arena);

        let multi = [
            arena.break_line(),
            arena.notate("source = "),
            source,
            arena.break_line(),
            arena.notate("field = "),
            field,
        ]
        .join_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

pub type RecordRestrictExpr = Node<RecordRestrict>;

// { y | x = 10 }
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RecordUpdate {
    pub source: Expr,
    pub field: Name,
    pub value: Expr,
}

impl Printable for RecordUpdate {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self {
            source,
            field,
            value,
        } = self;

        let head = format_args!("{}", "RecordUpdate".blue()).notate_in(arena);

        let source = source.notate(arena);
        let field = field.notate(arena);
        let value = value.notate(arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", field = "),
            field.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
        ]
        .join_in(arena);

        let multi = [
            arena.break_line(),
            arena.notate("source = "),
            source,
            arena.break_line(),
            arena.notate("field = "),
            field,
            arena.break_line(),
            arena.notate("value = "),
            value,
        ]
        .join_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

pub type RecordUpdateExpr = Node<RecordUpdate>;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOpKind {
    Neg,
    Not,
}

impl fmt::Display for UnaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Neg => write!(f, "Neg"),
            Self::Not => write!(f, "Not"),
        }
    }
}

impl Printable for UnaryOpKind {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        self.notate_in(arena)
    }
}

pub type UnaryOp = Node<UnaryOpKind>;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Unary {
    pub op: UnaryOp,
    pub target: Expr,
}

impl Printable for Unary {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self { op, target } = self;

        let head = format_args!("{}", "Unary".blue()).notate_in(arena);

        let body = [
            arena.break_line(),
            op.notate(arena),
            arena.break_line(),
            target.notate(arena),
        ]
        .join_in(arena)
        .indent(arena);

        head.then(body, arena)
    }
}

pub type UnaryExpr = Node<Unary>;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // Comparison
    Less,
    Greater,
    LessEq,
    GreaterEq,
    // Logical
    And,
    Or,
    Xor,
    // Equality
    Eq,
    NotEq,
}

impl fmt::Display for BinaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "Add"),
            Self::Sub => write!(f, "Sub"),
            Self::Mul => write!(f, "Mul"),
            Self::Div => write!(f, "Div"),
            Self::Rem => write!(f, "Rem"),
            Self::Less => write!(f, "Less"),
            Self::Greater => write!(f, "Greater"),
            Self::LessEq => write!(f, "LessEq"),
            Self::GreaterEq => write!(f, "GreaterEq"),
            Self::And => write!(f, "And"),
            Self::Or => write!(f, "Or"),
            Self::Xor => write!(f, "Xor"),
            Self::Eq => write!(f, "Eq"),
            Self::NotEq => write!(f, "NotEq"),
        }
    }
}

impl Printable for BinaryOpKind {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        self.notate_in(arena)
    }
}

pub type BinaryOp = Node<BinaryOpKind>;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Binary {
    pub op: BinaryOp,
    pub left: Expr,
    pub right: Expr,
}

impl Printable for Binary {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self { op, left, right } = self;

        let head = format_args!("{}", "Binary".blue()).notate_in(arena);

        let body = [
            arena.break_line(),
            left.notate(arena),
            arena.break_line(),
            op.notate(arena),
            arena.break_line(),
            right.notate(arena),
        ]
        .join_in(arena)
        .indent(arena);

        head.then(body, arena)
    }
}

impl Binary {
    pub fn kind(&self) -> BinaryOpKind {
        *self.op.inner()
    }
}

pub type BinaryExpr = Node<Binary>;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Let {
    pub name: Name,
    pub value: Expr,
    pub inside: Expr,
}

impl Printable for Let {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self {
            name,
            value,
            inside,
        } = self;

        let head = format_args!("{}", "Let".blue()).notate_in(arena);

        let name = name.notate(arena);
        let value = value.notate(arena);
        let inside = inside.notate(arena);

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
            arena.notate(", inside = "),
            inside.clone().flatten(arena),
        ]
        .join_in(arena);

        let multi = [
            arena.break_line(),
            arena.notate("name = "),
            name,
            arena.break_line(),
            arena.notate("value = "),
            value,
            arena.break_line(),
            arena.notate("inside = "),
            inside,
        ]
        .join_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

pub type LetExpr = Node<Let>;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct If {
    pub predicate: Expr,
    pub then: Expr,
    pub or: Expr,
}

impl Printable for If {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self {
            predicate,
            then,
            or,
        } = self;

        let head = format_args!("{}", "Let".blue()).notate_in(arena);

        let predicate = predicate.notate(arena);
        let then = then.notate(arena);
        let or = or.notate(arena);

        let single = [
            arena.notate(" predicate = "),
            predicate.clone().flatten(arena),
            arena.notate(", then = "),
            then.clone().flatten(arena),
            arena.notate(", or = "),
            or.clone().flatten(arena),
        ]
        .join_in(arena);

        let multi = [
            arena.break_line(),
            arena.notate("predicate = "),
            predicate,
            arena.break_line(),
            arena.notate("then = "),
            then,
            arena.break_line(),
            arena.notate("or = "),
            or,
        ]
        .join_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

pub type IfExpr = Node<If>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PatError {
    pub span: Span,
}

impl Printable for PatError {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self { span } = self;

        let head = span.notate_in(arena).then(arena.notate(":"), arena);
        let kind = format_args!("{}", "PatError".red()).notate_in(arena);

        let single = arena.notate(" ").then(kind.clone(), arena);
        let multi = arena.break_line().then(kind, arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Wildcard {
    pub span: Span,
    pub ty: MonoType,
}

impl Printable for Wildcard {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self { span, ty } = self;

        let head = span.notate_in(arena).then(arena.notate(":"), arena);

        let kind = format_args!("{}", "Wildcard".blue()).notate_in(arena);
        let ty = ty.notate_in(arena);

        let single = [
            arena.notate(" "),
            kind.clone(),
            arena.notate(": "),
            ty.clone().flatten(arena),
        ]
        .join_in(arena);

        let multi = [
            arena.break_line(),
            kind,
            arena.break_line(),
            arena.notate(":"),
            ty,
        ]
        .join_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl Wildcard {
    pub fn ty(&self) -> &MonoType {
        &self.ty
    }

    pub fn ty_mut(&mut self) -> &mut MonoType {
        &mut self.ty
    }
}

pub type LiteralPat = Node<Literal>;
pub type IdentPat = Node<Symbol>;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PropertyPat {
    pub key: Name,
    pub value: Option<Pat>,
    pub span: Span,
}

impl Printable for PropertyPat {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self { key, value, span } = self;

        let head = span.notate_in(arena).then(arena.notate(":"), arena);

        let kind = format_args!("{}", "PropertyPat".blue()).notate_in(arena);
        let key = key.notate(arena);
        let value = value.as_ref().map(|v| v.notate(arena));

        let single = [
            arena.notate(" "),
            kind.clone(),
            arena.notate(" key = "),
            key.clone().flatten(arena),
            value
                .clone()
                .map(|v| arena.notate(", value = ").then(v, arena))
                .or_not(arena),
        ]
        .join_in(arena);

        let multi = [
            arena.break_line(),
            kind,
            arena.break_line(),
            arena.notate("key = "),
            key,
            value
                .map(|v| [arena.break_line(), arena.notate("value = "), v].join_in(arena))
                .or_not(arena),
        ]
        .join_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl PropertyPat {
    pub fn value(&self) -> Option<&Pat> {
        self.value.as_ref()
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RecordPatRepr {
    pub fields: Vec<PropertyPat>,
}

impl Printable for RecordPatRepr {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let head = format_args!("{}", "RecordPat".blue()).notate_in(arena);

        let fields = self.fields.transform_in(arena);

        let single = fields
            .iter()
            .cloned()
            .map(|field| arena.notate(" ").then(field.flatten(arena), arena))
            .join_in(arena);
        let multi = fields
            .iter()
            .cloned()
            .map(|field| arena.break_line().then(field, arena))
            .join_in(arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl RecordPatRepr {
    pub fn get(&self, name: impl AsRef<str>) -> Option<&PropertyPat> {
        self.fields.iter().find(|p| &p.key.name == name.as_ref())
    }
}

pub type RecordPat = Node<RecordPatRepr>;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Pat {
    Error(PatError),
    Wildcard(Wildcard),
    Literal(LiteralPat),
    Ident(IdentPat),
    Record(RecordPat),
}

impl Printable for Pat {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        match self {
            Self::Error(e) => e.notate(arena),
            Self::Wildcard(w) => w.notate(arena),
            Self::Literal(l) => l.notate(arena),
            Self::Ident(i) => i.notate(arena),
            Self::Record(r) => r.notate(arena),
        }
    }
}

impl Pat {
    pub fn ty(&self) -> Result<&MonoType, PatError> {
        let ty = match self {
            Self::Error(e) => return Err(*e),
            Self::Wildcard(w) => w.ty(),
            Self::Literal(l) => l.ty(),
            Self::Ident(i) => i.ty(),
            Self::Record(r) => r.ty(),
        };
        Ok(ty)
    }

    pub fn ty_mut(&mut self) -> Result<&mut MonoType, PatError> {
        let ty = match self {
            Self::Error(e) => return Err(*e),
            Self::Wildcard(w) => w.ty_mut(),
            Self::Literal(l) => l.ty_mut(),
            Self::Ident(i) => i.ty_mut(),
            Self::Record(r) => r.ty_mut(),
        };
        Ok(ty)
    }

    pub fn as_error(&self) -> Option<&PatError> {
        match self {
            Self::Error(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_wildcard(&self) -> Option<&Wildcard> {
        match self {
            Self::Wildcard(w) => Some(w),
            _ => None,
        }
    }

    pub fn as_literal(&self) -> Option<&LiteralPat> {
        match self {
            Self::Literal(l) => Some(l),
            _ => None,
        }
    }

    pub fn as_ident(&self) -> Option<&IdentPat> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_record(&self) -> Option<&RecordPat> {
        match self {
            Self::Record(r) => Some(r),
            _ => None,
        }
    }

    pub fn is_error(&self) -> bool {
        self.as_error().is_some()
    }

    pub fn is_wildcard(&self) -> bool {
        self.as_wildcard().is_some()
    }

    pub fn is_literal(&self) -> bool {
        self.as_literal().is_some()
    }

    pub fn is_ident(&self) -> bool {
        self.as_ident().is_some()
    }

    pub fn is_record(&self) -> bool {
        self.as_record().is_some()
    }

    pub fn into_error(self) -> Option<PatError> {
        match self {
            Self::Error(e) => Some(e),
            _ => None,
        }
    }

    pub fn into_wildcard(self) -> Option<Wildcard> {
        match self {
            Self::Wildcard(w) => Some(w),
            _ => None,
        }
    }

    pub fn into_literal(self) -> Option<LiteralPat> {
        match self {
            Self::Literal(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<IdentPat> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }

    pub fn into_record(self) -> Option<RecordPat> {
        match self {
            Self::Record(r) => Some(r),
            _ => None,
        }
    }
}

impl From<Wildcard> for Pat {
    fn from(value: Wildcard) -> Self {
        Self::Wildcard(value)
    }
}

impl From<LiteralPat> for Pat {
    fn from(value: LiteralPat) -> Self {
        Self::Literal(value)
    }
}

impl From<IdentPat> for Pat {
    fn from(value: IdentPat) -> Self {
        Self::Ident(value)
    }
}

impl From<RecordPat> for Pat {
    fn from(value: RecordPat) -> Self {
        Self::Record(value)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Branch {
    pub pat: Pat,
    pub matches: Expr,
    pub span: Span,
}

impl Printable for Branch {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self { pat, matches, span } = self;

        let head = span.notate_in(arena).then(arena.notate(":"), arena);

        let kind = format_args!("{}", "Branch".blue()).notate_in(arena);
        let pat = pat.notate(arena);
        let matches = matches.notate(arena);

        let single = [
            arena.notate(" "),
            kind.clone(),
            arena.notate(" pat = "),
            pat.clone().flatten(arena),
            arena.notate(", matches = "),
            matches.clone().flatten(arena),
        ]
        .join_in(arena);

        let multi = [
            arena.break_line(),
            kind,
            arena.break_line(),
            arena.notate("pat = "),
            pat,
            arena.break_line(),
            arena.notate("matches = "),
            matches,
        ]
        .join_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Case {
    pub source: IdentExpr,
    pub branches: Vec<Branch>,
}

impl Printable for Case {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self { source, branches } = self;

        let head = format_args!("{}", "Case".blue()).notate_in(arena);

        let source = source.notate_in(arena);
        let branches = branches.transform_in(arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", branches = "),
            arena.join_slice(branches).flatten(arena),
        ]
        .join_in(arena);

        let multi = [
            arena.break_line(),
            arena.notate("source = "),
            source.clone(),
            arena.break_line(),
            arena.notate("branches = "),
            arena.join_slice(branches),
        ]
        .join_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

pub type CaseExpr = Node<Case>;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Func {
    pub param: IdentExpr,
    pub body: Expr,
}

impl Printable for Func {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self { param, body } = self;

        let head = format_args!("{}", "Func".blue()).notate_in(arena);

        let param = param.notate(arena);
        let body = body.notate(arena);

        let single = [
            arena.notate(" param = "),
            param.clone().flatten(arena),
            arena.notate(", body = "),
            body.clone().flatten(arena),
        ]
        .join_in(arena);

        let multi = [
            arena.break_line(),
            arena.notate("param = "),
            param,
            arena.break_line(),
            arena.notate("body = "),
            body,
        ]
        .join_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

pub type FuncExpr = Node<Func>;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Call {
    pub func: IdentExpr,
    pub arg: Expr,
}

impl Printable for Call {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self { func, arg } = self;

        let head = format_args!("{}", "Call".blue()).notate_in(arena);

        let func = func.notate(arena);
        let arg = arg.notate(arena);

        let single = [
            arena.notate(" func = "),
            func.clone().flatten(arena),
            arena.notate(", arg = "),
            arg.clone().flatten(arena),
        ]
        .join_in(arena);

        let multi = [
            arena.break_line(),
            arena.notate("func = "),
            func,
            arena.break_line(),
            arena.notate("arg = "),
            arg,
        ]
        .join_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

pub type CallExpr = Node<Call>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExprError {
    pub span: Span,
}

impl Printable for ExprError {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let Self { span } = self;

        let head = span.notate_in(arena).then(arena.notate(":"), arena);
        let kind = format_args!("{}", "ExprError".red()).notate_in(arena);

        let single = arena.notate(" ").then(kind.clone(), arena);
        let multi = arena.break_line().then(kind, arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    Error(ExprError),
    Literal(LiteralExpr),
    Ident(IdentExpr),
    List(ListExpr),
    Record(RecordExpr),
    RecordSelect(RecordSelectExpr),
    RecordExtend(RecordExtendExpr),
    RecordRestrict(RecordRestrictExpr),
    RecordUpdate(RecordUpdateExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Let(LetExpr),
    If(IfExpr),
    Case(CaseExpr),
    Func(FuncExpr),
    Call(CallExpr),
}

impl Printable for Expr {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        match self {
            Self::Error(e) => e.notate(arena),
            Self::Literal(l) => l.notate(arena),
            Self::Ident(i) => i.notate(arena),
            Self::List(l) => l.notate(arena),
            Self::Record(r) => r.notate(arena),
            Self::RecordSelect(r) => r.notate(arena),
            Self::RecordExtend(r) => r.notate(arena),
            Self::RecordRestrict(r) => r.notate(arena),
            Self::RecordUpdate(r) => r.notate(arena),
            Self::Unary(u) => u.notate(arena),
            Self::Binary(b) => b.notate(arena),
            Self::Let(l) => l.notate(arena),
            Self::If(i) => i.notate(arena),
            Self::Case(c) => c.notate(arena),
            Self::Func(f) => f.notate(arena),
            Self::Call(c) => c.notate(arena),
        }
    }
}

// TODO Function Call

impl Expr {
    pub fn ty(&self) -> Result<&MonoType, ExprError> {
        let ty = match self {
            Self::Error(e) => return Err(*e),
            Self::Literal(l) => l.ty(),
            Self::Ident(i) => i.ty(),
            Self::List(l) => l.ty(),
            Self::Record(r) => r.ty(),
            Self::RecordSelect(r) => r.ty(),
            Self::RecordExtend(r) => r.ty(),
            Self::RecordRestrict(r) => r.ty(),
            Self::RecordUpdate(r) => r.ty(),
            Self::Unary(u) => u.ty(),
            Self::Binary(b) => b.ty(),
            Self::Let(l) => l.ty(),
            Self::If(i) => i.ty(),
            Self::Case(c) => c.ty(),
            Self::Func(f) => f.ty(),
            Self::Call(c) => c.ty(),
        };

        Ok(ty)
    }

    pub fn ty_mut(&mut self) -> Result<&MonoType, ExprError> {
        let ty = match self {
            Self::Error(e) => return Err(*e),
            Self::Literal(l) => l.ty_mut(),
            Self::Ident(i) => i.ty_mut(),
            Self::List(l) => l.ty_mut(),
            Self::Record(r) => r.ty_mut(),
            Self::RecordSelect(r) => r.ty_mut(),
            Self::RecordExtend(r) => r.ty_mut(),
            Self::RecordRestrict(r) => r.ty_mut(),
            Self::RecordUpdate(r) => r.ty_mut(),
            Self::Unary(u) => u.ty_mut(),
            Self::Binary(b) => b.ty_mut(),
            Self::Let(l) => l.ty_mut(),
            Self::If(i) => i.ty_mut(),
            Self::Case(c) => c.ty_mut(),
            Self::Func(f) => f.ty_mut(),
            Self::Call(c) => c.ty_mut(),
        };

        Ok(ty)
    }

    pub fn as_error(&self) -> Option<&ExprError> {
        match self {
            Self::Error(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_literal(&self) -> Option<&LiteralExpr> {
        match self {
            Self::Literal(l) => Some(l),
            _ => None,
        }
    }

    pub fn as_ident(&self) -> Option<&IdentExpr> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_list(&self) -> Option<&ListExpr> {
        match self {
            Self::List(l) => Some(l),
            _ => None,
        }
    }

    pub fn as_record(&self) -> Option<&RecordExpr> {
        match self {
            Self::Record(r) => Some(r),
            _ => None,
        }
    }

    pub fn as_record_select(&self) -> Option<&RecordSelectExpr> {
        match self {
            Self::RecordSelect(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_record_extend(&self) -> Option<&RecordExtendExpr> {
        match self {
            Self::RecordExtend(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_record_update(&self) -> Option<&RecordUpdateExpr> {
        match self {
            Self::RecordUpdate(u) => Some(u),
            _ => None,
        }
    }

    pub fn as_unary(&self) -> Option<&UnaryExpr> {
        match self {
            Self::Unary(u) => Some(u),
            _ => None,
        }
    }

    pub fn as_binary(&self) -> Option<&BinaryExpr> {
        match self {
            Self::Binary(b) => Some(b),
            _ => None,
        }
    }

    pub fn as_let(&self) -> Option<&LetExpr> {
        match self {
            Self::Let(l) => Some(l),
            _ => None,
        }
    }

    pub fn as_if(&self) -> Option<&IfExpr> {
        match self {
            Self::If(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_case(&self) -> Option<&CaseExpr> {
        match self {
            Self::Case(c) => Some(c),
            _ => None,
        }
    }

    pub fn as_func(&self) -> Option<&FuncExpr> {
        match self {
            Self::Func(f) => Some(f),
            _ => None,
        }
    }

    pub fn as_call(&self) -> Option<&CallExpr> {
        match self {
            Self::Call(c) => Some(c),
            _ => None,
        }
    }

    pub fn into_error(self) -> Option<ExprError> {
        match self {
            Self::Error(e) => Some(e),
            _ => None,
        }
    }

    pub fn into_literal(self) -> Option<LiteralExpr> {
        match self {
            Self::Literal(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<IdentExpr> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }

    pub fn into_list(self) -> Option<ListExpr> {
        match self {
            Self::List(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_record(self) -> Option<RecordExpr> {
        match self {
            Self::Record(r) => Some(r),
            _ => None,
        }
    }

    pub fn into_record_select(self) -> Option<RecordSelectExpr> {
        match self {
            Self::RecordSelect(s) => Some(s),
            _ => None,
        }
    }

    pub fn into_record_extend(self) -> Option<RecordExtendExpr> {
        match self {
            Self::RecordExtend(e) => Some(e),
            _ => None,
        }
    }

    pub fn into_record_update(self) -> Option<RecordUpdateExpr> {
        match self {
            Self::RecordUpdate(u) => Some(u),
            _ => None,
        }
    }

    pub fn into_unary(self) -> Option<UnaryExpr> {
        match self {
            Self::Unary(u) => Some(u),
            _ => None,
        }
    }

    pub fn into_binary(self) -> Option<BinaryExpr> {
        match self {
            Self::Binary(b) => Some(b),
            _ => None,
        }
    }

    pub fn into_let(self) -> Option<LetExpr> {
        match self {
            Self::Let(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_if(self) -> Option<IfExpr> {
        match self {
            Self::If(i) => Some(i),
            _ => None,
        }
    }

    pub fn into_case(self) -> Option<CaseExpr> {
        match self {
            Self::Case(c) => Some(c),
            _ => None,
        }
    }

    pub fn into_func(self) -> Option<FuncExpr> {
        match self {
            Self::Func(f) => Some(f),
            _ => None,
        }
    }

    pub fn into_call(self) -> Option<CallExpr> {
        match self {
            Self::Call(c) => Some(c),
            _ => None,
        }
    }
}

impl From<LiteralExpr> for Expr {
    fn from(value: LiteralExpr) -> Self {
        Self::Literal(value)
    }
}

impl From<IdentExpr> for Expr {
    fn from(value: IdentExpr) -> Self {
        Self::Ident(value)
    }
}

impl From<ListExpr> for Expr {
    fn from(value: ListExpr) -> Self {
        Self::List(value)
    }
}

impl From<RecordExpr> for Expr {
    fn from(value: RecordExpr) -> Self {
        Self::Record(value)
    }
}

impl From<RecordSelectExpr> for Expr {
    fn from(value: RecordSelectExpr) -> Self {
        Self::RecordSelect(value)
    }
}

impl From<RecordExtendExpr> for Expr {
    fn from(value: RecordExtendExpr) -> Self {
        Self::RecordExtend(value)
    }
}

impl From<RecordRestrictExpr> for Expr {
    fn from(value: RecordRestrictExpr) -> Self {
        Self::RecordRestrict(value)
    }
}

impl From<RecordUpdateExpr> for Expr {
    fn from(value: RecordUpdateExpr) -> Self {
        Self::RecordUpdate(value)
    }
}

impl From<UnaryExpr> for Expr {
    fn from(value: UnaryExpr) -> Self {
        Self::Unary(value)
    }
}

impl From<BinaryExpr> for Expr {
    fn from(value: BinaryExpr) -> Self {
        Self::Binary(value)
    }
}

impl From<LetExpr> for Expr {
    fn from(value: LetExpr) -> Self {
        Self::Let(value)
    }
}

impl From<IfExpr> for Expr {
    fn from(value: IfExpr) -> Self {
        Self::If(value)
    }
}

impl From<CaseExpr> for Expr {
    fn from(value: CaseExpr) -> Self {
        Self::Case(value)
    }
}

impl From<FuncExpr> for Expr {
    fn from(value: FuncExpr) -> Self {
        Self::Func(value)
    }
}

impl From<CallExpr> for Expr {
    fn from(value: CallExpr) -> Self {
        Self::Call(value)
    }
}

// #[derive(Clone, Debug, PartialEq)]
// pub struct Binding {
//     pub pat: Pattern,
//     pub expr: Expr,
// }

// #[derive(Clone, Debug, PartialEq)]
// pub enum TypeExpr {
//     RecordExpr(Box<RecordExpr<'src, Self>>),
// }

// #[derive(Clone, Debug, PartialEq)]
// pub struct TypeBinding {
//     pub ident: &'src str,
//     pub expr: TypeExpr,
// }

// #[derive(Clone, Debug, PartialEq)]
// pub struct Module {
//     pub types: Vec<TypeBinding>,
//     pub stmts: Vec<Binding>,
// }
