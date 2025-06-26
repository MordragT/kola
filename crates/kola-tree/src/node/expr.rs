use derive_more::{Display, From, IntoIterator};
use enum_as_inner::EnumAsInner;
use kola_macros::{Inspector, Notate};
use kola_print::prelude::*;
use kola_utils::interner::StrKey;
use serde::{Deserialize, Serialize};

use super::{ModulePath, Pat, Type, ValueName};
use crate::{
    id::Id,
    print::NodePrinter,
    tree::{TreeBuilder, TreeView},
};

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(color = "red")]
pub struct ExprError;

impl ExprError {
    pub fn new_in(builder: &mut TreeBuilder) -> Id<Self> {
        builder.insert(Self)
    }
}

#[derive(Debug, EnumAsInner, From, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum LiteralExpr {
    Unit,
    Bool(bool),
    Num(f64),
    Char(char),
    Str(StrKey),
}

impl LiteralExpr {
    pub fn new_in(value: impl Into<Self>, builder: &mut TreeBuilder) -> Id<Self> {
        builder.insert(value.into())
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, LiteralExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "LiteralExpr".purple().display_in(arena);

        let lit = match *self.value {
            LiteralExpr::Unit => "Unit".yellow().display_in(arena),
            LiteralExpr::Bool(b) => b.yellow().display_in(arena),
            LiteralExpr::Num(n) => n.yellow().display_in(arena),
            LiteralExpr::Char(c) => c.yellow().display_in(arena),
            LiteralExpr::Str(s) => self
                .interner
                .get(s)
                .expect("Symbol not found")
                .yellow()
                .display_in(arena),
        }
        .enclose_by(arena.just('"'), arena);

        let single = arena.just(' ').then(lit.clone(), arena);
        let multi = arena.newline().then(lit, arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    IntoIterator,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
#[into_iterator(owned, ref)]
pub struct ListExpr(pub Vec<Id<Expr>>);

impl ListExpr {
    pub fn empty_in(builder: &mut TreeBuilder) -> Id<Self> {
        builder.insert(Self(Vec::new()))
    }

    pub fn new_in<I>(elements: I, builder: &mut TreeBuilder) -> Id<Self>
    where
        I: IntoIterator,
        I::Item: Into<Expr>,
    {
        let elements = elements
            .into_iter()
            .map(|e| builder.insert(e.into()))
            .collect();
        builder.insert(Self(elements))
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
pub struct RecordField {
    pub field: Id<ValueName>,
    pub type_: Option<Id<Type>>,
    pub value: Id<Expr>,
}

impl RecordField {
    pub fn new_in(
        field: impl Into<ValueName>,
        type_: Option<Type>,
        value: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let field = builder.insert(field.into());
        let type_ = type_.map(|t| builder.insert(t));
        let value = builder.insert(value.into());

        builder.insert(Self {
            field,
            type_,
            value,
        })
    }

    pub fn field(self, tree: &impl TreeView) -> ValueName {
        *self.field.get(tree)
    }

    pub fn type_(self, tree: &impl TreeView) -> Option<Type> {
        self.type_.map(|t| *t.get(tree))
    }

    pub fn value(self, tree: &impl TreeView) -> Expr {
        *self.value.get(tree)
    }
}

// { x = 10, y = 20 }
#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    IntoIterator,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
#[into_iterator(owned, ref)]
pub struct RecordExpr(pub Vec<Id<RecordField>>);

impl RecordExpr {
    pub fn new_in<I>(fields: I, builder: &mut TreeBuilder) -> Id<Self>
    where
        I: IntoIterator<Item = Id<RecordField>>,
    {
        let fields = fields.into_iter().collect();
        builder.insert(Self(fields))
    }
}

// { y [: type] | +x [: type] = 10 }
#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
pub struct RecordExtendExpr {
    pub source: Id<Expr>,
    pub source_type: Option<Id<Type>>,
    pub select: Id<FieldPath>,
    pub value: Id<Expr>,
    pub value_type: Option<Id<Type>>,
}

impl RecordExtendExpr {
    pub fn new_in(
        source: impl Into<Expr>,
        source_type: Option<Type>,
        select: impl Into<FieldPath>,
        value: impl Into<Expr>,
        value_type: Option<Type>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let source = builder.insert(source.into());
        let source_type = source_type.map(|t| builder.insert(t));
        let select = builder.insert(select.into());
        let value = builder.insert(value.into());
        let value_type = value_type.map(|t| builder.insert(t));

        builder.insert(Self {
            source,
            source_type,
            select,
            value,
            value_type,
        })
    }

    pub fn source(self, tree: &impl TreeView) -> Expr {
        *self.source.get(tree)
    }

    pub fn source_type(self, tree: &impl TreeView) -> Option<Type> {
        self.source_type.map(|t| *t.get(tree))
    }

    pub fn select(self, tree: &impl TreeView) -> &FieldPath {
        self.select.get(tree)
    }

    pub fn value(self, tree: &impl TreeView) -> Expr {
        *self.value.get(tree)
    }

    pub fn value_type(self, tree: &impl TreeView) -> Option<Type> {
        self.value_type.map(|t| *t.get(tree))
    }
}

// { y [: type] | -x [: type] }
#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
pub struct RecordRestrictExpr {
    pub source: Id<Expr>,
    pub source_type: Option<Id<Type>>,
    pub select: Id<FieldPath>,
    pub value_type: Option<Id<Type>>,
}

impl RecordRestrictExpr {
    pub fn new_in(
        source: impl Into<Expr>,
        source_type: Option<Type>,
        select: impl Into<FieldPath>,
        value_type: Option<Type>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let source = builder.insert(source.into());
        let source_type = source_type.map(|t| builder.insert(t));
        let select = builder.insert(select.into());
        let value_type = value_type.map(|t| builder.insert(t));

        builder.insert(Self {
            source,
            source_type,
            select,
            value_type,
        })
    }

    pub fn source(self, tree: &impl TreeView) -> Expr {
        *self.source.get(tree)
    }

    pub fn source_type(self, tree: &impl TreeView) -> Option<Type> {
        self.source_type.map(|t| *t.get(tree))
    }

    pub fn select(self, tree: &impl TreeView) -> &FieldPath {
        self.select.get(tree)
    }

    pub fn value_type(self, tree: &impl TreeView) -> Option<Type> {
        self.value_type.map(|t| *t.get(tree))
    }
}

#[derive(
    Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum RecordUpdateOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
}

impl<'a> Notate<'a> for NodePrinter<'a, RecordUpdateOp> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        self.value.red().display_in(arena)
    }
}

// { y | x = 10 }
#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
pub struct RecordUpdateExpr {
    pub source: Id<Expr>,
    pub source_type: Option<Id<Type>>,
    pub select: Id<FieldPath>,
    pub op: Id<RecordUpdateOp>,
    pub value: Id<Expr>,
    pub value_type: Option<Id<Type>>,
}

impl RecordUpdateExpr {
    pub fn new_in(
        source: impl Into<Expr>,
        source_type: Option<Type>,
        select: impl Into<FieldPath>,
        op: RecordUpdateOp,
        value: impl Into<Expr>,
        value_type: Option<Type>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let source = builder.insert(source.into());
        let source_type = source_type.map(|t| builder.insert(t));
        let select = builder.insert(select.into());
        let op = builder.insert(op);
        let value = builder.insert(value.into());
        let value_type = value_type.map(|t| builder.insert(t));

        builder.insert(Self {
            source,
            source_type,
            select,
            op,
            value,
            value_type,
        })
    }

    pub fn source(self, tree: &impl TreeView) -> Expr {
        *self.source.get(tree)
    }

    pub fn source_type(self, tree: &impl TreeView) -> Option<Type> {
        self.source_type.map(|t| *t.get(tree))
    }

    pub fn select(self, tree: &impl TreeView) -> &FieldPath {
        self.select.get(tree)
    }

    pub fn op(self, tree: &impl TreeView) -> RecordUpdateOp {
        *self.op.get(tree)
    }

    pub fn value(self, tree: &impl TreeView) -> Expr {
        *self.value.get(tree)
    }

    pub fn value_type(self, tree: &impl TreeView) -> Option<Type> {
        self.value_type.map(|t| *t.get(tree))
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    IntoIterator,
    Default,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
#[from(forward)]
#[into_iterator(owned, ref)]
pub struct FieldPath(pub Vec<Id<ValueName>>);

impl FieldPath {
    pub fn new_in<I>(fields: I, builder: &mut TreeBuilder) -> Id<Self>
    where
        I: IntoIterator,
        I::Item: Into<ValueName>,
    {
        let fields = fields
            .into_iter()
            .map(|f| builder.insert(f.into()))
            .collect();
        builder.insert(Self(fields))
    }

    pub fn get(&self, index: usize, tree: &impl TreeView) -> ValueName {
        *self.0[index].get(tree)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Id<ValueName>> {
        (&self).into_iter()
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "cyan")]
pub struct QualifiedExpr {
    pub path: Option<Id<ModulePath>>,
    pub source: Id<ValueName>,
    pub fields: Option<Id<FieldPath>>,
}

impl QualifiedExpr {
    pub fn new_in(
        path: Option<ModulePath>,
        source: impl Into<ValueName>,
        fields: Option<FieldPath>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let path = path.map(|p| builder.insert(p));
        let source = builder.insert(source.into());
        let fields = fields.map(|f| builder.insert(f));

        builder.insert(Self {
            path,
            source,
            fields,
        })
    }
}

#[derive(
    Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl<'a> Notate<'a> for NodePrinter<'a, UnaryOp> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        self.value.red().display_in(arena)
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
pub struct UnaryExpr {
    pub op: Id<UnaryOp>,
    pub operand: Id<Expr>,
}

impl UnaryExpr {
    pub fn new_in(op: UnaryOp, operand: impl Into<Expr>, builder: &mut TreeBuilder) -> Id<Self> {
        let op = builder.insert(op);
        let operand = builder.insert(operand.into());

        builder.insert(Self { op, operand })
    }

    pub fn op(self, tree: &impl TreeView) -> UnaryOp {
        *self.op.get(tree)
    }

    pub fn operand(self, tree: &impl TreeView) -> Expr {
        *self.operand.get(tree)
    }
}

#[derive(
    Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum BinaryOp {
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
    // Record
    Merge,
}

impl<'a> Notate<'a> for NodePrinter<'a, BinaryOp> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        self.value.red().display_in(arena)
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
pub struct BinaryExpr {
    pub op: Id<BinaryOp>,
    pub left: Id<Expr>,
    pub right: Id<Expr>,
}

impl BinaryExpr {
    pub fn new_in(
        op: BinaryOp,
        left: impl Into<Expr>,
        right: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let op = builder.insert(op);
        let left = builder.insert(left.into());
        let right = builder.insert(right.into());

        builder.insert(Self { op, left, right })
    }

    pub fn op(self, tree: &impl TreeView) -> BinaryOp {
        *self.op.get(tree)
    }

    pub fn left(self, tree: &impl TreeView) -> Expr {
        *self.left.get(tree)
    }

    pub fn right(self, tree: &impl TreeView) -> Expr {
        *self.right.get(tree)
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
pub struct LetExpr {
    pub name: Id<ValueName>,
    pub value_type: Option<Id<Type>>,
    pub value: Id<Expr>,
    pub inside: Id<Expr>,
}

impl LetExpr {
    pub fn new_in(
        name: impl Into<ValueName>,
        value_type: Option<Type>,
        value: impl Into<Expr>,
        inside: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let name = builder.insert(name.into());
        let value_type = value_type.map(|t| builder.insert(t));
        let value = builder.insert(value.into());
        let inside = builder.insert(inside.into());

        builder.insert(Self {
            name,
            value_type,
            value,
            inside,
        })
    }

    pub fn name(self, tree: &impl TreeView) -> ValueName {
        *self.name.get(tree)
    }

    pub fn value_type(self, tree: &impl TreeView) -> Option<&Type> {
        self.value_type.map(|t| t.get(tree))
    }

    pub fn value(self, tree: &impl TreeView) -> Expr {
        *self.value.get(tree)
    }

    pub fn inside(self, tree: &impl TreeView) -> Expr {
        *self.inside.get(tree)
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
pub struct IfExpr {
    pub predicate: Id<Expr>,
    pub then: Id<Expr>,
    pub or: Id<Expr>,
}

impl IfExpr {
    pub fn new_in(
        predicate: impl Into<Expr>,
        then: impl Into<Expr>,
        or: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let predicate = builder.insert(predicate.into());
        let then = builder.insert(then.into());
        let or = builder.insert(or.into());

        builder.insert(Self {
            predicate,
            then,
            or,
        })
    }

    pub fn predicate(self, tree: &impl TreeView) -> Expr {
        *self.predicate.get(tree)
    }

    pub fn then(self, tree: &impl TreeView) -> Expr {
        *self.then.get(tree)
    }

    pub fn or(self, tree: &impl TreeView) -> Expr {
        *self.or.get(tree)
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
pub struct CaseBranch {
    pub pat: Id<Pat>,
    pub matches: Id<Expr>,
}

impl CaseBranch {
    pub fn new_in(
        pat: impl Into<Pat>,
        matches: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let pat = builder.insert(pat.into());
        let matches = builder.insert(matches.into());

        builder.insert(Self { pat, matches })
    }

    pub fn pat(self, tree: &impl TreeView) -> Pat {
        *self.pat.get(tree)
    }

    pub fn matches(self, tree: &impl TreeView) -> Expr {
        *self.matches.get(tree)
    }
}

#[derive(
    Debug, Notate, Inspector, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(color = "blue")]
pub struct CaseExpr {
    pub source: Id<Expr>,
    pub branches: Vec<Id<CaseBranch>>,
}

impl CaseExpr {
    pub fn new_in<I>(source: impl Into<Expr>, branches: I, builder: &mut TreeBuilder) -> Id<Self>
    where
        I: IntoIterator<Item = Id<CaseBranch>>,
    {
        let source = builder.insert(source.into());
        let branches = branches.into_iter().collect();

        builder.insert(Self { source, branches })
    }

    pub fn source(&self, tree: &impl TreeView) -> Expr {
        *self.source.get(tree)
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
pub struct CallExpr {
    pub func: Id<Expr>,
    pub arg: Id<Expr>,
}

impl CallExpr {
    pub fn new_in(
        func: impl Into<Expr>,
        arg: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let func = builder.insert(func.into());
        let arg = builder.insert(arg.into());

        builder.insert(Self { func, arg })
    }

    pub fn func(self, tree: &impl TreeView) -> Expr {
        *self.func.get(tree)
    }

    pub fn arg(self, tree: &impl TreeView) -> Expr {
        *self.arg.get(tree)
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
pub struct LambdaExpr {
    pub param: Id<ValueName>, // TODO pattern
    pub param_type: Option<Id<Type>>,
    pub body: Id<Expr>,
}

impl LambdaExpr {
    pub fn new_in(
        param: impl Into<ValueName>,
        param_type: Option<Type>,
        body: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let param = builder.insert(param.into());
        let param_type = param_type.map(|t| builder.insert(t));
        let body = builder.insert(body.into());

        builder.insert(Self {
            param,
            param_type,
            body,
        })
    }

    pub fn param(self, tree: &impl TreeView) -> ValueName {
        *self.param.get(tree)
    }

    pub fn body(self, tree: &impl TreeView) -> Expr {
        *self.body.get(tree)
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
pub struct TagExpr(pub Id<ValueName>);

impl TagExpr {
    pub fn new_in(tag: impl Into<ValueName>, builder: &mut TreeBuilder) -> Id<Self> {
        let tag = builder.insert(tag.into());

        builder.insert(Self(tag))
    }
}

#[derive(
    Debug,
    EnumAsInner,
    Inspector,
    From,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub enum Expr {
    Error(Id<ExprError>),
    Literal(Id<LiteralExpr>),
    Qualified(Id<QualifiedExpr>),
    List(Id<ListExpr>),
    Record(Id<RecordExpr>),
    RecordExtend(Id<RecordExtendExpr>),
    RecordRestrict(Id<RecordRestrictExpr>),
    RecordUpdate(Id<RecordUpdateExpr>),
    Unary(Id<UnaryExpr>),
    Binary(Id<BinaryExpr>),
    Let(Id<LetExpr>),
    If(Id<IfExpr>),
    Case(Id<CaseExpr>),
    Call(Id<CallExpr>),
    Lambda(Id<LambdaExpr>),
    Tag(Id<TagExpr>),
}

impl<'a> Notate<'a> for NodePrinter<'a, Expr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            Expr::Error(e) => self.to_id(e).notate(arena),
            Expr::Literal(l) => self.to_id(l).notate(arena),
            Expr::Qualified(q) => self.to_id(q).notate(arena),
            Expr::List(l) => self.to_id(l).notate(arena),
            Expr::Record(r) => self.to_id(r).notate(arena),
            Expr::RecordExtend(r) => self.to_id(r).notate(arena),
            Expr::RecordRestrict(r) => self.to_id(r).notate(arena),
            Expr::RecordUpdate(r) => self.to_id(r).notate(arena),
            Expr::Unary(u) => self.to_id(u).notate(arena),
            Expr::Binary(b) => self.to_id(b).notate(arena),
            Expr::Let(l) => self.to_id(l).notate(arena),
            Expr::If(i) => self.to_id(i).notate(arena),
            Expr::Case(c) => self.to_id(c).notate(arena),
            Expr::Call(c) => self.to_id(c).notate(arena),
            Expr::Lambda(f) => self.to_id(f).notate(arena),
            Expr::Tag(t) => self.to_id(t).notate(arena),
        }
    }
}
