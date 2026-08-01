use derive_more::{Display, From};
use enum_as_inner::EnumAsInner;
use kola_macros::Inspector;
use kola_print::prelude::*;
use kola_utils::interner::StrKey;
use serde::{Deserialize, Serialize};

use super::{ModulePath, Pat, QualifiedType, TypeExpr, ValueName};
use crate::{
    id::Id,
    print::TreePrinter,
    slice::SliceId,
    tree::{TreeBuilder, TreeView},
};

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "red")]
pub struct ExprError;

impl ExprError {
    pub fn new_in(builder: &mut TreeBuilder) -> Id<Self> {
        builder.alloc(Self)
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
        builder.alloc(value.into())
    }
}

impl<'a> Notate<'a, LiteralExpr> for TreePrinter<'a> {
    fn notate(&self, value: &LiteralExpr, arena: &'a Bump) -> Notation<'a> {
        let head = "LiteralExpr".purple().display_in(arena);

        let lit = match *value {
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct ListExpr(pub SliceId<Expr>);

impl ListExpr {
    pub fn empty_in(builder: &mut TreeBuilder) -> Id<Self> {
        builder.alloc(Self(SliceId::empty()))
    }

    pub fn new_in<I>(elements: I, builder: &mut TreeBuilder) -> Id<Self>
    where
        I: IntoIterator,
        I::Item: Into<Expr>,
    {
        let ids = elements.into_iter().map(|e| builder.nodes.alloc(e.into()));
        let slice = builder.slices.alloc(ids);
        builder.alloc(Self(slice))
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct RecordField {
    pub label: Id<ValueName>,
    pub ty: Option<Id<TypeExpr>>,
    pub value: Id<Expr>,
}

impl RecordField {
    pub fn new_in(
        label: impl Into<ValueName>,
        ty: Option<TypeExpr>,
        value: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let label = builder.alloc(label.into());
        let ty = ty.map(|t| builder.alloc(t));
        let value = builder.alloc(value.into());

        builder.alloc(Self { label, ty, value })
    }

    pub fn label(self, storage: &impl TreeView) -> ValueName {
        *self.label.get(storage)
    }

    pub fn type_(self, storage: &impl TreeView) -> Option<TypeExpr> {
        self.ty.map(|t| *t.get(storage))
    }

    pub fn value(self, storage: &impl TreeView) -> Expr {
        *self.value.get(storage)
    }
}

// { x = 10, y = 20 }
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct RecordExpr(pub SliceId<RecordField>);

impl RecordExpr {
    pub fn new_in<I>(fields: I, builder: &mut TreeBuilder) -> Id<Self>
    where
        I: IntoIterator<Item = Id<RecordField>>,
    {
        let slice = builder.alloc_slice(fields);
        builder.alloc(Self(slice))
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct RecordExtendExpr {
    pub source: Id<Expr>,
    pub source_type: Option<Id<TypeExpr>>,
    pub field_path: Id<FieldPath>,
    pub value: Id<Expr>,
    pub value_type: Option<Id<TypeExpr>>,
}

impl RecordExtendExpr {
    pub fn new_in(
        source: impl Into<Expr>,
        source_type: Option<TypeExpr>,
        field_path: impl Into<FieldPath>,
        value: impl Into<Expr>,
        value_type: Option<TypeExpr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let source = builder.alloc(source.into());
        let source_type = source_type.map(|t| builder.alloc(t));
        let field_path = builder.alloc(field_path.into());
        let value = builder.alloc(value.into());
        let value_type = value_type.map(|t| builder.alloc(t));

        builder.alloc(Self {
            source,
            source_type,
            field_path,
            value,
            value_type,
        })
    }

    pub fn source(self, storage: &impl TreeView) -> Expr {
        *self.source.get(storage)
    }

    pub fn source_type(self, storage: &impl TreeView) -> Option<TypeExpr> {
        self.source_type.map(|t| *t.get(storage))
    }

    pub fn field_path(self, storage: &impl TreeView) -> &FieldPath {
        self.field_path.get(storage)
    }

    pub fn value(self, storage: &impl TreeView) -> Expr {
        *self.value.get(storage)
    }

    pub fn value_type(self, storage: &impl TreeView) -> Option<TypeExpr> {
        self.value_type.map(|t| *t.get(storage))
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct RecordRestrictExpr {
    pub source: Id<Expr>,
    pub source_type: Option<Id<TypeExpr>>,
    pub field_path: Id<FieldPath>,
    pub value_type: Option<Id<TypeExpr>>,
}

impl RecordRestrictExpr {
    pub fn new_in(
        source: impl Into<Expr>,
        source_type: Option<TypeExpr>,
        field_path: impl Into<FieldPath>,
        value_type: Option<TypeExpr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let source = builder.alloc(source.into());
        let source_type = source_type.map(|t| builder.alloc(t));
        let field_path = builder.alloc(field_path.into());
        let value_type = value_type.map(|t| builder.alloc(t));

        builder.alloc(Self {
            source,
            source_type,
            field_path,
            value_type,
        })
    }

    pub fn source(self, storage: &impl TreeView) -> Expr {
        *self.source.get(storage)
    }

    pub fn source_type(self, storage: &impl TreeView) -> Option<TypeExpr> {
        self.source_type.map(|t| *t.get(storage))
    }

    pub fn field_path(self, storage: &impl TreeView) -> &FieldPath {
        self.field_path.get(storage)
    }

    pub fn value_type(self, storage: &impl TreeView) -> Option<TypeExpr> {
        self.value_type.map(|t| *t.get(storage))
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

impl<'a> Notate<'a, RecordUpdateOp> for TreePrinter<'a> {
    fn notate(&self, value: &RecordUpdateOp, arena: &'a Bump) -> Notation<'a> {
        value.red().display_in(arena)
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct RecordUpdateExpr {
    pub source: Id<Expr>,
    pub source_type: Option<Id<TypeExpr>>,
    pub field_path: Id<FieldPath>,
    pub op: Id<RecordUpdateOp>,
    pub value: Id<Expr>,
    pub value_type: Option<Id<TypeExpr>>,
}

impl RecordUpdateExpr {
    pub fn new_in(
        source: impl Into<Expr>,
        source_type: Option<TypeExpr>,
        field_path: impl Into<FieldPath>,
        op: RecordUpdateOp,
        value: impl Into<Expr>,
        value_type: Option<TypeExpr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let source = builder.alloc(source.into());
        let source_type = source_type.map(|t| builder.alloc(t));
        let field_path = builder.alloc(field_path.into());
        let op = builder.alloc(op);
        let value = builder.alloc(value.into());
        let value_type = value_type.map(|t| builder.alloc(t));

        builder.alloc(Self {
            source,
            source_type,
            field_path,
            op,
            value,
            value_type,
        })
    }

    pub fn source(self, storage: &impl TreeView) -> Expr {
        *self.source.get(storage)
    }

    pub fn source_type(self, storage: &impl TreeView) -> Option<TypeExpr> {
        self.source_type.map(|t| *t.get(storage))
    }

    pub fn field_path(self, storage: &impl TreeView) -> &FieldPath {
        self.field_path.get(storage)
    }

    pub fn op(self, storage: &impl TreeView) -> RecordUpdateOp {
        *self.op.get(storage)
    }

    pub fn value(self, storage: &impl TreeView) -> Expr {
        *self.value.get(storage)
    }

    pub fn value_type(self, storage: &impl TreeView) -> Option<TypeExpr> {
        self.value_type.map(|t| *t.get(storage))
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct RecordMergeExpr {
    pub lhs: Id<Expr>,
    pub rhs: Id<Expr>,
}

impl RecordMergeExpr {
    pub fn new_in(
        lhs: impl Into<Expr>,
        rhs: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let lhs = builder.alloc(lhs.into());
        let rhs = builder.alloc(rhs.into());

        builder.alloc(Self { lhs, rhs })
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct FieldPath(pub SliceId<ValueName>);

impl FieldPath {
    pub fn new_in<I>(fields: I, builder: &mut TreeBuilder) -> Id<Self>
    where
        I: IntoIterator,
        I::Item: Into<ValueName>,
    {
        let ids = fields.into_iter().map(|f| builder.nodes.alloc(f.into()));
        let slice = builder.slices.alloc(ids);
        builder.alloc(Self(slice))
    }

    pub fn get(&self, index: usize, storage: &impl TreeView) -> ValueName {
        *self.0.iter(storage).nth(index).unwrap().get(storage)
    }

    pub fn iter<'a>(
        &'a self,
        storage: &'a impl TreeView,
    ) -> impl Iterator<Item = Id<ValueName>> + 'a {
        self.0.iter(storage)
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
#[notate(with = TreePrinter<'a>, color = "cyan")]
pub struct QualifiedExpr {
    pub module_path: Option<Id<ModulePath>>,
    pub source: Id<ValueName>,
    pub field_path: Option<Id<FieldPath>>,
}

impl QualifiedExpr {
    pub fn new_in(
        module_path: Option<Id<ModulePath>>,
        source: impl Into<ValueName>,
        field_path: Option<Id<FieldPath>>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let source = builder.alloc(source.into());

        builder.alloc(Self {
            module_path,
            source,
            field_path,
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

impl<'a> Notate<'a, UnaryOp> for TreePrinter<'a> {
    fn notate(&self, value: &UnaryOp, arena: &'a Bump) -> Notation<'a> {
        value.red().display_in(arena)
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct UnaryExpr {
    pub op: Id<UnaryOp>,
    pub operand: Id<Expr>,
}

impl UnaryExpr {
    pub fn new_in(op: UnaryOp, operand: impl Into<Expr>, builder: &mut TreeBuilder) -> Id<Self> {
        let op = builder.alloc(op);
        let operand = builder.alloc(operand.into());

        builder.alloc(Self { op, operand })
    }

    pub fn op(self, storage: &impl TreeView) -> UnaryOp {
        *self.op.get(storage)
    }

    pub fn operand(self, storage: &impl TreeView) -> Expr {
        *self.operand.get(storage)
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
    Less,
    Greater,
    LessEq,
    GreaterEq,
    And,
    Or,
    Eq,
    NotEq,
    Concat,
}

impl<'a> Notate<'a, BinaryOp> for TreePrinter<'a> {
    fn notate(&self, value: &BinaryOp, arena: &'a Bump) -> Notation<'a> {
        value.red().display_in(arena)
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct BinaryExpr {
    pub op: Id<BinaryOp>,
    pub lhs: Id<Expr>,
    pub rhs: Id<Expr>,
}

impl BinaryExpr {
    pub fn new_in(
        op: BinaryOp,
        lhs: impl Into<Expr>,
        rhs: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let op = builder.alloc(op);
        let lhs = builder.alloc(lhs.into());
        let rhs = builder.alloc(rhs.into());

        builder.alloc(Self { op, lhs, rhs })
    }

    pub fn op(self, storage: &impl TreeView) -> BinaryOp {
        *self.op.get(storage)
    }

    pub fn lhs(self, storage: &impl TreeView) -> Expr {
        *self.lhs.get(storage)
    }

    pub fn rhs(self, storage: &impl TreeView) -> Expr {
        *self.rhs.get(storage)
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct LetExpr {
    pub name: Id<ValueName>,
    pub value_type: Option<Id<TypeExpr>>,
    pub value: Id<Expr>,
    pub body: Id<Expr>,
}

impl LetExpr {
    pub fn new_in(
        name: impl Into<ValueName>,
        value_type: Option<TypeExpr>,
        value: impl Into<Expr>,
        body: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let name = builder.alloc(name.into());
        let value_type = value_type.map(|t| builder.alloc(t));
        let value = builder.alloc(value.into());
        let body = builder.alloc(body.into());

        builder.alloc(Self {
            name,
            value_type,
            value,
            body,
        })
    }

    pub fn name(self, storage: &impl TreeView) -> ValueName {
        *self.name.get(storage)
    }

    pub fn value_type(self, storage: &impl TreeView) -> Option<&TypeExpr> {
        self.value_type.map(|t| t.get(storage))
    }

    pub fn value(self, storage: &impl TreeView) -> Expr {
        *self.value.get(storage)
    }

    pub fn body(self, storage: &impl TreeView) -> Expr {
        *self.body.get(storage)
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct IfExpr {
    pub pred: Id<Expr>,
    pub then: Id<Expr>,
    pub or_else: Id<Expr>,
}

impl IfExpr {
    pub fn new_in(
        pred: impl Into<Expr>,
        then: impl Into<Expr>,
        or_else: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let pred = builder.alloc(pred.into());
        let then = builder.alloc(then.into());
        let or_else = builder.alloc(or_else.into());

        builder.alloc(Self {
            pred,
            then,
            or_else,
        })
    }

    pub fn pred(self, storage: &impl TreeView) -> Expr {
        *self.pred.get(storage)
    }

    pub fn then(self, storage: &impl TreeView) -> Expr {
        *self.then.get(storage)
    }

    pub fn or_else(self, storage: &impl TreeView) -> Expr {
        *self.or_else.get(storage)
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct CaseBranch {
    pub pat: Id<Pat>,
    pub body: Id<Expr>,
}

impl CaseBranch {
    pub fn new_in(
        pat: impl Into<Pat>,
        body: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let pat = builder.alloc(pat.into());
        let body = builder.alloc(body.into());

        builder.alloc(Self { pat, body })
    }

    pub fn pat(self, storage: &impl TreeView) -> Pat {
        *self.pat.get(storage)
    }

    pub fn body(self, storage: &impl TreeView) -> Expr {
        *self.body.get(storage)
    }
}

#[derive(
    Debug, Notate, Inspector, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct CaseExpr {
    pub source: Id<Expr>,
    pub branches: SliceId<CaseBranch>,
}

impl CaseExpr {
    pub fn new_in<I>(source: impl Into<Expr>, branches: I, builder: &mut TreeBuilder) -> Id<Self>
    where
        I: IntoIterator<Item = Id<CaseBranch>>,
    {
        let source = builder.alloc(source.into());
        let branches = builder.alloc_slice(branches);

        builder.alloc(Self { source, branches })
    }

    pub fn source(&self, storage: &impl TreeView) -> Expr {
        *self.source.get(storage)
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
#[notate(with = TreePrinter<'a>, color = "blue")]
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
        let func = builder.alloc(func.into());
        let arg = builder.alloc(arg.into());

        builder.alloc(Self { func, arg })
    }

    pub fn func(self, storage: &impl TreeView) -> Expr {
        *self.func.get(storage)
    }

    pub fn arg(self, storage: &impl TreeView) -> Expr {
        *self.arg.get(storage)
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct LambdaExpr {
    pub param: Id<ValueName>,
    pub param_type: Option<Id<TypeExpr>>,
    pub body: Id<Expr>,
}

impl LambdaExpr {
    pub fn new_in(
        param: impl Into<ValueName>,
        param_type: Option<TypeExpr>,
        body: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let param = builder.alloc(param.into());
        let param_type = param_type.map(|t| builder.alloc(t));
        let body = builder.alloc(body.into());

        builder.alloc(Self {
            param,
            param_type,
            body,
        })
    }

    pub fn param(self, storage: &impl TreeView) -> ValueName {
        *self.param.get(storage)
    }

    pub fn body(self, storage: &impl TreeView) -> Expr {
        *self.body.get(storage)
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct HandlerClause {
    pub op: Id<ValueName>,
    pub param: Id<ValueName>,
    pub body: Id<Expr>,
}

#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct HandleExpr {
    pub source: Id<Expr>,
    pub clauses: SliceId<HandlerClause>,
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct DoExpr {
    pub op: Id<ValueName>,
    pub arg: Id<Expr>,
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct TagExpr(pub Id<ValueName>);

impl TagExpr {
    pub fn new_in(tag: impl Into<ValueName>, builder: &mut TreeBuilder) -> Id<Self> {
        let tag = builder.alloc(tag.into());
        builder.alloc(Self(tag))
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub enum TypeWitnessExpr {
    Qualified(Id<QualifiedType>),
    Label(Id<ValueName>),
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
    RecordMerge(Id<RecordMergeExpr>),
    Unary(Id<UnaryExpr>),
    Binary(Id<BinaryExpr>),
    Let(Id<LetExpr>),
    If(Id<IfExpr>),
    Case(Id<CaseExpr>),
    Call(Id<CallExpr>),
    Lambda(Id<LambdaExpr>),
    Handle(Id<HandleExpr>),
    Do(Id<DoExpr>),
    Tag(Id<TagExpr>),
    TypeWitness(Id<TypeWitnessExpr>),
}

impl<'a> Notate<'a, Expr> for TreePrinter<'a> {
    fn notate(&self, value: &Expr, arena: &'a Bump) -> Notation<'a> {
        match value {
            Expr::Error(e) => self.notate(e, arena),
            Expr::Literal(l) => self.notate(l, arena),
            Expr::Qualified(q) => self.notate(q, arena),
            Expr::List(l) => self.notate(l, arena),
            Expr::Record(r) => self.notate(r, arena),
            Expr::RecordExtend(r) => self.notate(r, arena),
            Expr::RecordRestrict(r) => self.notate(r, arena),
            Expr::RecordUpdate(r) => self.notate(r, arena),
            Expr::RecordMerge(r) => self.notate(r, arena),
            Expr::Unary(u) => self.notate(u, arena),
            Expr::Binary(b) => self.notate(b, arena),
            Expr::Let(l) => self.notate(l, arena),
            Expr::If(i) => self.notate(i, arena),
            Expr::Case(c) => self.notate(c, arena),
            Expr::Call(c) => self.notate(c, arena),
            Expr::Lambda(f) => self.notate(f, arena),
            Expr::Handle(h) => self.notate(h, arena),
            Expr::Do(d) => self.notate(d, arena),
            Expr::Tag(t) => self.notate(t, arena),
            Expr::TypeWitness(t) => self.notate(t, arena),
        }
    }
}
