use derive_more::{Display, From};
use kola_print::prelude::*;
use kola_utils::as_variant;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Name, Pat, Symbol};
use crate::{
    id::NodeId,
    print::TreePrinter,
    tree::{NodeContainer, TreeBuilder},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ExprError;

impl Printable<TreePrinter> for ExprError {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        "ExprError".red().display_in(arena)
    }
}

#[derive(Debug, From, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum LiteralExpr {
    Bool(bool),
    Num(f64),
    Char(char),
    Str(Symbol),
}

impl Printable<TreePrinter> for LiteralExpr {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let kind = "LiteralExpr".purple().display_in(arena);

        let lit = match self {
            Self::Bool(b) => b.yellow().display_in(arena),
            Self::Num(n) => n.yellow().display_in(arena),
            Self::Char(c) => c.yellow().display_in(arena),
            Self::Str(s) => s.yellow().display_in(arena),
        }
        .enclose_by(arena.just('"'), arena);

        let single = arena.just(' ').then(lit.clone(), arena);
        let multi = arena.newline().then(lit, arena);

        kind.then(single.or(multi, arena), arena)
    }
}

/// Path segments can be modules but the result must allways be a value
#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[from(forward)]
pub struct PathExpr(pub Vec<NodeId<Name>>);

impl Printable<TreePrinter> for PathExpr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "PathExpr".cyan().display_in(arena);

        let path = self.0.gather(with, arena).concat_by(arena.just('.'), arena);
        let single = [arena.just(' '), path.clone()].concat_in(arena);
        let multi = [arena.newline(), path].concat_in(arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ListExpr(pub Vec<NodeId<Expr>>);

impl Printable<TreePrinter> for ListExpr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let kind = "List".blue().display_in(arena);

        let values = self.0.gather(with, arena);

        let single = values.clone().concat_map(
            |expr| arena.just(' ').then(expr.flatten(arena), arena),
            arena,
        );
        let multi = values
            .concat_map(|expr| arena.newline().then(expr, arena), arena)
            .indent(arena);

        kind.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordField {
    pub field: NodeId<Name>,
    pub value: NodeId<Expr>,
}

impl RecordField {
    pub fn field(self, tree: &impl NodeContainer) -> &Name {
        self.field.get(tree)
    }

    pub fn value(self, tree: &impl NodeContainer) -> Expr {
        *self.value.get(tree)
    }
}

impl Printable<TreePrinter> for RecordField {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { field, value } = self;

        let head = "Property".blue().display_in(arena);

        let field = field.notate(with, arena);
        let value = value.notate(with, arena);

        let single = [
            arena.notate(" key = "),
            field.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("key = "),
            field,
            arena.newline(),
            arena.notate("value = "),
            value,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// { x = 10, y = 20 }
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordExpr(pub Vec<NodeId<RecordField>>);

impl RecordExpr {
    pub fn get(&self, name: impl AsRef<str>, tree: &impl NodeContainer) -> Option<RecordField> {
        self.0.iter().find_map(|id| {
            let field = id.get(tree);
            (field.field(tree) == name.as_ref())
                .then_some(field)
                .copied()
        })
    }
}

impl Printable<TreePrinter> for RecordExpr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "Record".blue().display_in(arena);

        let fields = self.0.gather(with, arena);

        let single = fields.clone().concat_map(
            |field| arena.just(' ').then(field.flatten(arena), arena),
            arena,
        );
        let multi = fields.concat_map(|field| arena.newline().then(field, arena), arena);

        head.then(single.or(multi, arena), arena)
    }
}

// { y | +x = 10 }
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordExtendExpr {
    pub source: NodeId<Expr>,
    pub field: NodeId<Name>,
    pub value: NodeId<Expr>,
}

impl RecordExtendExpr {
    pub fn source(self, tree: &impl NodeContainer) -> Expr {
        *self.source.get(tree)
    }

    pub fn field(self, tree: &impl NodeContainer) -> &Name {
        self.field.get(tree)
    }

    pub fn value(self, tree: &impl NodeContainer) -> Expr {
        *self.value.get(tree)
    }
}

impl Printable<TreePrinter> for RecordExtendExpr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self {
            source,
            field,
            value,
        } = self;

        let head = "RecordExtend".blue().display_in(arena);

        let source = source.notate(with, arena);
        let field = field.notate(with, arena);
        let value = value.notate(with, arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", field = "),
            field.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("source = "),
            source,
            arena.newline(),
            arena.notate("field = "),
            field,
            arena.newline(),
            arena.notate("value = "),
            value,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// { y | -x }
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordRestrictExpr {
    pub source: NodeId<Expr>,
    pub field: NodeId<Name>,
}

impl RecordRestrictExpr {
    pub fn source(self, tree: &impl NodeContainer) -> Expr {
        *self.source.get(tree)
    }

    pub fn field(self, tree: &impl NodeContainer) -> &Name {
        self.field.get(tree)
    }
}

impl Printable<TreePrinter> for RecordRestrictExpr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { source, field } = self;

        let head = "RecordRestrict".blue().display_in(arena);

        let source = source.notate(with, arena);
        let field = field.notate(with, arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", field = "),
            field.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("source = "),
            source,
            arena.newline(),
            arena.notate("field = "),
            field,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
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

impl Printable<TreePrinter> for RecordUpdateOp {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        self.display_in(arena)
    }
}

// { y | x = 10 }
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordUpdateExpr {
    pub source: NodeId<Expr>,
    pub field: NodeId<Name>,
    pub op: NodeId<RecordUpdateOp>,
    pub value: NodeId<Expr>,
}

impl RecordUpdateExpr {
    pub fn source(self, tree: &impl NodeContainer) -> Expr {
        *self.source.get(tree)
    }

    pub fn field(self, tree: &impl NodeContainer) -> &Name {
        self.field.get(tree)
    }

    pub fn op(self, tree: &impl NodeContainer) -> RecordUpdateOp {
        *self.op.get(tree)
    }

    pub fn value(self, tree: &impl NodeContainer) -> Expr {
        *self.value.get(tree)
    }
}

impl Printable<TreePrinter> for RecordUpdateExpr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self {
            source,
            field,
            op,
            value,
        } = self;

        let head = "RecordUpdate".blue().display_in(arena);

        let source = source.notate(with, arena);
        let field = field.notate(with, arena);
        let op = op.notate(with, arena);
        let value = value.notate(with, arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", field = "),
            field.clone().flatten(arena),
            arena.notate(", op = "),
            op.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("source = "),
            source,
            arena.newline(),
            arena.notate("field = "),
            field,
            arena.newline(),
            arena.notate("op = "),
            op,
            arena.newline(),
            arena.notate("value = "),
            value,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl Printable<TreePrinter> for UnaryOp {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        self.display_in(arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct UnaryExpr {
    pub op: NodeId<UnaryOp>,
    pub operand: NodeId<Expr>,
}

impl UnaryExpr {
    pub fn new_in(op: UnaryOp, target: Expr, builder: &mut TreeBuilder) -> NodeId<Self> {
        let op = builder.insert(op);
        let operand = builder.insert(target);

        builder.insert(Self { op, operand })
    }

    pub fn op(self, tree: &impl NodeContainer) -> UnaryOp {
        *self.op.get(tree)
    }

    pub fn operand(self, tree: &impl NodeContainer) -> Expr {
        *self.operand.get(tree)
    }
}

impl Printable<TreePrinter> for UnaryExpr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { op, operand } = self;

        let head = "Unary".blue().display_in(arena);

        let op = op.notate(with, arena);
        let operand = operand.notate(with, arena);

        let single = [
            arena.just(' '),
            op.clone(),
            arena.just(' '),
            operand.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [arena.newline(), op, arena.newline(), operand]
            .concat_in(arena)
            .indent(arena);

        head.then(single.or(multi, arena), arena)
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

impl Printable<TreePrinter> for BinaryOp {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        self.display_in(arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct BinaryExpr {
    pub op: NodeId<BinaryOp>,
    pub left: NodeId<Expr>,
    pub right: NodeId<Expr>,
}

impl BinaryExpr {
    pub fn new_in(
        op: BinaryOp,
        left: Expr,
        right: Expr,
        builder: &mut TreeBuilder,
    ) -> NodeId<Self> {
        let op = builder.insert(op);
        let left = builder.insert(left);
        let right = builder.insert(right);

        builder.insert(Self { op, left, right })
    }

    pub fn op(self, tree: &impl NodeContainer) -> BinaryOp {
        *self.op.get(tree)
    }

    pub fn left(self, tree: &impl NodeContainer) -> Expr {
        *self.left.get(tree)
    }

    pub fn right(self, tree: &impl NodeContainer) -> Expr {
        *self.right.get(tree)
    }
}

impl Printable<TreePrinter> for BinaryExpr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { op, left, right } = self;

        let head = "Binary".blue().display_in(arena);

        let left = left.notate(with, arena);
        let op = op.notate(with, arena);
        let right = right.notate(with, arena);

        let single = [
            arena.just(' '),
            left.clone().flatten(arena),
            arena.just(' '),
            op.clone().flatten(arena),
            arena.just(' '),
            right.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            left,
            arena.newline(),
            op,
            arena.newline(),
            right,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct LetExpr {
    pub name: NodeId<Name>,
    pub value: NodeId<Expr>,
    pub inside: NodeId<Expr>,
}

impl LetExpr {
    pub fn new_in(
        name: Name,
        value: Expr,
        inside: Expr,
        builder: &mut TreeBuilder,
    ) -> NodeId<Self> {
        let name = builder.insert(name);
        let value = builder.insert(value);
        let inside = builder.insert(inside);

        builder.insert(Self {
            name,
            value,
            inside,
        })
    }

    pub fn name(self, tree: &impl NodeContainer) -> &Name {
        self.name.get(tree)
    }

    pub fn value(self, tree: &impl NodeContainer) -> Expr {
        *self.value.get(tree)
    }

    pub fn inside(self, tree: &impl NodeContainer) -> Expr {
        *self.inside.get(tree)
    }
}

impl Printable<TreePrinter> for LetExpr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self {
            name,
            value,
            inside,
        } = self;

        let head = "Let".blue().display_in(arena);

        let name = name.notate(with, arena);
        let value = value.notate(with, arena);
        let inside = inside.notate(with, arena);

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
            arena.notate(", inside = "),
            inside.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("name = "),
            name,
            arena.newline(),
            arena.notate("value = "),
            value,
            arena.newline(),
            arena.notate("inside = "),
            inside,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct IfExpr {
    pub predicate: NodeId<Expr>,
    pub then: NodeId<Expr>,
    pub or: NodeId<Expr>,
}

impl IfExpr {
    pub fn new_in(
        predicate: Expr,
        then: Expr,
        or: Expr,
        builder: &mut TreeBuilder,
    ) -> NodeId<Self> {
        let predicate = builder.insert(predicate);
        let then = builder.insert(then);
        let or = builder.insert(or);

        builder.insert(Self {
            predicate,
            then,
            or,
        })
    }

    pub fn predicate(self, tree: &impl NodeContainer) -> Expr {
        *self.predicate.get(tree)
    }

    pub fn then(self, tree: &impl NodeContainer) -> Expr {
        *self.then.get(tree)
    }

    pub fn or(self, tree: &impl NodeContainer) -> Expr {
        *self.or.get(tree)
    }
}

impl Printable<TreePrinter> for IfExpr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self {
            predicate,
            then,
            or,
        } = self;

        let head = "Let".blue().display_in(arena);

        let predicate = predicate.notate(with, arena);
        let then = then.notate(with, arena);
        let or = or.notate(with, arena);

        let single = [
            arena.notate(" predicate = "),
            predicate.clone().flatten(arena),
            arena.notate(", then = "),
            then.clone().flatten(arena),
            arena.notate(", or = "),
            or.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("predicate = "),
            predicate,
            arena.newline(),
            arena.notate("then = "),
            then,
            arena.newline(),
            arena.notate("or = "),
            or,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct CaseBranch {
    pub pat: NodeId<Pat>,
    pub matches: NodeId<Expr>,
}

impl CaseBranch {
    pub fn pat(self, tree: &impl NodeContainer) -> Pat {
        *self.pat.get(tree)
    }

    pub fn matches(self, tree: &impl NodeContainer) -> Expr {
        *self.matches.get(tree)
    }
}

impl Printable<TreePrinter> for CaseBranch {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { pat, matches } = self;

        let head = "Branch".blue().display_in(arena);

        let pat = pat.notate(with, arena);
        let matches = matches.notate(with, arena);

        let single = [
            arena.notate(" pat = "),
            pat.clone().flatten(arena),
            arena.notate(", matches = "),
            matches.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("pat = "),
            pat,
            arena.newline(),
            arena.notate("matches = "),
            matches,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct CaseExpr {
    pub source: NodeId<Name>,
    pub branches: Vec<NodeId<CaseBranch>>,
}

impl CaseExpr {
    pub fn source<'a>(&self, tree: &'a impl NodeContainer) -> &'a Name {
        self.source.get(tree)
    }
}

impl Printable<TreePrinter> for CaseExpr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { source, branches } = self;

        let head = "Case".blue().display_in(arena);

        let source = source.notate(with, arena);
        let branches = branches.gather(with, arena).concat_in(arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", branches = "),
            branches.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("source = "),
            source.clone(),
            arena.newline(),
            arena.notate("branches = "),
            branches,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct CallExpr {
    pub func: NodeId<Expr>,
    pub arg: NodeId<Expr>,
}

impl CallExpr {
    pub fn func(self, tree: &impl NodeContainer) -> Expr {
        *self.func.get(tree)
    }

    pub fn arg(self, tree: &impl NodeContainer) -> Expr {
        *self.arg.get(tree)
    }
}

impl Printable<TreePrinter> for CallExpr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { func, arg } = self;

        let head = "Call".blue().display_in(arena);

        let func = func.notate(with, arena);
        let arg = arg.notate(with, arena);

        let single = [
            arena.notate(" func = "),
            func.clone().flatten(arena),
            arena.notate(", arg = "),
            arg.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("func = "),
            func,
            arena.newline(),
            arena.notate("arg = "),
            arg,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct LambdaExpr {
    pub param: NodeId<Name>, // TODO pattern
    pub body: NodeId<Expr>,
}

impl LambdaExpr {
    pub fn param(self, tree: &impl NodeContainer) -> &Name {
        self.param.get(tree)
    }

    pub fn body(self, tree: &impl NodeContainer) -> Expr {
        *self.body.get(tree)
    }
}

impl Printable<TreePrinter> for LambdaExpr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { param, body } = self;

        let head = "Func".blue().display_in(arena);

        let param = param.notate(with, arena);
        let body = body.notate(with, arena);

        let single = [
            arena.notate(" param = "),
            param.clone().flatten(arena),
            arena.notate(", body = "),
            body.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("param = "),
            param,
            arena.newline(),
            arena.notate("body = "),
            body,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum Expr {
    Error(NodeId<ExprError>),
    Literal(NodeId<LiteralExpr>),
    Path(NodeId<PathExpr>),
    List(NodeId<ListExpr>),
    Record(NodeId<RecordExpr>),
    RecordExtend(NodeId<RecordExtendExpr>),
    RecordRestrict(NodeId<RecordRestrictExpr>),
    RecordUpdate(NodeId<RecordUpdateExpr>),
    Unary(NodeId<UnaryExpr>),
    Binary(NodeId<BinaryExpr>),
    Let(NodeId<LetExpr>),
    If(NodeId<IfExpr>),
    Case(NodeId<CaseExpr>),
    Lambda(NodeId<LambdaExpr>),
    Call(NodeId<CallExpr>),
}

impl Printable<TreePrinter> for Expr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        match self {
            Self::Error(e) => e.get(&with.tree).notate(with, arena),
            Self::Literal(l) => l.get(&with.tree).notate(with, arena),
            Self::Path(p) => p.get(&with.tree).notate(with, arena),
            Self::List(l) => l.get(&with.tree).notate(with, arena),
            Self::Record(r) => r.get(&with.tree).notate(with, arena),
            Self::RecordExtend(r) => r.get(&with.tree).notate(with, arena),
            Self::RecordRestrict(r) => r.get(&with.tree).notate(with, arena),
            Self::RecordUpdate(r) => r.get(&with.tree).notate(with, arena),
            Self::Unary(u) => u.get(&with.tree).notate(with, arena),
            Self::Binary(b) => b.get(&with.tree).notate(with, arena),
            Self::Let(l) => l.get(&with.tree).notate(with, arena),
            Self::If(i) => i.get(&with.tree).notate(with, arena),
            Self::Case(c) => c.get(&with.tree).notate(with, arena),
            Self::Lambda(f) => f.get(&with.tree).notate(with, arena),
            Self::Call(c) => c.get(&with.tree).notate(with, arena),
        }
    }
}

impl Expr {
    #[inline]
    pub fn to_error(self) -> Option<NodeId<ExprError>> {
        as_variant!(self, Self::Error)
    }

    #[inline]
    pub fn to_literal(self) -> Option<NodeId<LiteralExpr>> {
        as_variant!(self, Self::Literal)
    }

    #[inline]
    pub fn to_path(self) -> Option<NodeId<PathExpr>> {
        as_variant!(self, Self::Path)
    }

    #[inline]
    pub fn to_list(self) -> Option<NodeId<ListExpr>> {
        as_variant!(self, Self::List)
    }

    #[inline]
    pub fn to_record(self) -> Option<NodeId<RecordExpr>> {
        as_variant!(self, Self::Record)
    }

    #[inline]
    pub fn to_record_extend(self) -> Option<NodeId<RecordExtendExpr>> {
        as_variant!(self, Self::RecordExtend)
    }

    #[inline]
    pub fn to_record_update(self) -> Option<NodeId<RecordUpdateExpr>> {
        as_variant!(self, Self::RecordUpdate)
    }

    #[inline]
    pub fn to_unary(self) -> Option<NodeId<UnaryExpr>> {
        as_variant!(self, Self::Unary)
    }

    #[inline]
    pub fn to_binary(self) -> Option<NodeId<BinaryExpr>> {
        as_variant!(self, Self::Binary)
    }

    #[inline]
    pub fn to_let(self) -> Option<NodeId<LetExpr>> {
        as_variant!(self, Self::Let)
    }

    #[inline]
    pub fn to_if(self) -> Option<NodeId<IfExpr>> {
        as_variant!(self, Self::If)
    }

    #[inline]
    pub fn to_case(self) -> Option<NodeId<CaseExpr>> {
        as_variant!(self, Self::Case)
    }

    #[inline]
    pub fn to_lambda(self) -> Option<NodeId<LambdaExpr>> {
        as_variant!(self, Self::Lambda)
    }

    #[inline]
    pub fn to_call(self) -> Option<NodeId<CallExpr>> {
        as_variant!(self, Self::Call)
    }

    #[inline]
    pub fn is_error(self) -> bool {
        matches!(self, Self::Error(_))
    }

    #[inline]
    pub fn is_literal(self) -> bool {
        matches!(self, Self::Literal(_))
    }

    #[inline]
    pub fn is_path(self) -> bool {
        matches!(self, Self::Path(_))
    }

    #[inline]
    pub fn is_list(self) -> bool {
        matches!(self, Self::List(_))
    }

    #[inline]
    pub fn is_record(self) -> bool {
        matches!(self, Self::Record(_))
    }

    #[inline]
    pub fn is_record_extend(self) -> bool {
        matches!(self, Self::RecordExtend(_))
    }

    #[inline]
    pub fn is_record_update(self) -> bool {
        matches!(self, Self::RecordUpdate(_))
    }

    #[inline]
    pub fn is_unary(self) -> bool {
        matches!(self, Self::Unary(_))
    }

    #[inline]
    pub fn is_binary(self) -> bool {
        matches!(self, Self::Binary(_))
    }

    #[inline]
    pub fn is_let(self) -> bool {
        matches!(self, Self::Let(_))
    }

    #[inline]
    pub fn is_if(self) -> bool {
        matches!(self, Self::If(_))
    }

    #[inline]
    pub fn is_case(self) -> bool {
        matches!(self, Self::Case(_))
    }

    #[inline]
    pub fn is_lambda(self) -> bool {
        matches!(self, Self::Lambda(_))
    }

    #[inline]
    pub fn is_call(self) -> bool {
        matches!(self, Self::Call(_))
    }
}
