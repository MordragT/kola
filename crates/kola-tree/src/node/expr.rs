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

impl LiteralExpr {
    pub fn to_bool(&self) -> Option<bool> {
        as_variant!(self, Self::Bool).copied()
    }

    pub fn to_num(&self) -> Option<f64> {
        as_variant!(self, Self::Num).copied()
    }

    pub fn to_char(&self) -> Option<char> {
        as_variant!(self, Self::Char).copied()
    }

    pub fn to_str(&self) -> Option<&Symbol> {
        as_variant!(self, Self::Str)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(_))
    }

    pub fn is_num(&self) -> bool {
        matches!(self, Self::Num(_))
    }

    pub fn is_char(&self) -> bool {
        matches!(self, Self::Char(_))
    }

    pub fn is_str(&self) -> bool {
        matches!(self, Self::Str(_))
    }
}

impl Printable<TreePrinter> for LiteralExpr {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "LiteralExpr".purple().display_in(arena);

        let lit = match self {
            Self::Bool(b) => b.yellow().display_in(arena),
            Self::Num(n) => n.yellow().display_in(arena),
            Self::Char(c) => c.yellow().display_in(arena),
            Self::Str(s) => s.yellow().display_in(arena),
        }
        .enclose_by(arena.just('"'), arena);

        let single = arena.just(' ').then(lit.clone(), arena);
        let multi = arena.newline().then(lit, arena);

        head.then(single.or(multi, arena), arena)
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

impl PathExpr {
    pub fn get<'a>(&self, index: usize, tree: &'a impl NodeContainer) -> &'a Name {
        self.0[index].get(tree)
    }
}

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ListExpr(pub Vec<NodeId<Expr>>);

impl Printable<TreePrinter> for ListExpr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "List".blue().display_in(arena);

        let values = self.0.gather(with, arena);

        let single = values.clone().concat_map(
            |expr| arena.just(' ').then(expr.flatten(arena), arena),
            arena,
        );
        let multi = values
            .concat_map(|expr| arena.newline().then(expr, arena), arena)
            .indent(arena);

        head.then(single.or(multi, arena), arena)
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
    pub source: NodeId<Expr>,
    pub branches: Vec<NodeId<CaseBranch>>,
}

impl CaseExpr {
    pub fn source(&self, tree: &impl NodeContainer) -> Expr {
        *self.source.get(tree)
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
    pub fn to_record_restrict(self) -> Option<NodeId<RecordRestrictExpr>> {
        as_variant!(self, Self::RecordRestrict)
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
    pub fn is_record_restrict(self) -> bool {
        matches!(self, Self::RecordRestrict(_))
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

mod inspector {
    use super::*;
    use crate::inspector::*;

    impl<'t> NodeInspector<'t, NodeId<Expr>> {
        /// Check if this expression is an error
        pub fn as_error(self) -> Option<NodeInspector<'t, NodeId<ExprError>>> {
            let expr = self.node.get(self.tree);
            expr.to_error()
                .map(|err_id| NodeInspector::new(err_id, self.tree))
        }

        /// Check if this expression is a literal
        pub fn as_literal(self) -> Option<NodeInspector<'t, NodeId<LiteralExpr>>> {
            let expr = self.node.get(self.tree);
            expr.to_literal()
                .map(|lit_id| NodeInspector::new(lit_id, self.tree))
        }

        /// Check if this expression is a path
        pub fn as_path(self) -> Option<NodeInspector<'t, NodeId<PathExpr>>> {
            let expr = self.node.get(self.tree);
            expr.to_path()
                .map(|path_id| NodeInspector::new(path_id, self.tree))
        }

        /// Check if this expression is a list
        pub fn as_list(self) -> Option<NodeInspector<'t, NodeId<ListExpr>>> {
            let expr = self.node.get(self.tree);
            expr.to_list()
                .map(|list_id| NodeInspector::new(list_id, self.tree))
        }

        /// Check if this expression is a record
        pub fn as_record(self) -> Option<NodeInspector<'t, NodeId<RecordExpr>>> {
            let expr = self.node.get(self.tree);
            expr.to_record()
                .map(|rec_id| NodeInspector::new(rec_id, self.tree))
        }

        /// Check if this expression is a record extend
        pub fn as_record_extend(self) -> Option<NodeInspector<'t, NodeId<RecordExtendExpr>>> {
            let expr = self.node.get(self.tree);
            expr.to_record_extend()
                .map(|ext_id| NodeInspector::new(ext_id, self.tree))
        }

        /// Check if this expression is a record restrict
        pub fn as_record_restrict(self) -> Option<NodeInspector<'t, NodeId<RecordRestrictExpr>>> {
            let expr = self.node.get(self.tree);
            expr.to_record_restrict()
                .map(|res_id| NodeInspector::new(res_id, self.tree))
        }

        /// Check if this expression is a record update
        pub fn as_record_update(self) -> Option<NodeInspector<'t, NodeId<RecordUpdateExpr>>> {
            let expr = self.node.get(self.tree);
            expr.to_record_update()
                .map(|upd_id| NodeInspector::new(upd_id, self.tree))
        }

        /// Check if this expression is a unary operation
        pub fn as_unary(self) -> Option<NodeInspector<'t, NodeId<UnaryExpr>>> {
            let expr = self.node.get(self.tree);
            expr.to_unary()
                .map(|un_id| NodeInspector::new(un_id, self.tree))
        }

        /// Check if this expression is a binary operation
        pub fn as_binary(self) -> Option<NodeInspector<'t, NodeId<BinaryExpr>>> {
            let expr = self.node.get(self.tree);
            expr.to_binary()
                .map(|bin_id| NodeInspector::new(bin_id, self.tree))
        }

        /// Check if this expression is a let binding
        pub fn as_let(self) -> Option<NodeInspector<'t, NodeId<LetExpr>>> {
            let expr = self.node.get(self.tree);
            expr.to_let()
                .map(|let_id| NodeInspector::new(let_id, self.tree))
        }

        /// Check if this expression is an if condition
        pub fn as_if(self) -> Option<NodeInspector<'t, NodeId<IfExpr>>> {
            let expr = self.node.get(self.tree);
            expr.to_if()
                .map(|if_id| NodeInspector::new(if_id, self.tree))
        }

        /// Check if this expression is a case expression
        pub fn as_case(self) -> Option<NodeInspector<'t, NodeId<CaseExpr>>> {
            let expr = self.node.get(self.tree);
            expr.to_case()
                .map(|case_id| NodeInspector::new(case_id, self.tree))
        }

        /// Check if this expression is a lambda
        pub fn as_lambda(self) -> Option<NodeInspector<'t, NodeId<LambdaExpr>>> {
            let expr = self.node.get(self.tree);
            expr.to_lambda()
                .map(|lambda_id| NodeInspector::new(lambda_id, self.tree))
        }

        /// Check if this expression is a function call
        pub fn as_call(self) -> Option<NodeInspector<'t, NodeId<CallExpr>>> {
            let expr = self.node.get(self.tree);
            expr.to_call()
                .map(|call_id| NodeInspector::new(call_id, self.tree))
        }
    }

    impl<'t> NodeInspector<'t, NodeId<LiteralExpr>> {
        /// Check what kind of literal this is and assert its value
        pub fn is_bool(self, expected: bool) -> Self {
            let lit = self.node.get(self.tree);
            match lit {
                LiteralExpr::Bool(value) => {
                    assert_eq!(
                        *value, expected,
                        "Expected bool {} but found {}",
                        expected, value
                    );
                }
                _ => panic!("Expected bool literal but found {:?}", lit),
            }
            self
        }

        pub fn is_num(self, expected: f64) -> Self {
            let lit = self.node.get(self.tree);
            match lit {
                LiteralExpr::Num(value) => {
                    assert_eq!(
                        *value, expected,
                        "Expected num {} but found {}",
                        expected, value
                    );
                }
                _ => panic!("Expected num literal but found {:?}", lit),
            }
            self
        }

        pub fn is_char(self, expected: char) -> Self {
            let lit = self.node.get(self.tree);
            match lit {
                LiteralExpr::Char(value) => {
                    assert_eq!(
                        *value, expected,
                        "Expected char {} but found {}",
                        expected, value
                    );
                }
                _ => panic!("Expected char literal but found {:?}", lit),
            }
            self
        }

        pub fn is_string(self, expected: &str) -> Self {
            let lit = self.node.get(self.tree);
            match lit {
                LiteralExpr::Str(value) => {
                    assert_eq!(
                        value.as_str(),
                        expected,
                        "Expected string \"{}\" but found \"{}\"",
                        expected,
                        value
                    );
                }
                _ => panic!("Expected string literal but found {:?}", lit),
            }
            self
        }
    }

    impl<'t> NodeInspector<'t, NodeId<PathExpr>> {
        /// Assert the path has the specified number of segments
        pub fn has_segments(self, count: usize) -> Self {
            let segments_len = self.node.get(self.tree).0.len();
            assert_eq!(
                segments_len, count,
                "Expected {} segments but found {}",
                count, segments_len
            );
            self
        }

        /// Assert the path segment at the given index has the expected name
        pub fn segment_at_is(self, index: usize, expected: &str) -> Self {
            let path = self.node.get(self.tree);
            assert!(
                index < path.0.len(),
                "Segment index {} out of bounds (max {})",
                index,
                path.0.len() - 1
            );
            let segment = path.get(index, self.tree);
            assert_eq!(
                segment.as_str(),
                expected,
                "Expected segment '{}' but found '{}'",
                expected,
                segment.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, NodeId<UnaryExpr>> {
        /// Get the operator of this unary expression
        pub fn op(self) -> UnaryOp {
            let unary = self.node.get(self.tree);
            unary.op(self.tree)
        }

        /// Assert the operator is the expected one
        pub fn has_op(self, expected: UnaryOp) -> Self {
            let op = self.op();
            assert_eq!(
                op, expected,
                "Expected unary operator {:?} but found {:?}",
                expected, op
            );
            self
        }

        /// Get an inspector for the operand
        pub fn operand(self) -> NodeInspector<'t, NodeId<Expr>> {
            let unary = self.node.get(self.tree);
            NodeInspector::new(unary.operand, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, NodeId<BinaryExpr>> {
        /// Get the operator of this binary expression
        pub fn op(self) -> BinaryOp {
            let binary = self.node.get(self.tree);
            binary.op(self.tree)
        }

        /// Assert the operator is the expected one
        pub fn has_op(self, expected: BinaryOp) -> Self {
            let op = self.op();
            assert_eq!(
                op, expected,
                "Expected binary operator {:?} but found {:?}",
                expected, op
            );
            self
        }

        /// Get an inspector for the left operand
        pub fn left(self) -> NodeInspector<'t, NodeId<Expr>> {
            let binary = self.node.get(self.tree);
            NodeInspector::new(binary.left, self.tree)
        }

        /// Get an inspector for the right operand
        pub fn right(self) -> NodeInspector<'t, NodeId<Expr>> {
            let binary = self.node.get(self.tree);
            NodeInspector::new(binary.right, self.tree)
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, NodeId<LetExpr>> {
        fn assert_name(self, expected: &str, node_type: &str) -> Self {
            let name = self.node.get(self.tree).name(self.tree);
            assert_eq!(
                name.as_str(),
                expected,
                "Expected {} name '{}' but found '{}'",
                node_type,
                expected,
                name.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, NodeId<LetExpr>> {
        /// Assert the let binding has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "let binding")
        }

        /// Get an inspector for the value expression
        pub fn value(self) -> NodeInspector<'t, NodeId<Expr>> {
            let let_expr = self.node.get(self.tree);
            NodeInspector::new(let_expr.value, self.tree)
        }

        /// Get an inspector for the inside expression
        pub fn inside(self) -> NodeInspector<'t, NodeId<Expr>> {
            let let_expr = self.node.get(self.tree);
            NodeInspector::new(let_expr.inside, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, NodeId<IfExpr>> {
        /// Get an inspector for the predicate expression
        pub fn predicate(self) -> NodeInspector<'t, NodeId<Expr>> {
            let if_expr = self.node.get(self.tree);
            NodeInspector::new(if_expr.predicate, self.tree)
        }

        /// Get an inspector for the then branch
        pub fn then(self) -> NodeInspector<'t, NodeId<Expr>> {
            let if_expr = self.node.get(self.tree);
            NodeInspector::new(if_expr.then, self.tree)
        }

        /// Get an inspector for the else branch
        pub fn or(self) -> NodeInspector<'t, NodeId<Expr>> {
            let if_expr = self.node.get(self.tree);
            NodeInspector::new(if_expr.or, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, NodeId<CaseExpr>> {
        /// Get an inspector for the case source expression
        pub fn source(self) -> NodeInspector<'t, NodeId<Expr>> {
            let case_expr = self.node.get(self.tree);
            NodeInspector::new(case_expr.source, self.tree)
        }

        /// Assert the case has the specified number of branches
        pub fn has_branches(self, count: usize) -> Self {
            let branches_len = self.node.get(self.tree).branches.len();
            assert_eq!(
                branches_len, count,
                "Expected {} branches but found {}",
                count, branches_len
            );
            self
        }

        /// Get an inspector for the branch at the given index
        pub fn branch_at(self, index: usize) -> NodeInspector<'t, NodeId<CaseBranch>> {
            let case_expr = self.node.get(self.tree);
            assert!(
                index < case_expr.branches.len(),
                "Branch index {} out of bounds (max {})",
                index,
                case_expr.branches.len() - 1
            );
            let branch_id = case_expr.branches[index];
            NodeInspector::new(branch_id, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, NodeId<CaseBranch>> {
        /// Get an inspector for the pattern in this branch
        pub fn pat(self) -> NodeInspector<'t, NodeId<Pat>> {
            let branch = self.node.get(self.tree);
            NodeInspector::new(branch.pat, self.tree)
        }

        /// Get an inspector for the result expression in this branch
        pub fn matches(self) -> NodeInspector<'t, NodeId<Expr>> {
            let branch = self.node.get(self.tree);
            NodeInspector::new(branch.matches, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, NodeId<LambdaExpr>> {
        /// Get the parameter name of this lambda
        pub fn param_name(self) -> &'t str {
            let lambda = self.node.get(self.tree);
            lambda.param(self.tree).as_str()
        }

        /// Assert the parameter has the expected name
        pub fn has_param(self, expected: &str) -> Self {
            let param = self.param_name();
            assert_eq!(
                param, expected,
                "Expected parameter name '{}' but found '{}'",
                expected, param
            );
            self
        }

        /// Get an inspector for the lambda body
        pub fn body(self) -> NodeInspector<'t, NodeId<Expr>> {
            let lambda = self.node.get(self.tree);
            NodeInspector::new(lambda.body, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, NodeId<CallExpr>> {
        /// Get an inspector for the function expression
        pub fn func(self) -> NodeInspector<'t, NodeId<Expr>> {
            let call = self.node.get(self.tree);
            NodeInspector::new(call.func, self.tree)
        }

        /// Get an inspector for the argument expression
        pub fn arg(self) -> NodeInspector<'t, NodeId<Expr>> {
            let call = self.node.get(self.tree);
            NodeInspector::new(call.arg, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, NodeId<ListExpr>> {
        /// Assert the list has the specified number of elements
        pub fn has_elements(self, count: usize) -> Self {
            let elements_len = self.node.get(self.tree).0.len();
            assert_eq!(
                elements_len, count,
                "Expected {} elements but found {}",
                count, elements_len
            );
            self
        }

        /// Get an inspector for the element at the given index
        pub fn element_at(self, index: usize) -> NodeInspector<'t, NodeId<Expr>> {
            let list = self.node.get(self.tree);
            assert!(
                index < list.0.len(),
                "Element index {} out of bounds (max {})",
                index,
                list.0.len() - 1
            );
            let element_id = list.0[index];
            NodeInspector::new(element_id, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, NodeId<RecordExpr>> {
        /// Assert the record has the specified number of fields
        pub fn has_fields(self, count: usize) -> Self {
            let fields_len = self.node.get(self.tree).0.len();
            assert_eq!(
                fields_len, count,
                "Expected {} fields but found {}",
                count, fields_len
            );
            self
        }

        /// Get an inspector for the field at the given index
        pub fn field_at(self, index: usize) -> NodeInspector<'t, NodeId<RecordField>> {
            let record = self.node.get(self.tree);
            assert!(
                index < record.0.len(),
                "Field index {} out of bounds (max {})",
                index,
                record.0.len() - 1
            );
            let field_id = record.0[index];
            NodeInspector::new(field_id, self.tree)
        }

        /// Get an inspector for the field with the given name, if it exists
        pub fn field_named(self, name: &str) -> Option<NodeInspector<'t, NodeId<RecordField>>> {
            let record = self.node.get(self.tree);
            record.get(name, self.tree).map(|_| {
                let field_id = record
                    .0
                    .iter()
                    .find(|id| id.get(self.tree).field(self.tree).as_str() == name)
                    .unwrap();
                NodeInspector::new(*field_id, self.tree)
            })
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, NodeId<RecordField>> {
        fn assert_name(self, expected: &str, node_type: &str) -> Self {
            let name = self.node.get(self.tree).field(self.tree);
            assert_eq!(
                name.as_str(),
                expected,
                "Expected {} name '{}' but found '{}'",
                node_type,
                expected,
                name.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, NodeId<RecordField>> {
        /// Assert the record field has the expected name
        pub fn has_field_name(self, expected: &str) -> Self {
            self.assert_name(expected, "field")
        }

        /// Get an inspector for the field value
        pub fn value(self) -> NodeInspector<'t, NodeId<Expr>> {
            let field = self.node.get(self.tree);
            NodeInspector::new(field.value, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, NodeId<RecordExtendExpr>> {
        /// Get an inspector for the source record
        pub fn source(self) -> NodeInspector<'t, NodeId<Expr>> {
            let extend = self.node.get(self.tree);
            NodeInspector::new(extend.source, self.tree)
        }

        /// Assert the field being extended has the expected name
        pub fn has_field(self, expected: &str) -> Self {
            let field = self.node.get(self.tree).field(self.tree);
            assert_eq!(
                field.as_str(),
                expected,
                "Expected field name '{}' but found '{}'",
                expected,
                field.0
            );
            self
        }

        /// Get an inspector for the value being added
        pub fn value(self) -> NodeInspector<'t, NodeId<Expr>> {
            let extend = self.node.get(self.tree);
            NodeInspector::new(extend.value, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, NodeId<RecordRestrictExpr>> {
        /// Get an inspector for the source record
        pub fn source(self) -> NodeInspector<'t, NodeId<Expr>> {
            let restrict = self.node.get(self.tree);
            NodeInspector::new(restrict.source, self.tree)
        }

        /// Assert the field being restricted has the expected name
        pub fn has_field(self, expected: &str) -> Self {
            let field = self.node.get(self.tree).field(self.tree);
            assert_eq!(
                field.as_str(),
                expected,
                "Expected field name '{}' but found '{}'",
                expected,
                field.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, NodeId<RecordUpdateExpr>> {
        /// Get an inspector for the source record
        pub fn source(self) -> NodeInspector<'t, NodeId<Expr>> {
            let update = self.node.get(self.tree);
            NodeInspector::new(update.source, self.tree)
        }

        /// Assert the field being updated has the expected name
        pub fn has_field(self, expected: &str) -> Self {
            let field = self.node.get(self.tree).field(self.tree);
            assert_eq!(
                field.as_str(),
                expected,
                "Expected field name '{}' but found '{}'",
                expected,
                field.0
            );
            self
        }

        /// Get the update operation
        pub fn op(self) -> RecordUpdateOp {
            let update = self.node.get(self.tree);
            update.op(self.tree)
        }

        /// Assert the update operation is the expected one
        pub fn has_op(self, expected: RecordUpdateOp) -> Self {
            let op = self.op();
            assert_eq!(
                op, expected,
                "Expected record update operator {:?} but found {:?}",
                expected, op
            );
            self
        }

        /// Get an inspector for the new value
        pub fn value(self) -> NodeInspector<'t, NodeId<Expr>> {
            let update = self.node.get(self.tree);
            NodeInspector::new(update.value, self.tree)
        }
    }
}
