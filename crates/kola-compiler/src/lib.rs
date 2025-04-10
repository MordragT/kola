use kola_tree::Phase;

pub mod ir;
pub mod normalize;

pub mod prelude {
    pub use crate::normalize::Normalizer;
}

// #[derive(Debug, Clone, Copy)]
// pub struct CompilePhase;

// impl Phase for CompilePhase {
//     type Name = ir::InstrId<ir::Symbol>;
//     type Ident = ir::InstrId<ir::Symbol>;
//     type Literal = ir::InstrId<ir::Literal>;
//     type List = ();
//     type Property = ();
//     type Record = ();
//     type RecordSelect = ();
//     type RecordExtend = ();
//     type RecordRestrict = ();
//     type RecordUpdate = ();
//     type UnaryOp = ();
//     type Unary = ();
//     type BinaryOp = ();
//     type Binary = ();
//     type Let = ir::Let;
//     type PatError = ();
//     type Wildcard = ();
//     type LiteralPat = ();
//     type IdentPat = ();
//     type PropertyPat = ();
//     type RecordPat = ();
//     type Pat = ();
//     type Branch = ();
//     type Case = ();
//     type If = ir::Let;
//     type Func = ir::Func;
//     type Call = ir::Let;
//     type ExprError = ();
//     type Expr = ir::Expr;
// }
