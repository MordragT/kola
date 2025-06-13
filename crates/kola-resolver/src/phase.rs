// use std::ops::Index;

// use kola_collections::HashMap;
use kola_tree::meta::{MetaMap, Phase};

use crate::symbol::{ModuleSym, TypeSym, ValueSym};

// #[derive(Debug, Clone, Default)]
// pub struct Resolutions(HashMap<ModuleSym, ResolvedNodes>);

// impl Resolutions {
//     pub fn new() -> Self {
//         Self(HashMap::new())
//     }

//     pub fn insert(&mut self, module: ModuleSym, nodes: ResolvedNodes) {
//         self.0.insert(module, nodes);
//     }

//     pub fn get(&self, module: &ModuleSym) -> Option<&ResolvedNodes> {
//         self.0.get(module)
//     }
// }

// impl Index<ModuleSym> for Resolutions {
//     type Output = ResolvedNodes;

//     fn index(&self, module: ModuleSym) -> &Self::Output {
//         self.0
//             .get(&module)
//             .expect("Module not found in resolutions")
//     }
// }

pub type ResolvedNodes = MetaMap<ResolvePhase>;

#[derive(Debug, Clone, Copy)]
pub struct ResolvePhase;
impl Phase for ResolvePhase {
    // ===== NAMES =====
    // Names are the source of symbols, not targets of resolution
    type ModuleName = !;
    type TypeName = !;
    type ValueName = !;

    // ===== PATTERNS =====
    // TODO: Pattern matching will need symbols for bindings like `let {x, y} = record`
    // For now, patterns don't introduce symbols in our resolver
    type AnyPat = !;
    type LiteralPat = !;
    type IdentPat = !; // Future: ValueSym for pattern bindings
    type RecordFieldPat = !; // Future: ValueSym for destructured fields
    type RecordPat = !;
    type VariantCasePat = !;
    type VariantPat = !;
    type PatError = !;
    type Pat = !;

    // ===== EXPRESSIONS =====

    // Basic expressions - no symbols needed
    type LiteralExpr = !; // Constants don't need symbols
    type ListExpr = !; // List construction is structural
    type UnaryOp = !;
    type UnaryExpr = !;
    type BinaryOp = !;
    type BinaryExpr = !;
    type CaseBranch = !;
    type CaseExpr = !;
    type IfExpr = !;
    type CallExpr = !;
    type ExprError = !;
    type Expr = !;

    // Path expressions - resolve to existing value symbols
    // TODO: Consider splitting into PathExpr { path: Option<ModulePath>, select: SelectExpr }
    // where only SelectExpr gets the ValueSym for cleaner separation of navigation vs. selection
    type PathExpr = ValueSym; // Resolves to the referenced value symbol

    // Record operations - structural, no new symbols needed
    type RecordField = !;
    type RecordExpr = !; // TODO: Could introduce field symbols for dependent fields
    // e.g., { a: 1, b: a + 2 } where b references a
    type RecordExtendExpr = !; // { r | +field = value } - field is just a label
    type RecordRestrictExpr = !; // { r | -field } - no new symbols
    type RecordUpdateOp = !; // Update operations are structural
    type RecordUpdateExpr = !; // { r | field = value } - no new symbols

    // Binding expressions - create new symbols
    type LetExpr = ValueSym; // Creates symbol for the bound variable
    type LambdaExpr = ValueSym; // Creates symbol for the parameter binding

    // ===== TYPES =====
    // Type expressions are not needed for the untyped lowerer phase
    // Future: When adding typed IR, these could get ModuleSym for qualified types
    type TypePath = !; // Future: Could reference module for qualified types
    type TypeVar = !;
    type RecordFieldType = !; // Field names exist in value namespace but no symbols needed here
    type RecordType = !; // Structural type, no symbols
    type VariantCaseType = !; // Variant cases exist in value namespace
    type VariantType = !; // Structural type, no symbols
    type FuncType = !;
    type TypeApplication = !;
    type TypeExpr = !;
    type TypeError = !;
    type Type = !;

    // ===== BINDINGS =====
    // Top-level binding constructs - create symbols for what they bind
    type Vis = !; // Visibility modifiers don't need symbols
    type ValueBind = ValueSym; // Creates symbol for the bound value
    type TypeBind = TypeSym; // Creates symbol for the bound type
    type OpaqueTypeBind = TypeSym; // Creates symbol for the opaque type
    type ModuleBind = ModuleSym; // Creates symbol for the module alias
    type ModuleTypeBind = !; // Module type bindings - future feature
    type Bind = !; // Generic bind wrapper - symbols handled by specific binds

    // ===== MODULES =====
    // Module constructs - create or reference module symbols
    type Module = ModuleSym; // Creates symbol for the module definition
    type ModulePath = ModuleSym; // Resolves to the referenced module symbol
    type ModuleImport = ModuleSym; // Creates symbol for the imported module binding
    type ModuleExpr = !; // Future: First-class modules could get ModuleSym

    // ===== SPECIFICATIONS =====
    // Module signatures and specs - future feature for module system
    // These define names in their respective namespaces but don't need resolver symbols yet
    type ValueSpec = !; // Future: Could get ValueSym for signature checking
    type OpaqueTypeKind = !; // Future: Type kind specifications
    type OpaqueTypeSpec = !; // Future: Opaque type specifications
    type ModuleSpec = !; // Future: Module signature specifications
    type Spec = !; // Future: Generic specification wrapper
    type ModuleType = !; // Future: Module type expressions
}
