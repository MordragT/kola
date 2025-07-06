// use std::ops::Index;

use std::fmt;

use kola_builtins::{BuiltinEffect, BuiltinId, BuiltinType};
use kola_tree::{
    meta::{MetaMap, Phase},
    node::{EffectNamespace, ModuleNamespace, ModuleTypeNamespace, TypeNamespace, ValueNamespace},
};
use kola_utils::as_variant;

use crate::symbol::{
    AnySym, EffectSym, FunctorSym, ModuleSym, ModuleTypeSym, Substitute, TypeSym, ValueSym,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ResolvedValue {
    Reference(ValueSym),
    Builtin(BuiltinId),
}

impl ResolvedValue {
    pub fn into_builtin(self) -> Option<BuiltinId> {
        as_variant!(self, Self::Builtin)
    }

    pub fn into_reference(self) -> Option<ValueSym> {
        as_variant!(self, Self::Reference)
    }
}

impl fmt::Display for ResolvedValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ResolvedValue::Reference(sym) => sym.fmt(f),
            ResolvedValue::Builtin(id) => id.fmt(f),
        }
    }
}

impl Substitute<ValueNamespace> for ResolvedValue {
    fn try_substitute(&self, from: ValueSym, to: ValueSym) -> Option<Self>
    where
        Self: Sized,
    {
        if let Self::Reference(sym) = self
            && *sym == from
        {
            Some(Self::Reference(to))
        } else {
            None
        }
    }
}

impl Substitute<ModuleNamespace> for ResolvedValue {
    fn try_substitute(&self, _from: ModuleSym, _to: ModuleSym) -> Option<Self>
    where
        Self: Sized,
    {
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ResolvedType {
    Reference(TypeSym),
    Builtin(BuiltinType),
}

impl ResolvedType {
    pub fn into_builtin(self) -> Option<BuiltinType> {
        as_variant!(self, Self::Builtin)
    }

    pub fn into_reference(self) -> Option<TypeSym> {
        as_variant!(self, Self::Reference)
    }
}

impl fmt::Display for ResolvedType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ResolvedType::Reference(sym) => sym.fmt(f),
            ResolvedType::Builtin(ty) => ty.fmt(f),
        }
    }
}

impl Substitute<TypeNamespace> for ResolvedType {
    fn try_substitute(&self, from: TypeSym, to: TypeSym) -> Option<Self>
    where
        Self: Sized,
    {
        if let Self::Reference(sym) = self
            && *sym == from
        {
            Some(Self::Reference(to))
        } else {
            None
        }
    }
}

impl Substitute<ModuleNamespace> for ResolvedType {
    fn try_substitute(&self, _from: ModuleSym, _to: ModuleSym) -> Option<Self>
    where
        Self: Sized,
    {
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ResolvedEffect {
    Reference(EffectSym),
    Builtin(BuiltinEffect),
}

impl ResolvedEffect {
    pub fn into_builtin(self) -> Option<BuiltinEffect> {
        as_variant!(self, Self::Builtin)
    }

    pub fn into_reference(self) -> Option<EffectSym> {
        as_variant!(self, Self::Reference)
    }
}

impl fmt::Display for ResolvedEffect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ResolvedEffect::Reference(sym) => sym.fmt(f),
            ResolvedEffect::Builtin(ty) => ty.fmt(f),
        }
    }
}

impl Substitute<EffectNamespace> for ResolvedEffect {
    fn try_substitute(&self, from: EffectSym, to: EffectSym) -> Option<Self>
    where
        Self: Sized,
    {
        if let Self::Reference(sym) = self
            && *sym == from
        {
            Some(Self::Reference(to))
        } else {
            None
        }
    }
}

impl Substitute<ModuleNamespace> for ResolvedEffect {
    fn try_substitute(&self, _from: ModuleSym, _to: ModuleSym) -> Option<Self>
    where
        Self: Sized,
    {
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolvedModule(pub ModuleSym);

impl Substitute<ModuleNamespace> for ResolvedModule {
    fn try_substitute(&self, from: ModuleSym, to: ModuleSym) -> Option<Self>
    where
        Self: Sized,
    {
        if self.0 == from { Some(Self(to)) } else { None }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolvedModuleType(pub ModuleTypeSym);

impl Substitute<ModuleTypeNamespace> for ResolvedModuleType {
    fn try_substitute(&self, from: ModuleTypeSym, to: ModuleTypeSym) -> Option<Self>
    where
        Self: Sized,
    {
        if self.0 == from { Some(Self(to)) } else { None }
    }
}

impl Substitute<ModuleNamespace> for ResolvedModuleType {
    fn try_substitute(&self, _from: ModuleSym, _to: ModuleSym) -> Option<Self>
    where
        Self: Sized,
    {
        None
    }
}

pub type ResolvedNodes = MetaMap<ResolvePhase>;

#[derive(Debug, Clone, Copy)]
pub struct ResolvePhase;
impl Phase for ResolvePhase {
    // ===== NAMES =====
    // Names are the source of symbols, not targets of resolution
    type EffectName = !;
    type FunctorName = !;
    type ModuleTypeName = !;
    type ModuleName = !;
    type TypeName = !;
    type ValueName = !;

    // ===== PATTERNS =====
    // TODO: Pattern matching will need symbols for bindings like `let {x, y} = record`
    type AnyPat = !;
    type LiteralPat = !;
    type BindPat = ValueSym;
    type ListElPat = ValueSym;
    type ListPat = !;
    type RecordFieldPat = ValueSym; // Future: ValueSym for destructured fields
    type RecordPat = !;
    type VariantTagPat = !;
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
    type SymbolExpr = AnySym;
    type ExprError = !;
    type Expr = !;

    type QualifiedExpr = ResolvedValue;
    type HandleExpr = !;
    type DoExpr = !;
    type TagExpr = !;
    type FieldPath = !;

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
    type HandlerClause = ValueSym; // Creates symbol for the handler parameter

    // ===== TYPES =====
    // Type expressions are not needed for the untyped lowerer phase
    // Future: When adding typed IR, these could get ModuleSym for qualified types
    type QualifiedEffectType = ResolvedEffect;
    type EffectOpType = !;
    type EffectRowType = !;
    type EffectType = !;

    type QualifiedType = ResolvedType;
    type TypeVar = TypeSym; // Type variables only occur in forall quantifier definitions
    type RecordFieldType = !; // Field names exist in value namespace but no symbols needed here
    type RecordType = !; // Structural type, no symbols
    type TagType = !; // Variant tags exist in value namespace
    type VariantType = !; // Structural type, no symbols
    type FuncType = !;
    type TypeApplication = !;
    type CompType = !;
    type Type = !;
    type TypeError = !;
    type TypeScheme = !;

    // ===== BINDINGS =====
    // Top-level binding constructs - create symbols for what they bind
    type Vis = !; // Visibility modifiers don't need symbols
    type ValueBind = ValueSym; // Creates symbol for the bound value
    type TypeBind = TypeSym; // Creates symbol for the bound type
    type OpaqueTypeBind = TypeSym; // Creates symbol for the opaque type
    type EffectTypeBind = EffectSym;
    type ModuleBind = ModuleSym; // Creates symbol for the module alias
    type ModuleTypeBind = ModuleTypeSym; // Module type bindings - future feature
    type FunctorBind = FunctorSym; // Creates symbol for the functor binding
    type Bind = !; // Generic bind wrapper - symbols handled by specific binds

    // ===== MODULES =====
    // Module constructs - create or reference module symbols
    type ModuleError = !;
    type Module = ModuleSym; // Creates symbol for the module definition
    type ModulePath = ResolvedModule; // Resolves to the referenced module symbol
    type ModuleImport = ModuleSym; // Creates symbol for the imported module binding
    type FunctorApp = ModuleSym;
    type ModuleExpr = !; // Future: First-class modules could get ModuleSym

    // TODO maybe these should have a own ModuleTypeSym
    // ===== SPECIFICATIONS =====
    // Module signatures and specs - future feature for module system
    // These define names in their respective namespaces but don't need resolver symbols yet
    type ValueSpec = !; // Future: Could get ValueSym for signature checking
    type OpaqueTypeKind = !; // Future: Type kind specifications
    type OpaqueTypeSpec = !; // Future: Opaque type specifications
    type ModuleSpec = !; // Future: Module signature specifications
    type Spec = !; // Future: Generic specification wrapper
    type ConcreteModuleType = !;
    type QualifiedModuleType = ResolvedModuleType;
    type ModuleType = !; // Future: Module type expressions
}
