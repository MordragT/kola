//! # Module Compilation and Lowering
//!
//! This module implements the compilation strategy for Kola's module system, which transforms
//! high-level module definitions into executable IR while maintaining proper scoping and
//! cross-module reference capabilities.
//!
//! ## Compilation Strategy
//!
//! Each module is compiled into a **record of exported values**, preceded by a **let-chain**
//! that establishes proper scoping for intra-module references. This approach elegantly
//! handles both local dependencies within modules and cross-module references.
//!
//! ### Single Module Compilation
//!
//! A module like:
//! ```kola
//! {
//!     helper = \x => x + 1,
//!     result = helper 42,
//! }
//! ```
//!
//! Gets compiled to IR equivalent to:
//! ```
//! let helper = \x => x + 1 in
//! let result = helper 42 in
//! { helper: helper, result: result }
//! ```
//!
//! ### Multi-Module Program Structure
//!
//! For a program with multiple modules, the final IR structure becomes:
//!
//! ```
//! let <module_sym_1> =
//!   let <value_bind_1> = ... in
//!   let <value_bind_2> = ... in
//!   ...
//!   let <value_bind_n> = ... in
//!   { <exports...> } in
//! let <module_sym_2> =
//!   let <value_bind_1> = ... in
//!   let <value_bind_2> = <module_sym_1>.some_export in
//!   ...
//!   let <value_bind_m> = ... in
//!   { <exports...> } in
//! ...
//! let <module_sym_x> = ... in
//! <main_symbol>
//! ```
//!
//! ## Key Benefits
//!
//! ### 1. **Intra-Module References**
//! The let-chain structure naturally handles dependencies within a module:
//! - Topological sorting ensures correct evaluation order
//! - Later bindings can reference earlier ones seamlessly
//! - Local scoping is preserved
//!
//! ### 2. **Cross-Module References**
//! Module records enable clean cross-module access:
//! - `ModuleA.function` becomes simple record field access
//! - Module dependencies are explicit in the IR
//! - Type checker can verify module interfaces
//!
//! ### 3. **Entry Point Flexibility**
//! Programs can designate entry points in multiple ways:
//! - **Named main**: Look for `main` binding across modules
//! - **Entry module**: Designate a specific module as program entry
//! - **Explicit**: User specifies entry point at compile time
//!
//! ### 4. **Functor Readiness**
//! This compilation strategy naturally supports SML-style functors:
//! - Functors compile to parameterized module templates
//! - Functor application becomes module instantiation with parameter substitution
//! - Each functor application creates a new module record
//!
//! ## Implementation Details
//!
//! The compilation process follows these steps:
//!
//! 1. **Dependency Analysis**: Use resolver's topological sort for correct binding order
//! 2. **Value Compilation**: Normalize each value binding using the expression normalizer
//! 3. **Let-Chain Construction**: Build nested let expressions in reverse dependency order
//! 4. **Export Record**: Create record containing only exported values
//! 5. **Module Symbol Binding**: Assign the module record to its symbol
//!
//! Cross-module references are resolved by:
//! 1. Ensuring referenced modules are compiled first (module topological order)
//! 2. Replacing `ModuleA.field` with record access operations
//! 3. Maintaining proper symbol resolution through the resolver's symbol table
//!
//! ## Future Extensions
//!
//! This architecture supports several advanced features:
//!
//! - **Module Types/Signatures**: Interface verification through record type checking
//! - **First-Class Modules**: Modules as values, conditional module selection
//! - **Recursive Modules**: Handled naturally by topological dependency analysis
//! - **Open Records**: Module extension and record operations
//! - **Hot Reloading**: Individual module recompilation and replacement

use kola_ir::{
    instr as ir,
    ir::{Ir, IrBuilder},
};
use kola_resolver::{
    forest::Forest,
    resolver::ValueOrders,
    scope::{ModuleScope, ModuleScopes},
    symbol::{ModuleSym, ValueSym},
};
use kola_tree::{node, tree::Tree};

use crate::{normalizer::Normalizer, symbol::SymbolEnv};

pub struct LoweredModule {
    pub sym: ModuleSym,
    pub ir: Ir,
}

/// Compiles a single module into executable IR.
///
/// This function implements the core module compilation strategy described above,
/// transforming value bindings into a let-chain followed by an export record.
///
/// # Arguments
/// * `scope` - The module's scope containing bindings and definitions
/// * `value_order` - Topologically sorted value symbols (dependency order)
/// * `tree` - The AST tree containing module source code
///
/// # Returns
/// A `LoweredModule` containing the compiled IR that evaluates to the module's export record.
pub fn lower_module(scope: &ModuleScope, value_order: &[ValueSym], tree: &Tree) -> LoweredModule {
    let symbols = SymbolEnv::new(&scope.resolved);
    let mut builder = IrBuilder::new();

    let bind = ir::Symbol(scope.info.sym.id());
    let arg = builder.add(ir::Atom::Symbol(bind));
    let mut next = builder.add(ir::Expr::Ret(ir::RetExpr { arg }));

    let mut fields = Vec::new();

    for &value_sym in value_order {
        let id = scope.defs[value_sym].id();

        let value_bind = id.get(tree);
        let hole = symbols.symbol_of(id);

        let normalizer = Normalizer::new(value_bind.value, next, hole, &mut builder, symbols);
        next = normalizer.run(tree);

        let value = builder.add(ir::Atom::Symbol(hole));
        fields.push(ir::RecordField { label: hole, value });
    }

    let fields = builder.add(fields);
    next = builder.add(ir::Expr::Record(ir::RecordExpr { bind, fields, next }));

    let ir = builder.finish(next);

    LoweredModule {
        sym: scope.info.sym,
        ir,
    }
}

/// Compiles multiple modules into a complete program.
///
/// Takes modules in topological dependency order and creates a program IR
/// where each module is bound to its symbol, followed by the program entry point.
///
/// # Arguments
/// * `scopes` - All module scopes in the program
/// * `value_orders` - Value dependency orders for each module
/// * `forest` - AST forest containing all source files
///
/// # Returns
/// A vector of compiled modules that can be linked into a final program.
pub fn lower(
    scopes: &ModuleScopes,
    value_orders: &ValueOrders,
    forest: &Forest,
) -> Vec<LoweredModule> {
    let mut lowered_modules = Vec::new();

    for (sym, scope) in scopes.iter() {
        let value_order = &value_orders[sym];
        let tree = &*forest[scope.info.source];

        let lowered_module = lower_module(scope, value_order, tree);
        lowered_modules.push(lowered_module);
    }

    lowered_modules
}
