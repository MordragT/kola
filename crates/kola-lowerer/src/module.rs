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
    id::Id as InstrId,
    instr as ir,
    ir::{Ir, IrBuilder},
    print::IrPrinter,
};
use kola_print::prelude::*;
use kola_resolver::{
    forest::Forest,
    resolver::ValueOrders,
    scope::{ModuleScope, ModuleScopes},
    symbol::{ModuleSym, ValueSym},
};
use kola_tree::tree::Tree;
use kola_utils::interner::StrInterner;
use log::debug;

use crate::{normalizer::Normalizer, symbol::SymbolEnv};

#[derive(Debug, Clone)]
pub struct Program {
    pub ir: Ir,
    pub modules: Vec<LoweredModule>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LoweredModule {
    pub sym: ModuleSym,
}

/// Compiles a single module into executable IR.
///
/// This function implements the core module compilation strategy, transforming value bindings
/// into a let-chain followed by an export record. The compilation uses **reverse dependency order**
/// to build the let-chain correctly in CPS style.
///
/// ## Compilation Process
///
/// Given a module with value bindings in dependency order `[a, b, c]` where `c` depends on `b`
/// and `b` depends on `a`, the compilation process works as follows:
///
/// 1. **Create target record structure**: Build the module export record that will be the final result
/// 2. **Reverse iteration**: Process bindings in reverse order `[c, b, a]` to build let-chain
/// 3. **CPS compilation**: Each binding becomes a let-expression with the next binding as continuation
///
/// This produces IR equivalent to:
/// ```
/// let a = <expr_a> in
/// let b = <expr_b> in          // can reference a
/// let c = <expr_c> in          // can reference a, b
/// { a: a, b: b, c: c }         // export record
/// ```
///
/// ## Why Reverse Order?
///
/// The reverse iteration is essential because we're building a **continuation-passing style** let-chain.
/// Each normalization step creates a let-expression where:
/// - The binding gets the current value
/// - The continuation (`next`) is the rest of the computation
///
/// By processing in reverse dependency order:
/// 1. Last binding (`c`) gets the record creation as its continuation
/// 2. Second-to-last binding (`b`) gets the `c` let-expression as its continuation
/// 3. First binding (`a`) gets the entire `b`+`c`+record chain as its continuation
///
/// This ensures proper evaluation order: `a` → `b` → `c` → record creation.
///
/// # Arguments
/// * `scope` - The module's scope containing bindings and definitions
/// * `value_order` - Topologically sorted value symbols (dependency order from resolver)
/// * `tree` - The AST tree containing module source code
///
/// # Returns
/// A `LoweredModule` containing the compiled IR that evaluates to the module's export record.
pub fn lower_module(
    next: InstrId<ir::Expr>,
    builder: &mut IrBuilder,
    scope: &ModuleScope,
    value_order: &[ValueSym],
    tree: &Tree,
) -> InstrId<ir::Expr> {
    let symbols = SymbolEnv::new(&scope.resolved);

    // Start with the module result (record creation)
    let bind = ir::Symbol(scope.info.sym.id());
    let mut fields = ir::RecordExpr {
        bind,
        head: None,
        next,
    };

    // Collect field information first
    for &value_sym in value_order {
        let id = scope.defs[value_sym].id();
        let label = id.get(tree).name.get(tree).0;
        let hole = symbols.symbol_of(id);
        fields.add_field((label, ir::Atom::Symbol(hole)), builder);
    }

    // Create the record expression with the continuation
    let record_expr = builder.add(ir::Expr::Record(fields));

    // Now process value bindings in reverse order to build let-chain
    let mut next = record_expr;

    for &value_sym in value_order.iter().rev() {
        let id = scope.defs[value_sym].id();
        let value_bind = id.get(tree);
        let hole = symbols.symbol_of(id);

        let normalizer = Normalizer::new(value_bind.value, next, hole, builder, symbols);
        next = normalizer.run(tree);
    }

    next
}

/// Compiles multiple modules into a complete program.
///
/// Takes modules in topological dependency order and creates a program IR
/// where each module is bound to its symbol, followed by the program entry point.
///
/// The compilation creates a let-chain structure:
/// ```
/// let module1 = <module1_body> in
/// let module2 = <module2_body> in  // can reference module1
/// ...
/// <entry_point>  // TODO: determine entry point
/// ```
///
/// # Arguments
/// * `scopes` - All module scopes in the program
/// * `value_orders` - Value dependency orders for each module
/// * `forest` - AST forest containing all source files
///
/// # Returns
/// A `Program` containing the compiled IR and module metadata.
pub fn lower(
    entry_point: ValueSym,
    scopes: &ModuleScopes,
    value_orders: &ValueOrders,
    forest: &Forest,
    arena: &Bump,
    interner: &StrInterner,
    print_options: PrintOptions,
) -> Program {
    let mut builder = IrBuilder::new();
    let mut modules = Vec::new();

    let entry = ir::Symbol(entry_point.id());
    let arg = builder.add(ir::Atom::Symbol(entry));
    let mut next = builder.add(ir::Expr::Ret(ir::RetExpr { arg }));

    for (&sym, scope) in scopes {
        let value_order = &value_orders[&sym];
        let tree = &*forest[scope.info.source];

        next = lower_module(next, &mut builder, scope, value_order, tree);

        modules.push(LoweredModule { sym });
    }

    let ir = builder.finish(next);
    let ir_printer = IrPrinter::new(&ir, interner, next);

    debug!(
        "{}\n{}",
        "Intermediate Representation".bold().bright_white(),
        ir_printer.render(print_options, arena)
    );

    Program { ir, modules }
}
