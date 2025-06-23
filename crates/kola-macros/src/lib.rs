//! Procedural macros for the kola compiler
//!
//! This crate provides traits and derive macros to automatically generate
//! NodeInspector implementations based on struct/enum field introspection.

use proc_macro::TokenStream;
use syn::{DeriveInput, parse_macro_input};

mod inspector;

/// Derive macro that automatically generates NodeInspector methods
/// based on the structure of the AST nodes.
///
/// # Example
///
/// ```rust
/// #[derive(Inspector)]
/// struct TypeScheme {
///     vars: Vec<Id<TypeVar>>,       // Generates: has_vars_count(), vars_at()
///     ty: Id<Type>,                 // Generates: ty()
///
///     #[inspector(getter)]
///     name: String,                 // Generates: name() -> &String
///
///     #[inspector(skip)]
///     internal_data: ComplexType,   // Skipped - no methods generated
/// }
/// ```
///
/// # Attributes
///
/// - `#[inspector(skip)]` - Skip this field, generate no methods
/// - `#[inspector(getter)]` - Generate a simple getter for leaf/unknown types
#[proc_macro_derive(Inspector, attributes(inspector))]
pub fn derive_inspector(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match inspector::generate_inspector_impl(&input) {
        Ok(tokens) => tokens,
        Err(err) => err.to_compile_error().into(),
    }
}
