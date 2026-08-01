//! Procedural macros for the kola-print crate.
//!
//! This crate provides the `Notate` derive macro, which generates
//! `impl Notate<'a, T> for Cx` for a user-provided context type `Cx`.

use proc_macro::TokenStream;
use syn::{DeriveInput, parse_macro_input};

mod notate;

/// Derive macro that generates a `Notate` implementation for a context type.
///
/// The context type is provided via the required `#[notate(with = ...)]`
/// attribute. The generated impl is `impl<'a> Notate<'a, T> for Cx`.
///
/// # Example
///
/// ```rust
/// #[derive(Notate)]
/// #[notate(with = TreePrinter, color = "blue")]
/// struct ListExpr(#[notate(skip)] SliceId<Expr>);
/// ```
///
/// # Type Attributes
///
/// - `#[notate(with = "Cx")]` - The context type implementing `Notate` (required)
/// - `#[notate(color = "red")]` - Set the head color (default: "bright_blue")
/// - `#[notate(name = "CustomName")]` - Custom display name
///
/// # Field Attributes
///
/// - `#[notate(skip)]` - Skip this field in output
/// - `#[notate(display)]` - Use Display trait instead of Debug
/// - `#[notate(custom = "function_name")]` - Use custom formatter function
#[proc_macro_derive(Notate, attributes(notate))]
pub fn derive_notate(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match notate::generate_notate_impl(&input) {
        Ok(tokens) => tokens,
        Err(err) => err.to_compile_error().into(),
    }
}
