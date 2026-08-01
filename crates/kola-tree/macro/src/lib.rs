//! Procedural macros for the kola tree
//!
//! This crate provides traits and derive macros to automatically generate
//! NodeInspector implementations based on struct/enum field introspection.

use proc_macro::TokenStream;
use quote::quote;
use syn::{DeriveInput, Type, parse_macro_input};

mod inspect;

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

    match inspect::generate_inspector_impl(&input) {
        Ok(tokens) => tokens,
        Err(err) => err.to_compile_error().into(),
    }
}

#[derive(Debug, Clone)]
enum FieldTypeClass {
    SingleId(proc_macro2::TokenStream),
    SliceId(proc_macro2::TokenStream),
    OptionId(proc_macro2::TokenStream),
    Other,
}

fn classify_field_type(ty: &Type) -> FieldTypeClass {
    let Type::Path(type_path) = ty else {
        return FieldTypeClass::Other;
    };

    let Some(segment) = type_path.path.segments.first() else {
        return FieldTypeClass::Other;
    };

    let syn::PathArguments::AngleBracketed(args) = &segment.arguments else {
        return FieldTypeClass::Other;
    };

    let Some(syn::GenericArgument::Type(inner_ty)) = args.args.first() else {
        return FieldTypeClass::Other;
    };

    match segment.ident.to_string().as_str() {
        "SliceId" => FieldTypeClass::SliceId(quote!(#inner_ty)),
        "Id" => FieldTypeClass::SingleId(quote!(#inner_ty)),
        "Option" => extract_id_inner_type(inner_ty)
            .map(FieldTypeClass::OptionId)
            .unwrap_or(FieldTypeClass::Other),
        _ => FieldTypeClass::Other,
    }
}

fn extract_id_inner_type(ty: &Type) -> Option<proc_macro2::TokenStream> {
    let Type::Path(type_path) = ty else {
        return None;
    };

    let segment = type_path.path.segments.first()?;

    if segment.ident != "Id" {
        return None;
    }

    let syn::PathArguments::AngleBracketed(args) = &segment.arguments else {
        return None;
    };

    let Some(syn::GenericArgument::Type(inner_ty)) = args.args.first() else {
        return None;
    };

    Some(quote!(#inner_ty))
}
