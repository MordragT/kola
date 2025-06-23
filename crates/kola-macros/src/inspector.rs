use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    Attribute, Data, DataEnum, DataStruct, DeriveInput, Fields, FieldsNamed, FieldsUnnamed, Ident,
    Meta, MetaList, Type,
};

use super::{FieldTypeClass, classify_field_type};

pub fn generate_inspector_impl(input: &DeriveInput) -> syn::Result<TokenStream> {
    let name = &input.ident;

    let methods = match &input.data {
        Data::Struct(data_struct) => generate_struct_methods(name, data_struct)?,
        Data::Enum(data_enum) => generate_enum_methods(name, data_enum)?,
        Data::Union(_) => {
            return Err(syn::Error::new_spanned(
                name,
                "Inspector cannot be derived for unions",
            ));
        }
    };

    Ok(quote! {
        impl<'t, S: std::hash::BuildHasher> crate::inspector::NodeInspector<'t, crate::id::Id<#name>, S> {
            #(#methods)*
        }
    }.into())
}

fn generate_struct_methods(
    struct_name: &Ident,
    data_struct: &DataStruct,
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    match &data_struct.fields {
        Fields::Named(fields_named) => generate_named_field_methods(fields_named),
        Fields::Unnamed(fields_unnamed) => generate_unnamed_field_methods(fields_unnamed),
        Fields::Unit => {
            Ok(vec![]) // Unit structs get no generated methods
        }
    }
}

fn generate_named_field_methods(
    fields: &FieldsNamed,
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let mut methods = Vec::new();

    for field in &fields.named {
        let field_name = field.ident.as_ref().unwrap();
        let field_type = &field.ty;
        let field_attrs = parse_field_attributes(&field.attrs)?;

        let method =
            generate_field_method(FieldAccess::Named(field_name), field_type, &field_attrs)?;

        if !method.is_empty() {
            methods.push(method);
        }
    }

    Ok(methods)
}

fn generate_unnamed_field_methods(
    fields: &FieldsUnnamed,
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    // Only support tuple structs with exactly one field
    if fields.unnamed.len() != 1 {
        return Err(syn::Error::new_spanned(
            fields,
            "Inspector only supports tuple structs with exactly one field",
        ));
    }

    let field = &fields.unnamed[0];
    let field_attrs = parse_field_attributes(&field.attrs)?;

    // Use the generic field method generator with unnamed field parameters
    let method = generate_field_method(FieldAccess::Unnamed, &field.ty, &field_attrs)?;

    Ok(vec![method])
}

enum FieldAccess<'a> {
    Named(&'a Ident),
    Unnamed,
}

fn generate_field_method(
    field_access: FieldAccess,
    field_type: &Type,
    field_attrs: &FieldAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    // Check if field should be skipped
    if field_attrs.skip {
        return Ok(quote! {});
    }

    let name = match field_access {
        FieldAccess::Named(ident) => ident,
        FieldAccess::Unnamed => &Ident::new("inner", Span::call_site()),
    };

    let field_name = match field_access {
        FieldAccess::Named(ident) => quote!(#ident),
        FieldAccess::Unnamed => quote!(0),
    };

    match classify_field_type(field_type) {
        FieldTypeClass::SingleId(inner_type) => Ok(quote! {
            pub fn #name(self) -> crate::inspector::NodeInspector<'t, crate::id::Id<#inner_type>, S> {
                let node = self.node.get(self.tree);
                crate::inspector::NodeInspector::new(node.#field_name, self.tree, self.interner)
            }
        }),
        FieldTypeClass::VecId(inner_type) => {
            let has_name_count = quote::format_ident!("has_{}_count", name);
            let name_at = quote::format_ident!("{}_at", name);

            Ok(quote! {
                pub fn #has_name_count(self, expected: usize) -> Self {
                    let node = self.node.get(self.tree);
                    let actual = node.#field_name.len();
                    assert_eq!(
                        actual, expected,
                        "Expected {} {} but found {}",
                        expected, stringify!(#field_name), actual
                    );
                    self
                }

                pub fn #name_at(self, index: usize) -> crate::inspector::NodeInspector<'t, crate::id::Id<#inner_type>, S> {
                    let node = self.node.get(self.tree);
                    assert!(
                        index < node.#field_name.len(),
                        "{} index {} out of bounds (max {})",
                        stringify!(#field_name), index,
                        node.#field_name.len().saturating_sub(1)
                    );
                    let item_id = node.#field_name[index];
                    crate::inspector::NodeInspector::new(item_id, self.tree, self.interner)
                }
            })
        }
        FieldTypeClass::OptionId(inner_type) => {
            let try_name = quote::format_ident!("try_{}", name);
            let has_some_name = quote::format_ident!("has_some_{}", name);
            let has_none_name = quote::format_ident!("has_none_{}", name);

            Ok(quote! {
                pub fn #name(self) -> crate::inspector::NodeInspector<'t, crate::id::Id<#inner_type>, S> {
                    let node = self.node.get(self.tree);
                    let id = node.#field_name.expect(&format!("Expected {} to be Some", stringify!(#field_name)));
                    crate::inspector::NodeInspector::new(id, self.tree, self.interner)
                }

                pub fn #try_name(self) -> Option<crate::inspector::NodeInspector<'t, crate::id::Id<#inner_type>, S>> {
                    let node = self.node.get(self.tree);
                    node.#field_name.map(|id| crate::inspector::NodeInspector::new(id, self.tree, self.interner))
                }

                pub fn #has_some_name(self) -> Self {
                    let node = self.node.get(self.tree);
                    assert!(
                        node.#field_name.is_some(),
                        "Expected {} to be Some",
                        stringify!(#field_name)
                    );
                    self
                }

                pub fn #has_none_name(self) -> Self {
                    let node = self.node.get(self.tree);
                    assert!(
                        node.#field_name.is_none(),
                        "Expected {} to be None",
                        stringify!(#field_name)
                    );
                    self
                }
            })
        }
        FieldTypeClass::Other => {
            if field_attrs.getter {
                // Generate simple getter if explicitly requested
                Ok(quote! {
                    pub fn #name(self) -> &#field_type {
                        let node = self.node.get(self.tree);
                        &node.#field_name
                    }
                })
            } else {
                // Skip by default
                Ok(quote! {})
            }
        }
    }
}

fn generate_enum_methods(
    enum_name: &Ident,
    data_enum: &DataEnum,
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let methods = data_enum
        .variants
        .iter()
        .filter_map(|variant| generate_enum_variant_method(enum_name, variant))
        .collect();

    Ok(methods)
}

fn generate_enum_variant_method(
    enum_name: &Ident,
    variant: &syn::Variant,
) -> Option<proc_macro2::TokenStream> {
    let Fields::Unnamed(fields) = &variant.fields else {
        return None;
    };

    let field = fields.unnamed.first()?;

    let variant_name = &variant.ident;
    let snake_case_name = to_snake_case(&variant_name.to_string());

    match classify_field_type(&field.ty) {
        FieldTypeClass::SingleId(inner_type) => {
            let as_name = quote::format_ident!("as_{}", snake_case_name);
            let to_name = quote::format_ident!("to_{}", snake_case_name);
            let is_name = quote::format_ident!("is_{}", snake_case_name);
            let is_not_name = quote::format_ident!("is_not_{}", snake_case_name);

            Some(quote! {
                #[inline]
                pub fn #as_name(self) -> Option<crate::inspector::NodeInspector<'t, crate::id::Id<#inner_type>, S>> {
                    let node = self.node.get(self.tree);
                    match node {
                        #enum_name::#variant_name(id) => Some(crate::inspector::NodeInspector::new(*id, self.tree, self.interner)),
                        _ => None,
                    }
                }

                #[inline]
                pub fn #to_name(self) -> crate::inspector::NodeInspector<'t, crate::id::Id<#inner_type>, S> {
                    self.#as_name()
                        .expect(&format!("Expected enum to be {}", stringify!(#variant_name)))
                }

                #[inline]
                pub fn #is_name(self) -> Self {
                    if self.#as_name().is_none() {
                        panic!("Expected enum to be {}", stringify!(#variant_name));
                    }
                    self
                }

                #[inline]
                pub fn #is_not_name(self) -> Self {
                    if self.#as_name().is_some() {
                        panic!("Expected enum to not be {}", stringify!(#variant_name));
                    }
                    self
                }
            })
        }

        FieldTypeClass::OptionId(inner_type) => {
            let as_some_name = quote::format_ident!("as_some_{}", snake_case_name);
            let to_some_name = quote::format_ident!("to_some_{}", snake_case_name);
            let is_some_name = quote::format_ident!("is_some_{}", snake_case_name);
            let is_none_name = quote::format_ident!("is_none_{}", snake_case_name);
            let is_not_name = quote::format_ident!("is_not_{}", snake_case_name);

            Some(quote! {
                #[inline]
                pub fn #as_some_name(self) -> Option<crate::inspector::NodeInspector<'t, crate::id::Id<#inner_type>, S>> {
                    let node = self.node.get(self.tree);
                    match node {
                        #enum_name::#variant_name(Some(id)) => Some(crate::inspector::NodeInspector::new(*id, self.tree, self.interner)),
                        _ => None,
                    }
                }

                #[inline]
                pub fn #to_some_name(self) -> crate::inspector::NodeInspector<'t, crate::id::Id<#inner_type>, S> {
                    self.#as_some_name()
                        .expect(&format!("Expected enum to be {} and some value", stringify!(#variant_name)))
                }

                #[inline]
                pub fn #is_some_name(self) -> Self {
                    if self.#as_some_name().is_none() {
                        panic!("Expected enum to be {} and some value", stringify!(#variant_name));
                    }
                    self
                }

                #[inline]
                pub fn #is_none_name(self) -> Self {
                    if self.#as_some_name().is_some() {
                        panic!("Expected enum to be {} and none value", stringify!(#variant_name));
                    }
                    self
                }

                #[inline]
                pub fn #is_not_name(self) -> Self {
                    todo!()
                }
            })
        }
        _ => None,
    }
}

#[derive(Debug, Default)]
struct FieldAttributes {
    skip: bool,
    getter: bool,
}

fn parse_field_attributes(attrs: &[Attribute]) -> syn::Result<FieldAttributes> {
    let mut field_attrs = FieldAttributes::default();

    for attr in attrs {
        if !attr.path().is_ident("inspector") {
            continue;
        }

        match &attr.meta {
            Meta::List(MetaList { tokens, .. }) => {
                parse_inspector_tokens(&mut field_attrs, tokens, attr)?;
            }
            Meta::Path(_) => {
                // Handle #[inspector] without arguments - default behavior
            }
            _ => {
                return Err(syn::Error::new_spanned(
                    attr,
                    "Invalid inspector attribute format",
                ));
            }
        }
    }

    Ok(field_attrs)
}

fn parse_inspector_tokens(
    field_attrs: &mut FieldAttributes,
    tokens: &proc_macro2::TokenStream,
    attr: &Attribute,
) -> syn::Result<()> {
    let tokens_str = tokens.to_string();

    for token in tokens_str.split(',') {
        let token = token.trim();
        match token {
            "skip" => field_attrs.skip = true,
            "getter" => field_attrs.getter = true,
            _ => {
                return Err(syn::Error::new_spanned(
                    attr,
                    format!("Unknown inspector attribute: {}", token),
                ));
            }
        }
    }

    Ok(())
}

fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch.is_uppercase() && !result.is_empty() {
            if let Some(&next_ch) = chars.peek() {
                if next_ch.is_lowercase() {
                    result.push('_');
                }
            }
        }
        result.push(ch.to_lowercase().next().unwrap());
    }

    result
}

fn to_singular(s: &str) -> String {
    // Simple pluralization rules - could be enhanced
    match s {
        s if s.ends_with("ies") => s[..s.len() - 3].to_string() + "y",
        s if s.ends_with("es") => s[..s.len() - 2].to_string(),
        s if s.ends_with("s") => s[..s.len() - 1].to_string(),
        s => s.to_string(),
    }
}
