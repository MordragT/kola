use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Attribute, Data, DataEnum, DataStruct, DeriveInput, Fields, FieldsNamed, Ident, Meta, MetaList,
    Type,
};

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
        impl<'t, S: std::hash::BuildHasher> crate::inspector::NodeInspector<'t, crate::tree::Id<#name>, S> {
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
        Fields::Unnamed(_) => Err(syn::Error::new_spanned(
            struct_name,
            "Inspector only supports named fields",
        )),
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

        let method = generate_field_method(field_name, field_type, &field_attrs)?;
        if !method.is_empty() {
            methods.push(method);
        }
    }

    Ok(methods)
}

fn generate_field_method(
    field_name: &Ident,
    field_type: &Type,
    field_attrs: &FieldAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    // Check if field should be skipped
    if field_attrs.skip {
        return Ok(quote! {});
    }
    match classify_field_type(field_type) {
        FieldTypeClass::SingleId(inner_type) => Ok(quote! {
            pub fn #field_name(self) -> crate::inspector::NodeInspector<'t, crate::tree::Id<#inner_type>, S> {
                let node = self.node.get(self.tree);
                crate::inspector::NodeInspector::new(node.#field_name, self.tree, self.interner)
            }
        }),
        FieldTypeClass::VecId(inner_type) => {
            let has_count_method = quote::format_ident!("has_{}_count", field_name);
            let at_method = quote::format_ident!("{}_at", field_name);

            Ok(quote! {
                pub fn #has_count_method(self, expected: usize) -> Self {
                    let node = self.node.get(self.tree);
                    let actual = node.#field_name.len();
                    assert_eq!(
                        actual, expected,
                        "Expected {} {} but found {}",
                        expected, stringify!(#field_name), actual
                    );
                    self
                }

                pub fn #at_method(self, index: usize) -> crate::inspector::NodeInspector<'t, crate::tree::Id<#inner_type>, S> {
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
        FieldTypeClass::OptionId(inner_type) => Ok(quote! {
            pub fn #field_name(self) -> Option<crate::inspector::NodeInspector<'t, crate::tree::Id<#inner_type>, S>> {
                let node = self.node.get(self.tree);
                node.#field_name.map(|id| crate::inspector::NodeInspector::new(id, self.tree, self.interner))
            }
        }),
        FieldTypeClass::Other => {
            if field_attrs.getter {
                // Generate simple getter if explicitly requested
                Ok(quote! {
                    pub fn #field_name(self) -> &#field_type {
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

    let FieldTypeClass::SingleId(inner_type) = classify_field_type(&field.ty) else {
        return None;
    };

    let variant_name = &variant.ident;
    let method_name = quote::format_ident!("as_{}", to_snake_case(&variant_name.to_string()));

    Some(quote! {
        pub fn #method_name(self) -> Option<crate::inspector::NodeInspector<'t, crate::tree::Id<#inner_type>, S>> {
            let node = self.node.get(self.tree);
            match node {
                #enum_name::#variant_name(id) => Some(crate::inspector::NodeInspector::new(*id, self.tree, self.interner)),
                _ => None,
            }
        }
    })
}

#[derive(Debug, Clone)]
enum FieldTypeClass {
    SingleId(proc_macro2::TokenStream),
    VecId(proc_macro2::TokenStream),
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
        "Vec" => extract_id_inner_type(inner_ty)
            .map(FieldTypeClass::VecId)
            .unwrap_or(FieldTypeClass::Other),
        "Option" => extract_id_inner_type(inner_ty)
            .map(FieldTypeClass::OptionId)
            .unwrap_or(FieldTypeClass::Other),
        "Id" => FieldTypeClass::SingleId(quote!(#inner_ty)),
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
