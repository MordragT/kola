use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Attribute, Data, DataEnum, DataStruct, DeriveInput, Fields, FieldsNamed, Ident, Meta, MetaList,
    Type,
};

use super::{FieldTypeClass, classify_field_type};

pub fn generate_notate_impl(input: &DeriveInput) -> syn::Result<TokenStream> {
    let name = &input.ident;
    let type_attrs = parse_type_attributes(&input.attrs)?;

    let implementation = match &input.data {
        Data::Struct(data_struct) => generate_struct_notate(name, data_struct, &type_attrs)?,
        Data::Enum(data_enum) => generate_enum_notate(name, data_enum, &type_attrs)?,
        Data::Union(_) => {
            return Err(syn::Error::new_spanned(
                name,
                "Notate cannot be derived for unions",
            ));
        }
    };

    Ok(quote! {
        impl<'a> kola_print::Notate<'a> for crate::print::NodePrinter<'a, #name> {
            fn notate(&self, arena: &'a kola_print::bumpalo::Bump) -> kola_print::Notation<'a> {
                #implementation
            }
        }
    }
    .into())
}

fn generate_struct_notate(
    struct_name: &Ident,
    data_struct: &DataStruct,
    type_attrs: &TypeAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    let head_color = &type_attrs.color;
    let display_name = type_attrs.name.clone().unwrap_or(struct_name.to_string());

    match &data_struct.fields {
        Fields::Named(fields_named) => {
            let field_destructure = generate_field_destructure(fields_named)?;
            let field_bindings = generate_field_bindings(fields_named)?;
            let single_repr = generate_single_representation(fields_named)?;
            let multi_repr = generate_multi_representation(fields_named)?;

            Ok(quote! {
                let #struct_name { #field_destructure } = self.value;

                let head = #display_name.#head_color().display_in(arena);

                #(#field_bindings)*

                let single = [#single_repr].concat_in(arena).flatten(arena);
                let multi = [#multi_repr].concat_in(arena).indent(arena);

                head.then(single.or(multi, arena), arena)
            })
        }
        Fields::Unnamed(fields_unnamed) => {
            if fields_unnamed.unnamed.len() != 1 {
                return Err(syn::Error::new_spanned(
                    fields_unnamed,
                    "Notate only supports tuple structs with exactly one field",
                ));
            }

            let field = &fields_unnamed.unnamed[0];
            let field_class = classify_field_type(&field.ty);
            let field_attrs = parse_field_attributes(&field.attrs)?;
            let field_binding = generate_single_field_binding(&field.ty, &field_attrs)?;

            let single_repr = match field_class {
                FieldTypeClass::SingleId(_) | FieldTypeClass::Other => {
                    quote! { arena.just(' ').then(#field_binding.clone().flatten(arena), arena) }
                }
                FieldTypeClass::VecId(_) => {
                    quote! { arena.just(' ').then(#field_binding.clone().concat_by(arena.just(' '), arena).flatten(arena), arena) }
                }
                FieldTypeClass::OptionId(_) => {
                    quote! { #field_binding.as_ref().map(|notation| arena.just(' ').then(notation.clone(), arena)).or_not(arena) }
                }
            };

            let multi_repr = match field_class {
                FieldTypeClass::SingleId(_) | FieldTypeClass::Other => {
                    quote! { arena.newline().then(#field_binding, arena).indent(arena) }
                }
                FieldTypeClass::VecId(_) => {
                    quote! { arena.newline().then(#field_binding.concat_by(arena.newline(), arena), arena).indent(arena) }
                }
                FieldTypeClass::OptionId(_) => {
                    quote! { #field_binding.map(|notation| arena.newline().then(notation, arena)).or_not(arena).indent(arena) }
                }
            };

            Ok(quote! {
                let head = #display_name.#head_color().display_in(arena);

                let field_value = &self.value.0;
                let inner_notation = #field_binding;

                let single = #single_repr;
                let multi = #multi_repr;

                head.then(single.or(multi, arena), arena)

            })
        }
        Fields::Unit => Ok(quote! {
            #display_name.#head_color().display_in(arena)
        }),
    }
}

fn generate_field_destructure(fields: &FieldsNamed) -> syn::Result<proc_macro2::TokenStream> {
    let field_names: Vec<_> = fields
        .named
        .iter()
        .filter_map(|f| f.ident.as_ref())
        .collect();

    Ok(quote! { #(#field_names),* })
}

fn generate_field_bindings(fields: &FieldsNamed) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let mut bindings = Vec::new();

    for field in &fields.named {
        let field_name = field.ident.as_ref().unwrap();
        let field_attrs = parse_field_attributes(&field.attrs)?;

        if field_attrs.skip {
            continue;
        }

        let binding = generate_single_field_binding(&field.ty, &field_attrs)?;
        let binding_name = quote::format_ident!("{}_notation", field_name);

        bindings.push(quote! {
            let #binding_name = {
                let field_value = #field_name;
                #binding
            };
        });
    }

    Ok(bindings)
}

fn generate_single_field_binding(
    field_type: &Type,
    field_attrs: &FieldAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    if let Some(custom_handler) = &field_attrs.custom {
        return Ok(quote! {
            #custom_handler(field_value, arena)
        });
    }

    match classify_field_type(field_type) {
        FieldTypeClass::SingleId(_) => Ok(quote! {
            self.to_id(*field_value).notate(arena)
        }),
        FieldTypeClass::VecId(_) => Ok(quote! {
            self.to_slice(field_value).gather(arena)
        }),
        FieldTypeClass::OptionId(_) => Ok(quote! {
            field_value.map(|id| self.to_id(id).notate(arena))
        }),
        FieldTypeClass::Other => {
            if field_attrs.display {
                Ok(quote! {
                    format!("{}", field_value).display_in(arena)
                })
            } else {
                Ok(quote! {
                    format!("{:?}", field_value).display_in(arena)
                })
            }
        }
    }
}

fn generate_single_representation(fields: &FieldsNamed) -> syn::Result<proc_macro2::TokenStream> {
    let mut parts = Vec::new();

    for field in &fields.named {
        let field_name = field.ident.as_ref().unwrap();
        let field_attrs = parse_field_attributes(&field.attrs)?;

        if field_attrs.skip {
            continue;
        }

        let field_name_str = field_name.to_string();
        let binding_name = quote::format_ident!("{}_notation", field_name);

        match classify_field_type(&field.ty) {
            FieldTypeClass::SingleId(_) | FieldTypeClass::Other => {
                parts.push(quote! { arena.just(' ') });
                parts.push(quote! { format_args!("{} = ", #field_name_str).display_in(arena) });
                parts.push(quote! { #binding_name.clone() });
            }
            FieldTypeClass::VecId(_) => parts.push(quote! {
                [
                    arena.just(' '),
                    format_args!("{} = ", #field_name_str).display_in(arena),
                    #binding_name.clone().concat_by(arena.just(' '), arena),
                ].concat_in(arena).flatten(arena)
            }),
            FieldTypeClass::OptionId(_) => parts.push(quote! {
                #binding_name.as_ref().map(|notation| {
                    [
                        arena.just(' '),
                        format_args!("{} = ", #field_name_str).display_in(arena),
                        notation.clone(),
                    ].concat_in(arena)
                }).or_not(arena)
            }),
        }
    }

    Ok(quote! { #(#parts),* })
}

fn generate_multi_representation(fields: &FieldsNamed) -> syn::Result<proc_macro2::TokenStream> {
    let mut parts = Vec::new();

    for field in &fields.named {
        let field_name = field.ident.as_ref().unwrap();
        let field_attrs = parse_field_attributes(&field.attrs)?;

        if field_attrs.skip {
            continue;
        }

        let field_name_str = field_name.to_string();
        let binding_name = quote::format_ident!("{}_notation", field_name);

        match classify_field_type(&field.ty) {
            FieldTypeClass::SingleId(_) | FieldTypeClass::Other => {
                parts.push(quote! { arena.newline() });
                parts.push(quote! { format_args!("{} = ", #field_name_str).display_in(arena) });
                parts.push(quote! { #binding_name });
            }
            FieldTypeClass::VecId(_) => parts.push(quote! {
                [
                    arena.newline(),
                    format_args!("{} = ", #field_name_str).display_in(arena),
                    #binding_name.concat_by(arena.newline(), arena),
                ].concat_in(arena).indent(arena)
            }),
            FieldTypeClass::OptionId(_) => parts.push(quote! {
                #binding_name.map(|notation| {
                     [
                         arena.newline(),
                         format_args!("{} = ", #field_name_str).display_in(arena),
                         notation,
                     ].concat_in(arena)
                }).or_not(arena)
            }),
        };
    }

    Ok(quote! { #(#parts),* })
}

fn generate_enum_notate(
    enum_name: &Ident,
    data_enum: &DataEnum,
    type_attrs: &TypeAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    let head_color = &type_attrs.color;
    let display_name = type_attrs.name.clone().unwrap_or(enum_name.to_string());

    let match_arms: Vec<_> = data_enum
        .variants
        .iter()
        .map(|variant| generate_enum_variant_match(enum_name, variant, head_color, &display_name))
        .collect::<syn::Result<_>>()?;

    Ok(quote! {
        match self.value {
            #(#match_arms)*
        }
    })
}

fn generate_enum_variant_match(
    enum_name: &Ident,
    variant: &syn::Variant,
    head_color: &proc_macro2::TokenStream,
    enum_display_name: &str,
) -> syn::Result<proc_macro2::TokenStream> {
    let variant_name = &variant.ident;
    let variant_display = format!("{}::{}", enum_display_name, variant_name);

    match &variant.fields {
        Fields::Unit => Ok(quote! {
            #enum_name::#variant_name => #variant_display.#head_color().display_in(arena),
        }),
        Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
            let field = &fields.unnamed[0];
            let field_attrs = parse_field_attributes(&field.attrs)?;
            let field_binding = generate_single_field_binding(&field.ty, &field_attrs)?;

            Ok(quote! {
                #enum_name::#variant_name(field_value) => {
                    let head = #variant_display.#head_color().display_in(arena);
                    let inner_notation = #field_binding;
                    head.then(arena.notate("(").then(inner_notation, arena).then(arena.notate(")"), arena), arena)
                },
            })
        }
        _ => Err(syn::Error::new_spanned(
            variant,
            "Notate derive only supports unit variants and single-field tuple variants",
        )),
    }
}

#[derive(Debug, Default)]
struct TypeAttributes {
    color: proc_macro2::TokenStream,
    name: Option<String>,
}

#[derive(Debug, Default)]
struct FieldAttributes {
    skip: bool,
    display: bool,
    custom: Option<proc_macro2::TokenStream>,
}

fn parse_type_attributes(attrs: &[Attribute]) -> syn::Result<TypeAttributes> {
    let mut type_attrs = TypeAttributes {
        color: quote!(bright_blue), // default color
        name: None,
    };

    for attr in attrs {
        if !attr.path().is_ident("notate") {
            continue;
        }

        match &attr.meta {
            Meta::List(MetaList { tokens, .. }) => {
                parse_notate_type_tokens(&mut type_attrs, tokens, attr)?;
            }
            Meta::Path(_) => {
                // Handle #[notate] without arguments - use defaults
            }
            _ => {
                return Err(syn::Error::new_spanned(
                    attr,
                    "Invalid notate attribute format",
                ));
            }
        }
    }

    Ok(type_attrs)
}

fn parse_field_attributes(attrs: &[Attribute]) -> syn::Result<FieldAttributes> {
    let mut field_attrs = FieldAttributes::default();

    for attr in attrs {
        if !attr.path().is_ident("notate") {
            continue;
        }

        match &attr.meta {
            Meta::List(MetaList { tokens, .. }) => {
                parse_notate_field_tokens(&mut field_attrs, tokens, attr)?;
            }
            Meta::Path(_) => {
                // Handle #[notate] without arguments - default behavior
            }
            _ => {
                return Err(syn::Error::new_spanned(
                    attr,
                    "Invalid notate attribute format",
                ));
            }
        }
    }

    Ok(field_attrs)
}

fn parse_notate_type_tokens(
    type_attrs: &mut TypeAttributes,
    tokens: &proc_macro2::TokenStream,
    attr: &Attribute,
) -> syn::Result<()> {
    let tokens_str = tokens.to_string();

    for token in tokens_str.split(',') {
        let token = token.trim();
        if let Some(color) = token.strip_prefix("color = ") {
            let color = color.trim_matches('"');
            let color_ident = syn::Ident::new(color, proc_macro2::Span::call_site());
            type_attrs.color = quote!(#color_ident);
        } else if let Some(name) = token.strip_prefix("name = ") {
            let name = name.trim_matches('"');
            type_attrs.name = Some(name.to_string());
        } else {
            return Err(syn::Error::new_spanned(
                attr,
                format!("Unknown notate type attribute: {}", token),
            ));
        }
    }

    Ok(())
}

fn parse_notate_field_tokens(
    field_attrs: &mut FieldAttributes,
    tokens: &proc_macro2::TokenStream,
    attr: &Attribute,
) -> syn::Result<()> {
    let tokens_str = tokens.to_string();

    for token in tokens_str.split(',') {
        let token = token.trim();
        match token {
            "skip" => field_attrs.skip = true,
            "display" => field_attrs.display = true,
            _ if token.starts_with("custom = ") => {
                let custom = token.strip_prefix("custom = ").unwrap().trim_matches('"');
                let custom_ident = syn::Ident::new(custom, proc_macro2::Span::call_site());
                field_attrs.custom = Some(quote!(#custom_ident));
            }
            _ => {
                return Err(syn::Error::new_spanned(
                    attr,
                    format!("Unknown notate field attribute: {}", token),
                ));
            }
        }
    }

    Ok(())
}
