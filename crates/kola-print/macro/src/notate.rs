use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    Attribute, Data, DataEnum, DataStruct, DeriveInput, Fields, FieldsNamed, Ident, Lit, Meta,
    MetaList, Token, Type,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

pub fn generate_notate_impl(input: &DeriveInput) -> syn::Result<TokenStream> {
    let name = &input.ident;
    let type_attrs = parse_type_attributes(&input.attrs)?;

    let printer = type_attrs.printer.clone().ok_or_else(|| {
        syn::Error::new_spanned(
            name,
            "Notate requires a context type via #[notate(with = ...)]",
        )
    })?;

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
        impl<'a> kola_print::Notate<'a, #name> for #printer {
            fn notate(self, value: &#name, arena: &'a kola_print::bumpalo::Bump) -> kola_print::Notation<'a> {
                use kola_print::prelude::*;
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
) -> syn::Result<TokenStream2> {
    let head_color = &type_attrs.color;
    let display_name = type_attrs.name.clone().unwrap_or(struct_name.to_string());

    match &data_struct.fields {
        Fields::Named(fields_named) => {
            let field_destructure = generate_field_destructure(fields_named)?;
            let field_bindings = generate_field_bindings(fields_named)?;
            let single_repr = generate_single_representation(fields_named)?;
            let multi_repr = generate_multi_representation(fields_named)?;

            Ok(quote! {
                let #struct_name { #field_destructure } = value;

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
            let field_attrs = parse_field_attributes(&field.attrs)?;
            let field_binding = generate_single_field_binding(&field_attrs)?;

            Ok(quote! {
                let head = #display_name.#head_color().display_in(arena);

                let field_value = &value.0;
                let inner_notation = #field_binding;

                let single = arena.just(' ').then(inner_notation.clone().flatten(arena), arena);
                let multi = arena.newline().then(inner_notation, arena).indent(arena);

                head.then(single.or(multi, arena), arena)
            })
        }
        Fields::Unit => Ok(quote! {
            #display_name.#head_color().display_in(arena)
        }),
    }
}

fn generate_field_destructure(fields: &FieldsNamed) -> syn::Result<TokenStream2> {
    let field_names: Vec<_> = fields
        .named
        .iter()
        .filter_map(|f| f.ident.as_ref())
        .collect();

    Ok(quote! { #(#field_names),* })
}

fn generate_field_bindings(fields: &FieldsNamed) -> syn::Result<Vec<TokenStream2>> {
    let mut bindings = Vec::new();

    for field in &fields.named {
        let field_name = field.ident.as_ref().unwrap();
        let field_attrs = parse_field_attributes(&field.attrs)?;

        if field_attrs.skip {
            continue;
        }

        let binding = generate_single_field_binding(&field_attrs)?;
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

fn generate_single_field_binding(field_attrs: &FieldAttributes) -> syn::Result<TokenStream2> {
    if let Some(custom) = &field_attrs.custom {
        return Ok(quote! {
            #custom(field_value, arena)
        });
    }

    if field_attrs.display {
        return Ok(quote! {
            format!("{}", field_value).display_in(arena)
        });
    }

    Ok(quote! {
        self.notate(field_value, arena)
    })
}

fn generate_single_representation(fields: &FieldsNamed) -> syn::Result<TokenStream2> {
    let mut parts = Vec::new();

    for field in &fields.named {
        let field_name = field.ident.as_ref().unwrap();
        let field_attrs = parse_field_attributes(&field.attrs)?;

        if field_attrs.skip {
            continue;
        }

        let field_name_str = field_name.to_string();
        let binding_name = quote::format_ident!("{}_notation", field_name);

        parts.push(quote! {
            [
                arena.just(' '),
                format_args!("{} = ", #field_name_str).display_in(arena),
                #binding_name.clone(),
            ].concat_in(arena)
        });
    }

    Ok(quote! { #(#parts),* })
}

fn generate_multi_representation(fields: &FieldsNamed) -> syn::Result<TokenStream2> {
    let mut parts = Vec::new();

    for field in &fields.named {
        let field_name = field.ident.as_ref().unwrap();
        let field_attrs = parse_field_attributes(&field.attrs)?;

        if field_attrs.skip {
            continue;
        }

        let field_name_str = field_name.to_string();
        let binding_name = quote::format_ident!("{}_notation", field_name);

        parts.push(quote! {
            [
                arena.newline(),
                format_args!("{} = ", #field_name_str).display_in(arena),
                #binding_name,
            ].concat_in(arena)
        });
    }

    Ok(quote! { #(#parts),* })
}

fn generate_enum_notate(
    enum_name: &Ident,
    data_enum: &DataEnum,
    type_attrs: &TypeAttributes,
) -> syn::Result<TokenStream2> {
    let head_color = &type_attrs.color;
    let display_name = type_attrs.name.clone().unwrap_or(enum_name.to_string());

    let match_arms: Vec<_> = data_enum
        .variants
        .iter()
        .map(|variant| generate_enum_variant_match(enum_name, variant, head_color, &display_name))
        .collect::<syn::Result<_>>()?;

    Ok(quote! {
        match value {
            #(#match_arms)*
        }
    })
}

fn generate_enum_variant_match(
    enum_name: &Ident,
    variant: &syn::Variant,
    head_color: &TokenStream2,
    enum_display_name: &str,
) -> syn::Result<TokenStream2> {
    let variant_name = &variant.ident;
    let variant_display = format!("{}::{}", enum_display_name, variant_name);

    match &variant.fields {
        Fields::Unit => Ok(quote! {
            #enum_name::#variant_name => #variant_display.#head_color().display_in(arena),
        }),
        Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
            let field = &fields.unnamed[0];
            let field_attrs = parse_field_attributes(&field.attrs)?;
            let field_binding = generate_single_field_binding(&field_attrs)?;

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

#[derive(Default)]
struct TypeAttributes {
    printer: Option<Type>,
    color: TokenStream2,
    name: Option<String>,
}

#[derive(Debug, Default)]
struct FieldAttributes {
    skip: bool,
    display: bool,
    custom: Option<TokenStream2>,
}

enum TypeAttr {
    With(Type),
    Color(Ident),
    Name(String),
}

impl Parse for TypeAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;
        let name = ident.to_string();

        match name.as_str() {
            "with" => {
                input.parse::<Token![=]>()?;
                let ty: Type = input.parse()?;
                Ok(TypeAttr::With(ty))
            }
            "color" => {
                input.parse::<Token![=]>()?;
                let lit: Lit = input.parse()?;
                let color = lit_to_string(&lit)?;
                let ident = Ident::new(&color, proc_macro2::Span::call_site());
                Ok(TypeAttr::Color(ident))
            }
            "name" => {
                input.parse::<Token![=]>()?;
                let lit: Lit = input.parse()?;
                Ok(TypeAttr::Name(lit_to_string(&lit)?))
            }
            _ => Err(syn::Error::new(
                ident.span(),
                format!("Unknown notate type attribute: {name}"),
            )),
        }
    }
}

enum FieldAttr {
    Skip,
    Display,
    Custom(TokenStream2),
}

impl Parse for FieldAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;
        let name = ident.to_string();

        match name.as_str() {
            "skip" => Ok(FieldAttr::Skip),
            "display" => Ok(FieldAttr::Display),
            "custom" => {
                input.parse::<Token![=]>()?;
                let lit: Lit = input.parse()?;
                let custom = lit_to_string(&lit)?;
                let ident = Ident::new(&custom, proc_macro2::Span::call_site());
                Ok(FieldAttr::Custom(quote!(#ident)))
            }
            _ => Err(syn::Error::new(
                ident.span(),
                format!("Unknown notate field attribute: {name}"),
            )),
        }
    }
}

fn parse_type_attributes(attrs: &[Attribute]) -> syn::Result<TypeAttributes> {
    let mut type_attrs = TypeAttributes {
        printer: None,
        color: quote!(bright_blue), // default color
        name: None,
    };

    for attr in attrs {
        if !attr.path().is_ident("notate") {
            continue;
        }

        match &attr.meta {
            Meta::List(MetaList { tokens: _, .. }) => {
                let parsed =
                    attr.parse_args_with(Punctuated::<TypeAttr, Token![,]>::parse_terminated)?;
                for item in parsed {
                    match item {
                        TypeAttr::With(ty) => type_attrs.printer = Some(ty),
                        TypeAttr::Color(color) => type_attrs.color = quote!(#color),
                        TypeAttr::Name(name) => type_attrs.name = Some(name),
                    }
                }
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
            Meta::List(MetaList { tokens: _, .. }) => {
                let parsed =
                    attr.parse_args_with(Punctuated::<FieldAttr, Token![,]>::parse_terminated)?;
                for item in parsed {
                    match item {
                        FieldAttr::Skip => field_attrs.skip = true,
                        FieldAttr::Display => field_attrs.display = true,
                        FieldAttr::Custom(custom) => field_attrs.custom = Some(custom),
                    }
                }
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

fn lit_to_string(lit: &Lit) -> syn::Result<String> {
    match lit {
        Lit::Str(s) => Ok(s.value()),
        _ => Err(syn::Error::new_spanned(lit, "Expected a string literal")),
    }
}
