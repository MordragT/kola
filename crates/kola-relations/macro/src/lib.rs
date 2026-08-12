use proc_macro::TokenStream;
use quote::quote;
use syn::{DeriveInput, Type, parse_macro_input};

fn extract_id_inner_type(ty: &Type) -> Option<Type> {
    let Type::Path(type_path) = ty else {
        return None;
    };
    let segment = type_path.path.segments.last()?;
    if segment.ident != "Id" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(args) = &segment.arguments else {
        return None;
    };
    let syn::GenericArgument::Type(inner_ty) = args.args.first()? else {
        return None;
    };
    Some(inner_ty.clone())
}

#[proc_macro_derive(TryProject)]
pub fn derive_try_project(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let enum_name = &input.ident;
    let syn::Data::Enum(data_enum) = &input.data else {
        return syn::Error::new_spanned(&input.ident, "TryProject can only be derived for enums")
            .to_compile_error()
            .into();
    };

    let mut impls = Vec::new();
    for variant in &data_enum.variants {
        let variant_name = &variant.ident;
        let syn::Fields::Unnamed(fields) = &variant.fields else {
            continue;
        };
        if fields.unnamed.len() != 1 {
            continue;
        }
        let field = &fields.unnamed[0];
        if let Some(target_type) = extract_id_inner_type(&field.ty) {
            let impl_block = quote! {
                impl kola_relations::TryProject<#target_type> for #enum_name {
                    #[inline]
                    fn try_project(&self) -> Option<kola_relations::Id<#target_type>> {
                        match self {
                            #enum_name::#variant_name(id) => Some(*id),
                            _ => None,
                        }
                    }
                }
            };
            impls.push(impl_block);
        }
    }

    let expanded = quote! {
        #(#impls)*
    };
    expanded.into()
}

#[proc_macro_derive(RelationProjections)]
pub fn derive_relation_projections(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;
    let syn::Data::Struct(data_struct) = &input.data else {
        return syn::Error::new_spanned(
            &input.ident,
            "RelationProjections can only be derived for structs",
        )
        .to_compile_error()
        .into();
    };

    let ext_trait_name = quote::format_ident!("{}QueryExt", struct_name);
    let mut methods = Vec::new();
    let mut impl_methods = Vec::new();

    for field in &data_struct.fields {
        let Some(field_name) = &field.ident else {
            continue;
        };
        if let Some(target_type) = extract_id_inner_type(&field.ty) {
            let method_name = quote::format_ident!("project_{}", field_name);

            let method_sig = quote! {
                fn #method_name(self) -> kola_relations::KeyedQuery<kola_relations::query::Project<P, #target_type, fn(&&#struct_name) -> kola_relations::Id<#target_type>>>;
            };
            methods.push(method_sig);

            let method_impl = quote! {
                fn #method_name(self) -> kola_relations::KeyedQuery<kola_relations::query::Project<P, #target_type, fn(&&#struct_name) -> kola_relations::Id<#target_type>>> {
                    self.project(|node: &&#struct_name| node.#field_name)
                }
            };
            impl_methods.push(method_impl);
        }
    }

    if methods.is_empty() {
        return quote! {}.into();
    }

    let expanded = quote! {
        pub trait #ext_trait_name<P> {
            #(#methods)*
        }

        impl<P> #ext_trait_name<P> for kola_relations::KeyedQuery<P> {
            #(#impl_methods)*
        }
    };
    expanded.into()
}
