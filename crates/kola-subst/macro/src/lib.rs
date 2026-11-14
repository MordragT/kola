use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, quote};
use syn::{Data, DeriveInput, Index, parse_macro_input, parse_quote};

#[proc_macro_derive(Substitutable, attributes(substitutable))]
pub fn derive_substitutable(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    derive(&input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

enum Accessor {
    Named(syn::Ident),
    Indexed(Index),
}

impl ToTokens for Accessor {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Self::Named(ident) => ident.to_tokens(tokens),
            Self::Indexed(index) => index.to_tokens(tokens),
        }
    }
}

fn field_has_skip(attrs: &[syn::Attribute]) -> syn::Result<bool> {
    let mut skip = false;

    for attr in attrs.iter().filter(|a| a.path().is_ident("substitutable")) {
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("skip") {
                skip = true;
            }
            Ok(())
        })?;
    }

    Ok(skip)
}

fn derive(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let name = &input.ident;

    let data = match &input.data {
        Data::Struct(data) => data,
        Data::Enum(_) | Data::Union(_) => {
            return Err(syn::Error::new_spanned(
                input,
                "Substitutable can only be derived for structs",
            ));
        }
    };

    let mut fields = Vec::new();

    for (index, field) in data.fields.iter().enumerate() {
        if field_has_skip(&field.attrs)? {
            continue;
        }

        let accessor = match &field.ident {
            Some(ident) => Accessor::Named(ident.clone()),
            None => Accessor::Indexed(Index::from(index)),
        };

        fields.push((accessor, field.ty.clone()));
    }

    let field_types = fields.iter().map(|(_, ty)| ty).collect::<Vec<_>>();

    let mut generics = input.generics.clone();
    generics.params.push(parse_quote!(__S));
    generics
        .make_where_clause()
        .predicates
        .push(parse_quote!(Self: Clone));

    for ty in &field_types {
        generics
            .make_where_clause()
            .predicates
            .push(parse_quote!(#ty: kola_subst::Substitutable<__S>));
    }

    let (impl_generics, _, where_clause) = generics.split_for_impl();
    let (_, ty_generics, _) = input.generics.split_for_impl();

    let accessors = fields
        .iter()
        .map(|(accessor, _)| accessor)
        .collect::<Vec<_>>();
    let try_accessors = accessors.iter();
    let mut_accessors = accessors.iter();

    let substitutable = quote! {
        impl #impl_generics kola_subst::Substitutable<__S> for #name #ty_generics #where_clause {
            fn try_apply(&self, s: &mut __S) -> Option<Self> {
                let mut result = None;

                #(
                    if let Some(value) = self.#try_accessors.try_apply(s) {
                        result.get_or_insert_with(|| self.clone()).#try_accessors = value;
                    }
                )*

                result
            }

            fn apply_mut(&mut self, s: &mut __S) {
                #( self.#mut_accessors.apply_mut(s); )*
            }
        }
    };

    Ok(substitutable)
}
