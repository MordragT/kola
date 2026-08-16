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

fn peel_smart_pointers(ty: &Type) -> Type {
    let Type::Path(type_path) = ty else {
        return ty.clone();
    };
    let Some(segment) = type_path.path.segments.last() else {
        return ty.clone();
    };
    if segment.ident == "Arc" || segment.ident == "Rc" || segment.ident == "Ptr" {
        let syn::PathArguments::AngleBracketed(args) = &segment.arguments else {
            return ty.clone();
        };
        let Some(syn::GenericArgument::Type(inner_ty)) = args.args.first() else {
            return ty.clone();
        };
        return peel_smart_pointers(inner_ty);
    }
    ty.clone()
}

enum Container {
    Vec(Type),
    SideVec(Type, Type),
    SideMap(Type, Type),
}

fn classify_container(ty: &Type) -> Option<Container> {
    let Type::Path(type_path) = ty else {
        return None;
    };
    let segment = type_path.path.segments.last()?;
    let syn::PathArguments::AngleBracketed(args) = &segment.arguments else {
        return None;
    };

    if segment.ident == "Vec" {
        let syn::GenericArgument::Type(v) = args.args.first()? else {
            return None;
        };
        Some(Container::Vec(v.clone()))
    } else if segment.ident == "SideVec" {
        let syn::GenericArgument::Type(k) = args.args.first()? else {
            return None;
        };
        let syn::GenericArgument::Type(v) = args.args.get(1)? else {
            return None;
        };
        Some(Container::SideVec(k.clone(), v.clone()))
    } else if segment.ident == "SideMap" {
        let syn::GenericArgument::Type(k) = args.args.first()? else {
            return None;
        };
        let syn::GenericArgument::Type(v) = args.args.get(1)? else {
            return None;
        };
        Some(Container::SideMap(k.clone(), v.clone()))
    } else {
        None
    }
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

        impl<P: kola_relations::QueryPlan> #ext_trait_name<P> for kola_relations::KeyedQuery<P> {
            #(#impl_methods)*
        }
    };
    expanded.into()
}

#[proc_macro_derive(RelationStorage, attributes(relation))]
pub fn derive_relation_storage(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;
    let syn::Data::Struct(data_struct) = &input.data else {
        return syn::Error::new_spanned(
            &input.ident,
            "RelationStorage can only be derived for structs",
        )
        .to_compile_error()
        .into();
    };

    let mut impls = Vec::new();
    for field in &data_struct.fields {
        let Some(field_name) = &field.ident else {
            continue;
        };

        let mut has_relation = false;
        for attr in &field.attrs {
            if attr.path().is_ident("relation") {
                has_relation = true;
                break;
            }
        }
        if !has_relation {
            continue;
        }

        let field_type = &field.ty;
        let peeled_type = peel_smart_pointers(field_type);
        let Some(container) = classify_container(&peeled_type) else {
            continue;
        };

        match container {
            Container::Vec(v) => {
                let impl_block = quote! {
                    impl kola_relations::RelationIds<#v> for #struct_name {
                        type Ids<'a> = <#field_type as kola_relations::RelationIds<#v>>::Ids<'a> where Self: 'a;

                        #[inline]
                        fn ids(&self) -> Self::Ids<'_> {
                            kola_relations::RelationIds::<#v>::ids(&self.#field_name)
                        }
                    }

                    impl kola_relations::PartialRelation<#v, #v> for #struct_name {
                        #[inline]
                        fn get_opt(&self, id: kola_relations::Id<#v>) -> Option<&#v> {
                            kola_relations::PartialRelation::<#v, #v>::get_opt(&self.#field_name, id)
                        }
                    }

                    impl kola_relations::TotalRelation<#v, #v> for #struct_name {
                        #[inline]
                        fn get(&self, id: kola_relations::Id<#v>) -> &#v {
                            kola_relations::TotalRelation::<#v, #v>::get(&self.#field_name, id)
                        }

                        type Rows<'a> = <#field_type as kola_relations::TotalRelation<#v, #v>>::Rows<'a> where Self: 'a;

                        #[inline]
                        fn rows(&self) -> Self::Rows<'_> {
                            kola_relations::TotalRelation::<#v, #v>::rows(&self.#field_name)
                        }
                    }

                    impl kola_relations::TotalRelationMut<#v, #v> for #struct_name {
                        #[inline]
                        fn get_mut(&mut self, id: kola_relations::Id<#v>) -> &mut #v {
                            kola_relations::TotalRelationMut::<#v, #v>::get_mut(&mut self.#field_name, id)
                        }

                        type RowsMut<'a> = <#field_type as kola_relations::TotalRelationMut<#v, #v>>::RowsMut<'a> where Self: 'a;

                        #[inline]
                        fn rows_mut(&mut self) -> Self::RowsMut<'_> {
                            kola_relations::TotalRelationMut::<#v, #v>::rows_mut(&mut self.#field_name)
                        }
                    }

                    impl kola_relations::Allocate<#v> for #struct_name {
                        type Key = <#field_type as kola_relations::Allocate<#v>>::Key;

                        #[inline]
                        fn alloc(&mut self, value: #v) -> kola_relations::Id<Self::Key> {
                            kola_relations::Allocate::<#v>::alloc(&mut self.#field_name, value)
                        }
                    }
                };
                impls.push(impl_block);
            }
            Container::SideVec(k, v) => {
                let impl_block = quote! {
                    impl kola_relations::RelationIds<#k> for #struct_name {
                        type Ids<'a> = <#field_type as kola_relations::RelationIds<#k>>::Ids<'a> where Self: 'a;

                        #[inline]
                        fn ids(&self) -> Self::Ids<'_> {
                            kola_relations::RelationIds::<#k>::ids(&self.#field_name)
                        }
                    }

                    impl kola_relations::PartialRelation<#k, #v> for #struct_name {
                        #[inline]
                        fn get_opt(&self, id: kola_relations::Id<#k>) -> Option<&#v> {
                            kola_relations::PartialRelation::<#k, #v>::get_opt(&self.#field_name, id)
                        }
                    }

                    impl kola_relations::TotalRelation<#k, #v> for #struct_name {
                        #[inline]
                        fn get(&self, id: kola_relations::Id<#k>) -> &#v {
                            kola_relations::TotalRelation::<#k, #v>::get(&self.#field_name, id)
                        }

                        type Rows<'a> = <#field_type as kola_relations::TotalRelation<#k, #v>>::Rows<'a> where Self: 'a;

                        #[inline]
                        fn rows(&self) -> Self::Rows<'_> {
                            kola_relations::TotalRelation::<#k, #v>::rows(&self.#field_name)
                        }
                    }

                    impl kola_relations::TotalRelationMut<#k, #v> for #struct_name {
                        #[inline]
                        fn get_mut(&mut self, id: kola_relations::Id<#k>) -> &mut #v {
                            kola_relations::TotalRelationMut::<#k, #v>::get_mut(&mut self.#field_name, id)
                        }

                        type RowsMut<'a> = <#field_type as kola_relations::TotalRelationMut<#k, #v>>::RowsMut<'a> where Self: 'a;

                        #[inline]
                        fn rows_mut(&mut self) -> Self::RowsMut<'_> {
                            kola_relations::TotalRelationMut::<#k, #v>::rows_mut(&mut self.#field_name)
                        }
                    }
                };
                impls.push(impl_block);
            }
            Container::SideMap(k, v) => {
                let impl_block = quote! {
                    impl kola_relations::RelationIds<#k> for #struct_name {
                        type Ids<'a> = <#field_type as kola_relations::RelationIds<#k>>::Ids<'a> where Self: 'a;

                        #[inline]
                        fn ids(&self) -> Self::Ids<'_> {
                            kola_relations::RelationIds::<#k>::ids(&self.#field_name)
                        }
                    }

                    impl kola_relations::PartialRelation<#k, #v> for #struct_name {
                        #[inline]
                        fn get_opt(&self, id: kola_relations::Id<#k>) -> Option<&#v> {
                            kola_relations::PartialRelation::<#k, #v>::get_opt(&self.#field_name, id)
                        }
                    }
                };
                impls.push(impl_block);
            }
        }
    }

    let expanded = quote! {
        #(#impls)*
    };
    expanded.into()
}
