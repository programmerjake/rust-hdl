// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use proc_macro2::{Ident, Literal, Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    Attribute, Data, DataEnum, DataStruct, DeriveInput, Error, Field, Fields, GenericParam,
    Generics, Lifetime, LifetimeDef, Path, Token, Variant, VisRestricted, Visibility, WhereClause,
    WherePredicate,
};

mod val;

mod kw {
    syn::custom_keyword!(ignored);
    syn::custom_keyword!(real_type_name);
}

enum RustHdlAttributeArg {
    Crate {
        crate_kw: Token![crate],
        eq: Token![=],
        path: Path,
    },
    RealTypeName {
        real_type_name_kw: kw::real_type_name,
        eq: Token![=],
        ident: Ident,
    },
}

impl Parse for RustHdlAttributeArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![crate]) {
            Ok(Self::Crate {
                crate_kw: input.parse()?,
                eq: input.parse()?,
                path: input.parse()?,
            })
        } else if input.peek(kw::real_type_name) {
            Ok(Self::RealTypeName {
                real_type_name_kw: input.parse()?,
                eq: input.parse()?,
                ident: input.parse()?,
            })
        } else {
            Err(input.error("expected `crate` or `real_type_name`"))
        }
    }
}

struct RustHdlAttributes {
    crate_path: Path,
    real_type_name: Option<Ident>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AttributesFor {
    Derive,
    Val,
}

impl RustHdlAttributes {
    fn parse(attrs: &[Attribute], attributes_for: AttributesFor) -> syn::Result<Self> {
        let mut crate_path = None;
        let mut real_type_name = None;
        for attribute in attrs {
            if attribute.path.is_ident("rust_hdl") {
                let args = attribute.parse_args_with(
                    Punctuated::<RustHdlAttributeArg, Token![,]>::parse_separated_nonempty,
                )?;
                for arg in args {
                    match arg {
                        RustHdlAttributeArg::Crate { crate_kw, eq, path } => {
                            let _ = eq;
                            if crate_path.replace(path).is_some() {
                                return Err(Error::new_spanned(
                                    crate_kw,
                                    "`crate` specified twice",
                                ));
                            }
                        }
                        RustHdlAttributeArg::RealTypeName {
                            real_type_name_kw,
                            eq,
                            ident,
                        } => {
                            if attributes_for == AttributesFor::Val {
                                return Err(Error::new_spanned(
                                    real_type_name_kw,
                                    "`real_type_name` is not allowed here",
                                ));
                            }
                            let _ = eq;
                            if real_type_name.replace(ident).is_some() {
                                return Err(Error::new_spanned(
                                    real_type_name_kw,
                                    "`real_type_name` specified twice",
                                ));
                            }
                        }
                    }
                }
            } else if attributes_for == AttributesFor::Val {
                return Err(Error::new_spanned(
                    attribute,
                    "attributes other than #[rust_hdl(...)] are not allowed here",
                ));
            }
        }
        let crate_path = crate_path.unwrap_or_else(|| {
            let mut path = Path::from(Ident::new("rust_hdl", Span::call_site()));
            path.leading_colon = Some(<Token![::]>::default());
            path
        });
        Ok(Self {
            crate_path,
            real_type_name,
        })
    }
}

enum RustHdlFieldAttributeArg {
    Ignored { ignored: kw::ignored },
}

impl Parse for RustHdlFieldAttributeArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(kw::ignored) {
            Ok(Self::Ignored {
                ignored: input.parse()?,
            })
        } else {
            Err(input.error("expected `ignored`"))
        }
    }
}

struct RustHdlFieldAttributes {
    ignored: bool,
}

impl RustHdlFieldAttributes {
    fn parse(attrs: &[Attribute]) -> syn::Result<Self> {
        let mut ignored = false;
        for attribute in attrs {
            if attribute.path.is_ident("rust_hdl") {
                let args = attribute.parse_args_with(
                    Punctuated::<RustHdlFieldAttributeArg, Token![,]>::parse_separated_nonempty,
                )?;
                for arg in args {
                    match arg {
                        RustHdlFieldAttributeArg::Ignored { ignored: _ignored } => ignored = true,
                    }
                }
            }
        }
        Ok(Self { ignored })
    }
}

enum RustHdlVariantAttributeArg {}

impl Parse for RustHdlVariantAttributeArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Err(input.error("can't use #[rust_hdl(...)] on enum variants"))
    }
}

struct RustHdlVariantAttributes {}

impl RustHdlVariantAttributes {
    fn parse(attrs: &[Attribute]) -> syn::Result<Self> {
        for attribute in attrs {
            if attribute.path.is_ident("rust_hdl") {
                let args = attribute.parse_args_with(
                    Punctuated::<RustHdlVariantAttributeArg, Token![,]>::parse_separated_nonempty,
                )?;
                for arg in args {
                    match arg {}
                }
            }
        }
        Ok(Self {})
    }
}

struct GenericsWithAddedLifetimes {
    generics: Generics,
    ctx_lifetime: Lifetime,
}

impl GenericsWithAddedLifetimes {
    fn get_or_add_lifetime(
        mut generics: Generics,
        ident: &str,
        additional_bounds: impl IntoIterator<Item = Lifetime>,
    ) -> (Generics, Lifetime) {
        let mut additional_bounds = Some(additional_bounds);
        let lifetime = generics.lifetimes_mut().find_map(|l| {
            if l.lifetime.ident == ident {
                l.bounds.extend(additional_bounds.take().unwrap());
                Some(l.lifetime.clone())
            } else {
                None
            }
        });
        let lifetime = match lifetime {
            Some(v) => v,
            None => {
                let ctx_lifetime = Lifetime::new(&format!("'{}", ident), Span::call_site());
                let mut lifetime_def = LifetimeDef::new(ctx_lifetime.clone());
                lifetime_def.bounds.extend(additional_bounds.unwrap());
                generics.params.push(GenericParam::Lifetime(lifetime_def));
                ctx_lifetime
            }
        };
        (generics, lifetime)
    }
    fn new(generics: Generics) -> Self {
        let (generics, ctx_lifetime) = Self::get_or_add_lifetime(generics, "ctx", []);
        Self {
            generics,
            ctx_lifetime,
        }
    }
}

fn where_clause_with_bound_on_generics(
    generics: &Generics,
    get_bound: impl Fn(&Ident) -> WherePredicate,
) -> WhereClause {
    let mut where_clause = generics
        .where_clause
        .clone()
        .unwrap_or_else(|| WhereClause {
            where_token: Default::default(),
            predicates: Default::default(),
        });
    for type_param in generics.type_params() {
        where_clause.predicates.push(get_bound(&type_param.ident));
    }
    where_clause
}

struct ValueImplStruct {
    type_defs: TokenStream,
    struct_of_field_enums_const: TokenStream,
    visit_fields: Vec<TokenStream>,
    visit_field_types: Vec<TokenStream>,
    visit_field_fixed_types: Vec<TokenStream>,
    get_field_values: TokenStream,
    field_count: usize,
    where_clause_with_value: WhereClause,
    where_clause_with_fixed_type_value: WhereClause,
}

impl ValueImplStruct {
    fn new(data: DataStruct, common: &ValueImplCommon) -> syn::Result<Self> {
        let ValueImplCommon {
            crate_path,
            ctx_lifetime,
            generics_with_added_lifetimes,
            name: struct_name,
            original_generics,
            top_vis,
        } = common;
        let (_, ty_generics, _) = original_generics.split_for_impl();
        let where_clause_with_value = where_clause_with_bound_on_generics(
            original_generics,
            |ident| parse_quote! { #ident: #crate_path::values::Value<#ctx_lifetime> },
        );
        let where_clause_with_fixed_type_value = where_clause_with_bound_on_generics(
            original_generics,
            |ident| parse_quote! { #ident: #crate_path::values::FixedTypeValue<#ctx_lifetime> },
        );
        let (impl_generics, ty_generics_with_added_lifetimes, _) =
            generics_with_added_lifetimes.split_for_impl();
        let type_defs;
        let struct_of_field_enums_const;
        let mut visit_fields = Vec::new();
        let mut visit_field_types = Vec::new();
        let mut visit_field_fixed_types = Vec::new();
        let get_field_values;
        let field_count;
        match data.fields {
            Fields::Named(fields) => {
                let mut enum_fields = Vec::new();
                let mut struct_of_field_enums_fields = Vec::new();
                let mut struct_of_field_enums_const_fields = Vec::new();
                let mut struct_of_field_values_fields = Vec::new();
                let mut get_field_values_fields = Vec::new();
                let mut struct_of_field_values_clone_fields = Vec::new();
                for field in &fields.named {
                    let RustHdlFieldAttributes { ignored } =
                        RustHdlFieldAttributes::parse(&field.attrs)?;
                    if ignored {
                        continue;
                    }
                    let name = field.ident.as_ref().unwrap();
                    let ty = &field.ty;
                    let vis = &field.vis;
                    let name_str = name.to_string();
                    enum_fields.push(quote! {#name,});
                    struct_of_field_enums_fields.push(quote! {
                        #vis #name: __FieldEnum,
                    });
                    struct_of_field_enums_const_fields.push(quote! {
                        #name: __FieldEnum::#name,
                    });
                    get_field_values_fields.push(quote! {
                        #name: value.extract_field_unchecked::<#ty>(__FieldEnum::#name),
                    });
                    struct_of_field_values_fields.push(quote! {
                        #vis #name: #crate_path::values::Val<#ctx_lifetime, #ty>,
                    });
                    struct_of_field_values_clone_fields.push(quote! {
                        #name: ::core::clone::Clone::clone(&self.#name),
                    });
                    visit_fields.push(quote! {
                        let __visitor = #crate_path::values::aggregate::StructFieldVisitor::field(
                            __visitor,
                            #name_str,
                            __FieldEnum::#name,
                            &self.#name,
                        )?;
                    });
                    visit_field_types.push(quote! {
                        let __visitor = #crate_path::values::aggregate::StructFieldTypeVisitor::field_with_type_hint(
                            __visitor,
                            #name_str,
                            __FieldEnum::#name,
                            |v: &Self, _| &v.#name,
                        )?;
                    });
                    visit_field_fixed_types.push(quote! {
                        let __visitor = #crate_path::values::aggregate::StructFieldFixedTypeVisitor::field_with_type_hint(
                            __visitor,
                            #name_str,
                            <Self as #crate_path::values::aggregate::StructValue<#ctx_lifetime>>::FieldEnum::#name,
                            |v: &Self, _| &v.#name,
                        )?;
                    });
                }
                struct_of_field_enums_const = quote! {
                    __StructOfFieldEnums {
                        #(#struct_of_field_enums_const_fields)*
                    }
                };
                get_field_values = quote! {
                    __AggregateOfFieldValues {
                        #(#get_field_values_fields)*
                        __struct_phantom: ::core::marker::PhantomData,
                    }
                };
                let field_enum_repr = if enum_fields.is_empty() {
                    quote! {}
                } else {
                    quote! {#[repr(usize)]}
                };
                field_count = enum_fields.len();
                type_defs = quote! {
                    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
                    #[allow(non_camel_case_types)]
                    #field_enum_repr
                    #top_vis enum __FieldEnum {
                        #(#enum_fields)*
                    }

                    impl ::core::convert::From<__FieldEnum> for usize {
                        fn from(v: __FieldEnum) -> Self {
                            v as usize
                        }
                    }

                    #[derive(Clone, Copy)]
                    #[allow(non_snake_case)]
                    #top_vis struct __StructOfFieldEnums {
                        #(#struct_of_field_enums_fields)*
                    }

                    #[allow(non_snake_case)]
                    #top_vis struct __AggregateOfFieldValues #generics_with_added_lifetimes #where_clause_with_value {
                        #(#struct_of_field_values_fields)*
                        __struct_phantom: ::core::marker::PhantomData<(&#ctx_lifetime (), #struct_name #ty_generics)>,
                    }

                    #[automatically_derived]
                    impl #impl_generics ::core::marker::Copy for __AggregateOfFieldValues #ty_generics_with_added_lifetimes #where_clause_with_value {}
                    #[automatically_derived]
                    impl #impl_generics ::core::clone::Clone for __AggregateOfFieldValues #ty_generics_with_added_lifetimes #where_clause_with_value {
                        fn clone(&self) -> Self {
                            *self
                        }
                    }
                };
            }
            Fields::Unnamed(fields) => {
                let mut struct_of_field_enums_fields = Vec::new();
                let mut struct_of_field_enums_const_fields = Vec::new();
                let mut struct_of_field_values_fields = Vec::new();
                let mut get_field_values_fields = Vec::new();
                let mut struct_of_field_values_clone_fields = Vec::new();
                for (name, field) in fields.unnamed.iter().enumerate() {
                    let RustHdlFieldAttributes { ignored } =
                        RustHdlFieldAttributes::parse(&field.attrs)?;
                    let name_str = name.to_string();
                    let name = Literal::usize_unsuffixed(name);
                    let vis = &field.vis;
                    if ignored {
                        struct_of_field_enums_fields.push(quote! {
                            #vis (),
                        });
                        struct_of_field_enums_const_fields.push(quote! {
                            (),
                        });
                        get_field_values_fields.push(quote! {
                            (),
                        });
                        struct_of_field_values_fields.push(quote! {
                            #vis (),
                        });
                        struct_of_field_values_clone_fields.push(quote! {
                            (),
                        });
                        continue;
                    }
                    let ty = &field.ty;
                    struct_of_field_enums_fields.push(quote! {
                        #vis __FieldEnum,
                    });
                    struct_of_field_enums_const_fields.push(quote! {
                        #name,
                    });
                    get_field_values_fields.push(quote! {
                        value.extract_field_unchecked::<#ty>(#name),
                    });
                    struct_of_field_values_fields.push(quote! {
                        #vis #crate_path::values::Val<#ctx_lifetime, #ty>,
                    });
                    struct_of_field_values_clone_fields.push(quote! {
                        ::core::clone::Clone::clone(&self.#name),
                    });
                    visit_fields.push(quote! {
                        let __visitor = #crate_path::values::aggregate::StructFieldVisitor::field(
                            __visitor,
                            #name_str,
                            #name,
                            &self.#name,
                        )?;
                    });
                    visit_field_types.push(quote! {
                        let __visitor = #crate_path::values::aggregate::StructFieldTypeVisitor::field_with_type_hint(
                            __visitor,
                            #name_str,
                            #name,
                            |v: &Self, _| &v.#name,
                        )?;
                    });
                    visit_field_fixed_types.push(quote! {
                        let __visitor = #crate_path::values::aggregate::StructFieldFixedTypeVisitor::field_with_type_hint(
                            __visitor,
                            #name_str,
                            #name,
                            |v: &Self, _| &v.#name,
                        )?;
                    });
                }
                type_defs = quote! {
                    #[allow(non_camel_case_types)]
                    #top_vis type __FieldEnum = usize;

                    #[derive(Clone, Copy)]
                    #[allow(non_snake_case)]
                    #top_vis struct __StructOfFieldEnums(
                        #(#struct_of_field_enums_fields)*
                    );

                    #[allow(non_snake_case)]
                    #top_vis struct __AggregateOfFieldValues #generics_with_added_lifetimes(
                        #(#struct_of_field_values_fields)*
                        ::core::marker::PhantomData<(&#ctx_lifetime (), #struct_name #ty_generics)>,
                    ) #where_clause_with_value;

                    #[automatically_derived]
                    impl #impl_generics ::core::marker::Copy for __AggregateOfFieldValues #ty_generics_with_added_lifetimes #where_clause_with_value {}
                    #[automatically_derived]
                    impl #impl_generics ::core::clone::Clone for __AggregateOfFieldValues #ty_generics_with_added_lifetimes #where_clause_with_value {
                        fn clone(&self) -> Self {
                            *self
                        }
                    }
                };
                struct_of_field_enums_const = quote! {
                    __StructOfFieldEnums(
                        #(#struct_of_field_enums_const_fields)*
                    )
                };
                get_field_values = quote! {
                    __AggregateOfFieldValues(
                        #(#get_field_values_fields)*
                        ::core::marker::PhantomData,
                    )
                };
                field_count = visit_fields.len();
            }
            Fields::Unit => {
                type_defs = quote! {
                    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
                    #[allow(non_camel_case_types)]
                    #top_vis enum __FieldEnum {}

                    impl ::core::convert::From<__FieldEnum> for usize {
                        fn from(v: __FieldEnum) -> Self {
                            match v {}
                        }
                    }

                    #[derive(Clone, Copy)]
                    #[allow(non_camel_case_types)]
                    #top_vis struct __StructOfFieldEnums;

                    #[allow(non_camel_case_types)]
                    #top_vis struct __AggregateOfFieldValues #generics_with_added_lifetimes #where_clause_with_value {
                        __struct_phantom: ::core::marker::PhantomData<(&#ctx_lifetime (), #struct_name #ty_generics)>,
                    }

                    #[automatically_derived]
                    impl #impl_generics ::core::marker::Copy for __AggregateOfFieldValues #ty_generics_with_added_lifetimes #where_clause_with_value {}
                    #[automatically_derived]
                    impl #impl_generics ::core::clone::Clone for __AggregateOfFieldValues #ty_generics_with_added_lifetimes #where_clause_with_value {
                        fn clone(&self) -> Self {
                            *self
                        }
                    }
                };
                struct_of_field_enums_const = quote! { __StructOfFieldEnums };
                field_count = 0;
                get_field_values = quote! { __AggregateOfFieldValues { __struct_phantom: ::core::marker::PhantomData } };
            }
        }
        Ok(Self {
            type_defs,
            struct_of_field_enums_const,
            visit_fields,
            visit_field_types,
            visit_field_fixed_types,
            get_field_values,
            field_count,
            where_clause_with_value,
            where_clause_with_fixed_type_value,
        })
    }
    fn derive_value(self, common: ValueImplCommon) -> syn::Result<TokenStream> {
        let Self {
            type_defs,
            struct_of_field_enums_const,
            visit_fields,
            visit_field_types,
            get_field_values,
            field_count,
            where_clause_with_value,
            ..
        } = self;
        let ValueImplCommon {
            crate_path,
            original_generics,
            generics_with_added_lifetimes,
            ctx_lifetime,
            name,
            top_vis: _,
        } = common;
        let (_, ty_generics, _) = original_generics.split_for_impl();
        let (impl_generics, ty_generics_with_added_lifetimes, _) =
            generics_with_added_lifetimes.split_for_impl();
        Ok(quote! {
            const _: () = {
                #type_defs
                #[automatically_derived]
                impl #impl_generics #crate_path::values::aggregate::AggregateOfFieldValues<#ctx_lifetime> for __AggregateOfFieldValues #ty_generics_with_added_lifetimes #where_clause_with_value {
                    type Aggregate = #name #ty_generics;
                }
                #[automatically_derived]
                impl #impl_generics #crate_path::values::aggregate::AggregateValue<#ctx_lifetime> for #name #ty_generics #where_clause_with_value {
                    type AggregateValueKind = #crate_path::values::aggregate::StructAggregateValueKind<#ctx_lifetime, Self>;
                    type DiscriminantShape = #crate_path::values::integer::UIntShape<0>;
                    type AggregateOfFieldValues = __AggregateOfFieldValues #ty_generics_with_added_lifetimes;
                }
                #[automatically_derived]
                impl #impl_generics #crate_path::values::aggregate::StructValue<#ctx_lifetime> for #name #ty_generics #where_clause_with_value {
                    type FieldEnum = __FieldEnum;
                    type StructOfFieldEnums = __StructOfFieldEnums;
                    const STRUCT_OF_FIELD_ENUMS: Self::StructOfFieldEnums = #struct_of_field_enums_const;
                    const FIELD_COUNT: usize = #field_count;
                    fn visit_fields<__V: #crate_path::values::aggregate::StructFieldVisitor<#ctx_lifetime, Self>>(
                        &self,
                        __visitor: __V,
                    ) -> ::core::result::Result<__V, __V::BreakType> {
                        #(#visit_fields)*
                        ::core::result::Result::Ok(__visitor)
                    }
                    fn visit_field_types<__V: #crate_path::values::aggregate::StructFieldTypeVisitor<#ctx_lifetime, Self>>(
                        __visitor: __V,
                    ) -> ::core::result::Result<__V, __V::BreakType> {
                        #(#visit_field_types)*
                        ::core::result::Result::Ok(__visitor)
                    }
                    fn get_field_values(value: #crate_path::values::Val<#ctx_lifetime, Self>) -> Self::AggregateOfFieldValues {
                        #get_field_values
                    }
                }
            };
        })
    }
    fn derive_fixed_type_value(self, common: ValueImplCommon) -> syn::Result<TokenStream> {
        let Self {
            visit_field_fixed_types,
            where_clause_with_fixed_type_value,
            ..
        } = self;
        let ValueImplCommon {
            crate_path,
            original_generics,
            generics_with_added_lifetimes,
            ctx_lifetime,
            name,
            top_vis: _,
        } = common;
        let (_, ty_generics, _) = original_generics.split_for_impl();
        let impl_generics = generics_with_added_lifetimes.split_for_impl().0;
        Ok(quote! {
            #[automatically_derived]
            impl #impl_generics #crate_path::values::aggregate::FixedTypeStructValue<#ctx_lifetime> for #name #ty_generics #where_clause_with_fixed_type_value {
                fn visit_field_fixed_types<__V: #crate_path::values::aggregate::StructFieldFixedTypeVisitor<#ctx_lifetime, Self>>(
                    __visitor: __V,
                ) -> ::core::result::Result<__V, __V::BreakType> {
                    #(#visit_field_fixed_types)*
                    ::core::result::Result::Ok(__visitor)
                }
            }
        })
    }
}

struct ValueImplEnum {
    type_defs: TokenStream,
    visit_variant_match_arms: Vec<TokenStream>,
    visit_variant_types: Vec<TokenStream>,
    aggregate_of_field_values_needs_type_arguments: bool,
    adjusted_where_clause: WhereClause,
    match_value: TokenStream,
}

impl ValueImplEnum {
    fn new(data: DataEnum, common: &ValueImplCommon) -> syn::Result<Self> {
        let ValueImplCommon {
            name,
            crate_path,
            original_generics,
            generics_with_added_lifetimes,
            ctx_lifetime,
            top_vis,
        } = common;
        let variant_vis = match top_vis {
            Visibility::Public(_) | Visibility::Crate(_) => top_vis.clone(),
            Visibility::Restricted(VisRestricted {
                pub_token: _,
                paren_token: _,
                in_token: None,
                path,
            }) if path.is_ident("crate") => top_vis.clone(),
            Visibility::Inherited => Visibility::Restricted(VisRestricted {
                pub_token: Default::default(),
                paren_token: Default::default(),
                in_token: None,
                path: Path::from(Ident::new("super", Span::call_site())).into(),
            }),
            Visibility::Restricted(_) => {
                return Err(Error::new_spanned(
                    top_vis,
                    "unsupported visibility on enum with #[derive(Value)], use `pub(crate)` instead",
                ));
            }
        };
        fn assert_field_visibility_is_inherited(vis: Visibility) -> syn::Result<()> {
            if let Visibility::Inherited = vis {
                Ok(())
            } else {
                Err(Error::new_spanned(
                    vis,
                    "#[derive(Value)] requires enum fields to have default (inherited) visibility",
                ))
            }
        }
        let any_discriminants = data.variants.iter().any(|v| v.discriminant.is_some());
        let mut discriminants = Vec::with_capacity(data.variants.len());
        let mut visit_variant_match_arms = Vec::with_capacity(data.variants.len());
        let mut visit_variant_types = Vec::with_capacity(data.variants.len());
        let mut aggregate_of_field_values_variants = Vec::with_capacity(data.variants.len());
        let mut aggregate_of_field_values_clone_variants = Vec::with_capacity(data.variants.len());
        let mut aggregate_of_field_values_needs_type_arguments = false;
        let mut variant_structs = Vec::with_capacity(data.variants.len());
        let (impl_generics, ty_generics_with_added_lifetimes, _) =
            generics_with_added_lifetimes.split_for_impl();
        let (_, ty_generics, _) = original_generics.split_for_impl();
        let mut variant_struct_generics = generics_with_added_lifetimes.clone();
        let variant_struct_lifetime = Lifetime::new("'__a", Span::call_site());
        variant_struct_generics
            .params
            .push(GenericParam::Lifetime(LifetimeDef::new(
                variant_struct_lifetime.clone(),
            )));
        variant_struct_generics
            .lifetimes_mut()
            .find(|v| v.lifetime == *ctx_lifetime)
            .unwrap()
            .bounds
            .push(variant_struct_lifetime.clone());
        let (variant_struct_impl_generics, variant_struct_ty_generics, _) =
            variant_struct_generics.split_for_impl();
        let mut variant_struct_generics_with_implicit_lifetime = variant_struct_generics.clone();
        variant_struct_generics_with_implicit_lifetime
            .lifetimes_mut()
            .find(|v| v.lifetime == variant_struct_lifetime)
            .unwrap()
            .lifetime = parse_quote! { '_ };
        let variant_struct_ty_generics_with_implicit_lifetime =
            variant_struct_generics_with_implicit_lifetime
                .split_for_impl()
                .1;
        let adjusted_where_clause = where_clause_with_bound_on_generics(
            original_generics,
            |ident| parse_quote! { #ident: #crate_path::values::FixedTypeValue<#ctx_lifetime> },
        );
        let mut match_value_variants = Vec::new();
        for (
            index,
            Variant {
                attrs: variant_attrs,
                ident: variant_name,
                fields,
                discriminant: _,
            },
        ) in data.variants.into_iter().enumerate()
        {
            let discriminant = if any_discriminants {
                quote! { #name::#variant_name as i128 }
            } else {
                let discriminant = index as i128;
                quote! { #discriminant }
            };
            let variant_name_str = variant_name.to_string();
            let RustHdlVariantAttributes {} = RustHdlVariantAttributes::parse(&variant_attrs)?;
            let match_value_variant;
            match fields {
                Fields::Named(named) => {
                    let mut aggregate_of_field_values_fields =
                        Vec::with_capacity(named.named.len());
                    let mut aggregate_of_field_values_clone_field_patterns =
                        Vec::with_capacity(named.named.len());
                    let mut aggregate_of_field_values_clone_fields =
                        Vec::with_capacity(named.named.len());
                    let mut match_value_variant_fields = Vec::with_capacity(named.named.len());
                    let mut match_fields = Vec::with_capacity(named.named.len());
                    let mut field_names = Vec::with_capacity(named.named.len());
                    let mut variant_struct_fields = Vec::with_capacity(named.named.len());
                    let mut visit_fields = Vec::with_capacity(named.named.len());
                    let mut visit_field_types = Vec::with_capacity(named.named.len());
                    for Field {
                        attrs,
                        vis,
                        ident,
                        ty,
                        ..
                    } in named.named
                    {
                        let RustHdlFieldAttributes { ignored } =
                            RustHdlFieldAttributes::parse(&attrs)?;
                        if ignored {
                            match_fields.push(quote! { #ident: _, });
                            continue;
                        }
                        assert_field_visibility_is_inherited(vis)?;
                        let name_str = ident.as_ref().unwrap().to_string();
                        let field_index = visit_fields.len();
                        visit_fields.push(quote! {
                            let __visitor = #crate_path::values::aggregate::EnumVariantFieldVisitor::field(
                                __visitor,
                                #name_str,
                                #field_index,
                                self.#ident,
                            )?;
                        });
                        visit_field_types.push(quote! {
                            let __visitor = #crate_path::values::aggregate::EnumVariantFieldTypeVisitor::field::<#ty>(
                                __visitor,
                                #name_str,
                                #field_index,
                            )?;
                        });
                        match_fields.push(quote! { #ident, });
                        aggregate_of_field_values_needs_type_arguments = true;
                        aggregate_of_field_values_fields.push(quote! {
                            #ident: #crate_path::values::Val<#ctx_lifetime, #ty>,
                        });
                        aggregate_of_field_values_clone_field_patterns.push(quote! {
                            #ident,
                        });
                        aggregate_of_field_values_clone_fields.push(quote! {
                            #ident: ::core::clone::Clone::clone(#ident),
                        });
                        variant_struct_fields.push(quote! {
                            #variant_vis #ident: &#variant_struct_lifetime #ty,
                        });
                        match_value_variant_fields.push(quote! {
                            #ident: #crate_path::values::ops::get_enum_variant_field_unchecked(value, #discriminant, #field_index, match_arm_scope),
                        });
                        field_names.push(ident);
                    }
                    aggregate_of_field_values_variants.push(quote! {
                        #variant_name {
                            #(#aggregate_of_field_values_fields)*
                        },
                    });
                    aggregate_of_field_values_clone_variants.push(quote! {
                        Self::#variant_name {
                            #(#aggregate_of_field_values_clone_field_patterns)*
                        } => Self::#variant_name {
                            #(#aggregate_of_field_values_clone_fields)*
                        },
                    });
                    visit_variant_match_arms.push(quote! {
                        Self::#variant_name { #(#match_fields)* } => {
                            #crate_path::values::aggregate::EnumVariantVisitor::variant(
                                __visitor,
                                #variant_name_str,
                                #crate_path::values::Int::wrapping_new(#discriminant),
                                variant_structs::#variant_name::#variant_struct_ty_generics_with_implicit_lifetime {
                                    #(#field_names,)*
                                    __enum_phantom: ::core::marker::PhantomData,
                                },
                            )
                        }
                    });
                    visit_variant_types.push(quote! {
                        let __visitor = #crate_path::values::aggregate::EnumVariantTypeVisitor::variant::<variant_structs::#variant_name #variant_struct_ty_generics_with_implicit_lifetime>(
                            __visitor,
                            #variant_name_str,
                            #crate_path::values::Int::wrapping_new(#discriminant),
                        )?;
                    });
                    variant_structs.push(quote! {
                        #variant_vis struct #variant_name #variant_struct_generics #adjusted_where_clause {
                            #(#variant_struct_fields)*
                            #variant_vis __enum_phantom: ::core::marker::PhantomData<(
                                &#ctx_lifetime (),
                                &#variant_struct_lifetime super::#name #ty_generics,
                            )>,
                        }
                        #[automatically_derived]
                        impl #variant_struct_impl_generics ::core::marker::Copy for #variant_name #variant_struct_ty_generics #adjusted_where_clause {}
                        #[automatically_derived]
                        impl #variant_struct_impl_generics ::core::clone::Clone for #variant_name #variant_struct_ty_generics #adjusted_where_clause {
                            fn clone(&self) -> Self {
                                *self
                            }
                        }
                        #[automatically_derived]
                        impl #variant_struct_impl_generics #crate_path::values::aggregate::EnumVariantRef<#ctx_lifetime, super::#name #ty_generics> for #variant_name #variant_struct_ty_generics #adjusted_where_clause {
                            fn visit_fields<__V: #crate_path::values::aggregate::EnumVariantFieldVisitor<#ctx_lifetime, super::#name #ty_generics, Self>>(
                                self,
                                __visitor: __V,
                            ) -> ::core::result::Result<__V, __V::BreakType> {
                                #(#visit_fields)*
                                ::core::result::Result::Ok(__visitor)
                            }
                            fn visit_field_types<__V: #crate_path::values::aggregate::EnumVariantFieldTypeVisitor<#ctx_lifetime, super::#name #ty_generics, Self>>(
                                __visitor: __V,
                            ) -> ::core::result::Result<__V, __V::BreakType> {
                                #(#visit_field_types)*
                                ::core::result::Result::Ok(__visitor)
                            }
                        }
                    });
                    match_value_variant = quote! {
                        __AggregateOfFieldValues::#variant_name {
                            #(#match_value_variant_fields)*
                        }
                    };
                }
                Fields::Unnamed(unnamed) => {
                    let mut aggregate_of_field_values_fields =
                        Vec::with_capacity(unnamed.unnamed.len());
                    let mut aggregate_of_field_values_clone_field_patterns =
                        Vec::with_capacity(unnamed.unnamed.len());
                    let mut aggregate_of_field_values_clone_fields =
                        Vec::with_capacity(unnamed.unnamed.len());
                    let mut match_value_variant_fields = Vec::with_capacity(unnamed.unnamed.len());
                    let mut match_fields = Vec::with_capacity(unnamed.unnamed.len());
                    let mut field_values = Vec::with_capacity(unnamed.unnamed.len());
                    let mut variant_struct_fields = Vec::with_capacity(unnamed.unnamed.len());
                    let mut visit_fields = Vec::with_capacity(unnamed.unnamed.len());
                    let mut visit_field_types = Vec::with_capacity(unnamed.unnamed.len());
                    for (index, Field { vis, ty, attrs, .. }) in
                        unnamed.unnamed.into_iter().enumerate()
                    {
                        let RustHdlFieldAttributes { ignored } =
                            RustHdlFieldAttributes::parse(&attrs)?;
                        if ignored {
                            field_values.push(quote! { (), });
                            match_fields.push(quote! { _, });
                            aggregate_of_field_values_fields.push(quote! { (), });
                            aggregate_of_field_values_clone_field_patterns.push(quote! { (), });
                            aggregate_of_field_values_clone_fields.push(quote! { (), });
                            variant_struct_fields.push(quote! {
                                #variant_vis (),
                            });
                            match_value_variant_fields.push(quote! { (), });
                            continue;
                        }
                        assert_field_visibility_is_inherited(vis)?;
                        let ident = Literal::usize_unsuffixed(index);
                        let name_str = ident.to_string();
                        let field_index = visit_fields.len();
                        visit_fields.push(quote! {
                            let __visitor = #crate_path::values::aggregate::EnumVariantFieldVisitor::field(
                                __visitor,
                                #name_str,
                                #field_index,
                                self.#ident,
                            )?;
                        });
                        visit_field_types.push(quote! {
                            let __visitor = #crate_path::values::aggregate::EnumVariantFieldTypeVisitor::field::<#ty>(
                                __visitor,
                                #name_str,
                                #field_index,
                            )?;
                        });
                        let var = Ident::new(&format!("v{}", index), Span::call_site());
                        field_values.push(quote! { #var, });
                        match_fields.push(quote! { #var, });
                        aggregate_of_field_values_needs_type_arguments = true;
                        aggregate_of_field_values_fields
                            .push(quote! { #crate_path::values::Val<#ctx_lifetime, #ty>, });
                        aggregate_of_field_values_clone_field_patterns.push(quote! {
                            #var,
                        });
                        aggregate_of_field_values_clone_fields.push(quote! {
                            ::core::clone::Clone::clone(#var),
                        });
                        variant_struct_fields.push(quote! {
                            #variant_vis &#variant_struct_lifetime #ty,
                        });
                        match_value_variant_fields.push(quote! {
                            #crate_path::values::ops::get_enum_variant_field_unchecked(value, #discriminant, #field_index, match_arm_scope),
                        });
                    }
                    aggregate_of_field_values_variants.push(quote! {
                        #variant_name(
                            #(#aggregate_of_field_values_fields)*
                        ),
                    });
                    aggregate_of_field_values_clone_variants.push(quote! {
                        Self::#variant_name(
                            #(#aggregate_of_field_values_clone_field_patterns)*
                        ) => Self::#variant_name(
                            #(#aggregate_of_field_values_clone_fields)*
                        ),
                    });
                    visit_variant_match_arms.push(quote! {
                        Self::#variant_name(#(#match_fields)*) => {
                            #crate_path::values::aggregate::EnumVariantVisitor::variant(
                                __visitor,
                                #variant_name_str,
                                #crate_path::values::Int::wrapping_new(#discriminant),
                                variant_structs::#variant_name::#variant_struct_ty_generics_with_implicit_lifetime(
                                    #(#field_values)*
                                    ::core::marker::PhantomData,
                                ),
                            )
                        }
                    });
                    visit_variant_types.push(quote! {
                        let __visitor = #crate_path::values::aggregate::EnumVariantTypeVisitor::variant::<variant_structs::#variant_name #variant_struct_ty_generics_with_implicit_lifetime>(
                            __visitor,
                            #variant_name_str,
                            #crate_path::values::Int::wrapping_new(#discriminant),
                        )?;
                    });
                    variant_structs.push(quote! {
                        #variant_vis struct #variant_name #variant_struct_generics(
                            #(#variant_struct_fields)*
                            #variant_vis ::core::marker::PhantomData<(
                                &#ctx_lifetime (),
                                &#variant_struct_lifetime super::#name #ty_generics,
                            )>,
                        ) #adjusted_where_clause;
                        #[automatically_derived]
                        impl #variant_struct_impl_generics ::core::marker::Copy for #variant_name #variant_struct_ty_generics #adjusted_where_clause {}
                        #[automatically_derived]
                        impl #variant_struct_impl_generics ::core::clone::Clone for #variant_name #variant_struct_ty_generics #adjusted_where_clause {
                            fn clone(&self) -> Self {
                                *self
                            }
                        }
                        #[automatically_derived]
                        impl #variant_struct_impl_generics #crate_path::values::aggregate::EnumVariantRef<#ctx_lifetime, super::#name #ty_generics> for #variant_name #variant_struct_ty_generics #adjusted_where_clause {
                            fn visit_fields<__V: #crate_path::values::aggregate::EnumVariantFieldVisitor<#ctx_lifetime, super::#name #ty_generics, Self>>(
                                self,
                                __visitor: __V,
                            ) -> ::core::result::Result<__V, __V::BreakType> {
                                #(#visit_fields)*
                                ::core::result::Result::Ok(__visitor)
                            }
                            fn visit_field_types<__V: #crate_path::values::aggregate::EnumVariantFieldTypeVisitor<#ctx_lifetime, super::#name #ty_generics, Self>>(
                                __visitor: __V,
                            ) -> ::core::result::Result<__V, __V::BreakType> {
                                #(#visit_field_types)*
                                ::core::result::Result::Ok(__visitor)
                            }
                        }
                    });
                    match_value_variant = quote! {
                        __AggregateOfFieldValues::#variant_name(#(#match_value_variant_fields)*)
                    };
                }
                Fields::Unit => {
                    aggregate_of_field_values_variants.push(quote! { #variant_name, });
                    aggregate_of_field_values_clone_variants.push(quote! {
                        Self::#variant_name => Self::#variant_name,
                    });
                    visit_variant_match_arms.push(quote! {
                        Self::#variant_name => {
                            #crate_path::values::aggregate::EnumVariantVisitor::variant(
                                __visitor,
                                #variant_name_str,
                                #crate_path::values::Int::wrapping_new(#discriminant),
                                variant_structs::#variant_name,
                            )
                        }
                    });
                    visit_variant_types.push(quote! {
                        let __visitor = #crate_path::values::aggregate::EnumVariantTypeVisitor::variant::<variant_structs::#variant_name>(
                            __visitor,
                            #variant_name_str,
                            #crate_path::values::Int::wrapping_new(#discriminant),
                        )?;
                    });
                    variant_structs.push(quote! {
                        #[derive(Copy, Clone)]
                        #variant_vis struct #variant_name;
                        #[automatically_derived]
                        impl #impl_generics #crate_path::values::aggregate::EnumVariantRef<#ctx_lifetime, super::#name #ty_generics> for #variant_name #adjusted_where_clause {
                            fn visit_fields<__V: #crate_path::values::aggregate::EnumVariantFieldVisitor<#ctx_lifetime, super::#name #ty_generics, Self>>(
                                self,
                                visitor: __V,
                            ) -> ::core::result::Result<__V, __V::BreakType> {
                                ::core::result::Result::Ok(visitor)
                            }
                            fn visit_field_types<__V: #crate_path::values::aggregate::EnumVariantFieldTypeVisitor<#ctx_lifetime, super::#name #ty_generics, Self>>(
                                visitor: __V,
                            ) -> ::core::result::Result<__V, __V::BreakType> {
                                ::core::result::Result::Ok(visitor)
                            }
                        }
                    });
                    match_value_variant = quote! { __AggregateOfFieldValues::#variant_name };
                }
            }
            match_value_variants.push(quote_spanned! {variant_name.span()=>
                {
                    let match_arm_scope = #crate_path::ir::scope::Scope::new(match_result_scope, match_result_scope.source_location());
                    let input = #crate_path::values::aggregate::AggregateValueMatchArm {
                        value: #match_value_variant,
                        match_arm_scope,
                    };
                    let result = f(input)?;
                    #crate_path::values::ops::MatchEnumUncheckedMatchArm {
                        discriminant: #discriminant,
                        result: #crate_path::values::ToVal::to_val(&result, value),
                        match_arm_scope,
                    }
                },
            });
            discriminants.push(discriminant);
        }
        let match_value = quote! {
            let match_arms = [
                #(#match_value_variants)*
            ];
            ::core::result::Result::Ok(#crate_path::values::ops::match_enum_unchecked(value, match_arms, match_result_scope))
        };
        let declare_aggregate_of_field_values = if aggregate_of_field_values_needs_type_arguments {
            quote! {
                #top_vis enum __AggregateOfFieldValues #generics_with_added_lifetimes #adjusted_where_clause {
                    #(#aggregate_of_field_values_variants)*
                }
                #[automatically_derived]
                impl #impl_generics ::core::marker::Copy for __AggregateOfFieldValues #ty_generics_with_added_lifetimes #adjusted_where_clause {}
                #[automatically_derived]
                impl #impl_generics ::core::clone::Clone for __AggregateOfFieldValues #ty_generics_with_added_lifetimes #adjusted_where_clause {
                    fn clone(&self) -> Self {
                        *self
                    }
                }
            }
        } else {
            quote! {
                #[derive(Clone, Copy)]
                #top_vis enum __AggregateOfFieldValues {
                    #(#aggregate_of_field_values_variants)*
                }
            }
        };
        let type_defs = quote! {
            mod variant_structs {
                #![no_implicit_prelude]
                use super::*;
                #(#variant_structs)*
            }
            #declare_aggregate_of_field_values
            const __DISCRIMINANT_SHAPE: #crate_path::values::integer::IntShape =
                #crate_path::values::aggregate::EnumDiscriminantShapeCalculator::new()
                    #(.add_discriminant(#discriminants))*
                    .get_shape();
        };
        if visit_variant_match_arms.is_empty() {
            visit_variant_match_arms.push(quote! {
                v => match *v {},
            });
        }
        Ok(Self {
            type_defs,
            visit_variant_match_arms,
            visit_variant_types,
            aggregate_of_field_values_needs_type_arguments,
            adjusted_where_clause,
            match_value,
        })
    }
    fn derive_value(self, common: ValueImplCommon) -> syn::Result<TokenStream> {
        let Self {
            type_defs,
            visit_variant_match_arms,
            visit_variant_types,
            aggregate_of_field_values_needs_type_arguments,
            adjusted_where_clause,
            match_value,
        } = self;
        let ValueImplCommon {
            crate_path,
            original_generics,
            generics_with_added_lifetimes,
            ctx_lifetime,
            name,
            top_vis: _,
        } = common;
        let (impl_generics, ty_generics_with_added_lifetimes, _) =
            generics_with_added_lifetimes.split_for_impl();
        let (_, ty_generics, _) = original_generics.split_for_impl();
        let aggregate_of_field_values_ty_generics = aggregate_of_field_values_needs_type_arguments
            .then(|| ty_generics_with_added_lifetimes);
        Ok(quote! {
            const _: () = {
                #type_defs
                #[automatically_derived]
                impl #impl_generics #crate_path::values::aggregate::AggregateOfFieldValues<#ctx_lifetime> for __AggregateOfFieldValues #aggregate_of_field_values_ty_generics #adjusted_where_clause {
                    type Aggregate = #name #ty_generics;
                }
                #[automatically_derived]
                impl #impl_generics #crate_path::values::aggregate::AggregateValue<#ctx_lifetime> for #name #ty_generics #adjusted_where_clause {
                    type AggregateValueKind = #crate_path::values::aggregate::EnumAggregateValueKind<#ctx_lifetime, Self>;
                    type DiscriminantShape = #crate_path::values::integer::ConstIntShape<
                        { __DISCRIMINANT_SHAPE.bit_count },
                        { __DISCRIMINANT_SHAPE.signed },
                    >;
                    type AggregateOfFieldValues = __AggregateOfFieldValues #aggregate_of_field_values_ty_generics;
                }
                #[automatically_derived]
                impl #impl_generics #crate_path::values::aggregate::AggregateValueMatch<#ctx_lifetime> for #name #ty_generics #adjusted_where_clause {
                    fn match_value<RV, R, E, F>(
                        value: #crate_path::values::Val<#ctx_lifetime, Self>,
                        match_result_scope: #crate_path::ir::scope::ScopeRef<#ctx_lifetime>,
                        mut f: F,
                        caller: &#crate_path::ir::SourceLocation<#ctx_lifetime>,
                    ) -> ::core::result::Result<#crate_path::values::Val<#ctx_lifetime, RV>, E>
                    where
                        RV: #crate_path::values::FixedTypeValue<#ctx_lifetime>,
                        R: #crate_path::values::ToVal<#ctx_lifetime, ValueType = RV>,
                        F: ::core::ops::FnMut(
                            #crate_path::values::aggregate::AggregateValueMatchArm<
                            #ctx_lifetime,
                                <Self as #crate_path::values::aggregate::AggregateValue<#ctx_lifetime>>::AggregateOfFieldValues,
                            >,
                        ) -> ::core::result::Result<R, E>,
                    {
                        #match_value
                    }
                }
                #[automatically_derived]
                impl #impl_generics #crate_path::values::aggregate::EnumValue<#ctx_lifetime> for #name #ty_generics #adjusted_where_clause {
                    fn visit_variant<__V: #crate_path::values::aggregate::EnumVariantVisitor<#ctx_lifetime, Self>>(
                        &self,
                        __visitor: __V,
                    ) -> __V::ResultType {
                        match self {
                            #(#visit_variant_match_arms)*
                        }
                    }
                    fn visit_variant_types<__V: #crate_path::values::aggregate::EnumVariantTypeVisitor<#ctx_lifetime, Self>>(
                        __visitor: __V,
                    ) -> ::core::result::Result<__V, __V::BreakType> {
                        #(#visit_variant_types)*
                        ::core::result::Result::Ok(__visitor)
                    }
                }
            };
        })
    }
    fn derive_fixed_type_value(self, common: ValueImplCommon) -> syn::Result<TokenStream> {
        let ValueImplCommon {
            crate_path,
            original_generics,
            generics_with_added_lifetimes,
            ctx_lifetime,
            name,
            top_vis: _,
        } = common;
        let Self {
            type_defs: _,
            visit_variant_match_arms: _,
            visit_variant_types: _,
            aggregate_of_field_values_needs_type_arguments: _,
            adjusted_where_clause,
            match_value: _,
        } = self;
        let impl_generics = generics_with_added_lifetimes.split_for_impl().0;
        let (_, ty_generics, _) = original_generics.split_for_impl();
        Ok(quote! {
            #[automatically_derived]
            impl #impl_generics #crate_path::values::aggregate::FixedTypeEnumValue<#ctx_lifetime> for #name #ty_generics #adjusted_where_clause {}
        })
    }
}

enum ValueImplData {
    Struct(ValueImplStruct),
    Enum(ValueImplEnum),
}

struct ValueImplCommon {
    crate_path: Path,
    original_generics: Generics,
    generics_with_added_lifetimes: Generics,
    ctx_lifetime: Lifetime,
    name: Ident,
    top_vis: Visibility,
}

struct ValueImpl {
    common: ValueImplCommon,
    data: ValueImplData,
}

impl ValueImpl {
    fn new(ast: DeriveInput) -> syn::Result<Self> {
        let RustHdlAttributes {
            crate_path,
            real_type_name,
        } = RustHdlAttributes::parse(&ast.attrs, AttributesFor::Derive)?;
        let GenericsWithAddedLifetimes {
            generics: generics_with_added_lifetimes,
            ctx_lifetime,
        } = GenericsWithAddedLifetimes::new(ast.generics.clone());
        let common = ValueImplCommon {
            crate_path,
            generics_with_added_lifetimes,
            ctx_lifetime,
            original_generics: ast.generics,
            name: real_type_name.unwrap_or(ast.ident),
            top_vis: ast.vis,
        };
        let data = match ast.data {
            Data::Struct(v) => ValueImplData::Struct(ValueImplStruct::new(v, &common)?),
            Data::Enum(v) => ValueImplData::Enum(ValueImplEnum::new(v, &common)?),
            Data::Union(_) => {
                return Err(Error::new_spanned(
                    common.name,
                    "#[derive(Value)] can't be used on unions",
                ));
            }
        };
        Ok(Self { common, data })
    }
    fn derive_value(self) -> syn::Result<TokenStream> {
        match self.data {
            ValueImplData::Struct(v) => v.derive_value(self.common),
            ValueImplData::Enum(v) => v.derive_value(self.common),
        }
    }
    fn derive_fixed_type_value(self) -> syn::Result<TokenStream> {
        match self.data {
            ValueImplData::Struct(v) => v.derive_fixed_type_value(self.common),
            ValueImplData::Enum(v) => v.derive_fixed_type_value(self.common),
        }
    }
}

fn derive_value_impl(ast: DeriveInput) -> syn::Result<TokenStream> {
    ValueImpl::new(ast)?.derive_value()
}

fn derive_fixed_type_value_impl(ast: DeriveInput) -> syn::Result<TokenStream> {
    ValueImpl::new(ast)?.derive_fixed_type_value()
}

fn derive_io_impl(ast: DeriveInput) -> syn::Result<TokenStream> {
    let RustHdlAttributes {
        crate_path,
        real_type_name,
    } = RustHdlAttributes::parse(&ast.attrs, AttributesFor::Derive)?;
    let (_, ty_generics, _) = ast.generics.split_for_impl();
    let GenericsWithAddedLifetimes {
        generics,
        ctx_lifetime,
    } = GenericsWithAddedLifetimes::new(ast.generics.clone());
    let adjusted_where_clause = where_clause_with_bound_on_generics(
        &ast.generics,
        |ident| parse_quote! { #ident: #crate_path::io::IO<#ctx_lifetime> },
    );
    let name = real_type_name.unwrap_or(ast.ident);
    let data_struct = match ast.data {
        Data::Struct(v) => v,
        _ => {
            return Err(Error::new_spanned(
                name,
                "#[derive(IO)] only valid on structs",
            ))
        }
    };
    let impl_generics = generics.split_for_impl().0;
    let mut visit_io_fields = Vec::new();
    match data_struct.fields {
        Fields::Named(fields) => {
            for field in fields.named {
                let name = field.ident.as_ref().unwrap();
                let name_str = name.to_string();
                visit_io_fields.push(quote! {.field(#name_str, &mut self.#name)});
            }
        }
        Fields::Unnamed(fields) => {
            for (name, _field) in fields.unnamed.iter().enumerate() {
                let name_str = name.to_string();
                let name = Literal::usize_unsuffixed(name);
                visit_io_fields.push(quote! {.field(#name_str, &mut self.#name)});
            }
        }
        Fields::Unit => {}
    }
    Ok(quote! {
        #[automatically_derived]
        impl #impl_generics #crate_path::io::IO<#ctx_lifetime> for #name #ty_generics #adjusted_where_clause {
            fn visit_io(&mut self, visitor: #crate_path::io::IOVisitor<'_, #ctx_lifetime>) {
                visitor
                    .visit_struct()
                    #(#visit_io_fields)*
                    .finish()
            }
        }
    })
}

fn derive_plain_io_impl(ast: DeriveInput) -> syn::Result<TokenStream> {
    let RustHdlAttributes {
        crate_path,
        real_type_name,
    } = RustHdlAttributes::parse(&ast.attrs, AttributesFor::Derive)?;
    let (_, ty_generics, _) = ast.generics.split_for_impl();
    let GenericsWithAddedLifetimes {
        generics,
        ctx_lifetime,
    } = GenericsWithAddedLifetimes::new(ast.generics.clone());
    let adjusted_where_clause = where_clause_with_bound_on_generics(
        &ast.generics,
        |ident| parse_quote! { #ident: #crate_path::io::PlainIO<#ctx_lifetime> },
    );
    let name = real_type_name.unwrap_or(ast.ident);
    let data_struct = match ast.data {
        Data::Struct(v) => v,
        _ => {
            return Err(Error::new_spanned(
                name,
                "#[derive(PlainIO)] only valid on structs",
            ))
        }
    };
    let impl_generics = generics.split_for_impl().0;
    let external_body = match data_struct.fields {
        Fields::Named(fields) => {
            let mut external_fields = Vec::new();
            for field in fields.named {
                let name = field.ident.as_ref().unwrap();
                external_fields.push(quote! {#name: ctx.external()});
            }
            quote! { Self { #(#external_fields,)* } }
        }
        Fields::Unnamed(fields) => {
            let mut external_fields = Vec::new();
            for _ in &fields.unnamed {
                external_fields.push(quote! {ctx.external()});
            }
            quote! { Self(#(#external_fields,)*) }
        }
        Fields::Unit => quote! { Self },
    };
    Ok(quote! {
        #[automatically_derived]
        impl #impl_generics #crate_path::io::PlainIO<#ctx_lifetime> for #name #ty_generics #adjusted_where_clause {
            fn external<__Ctx: #crate_path::context::AsContext<#ctx_lifetime>>(ctx: __Ctx) -> Self {
                let ctx = #crate_path::context::AsContext::ctx(&ctx);
                #external_body
            }
        }
    })
}

fn debug_input(input: &impl quote::ToTokens, name: &str) {
    #[cfg(feature = "debug-tokens")]
    eprintln!(
        "--------INPUT: {}\n{}\n--------",
        name,
        quote::ToTokens::to_token_stream(&input)
    );
    #[cfg(not(feature = "debug-tokens"))]
    {
        let _ = input;
        let _ = name;
    }
}

fn debug_output(output: &TokenStream, name: &str) {
    #[cfg(feature = "debug-tokens")]
    eprintln!("--------OUTPUT: {}\n{}\n--------", name, output);
    #[cfg(not(feature = "debug-tokens"))]
    {
        let _ = output;
        let _ = name;
    }
}

#[proc_macro_derive(Value, attributes(rust_hdl))]
pub fn derive_value(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    debug_input(&ast, "Value");
    let retval = match derive_value_impl(ast) {
        Ok(retval) => retval,
        Err(e) => e.into_compile_error(),
    };
    debug_output(&retval, "Value");
    retval.into()
}

#[proc_macro_derive(FixedTypeValue, attributes(rust_hdl))]
pub fn derive_fixed_type_value(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    debug_input(&ast, "FixedTypeValue");
    let retval = match derive_fixed_type_value_impl(ast) {
        Ok(retval) => retval,
        Err(e) => e.into_compile_error(),
    };
    debug_output(&retval, "FixedTypeValue");
    retval.into()
}

#[proc_macro_derive(IO, attributes(rust_hdl))]
pub fn derive_io(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    debug_input(&ast, "IO");
    let retval = match derive_io_impl(ast) {
        Ok(retval) => retval,
        Err(e) => e.into_compile_error(),
    };
    debug_output(&retval, "IO");
    retval.into()
}

#[proc_macro_derive(PlainIO, attributes(rust_hdl))]
pub fn derive_plain_io(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    debug_input(&ast, "PlainIO");
    let retval = match derive_plain_io_impl(ast) {
        Ok(retval) => retval,
        Err(e) => e.into_compile_error(),
    };
    debug_output(&retval, "PlainIO");
    retval.into()
}

#[proc_macro]
pub fn val(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as val::ValInput);
    debug_input(&ast, "val");
    let retval = match val::val_impl(ast) {
        Ok(retval) => retval,
        Err(e) => e.into_compile_error(),
    };
    debug_output(&retval, "val");
    retval.into()
}
