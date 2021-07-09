// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use proc_macro2::{Ident, Literal, Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    Attribute, Data, DataEnum, DataStruct, DeriveInput, Error, Field, Fields, GenericParam,
    Generics, Lifetime, LifetimeDef, Path, Token, Type, Variant,
};

mod kw {
    syn::custom_keyword!(deref_fields);
    syn::custom_keyword!(ignored);
}

enum RustHdlAttributeArg {
    Crate {
        crate_kw: Token![crate],
        eq: Token![=],
        path: Path,
    },
    DerefFields {
        deref_fields: kw::deref_fields,
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
        } else if input.peek(kw::deref_fields) {
            Ok(Self::DerefFields {
                deref_fields: input.parse()?,
            })
        } else {
            Err(input.error("expected `crate` or `deref_fields`"))
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct DerefFieldsCount(usize);

impl ToTokens for DerefFieldsCount {
    fn to_tokens(&self, token_stream: &mut TokenStream) {
        let star = <Token![*]>::default();
        for _ in 0..self.0 {
            star.to_tokens(token_stream);
        }
    }
}

struct RustHdlAttributes {
    crate_path: Option<Path>,
    deref_fields_count: DerefFieldsCount,
}

impl RustHdlAttributes {
    fn parse(attrs: &[Attribute]) -> syn::Result<Self> {
        let mut crate_path = None;
        let mut deref_fields_count = DerefFieldsCount(0);
        for attribute in attrs {
            if attribute.path.is_ident("rust_hdl") {
                let args = attribute.parse_args_with(
                    Punctuated::<RustHdlAttributeArg, Token![,]>::parse_separated_nonempty,
                )?;
                for arg in args {
                    match arg {
                        RustHdlAttributeArg::Crate { crate_kw, eq, path } => {
                            let _ = eq;
                            if crate_path.is_some() {
                                return Err(Error::new_spanned(
                                    crate_kw,
                                    "crate path specified twice",
                                ));
                            }
                            crate_path = Some(path);
                        }
                        RustHdlAttributeArg::DerefFields { deref_fields } => {
                            let _ = deref_fields;
                            deref_fields_count.0 += 1;
                        }
                    }
                }
            }
        }
        Ok(Self {
            crate_path,
            deref_fields_count,
        })
    }
    fn get_crate_path(&self) -> Path {
        self.crate_path.clone().unwrap_or_else(|| {
            let mut path = Path::from(Ident::new("rust_hdl", Span::call_site()));
            path.leading_colon = Some(<Token![::]>::default());
            path
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
    scope_lifetime: Lifetime,
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
        let (generics, scope_lifetime) = Self::get_or_add_lifetime(generics, "scope", []);
        let (generics, ctx_lifetime) =
            Self::get_or_add_lifetime(generics, "ctx", [scope_lifetime.clone()]);
        Self {
            generics,
            ctx_lifetime,
            scope_lifetime,
        }
    }
}

struct ValueImplStruct {
    type_defs: TokenStream,
    struct_of_field_enums_const: TokenStream,
    visit_fields: Vec<TokenStream>,
    visit_field_types: Vec<TokenStream>,
    visit_field_fixed_types: Vec<TokenStream>,
    field_count: usize,
}

impl ValueImplStruct {
    fn new(data: DataStruct, common: &ValueImplCommon) -> syn::Result<Self> {
        let ValueImplCommon {
            crate_path,
            ctx_lifetime,
            generics_with_added_lifetimes,
            scope_lifetime,
            deref_fields_count,
            name: struct_name,
            original_generics,
        } = common;
        let (_, ty_generics, where_clause) = original_generics.split_for_impl();
        let type_defs;
        let struct_of_field_enums_const;
        let mut visit_fields = Vec::new();
        let mut visit_field_types = Vec::new();
        let mut visit_field_fixed_types = Vec::new();
        let field_count;
        match data.fields {
            Fields::Named(fields) => {
                let mut enum_fields = Vec::new();
                let mut struct_of_field_enums_fields = Vec::new();
                let mut struct_of_field_enums_const_fields = Vec::new();
                let mut struct_of_field_values_fields = Vec::new();
                for field in &fields.named {
                    let RustHdlFieldAttributes { ignored } =
                        RustHdlFieldAttributes::parse(&field.attrs)?;
                    if ignored {
                        continue;
                    }
                    let name = field.ident.as_ref().unwrap();
                    let ty = deref_type(&field.ty, *deref_fields_count)?;
                    let vis = &field.vis;
                    let name_str = name.to_string();
                    enum_fields.push(quote! {#name,});
                    struct_of_field_enums_fields.push(quote! {
                        #vis #name: __FieldEnum,
                    });
                    struct_of_field_enums_const_fields.push(quote! {
                        #name: __FieldEnum::#name,
                    });
                    struct_of_field_values_fields.push(quote! {
                        #vis #name: #crate_path::values::Val<#ctx_lifetime, #scope_lifetime, #ty>,
                    });
                    visit_fields.push(quote! {
                        let visitor = #crate_path::values::aggregate::StructFieldVisitor::field(
                            visitor,
                            #name_str,
                            __FieldEnum::#name,
                            &#deref_fields_count self.#name,
                        )?;
                    });
                    visit_field_types.push(quote! {
                        let visitor = #crate_path::values::aggregate::StructFieldTypeVisitor::field_with_type_hint(
                            visitor,
                            #name_str,
                            __FieldEnum::#name,
                            |v: &Self, _| &#deref_fields_count v.#name,
                        )?;
                    });
                    visit_field_fixed_types.push(quote! {
                        let visitor = #crate_path::values::aggregate::StructFieldFixedTypeVisitor::field_with_type_hint(
                            visitor,
                            #name_str,
                            <Self as #crate_path::values::aggregate::StructValue<#ctx_lifetime, #scope_lifetime>>::FieldEnum::#name,
                            |v: &Self, _| &#deref_fields_count v.#name,
                        )?;
                    });
                }
                struct_of_field_enums_const = quote! {
                    __StructOfFieldEnums {
                        #(#struct_of_field_enums_const_fields)*
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
                    pub enum __FieldEnum {
                        #(#enum_fields)*
                    }

                    impl ::core::convert::From<__FieldEnum> for usize {
                        fn from(v: __FieldEnum) -> Self {
                            v as usize
                        }
                    }

                    #[derive(Clone, Copy)]
                    #[allow(non_snake_case)]
                    pub struct __StructOfFieldEnums {
                        #(#struct_of_field_enums_fields)*
                    }

                    #[allow(non_snake_case)]
                    pub struct __AggregateOfFieldValues #generics_with_added_lifetimes #where_clause {
                        #(#struct_of_field_values_fields)*
                        __struct_phantom: ::core::marker::PhantomData<(&#scope_lifetime &#ctx_lifetime (), #struct_name #ty_generics)>,
                    }
                };
            }
            Fields::Unnamed(fields) => {
                let mut struct_of_field_enums_fields = Vec::new();
                let mut struct_of_field_enums_const_fields = Vec::new();
                let mut struct_of_field_values_fields = Vec::new();
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
                        struct_of_field_values_fields.push(quote! {
                            #vis (),
                        });
                        continue;
                    }
                    let ty = deref_type(&field.ty, *deref_fields_count)?;
                    struct_of_field_enums_fields.push(quote! {
                        #vis __FieldEnum,
                    });
                    struct_of_field_enums_const_fields.push(quote! {
                        #name,
                    });
                    struct_of_field_values_fields.push(quote! {
                        #vis #crate_path::values::Val<#ctx_lifetime, #scope_lifetime, #ty>,
                    });
                    visit_fields.push(quote! {
                        let visitor = #crate_path::values::aggregate::StructFieldVisitor::field(
                            visitor,
                            #name_str,
                            #name,
                            &#deref_fields_count self.#name,
                        )?;
                    });
                    visit_field_types.push(quote! {
                        let visitor = #crate_path::values::aggregate::StructFieldTypeVisitor::field_with_type_hint(
                            visitor,
                            #name_str,
                            #name,
                            |v: &Self, _| &#deref_fields_count v.#name,
                        )?;
                    });
                    visit_field_fixed_types.push(quote! {
                        let visitor = #crate_path::values::aggregate::StructFieldFixedTypeVisitor::field_with_type_hint(
                            visitor,
                            #name_str,
                            #name,
                            |v: &Self, _| &#deref_fields_count v.#name,
                        )?;
                    });
                }
                type_defs = quote! {
                    #[allow(non_camel_case_types)]
                    pub type __FieldEnum = usize;

                    #[derive(Clone, Copy)]
                    #[allow(non_snake_case)]
                    pub struct __StructOfFieldEnums(
                        #(#struct_of_field_enums_fields)*
                    );

                    #[allow(non_snake_case)]
                    pub struct __AggregateOfFieldValues #generics_with_added_lifetimes(
                        #(#struct_of_field_values_fields)*
                        ::core::marker::PhantomData<(&#scope_lifetime &#ctx_lifetime (), #struct_name #ty_generics)>,
                    ) #where_clause;
                };
                struct_of_field_enums_const = quote! {
                    __StructOfFieldEnums(
                        #(#struct_of_field_enums_const_fields)*
                    )
                };
                field_count = visit_fields.len();
            }
            Fields::Unit => {
                type_defs = quote! {
                    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
                    #[allow(non_camel_case_types)]
                    pub enum __FieldEnum {}

                    impl ::core::convert::From<__FieldEnum> for usize {
                        fn from(v: __FieldEnum) -> Self {
                            match v {}
                        }
                    }

                    #[derive(Clone, Copy)]
                    #[allow(non_camel_case_types)]
                    pub struct __StructOfFieldEnums;

                    #[allow(non_camel_case_types)]
                    pub struct __AggregateOfFieldValues #generics_with_added_lifetimes #where_clause {
                        __struct_phantom: ::core::marker::PhantomData<(&#scope_lifetime &#ctx_lifetime (), #struct_name #ty_generics)>,
                    }
                };
                struct_of_field_enums_const = quote! { __StructOfFieldEnums };
                field_count = 0;
            }
        }
        Ok(Self {
            type_defs,
            struct_of_field_enums_const,
            visit_fields,
            visit_field_types,
            visit_field_fixed_types,
            field_count,
        })
    }
    fn derive_value(self, common: ValueImplCommon) -> syn::Result<TokenStream> {
        let Self {
            type_defs,
            struct_of_field_enums_const,
            visit_fields,
            visit_field_types,
            field_count,
            ..
        } = self;
        let ValueImplCommon {
            crate_path,
            original_generics,
            generics_with_added_lifetimes,
            ctx_lifetime,
            scope_lifetime,
            name,
            deref_fields_count: _,
        } = common;
        let (_, ty_generics, where_clause) = original_generics.split_for_impl();
        let (impl_generics, ty_generics_with_added_lifetimes, _) =
            generics_with_added_lifetimes.split_for_impl();
        Ok(quote! {
            const _: () = {
                #type_defs
                #[automatically_derived]
                impl #impl_generics Copy for __AggregateOfFieldValues #ty_generics_with_added_lifetimes #where_clause {}
                #[automatically_derived]
                impl #impl_generics Clone for __AggregateOfFieldValues #ty_generics_with_added_lifetimes #where_clause {
                    fn clone(&self) -> Self {
                        *self
                    }
                }
                #[automatically_derived]
                impl #impl_generics #crate_path::values::aggregate::AggregateValue<#ctx_lifetime, #scope_lifetime> for #name #ty_generics #where_clause {
                    type AggregateValueKind = #crate_path::values::aggregate::StructAggregateValueKind<#ctx_lifetime, #scope_lifetime, Self>;
                    type DiscriminantShape = #crate_path::values::integer::UIntShape<0>;
                    type AggregateOfFieldValues = __AggregateOfFieldValues #ty_generics_with_added_lifetimes;
                }
                #[automatically_derived]
                impl #impl_generics #crate_path::values::aggregate::StructValue<#ctx_lifetime, #scope_lifetime> for #name #ty_generics #where_clause {
                    type FieldEnum = __FieldEnum;
                    type StructOfFieldEnums = __StructOfFieldEnums;
                    const STRUCT_OF_FIELD_ENUMS: Self::StructOfFieldEnums = #struct_of_field_enums_const;
                    const FIELD_COUNT: usize = #field_count;
                    fn visit_fields<V: #crate_path::values::aggregate::StructFieldVisitor<#ctx_lifetime, #scope_lifetime, Self>>(
                        &self,
                        visitor: V,
                    ) -> ::core::result::Result<V, V::BreakType> {
                        #(#visit_fields)*
                        ::core::result::Result::Ok(visitor)
                    }
                    fn visit_field_types<V: #crate_path::values::aggregate::StructFieldTypeVisitor<#ctx_lifetime, #scope_lifetime, Self>>(
                        visitor: V,
                    ) -> ::core::result::Result<V, V::BreakType> {
                        #(#visit_field_types)*
                        ::core::result::Result::Ok(visitor)
                    }
                }
            };
        })
    }
    fn derive_fixed_type_value(self, common: ValueImplCommon) -> syn::Result<TokenStream> {
        let Self {
            visit_field_fixed_types,
            ..
        } = self;
        let ValueImplCommon {
            crate_path,
            original_generics,
            generics_with_added_lifetimes,
            ctx_lifetime,
            scope_lifetime,
            name,
            deref_fields_count: _,
        } = common;
        let (_, ty_generics, where_clause) = original_generics.split_for_impl();
        let impl_generics = generics_with_added_lifetimes.split_for_impl().0;
        Ok(quote! {
            #[automatically_derived]
            impl #impl_generics #crate_path::values::aggregate::FixedTypeStructValue<#ctx_lifetime, #scope_lifetime> for #name #ty_generics #where_clause {
                fn visit_field_fixed_types<V: #crate_path::values::aggregate::StructFieldFixedTypeVisitor<#ctx_lifetime, #scope_lifetime, Self>>(
                    visitor: V,
                ) -> ::core::result::Result<V, V::BreakType> {
                    #(#visit_field_fixed_types)*
                    ::core::result::Result::Ok(visitor)
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
    variant_struct_lifetime: Lifetime,
}

impl ValueImplEnum {
    // TODO: switch to using IrEnumType
    fn new(data: DataEnum, common: &ValueImplCommon) -> syn::Result<Self> {
        let ValueImplCommon {
            name,
            crate_path,
            original_generics,
            generics_with_added_lifetimes,
            ctx_lifetime,
            scope_lifetime,
            deref_fields_count,
        } = common;
        let any_discriminants = data.variants.iter().any(|v| v.discriminant.is_some());
        let mut discriminants = Vec::with_capacity(data.variants.len());
        let mut visit_variant_match_arms = Vec::with_capacity(data.variants.len());
        let mut visit_variant_types = Vec::with_capacity(data.variants.len());
        let mut aggregate_of_field_values_variants = Vec::with_capacity(data.variants.len());
        let mut aggregate_of_field_values_needs_type_arguments = false;
        let mut variant_structs = Vec::with_capacity(data.variants.len());
        let (impl_generics, ty_generics_with_added_lifetimes, _) =
            generics_with_added_lifetimes.split_for_impl();
        let (_, ty_generics, where_clause) = original_generics.split_for_impl();
        let ty_generics_as_turbofish = ty_generics.as_turbofish();
        let mut variant_struct_generics = generics_with_added_lifetimes.clone();
        let variant_struct_lifetime = Lifetime::new("'__a", Span::call_site());
        let mut variant_struct_lifetime_def = LifetimeDef::new(variant_struct_lifetime.clone());
        variant_struct_lifetime_def
            .bounds
            .push(scope_lifetime.clone());
        variant_struct_generics
            .params
            .push(GenericParam::Lifetime(variant_struct_lifetime_def));
        let (variant_struct_impl_generics, variant_struct_ty_generics, _) =
            variant_struct_generics.split_for_impl();
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
                let index = Literal::usize_unsuffixed(index);
                quote! { #index }
            };
            let variant_name_str = variant_name.to_string();
            let RustHdlVariantAttributes {} = RustHdlVariantAttributes::parse(&variant_attrs)?;
            match fields {
                Fields::Named(named) => {
                    let mut aggregate_of_field_values_fields =
                        Vec::with_capacity(named.named.len());
                    let mut field_names = Vec::with_capacity(named.named.len());
                    let mut variant_struct_fields = Vec::with_capacity(named.named.len());
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
                            continue;
                        }
                        let ty = deref_type(&ty, *deref_fields_count)?;
                        aggregate_of_field_values_needs_type_arguments = true;
                        aggregate_of_field_values_fields.push(quote! {
                            #vis #ident: #crate_path::values::Val<#ctx_lifetime, #scope_lifetime, #ty>,
                        });
                        variant_struct_fields.push(quote! {
                            pub #ident: &#variant_struct_lifetime #ty,
                        });
                        field_names.push(ident);
                    }
                    aggregate_of_field_values_variants.push(quote! {
                        #variant_name {
                            #(#aggregate_of_field_values_fields)*
                        },
                    });
                    visit_variant_match_arms.push(quote! {
                        Self::#variant_name { #(#field_names,)* } => {
                            #crate_path::values::aggregate::EnumVariantVisitor::variant(
                                __visitor,
                                #variant_name_str,
                                #crate_path::values::Int::wrapping_new(#discriminant),
                                &variant_structs::#variant_name #ty_generics_as_turbofish {
                                    #(#field_names,)*
                                    __enum_phantom: ::core::marker::PhantomData,
                                },
                            )
                        }
                    });
                    visit_variant_types.push(quote! {
                        let __visitor = #crate_path::values::aggregate::EnumVariantTypeVisitor::variant::<variant_structs::#variant_name #ty_generics>(
                            __visitor,
                            #variant_name_str,
                            #crate_path::values::Int::wrapping_new(#discriminant),
                        )?;
                    });
                    variant_structs.push(quote! {
                        #[derive(#crate_path::values::Value, #crate_path::values::FixedTypeValue)]
                        #[rust_hdl(crate = #crate_path, deref_fields)]
                        pub struct #variant_name #variant_struct_generics #where_clause {
                            #(#variant_struct_fields)*
                            #[rust_hdl(ignored)]
                            pub __enum_phantom: ::core::marker::PhantomData<(
                                &#scope_lifetime &#ctx_lifetime (),
                                &#variant_struct_lifetime super::#name #ty_generics,
                            )>,
                        }
                    });
                }
                Fields::Unnamed(unnamed) => {
                    let mut aggregate_of_field_values_fields =
                        Vec::with_capacity(unnamed.unnamed.len());
                    let mut field_names = Vec::with_capacity(unnamed.unnamed.len());
                    let mut variant_struct_fields = Vec::with_capacity(unnamed.unnamed.len());
                    for (index, Field { vis, ty, attrs, .. }) in
                        unnamed.unnamed.into_iter().enumerate()
                    {
                        let RustHdlFieldAttributes { ignored } =
                            RustHdlFieldAttributes::parse(&attrs)?;
                        field_names.push(Ident::new(&format!("v{}", index), Span::call_site()));
                        if ignored {
                            aggregate_of_field_values_fields.push(quote! { #vis (), });
                            variant_struct_fields.push(quote! {
                                #[rust_hdl(ignored)]
                                pub (),
                            });
                            continue;
                        }
                        let ty = deref_type(&ty, *deref_fields_count)?;
                        aggregate_of_field_values_needs_type_arguments = true;
                        aggregate_of_field_values_fields.push(quote! { #vis #crate_path::values::Val<#ctx_lifetime, #scope_lifetime, #ty>, });
                        variant_struct_fields.push(quote! {
                            pub &#variant_struct_lifetime #ty,
                        });
                    }
                    aggregate_of_field_values_variants.push(quote! {
                        #variant_name(
                            #(#aggregate_of_field_values_fields)*
                        ),
                    });
                    visit_variant_match_arms.push(quote! {
                        Self::#variant_name(#(#field_names,)*) => {
                            #crate_path::values::aggregate::EnumVariantVisitor::variant(
                                __visitor,
                                #variant_name_str,
                                #crate_path::values::Int::wrapping_new(#discriminant),
                                &variant_structs::#variant_name #ty_generics_as_turbofish(
                                    #(#field_names,)*
                                    ::core::marker::PhantomData,
                                ),
                            )
                        }
                    });
                    visit_variant_types.push(quote! {
                        let __visitor = #crate_path::values::aggregate::EnumVariantTypeVisitor::variant::<variant_structs::#variant_name #ty_generics>(
                            __visitor,
                            #variant_name_str,
                            #crate_path::values::Int::wrapping_new(#discriminant),
                        )?;
                    });
                    variant_structs.push(quote! {
                        #[derive(#crate_path::values::Value, #crate_path::values::FixedTypeValue)]
                        #[rust_hdl(crate = #crate_path, deref_fields)]
                        pub struct #variant_name #variant_struct_generics(
                            #(#variant_struct_fields)*
                            #[rust_hdl(ignored)]
                            pub ::core::marker::PhantomData<(
                                &#scope_lifetime &#ctx_lifetime (),
                                &#variant_struct_lifetime super::#name #ty_generics,
                            )>,
                        ) #where_clause;
                    });
                }
                Fields::Unit => {
                    aggregate_of_field_values_variants.push(quote! { #variant_name, });
                    visit_variant_match_arms.push(quote! {
                        Self::#variant_name => {
                            #crate_path::values::aggregate::EnumVariantVisitor::variant(
                                __visitor,
                                #variant_name_str,
                                #crate_path::values::Int::wrapping_new(#discriminant),
                                &variant_structs::#variant_name,
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
                        #[derive(#crate_path::values::Value, #crate_path::values::FixedTypeValue)]
                        #[rust_hdl(crate = #crate_path)]
                        pub struct #variant_name;
                    });
                }
            }
            discriminants.push(discriminant);
        }
        let declare_aggregate_of_field_values = if aggregate_of_field_values_needs_type_arguments {
            quote! {
                pub enum __AggregateOfFieldValues #generics_with_added_lifetimes #where_clause {
                    #(#aggregate_of_field_values_variants)*
                }
                #[automatically_derived]
                impl #impl_generics Copy for __AggregateOfFieldValues #ty_generics_with_added_lifetimes #where_clause {}
                #[automatically_derived]
                impl #impl_generics Clone for __AggregateOfFieldValues #ty_generics_with_added_lifetimes #where_clause {
                    fn clone(&self) -> Self {
                        *self
                    }
                }
            }
        } else {
            quote! {
                #[derive(Copy, Clone)]
                pub enum __AggregateOfFieldValues {
                    #(#aggregate_of_field_values_variants)*
                }
            }
        };
        let type_defs = quote! {
            mod variant_structs {
                #(#variant_structs)*
            }
            #declare_aggregate_of_field_values
            const __DISCRIMINANT_SHAPE: #crate_path::values::integer::IntShape =
                #crate_path::values::aggregate::EnumDiscriminantShapeCalculator::new()
                    #(.add_discriminant(#discriminants))*
                    .get_shape();
        };
        Ok(Self {
            type_defs,
            visit_variant_match_arms,
            visit_variant_types,
            aggregate_of_field_values_needs_type_arguments,
            variant_struct_lifetime,
        })
    }
    fn derive_value(self, common: ValueImplCommon) -> syn::Result<TokenStream> {
        let Self {
            type_defs,
            visit_variant_match_arms,
            visit_variant_types,
            aggregate_of_field_values_needs_type_arguments,
            variant_struct_lifetime,
        } = self;
        let ValueImplCommon {
            crate_path,
            original_generics,
            generics_with_added_lifetimes,
            ctx_lifetime,
            scope_lifetime,
            name,
            deref_fields_count,
        } = common;
        let (impl_generics, ty_generics_with_added_lifetimes, _) =
            generics_with_added_lifetimes.split_for_impl();
        let (_, ty_generics, where_clause) = original_generics.split_for_impl();
        let aggregate_of_field_values_ty_generics = aggregate_of_field_values_needs_type_arguments
            .then(|| ty_generics_with_added_lifetimes);
        Ok(quote! {
            const _: () = {
                #type_defs
                #[automatically_derived]
                impl #impl_generics #crate_path::values::aggregate::AggregateValue<#ctx_lifetime, #scope_lifetime> for #name #ty_generics #where_clause {
                    type AggregateValueKind = #crate_path::values::aggregate::EnumAggregateValueKind<#ctx_lifetime, #scope_lifetime, Self>;
                    type DiscriminantShape = #crate_path::values::integer::ConstIntShape<
                        { __DISCRIMINANT_SHAPE.bit_count },
                        { __DISCRIMINANT_SHAPE.signed },
                    >;
                    type AggregateOfFieldValues = __AggregateOfFieldValues #aggregate_of_field_values_ty_generics;
                }
                #[automatically_derived]
                impl #impl_generics #crate_path::values::aggregate::EnumValue<#ctx_lifetime, #scope_lifetime> for #name #ty_generics #where_clause {
                    fn visit_variant<
                        #variant_struct_lifetime,
                        V: #crate_path::values::aggregate::EnumVariantVisitor<#ctx_lifetime, #scope_lifetime, Self>
                    >(
                        &#variant_struct_lifetime self,
                        __visitor: V,
                    ) -> V::ResultType {
                        match self {
                            #(#visit_variant_match_arms)*
                        }
                    }
                    fn visit_variant_types<V: #crate_path::values::aggregate::EnumVariantTypeVisitor<#ctx_lifetime, #scope_lifetime, Self>>(
                        __visitor: V,
                    ) -> ::core::result::Result<V, V::BreakType> {
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
            scope_lifetime,
            name,
            deref_fields_count,
        } = common;
        let impl_generics = generics_with_added_lifetimes.split_for_impl().0;
        let (_, ty_generics, where_clause) = original_generics.split_for_impl();
        Ok(quote! {
            #[automatically_derived]
            impl #impl_generics #crate_path::values::aggregate::FixedTypeEnumValue<#ctx_lifetime, #scope_lifetime> for #name #ty_generics #where_clause {}
        })
    }
}

enum ValueImplData {
    Struct(ValueImplStruct),
    Enum(ValueImplEnum),
}

struct ValueImplCommon {
    crate_path: Path,
    deref_fields_count: DerefFieldsCount,
    original_generics: Generics,
    generics_with_added_lifetimes: Generics,
    ctx_lifetime: Lifetime,
    scope_lifetime: Lifetime,
    name: Ident,
}

fn deref_type(mut ty: &Type, count: DerefFieldsCount) -> syn::Result<&Type> {
    for _ in 0..count.0 {
        ty = match ty {
            Type::Reference(v) => &v.elem,
            _ => {
                return Err(Error::new_spanned(
                    ty,
                    "dereferencing only supported on reference types",
                ))
            }
        };
    }
    Ok(ty)
}

struct ValueImpl {
    common: ValueImplCommon,
    data: ValueImplData,
}

impl ValueImpl {
    fn new(ast: DeriveInput) -> syn::Result<Self> {
        let rust_hdl_attributes = RustHdlAttributes::parse(&ast.attrs)?;
        let crate_path = rust_hdl_attributes.get_crate_path();
        let GenericsWithAddedLifetimes {
            generics: generics_with_added_lifetimes,
            ctx_lifetime,
            scope_lifetime,
        } = GenericsWithAddedLifetimes::new(ast.generics.clone());
        let common = ValueImplCommon {
            crate_path,
            generics_with_added_lifetimes,
            ctx_lifetime,
            scope_lifetime,
            original_generics: ast.generics,
            name: ast.ident,
            deref_fields_count: rust_hdl_attributes.deref_fields_count,
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
    let rust_hdl_attributes = RustHdlAttributes::parse(&ast.attrs)?;
    let crate_path = rust_hdl_attributes.get_crate_path();
    let (_, ty_generics, where_clause) = ast.generics.split_for_impl();
    let GenericsWithAddedLifetimes {
        generics,
        ctx_lifetime,
        scope_lifetime: _,
    } = GenericsWithAddedLifetimes::new(ast.generics.clone());
    let name = ast.ident;
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
        impl #impl_generics #crate_path::io::IO<#ctx_lifetime> for #name #ty_generics #where_clause {
            fn visit_io(&mut self, visitor: #crate_path::io::IOVisitor<'_, 'ctx>) {
                visitor
                    .visit_struct()
                    #(#visit_io_fields)*
                    .finish()
            }
        }
    })
}

fn derive_plain_io_impl(ast: DeriveInput) -> syn::Result<TokenStream> {
    let rust_hdl_attributes = RustHdlAttributes::parse(&ast.attrs)?;
    let crate_path = rust_hdl_attributes.get_crate_path();
    let (_, ty_generics, where_clause) = ast.generics.split_for_impl();
    let GenericsWithAddedLifetimes {
        generics,
        ctx_lifetime,
        scope_lifetime: _,
    } = GenericsWithAddedLifetimes::new(ast.generics.clone());
    let name = ast.ident;
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
        impl #impl_generics #crate_path::io::PlainIO<#ctx_lifetime> for #name #ty_generics #where_clause {
            fn external(ctx: #crate_path::context::ContextRef<'ctx>) -> Self {
                #external_body
            }
        }
    })
}

fn debug_input(input: &DeriveInput, derive_name: &str) {
    eprintln!(
        "--------INPUT: {}\n{}\n--------",
        derive_name,
        input.to_token_stream()
    );
}

#[track_caller]
fn debug_output(output: &TokenStream, derive_name: &str) {
    eprintln!("--------OUTPUT: {}\n{}\n--------", derive_name, output);
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
    let retval = match derive_io_impl(ast) {
        Ok(retval) => retval,
        Err(e) => e.into_compile_error(),
    };
    retval.into()
}

#[proc_macro_derive(PlainIO, attributes(rust_hdl))]
pub fn derive_plain_io(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let retval = match derive_plain_io_impl(ast) {
        Ok(retval) => retval,
        Err(e) => e.into_compile_error(),
    };
    retval.into()
}
