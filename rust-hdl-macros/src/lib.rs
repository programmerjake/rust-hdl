// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use proc_macro2::{Ident, Literal, Span, TokenStream};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    Attribute, Data, DataEnum, DataStruct, DeriveInput, Error, Fields, GenericParam, Generics,
    Lifetime, LifetimeDef, Path, Token, Variant,
};

enum RustHdlAttributeArg {
    Crate {
        crate_kw: Token![crate],
        eq: Token![=],
        path: Path,
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
        } else {
            Err(input.error("expected crate"))
        }
    }
}

struct RustHdlAttributes {
    crate_path: Option<Path>,
}

impl RustHdlAttributes {
    fn parse(attrs: &[Attribute]) -> syn::Result<Self> {
        let mut crate_path = None;
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
                    }
                }
            }
        }
        Ok(Self { crate_path })
    }
    fn get_crate_path(&self) -> Path {
        self.crate_path.clone().unwrap_or_else(|| {
            let mut path = Path::from(Ident::new("rust_hdl", Span::call_site()));
            path.leading_colon = Some(<Token![::]>::default());
            path
        })
    }
}

struct GenericsWithAddedLifetimes {
    generics: Generics,
    ctx_lifetime: Lifetime,
    scope_lifetime: Lifetime,
}

impl GenericsWithAddedLifetimes {
    fn new(mut generics: Generics) -> Self {
        let scope_lifetime = Lifetime::new("'__scope", Span::call_site());
        let ctx_lifetime = generics.lifetimes_mut().find_map(|l| {
            if l.lifetime.ident == "ctx" {
                l.bounds.push(scope_lifetime.clone());
                Some(l.lifetime.clone())
            } else {
                None
            }
        });
        let ctx_lifetime = match ctx_lifetime {
            Some(v) => v,
            None => {
                let ctx_lifetime = Lifetime::new("'ctx", Span::call_site());
                let mut lifetime_def = LifetimeDef::new(ctx_lifetime.clone());
                lifetime_def.bounds.push(scope_lifetime.clone());
                generics.params.push(GenericParam::Lifetime(lifetime_def));
                ctx_lifetime
            }
        };
        generics
            .params
            .push(GenericParam::Lifetime(LifetimeDef::new(
                scope_lifetime.clone(),
            )));
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
            ..
        } = common;
        let where_clause = &generics_with_added_lifetimes.where_clause;
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
                field_count = fields.named.len();
                for field in &fields.named {
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
                    struct_of_field_values_fields.push(quote! {
                        #vis #name: #crate_path::values::Val<#ctx_lifetime, #scope_lifetime, #ty>,
                    });
                    visit_fields.push(quote! {
                        let visitor = #crate_path::values::aggregate::StructFieldVisitor::field(
                            visitor,
                            #name_str,
                            __FieldEnum::#name,
                            &self.#name,
                        )?;
                    });
                    visit_field_types.push(quote! {
                        let visitor = #crate_path::values::aggregate::StructFieldTypeVisitor::field_with_type_hint(
                            visitor,
                            #name_str,
                            __FieldEnum::#name,
                            |v: &Self, _| &v.#name,
                        )?;
                    });
                    visit_field_fixed_types.push(quote! {
                        let visitor = #crate_path::values::aggregate::StructFieldFixedTypeVisitor::field_with_type_hint(
                            visitor,
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
                let field_enum_repr = if enum_fields.is_empty() {
                    quote! {}
                } else {
                    quote! {#[repr(usize)]}
                };
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
                    pub struct __StructOfFieldValues #generics_with_added_lifetimes #where_clause {
                        #(#struct_of_field_values_fields)*
                    }
                };
            }
            Fields::Unnamed(fields) => {
                let mut struct_of_field_enums_fields = Vec::new();
                let mut struct_of_field_enums_const_fields = Vec::new();
                let mut struct_of_field_values_fields = Vec::new();
                field_count = fields.unnamed.len();
                for (name, field) in fields.unnamed.iter().enumerate() {
                    let name_str = name.to_string();
                    let name = Literal::usize_unsuffixed(name);
                    let vis = &field.vis;
                    let ty = &field.ty;
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
                            &self.#name,
                        )?;
                    });
                    visit_field_types.push(quote! {
                        let visitor = #crate_path::values::aggregate::StructFieldTypeVisitor::field_with_type_hint(
                            visitor,
                            #name_str,
                            #name,
                            |v: &Self, _| &v.#name,
                        )?;
                    });
                    visit_field_fixed_types.push(quote! {
                        let visitor = #crate_path::values::aggregate::StructFieldFixedTypeVisitor::field_with_type_hint(
                            visitor,
                            #name_str,
                            #name,
                            |v: &Self, _| &v.#name,
                        )?;
                    });
                }
                type_defs = quote! {
                    #[allow(non_camel_case_types)]
                    pub type __FieldEnum = usize;

                    #[allow(non_snake_case)]
                    pub struct __StructOfFieldEnums(
                        #(#struct_of_field_enums_fields)*
                    );

                    #[allow(non_snake_case)]
                    pub struct __StructOfFieldValues #generics_with_added_lifetimes(
                        #(#struct_of_field_values_fields)*
                    ) #where_clause;
                };
                struct_of_field_enums_const = quote! {
                    __StructOfFieldEnums(
                        #(#struct_of_field_enums_const_fields)*
                    )
                };
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

                    #[allow(non_camel_case_types)]
                    pub struct __StructOfFieldEnums;

                    #[allow(non_camel_case_types)]
                    pub struct __StructOfFieldValues #generics_with_added_lifetimes #where_clause;
                };
                field_count = 0;
                struct_of_field_enums_const = quote! { __StructOfFieldEnums };
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
        } = common;
        let (_, ty_generics, where_clause) = original_generics.split_for_impl();
        let (impl_generics, ty_generics_with_added_lifetimes, _) =
            generics_with_added_lifetimes.split_for_impl();
        Ok(quote! {
            const _: () = {
                #type_defs
                #[automatically_derived]
                impl #impl_generics Copy for __StructOfFieldValues #ty_generics_with_added_lifetimes #where_clause {}
                #[automatically_derived]
                impl #impl_generics Clone for __StructOfFieldValues #ty_generics_with_added_lifetimes #where_clause {
                    fn clone(&self) -> Self {
                        *self
                    }
                }
                #[automatically_derived]
                impl #impl_generics #crate_path::values::aggregate::StructValueStructOfFieldValues<#ctx_lifetime, #scope_lifetime> for #name #ty_generics #where_clause {
                    type StructOfFieldValues = __StructOfFieldValues #ty_generics_with_added_lifetimes;
                }
                #[automatically_derived]
                impl #impl_generics #crate_path::values::aggregate::AggregateValue<#ctx_lifetime> for #name #ty_generics #where_clause {
                    type AggregateValueKind = #crate_path::values::aggregate::StructAggregateValueKind<#ctx_lifetime, Self>;
                    type DiscriminantShape = #crate_path::values::integer::UIntShape<0>;
                }
                #[automatically_derived]
                impl #impl_generics #crate_path::values::aggregate::StructValue<#ctx_lifetime> for #name #ty_generics #where_clause {
                    type FieldEnum = __FieldEnum;
                    type StructOfFieldEnums = __StructOfFieldEnums;
                    const STRUCT_OF_FIELD_ENUMS: Self::StructOfFieldEnums = #struct_of_field_enums_const;
                    const FIELD_COUNT: usize = #field_count;
                    fn visit_fields<V: #crate_path::values::aggregate::StructFieldVisitor<#ctx_lifetime, Self>>(
                        &self,
                        visitor: V,
                    ) -> ::core::result::Result<V, V::BreakType> {
                        #(#visit_fields)*
                        ::core::result::Result::Ok(visitor)
                    }
                    fn visit_field_types<V: #crate_path::values::aggregate::StructFieldTypeVisitor<#ctx_lifetime, Self>>(
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
        } = common;
        let (_, ty_generics, where_clause) = original_generics.split_for_impl();
        let impl_generics = generics_with_added_lifetimes.split_for_impl().0;
        Ok(quote! {
            #[automatically_derived]
            impl #impl_generics #crate_path::values::aggregate::FixedTypeStructValue<#ctx_lifetime> for #name #ty_generics #where_clause {
                fn visit_field_fixed_types<V: #crate_path::values::aggregate::StructFieldFixedTypeVisitor<#ctx_lifetime, Self>>(
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
    get_value_match_arms: Vec<TokenStream>,
    needed_bits: u32,
}

impl ValueImplEnum {
    // TODO: switch to using IrEnumType
    fn new(data: DataEnum, common: &ValueImplCommon) -> syn::Result<Self> {
        let _ = common;
        let mut variants = Vec::with_capacity(data.variants.len());
        for Variant {
            attrs: _,
            ident,
            fields,
            discriminant,
        } in data.variants
        {
            match fields {
                Fields::Named(_) | Fields::Unnamed(_) => {
                    return Err(Error::new_spanned(
                        ident,
                        "enum variants with fields not allowed with #[derive(Value)]",
                    ))
                }
                Fields::Unit => {}
            }
            if let Some(discriminant) = discriminant {
                return Err(Error::new_spanned(
                    discriminant.1,
                    "enum variants with discriminants not allowed with #[derive(Value)]",
                ));
            }
            variants.push(ident);
        }
        let mut needed_bits = 0;
        while (1 << needed_bits) < variants.len() as u128 {
            needed_bits += 1;
        }
        let mut get_value_match_arms = Vec::with_capacity(variants.len());
        for (index, variant) in variants.into_iter().enumerate() {
            let index = Literal::usize_unsuffixed(index);
            get_value_match_arms.push(quote! {
                Self::#variant => #index,
            });
        }
        Ok(Self {
            get_value_match_arms,
            needed_bits,
        })
    }
    fn derive_value(self, common: ValueImplCommon) -> syn::Result<TokenStream> {
        let Self {
            get_value_match_arms,
            needed_bits,
        } = self;
        let ValueImplCommon {
            crate_path,
            original_generics,
            generics_with_added_lifetimes,
            ctx_lifetime,
            scope_lifetime,
            name,
        } = common;
        let impl_generics = generics_with_added_lifetimes.split_for_impl().0;
        let (_, ty_generics, where_clause) = original_generics.split_for_impl();
        if get_value_match_arms.is_empty() {
            Ok(quote! {
                #[automatically_derived]
                impl #impl_generics #crate_path::values::Value<#ctx_lifetime> for #name #ty_generics #where_clause {
                    fn get_value(&self, ctx: #crate_path::context::ContextRef<#ctx_lifetime>) -> #crate_path::values::Val<#ctx_lifetime, #ctx_lifetime, Self> {
                        match *self {}
                    }
                    fn static_value_type_opt(ctx: #crate_path::context::ContextRef<#ctx_lifetime>) -> ::core::option::Option<#crate_path::values::ValueType<#ctx_lifetime, Self>> {
                        ::core::panic!("can't create uninhabited type")
                    }
                }
            })
        } else {
            Ok(quote! {
                #[automatically_derived]
                impl #impl_generics #crate_path::values::Value<#ctx_lifetime> for #name #ty_generics #where_clause {
                    fn get_value(&self, ctx: #crate_path::context::ContextRef<#ctx_lifetime>) -> #crate_path::values::Val<#ctx_lifetime, #ctx_lifetime, Self> {
                        let value: u128 = match *self {
                            #(#get_value_match_arms)*
                        };
                        let value = #crate_path::values::UInt::<#needed_bits>::wrapping_new(value);
                        let value = #crate_path::values::Value::get_value(&value, ctx);
                        #crate_path::values::Val::from_ir_and_type_unchecked(
                            value.ir(),
                            #crate_path::values::ValueType::from_ir_unchecked(
                                ctx,
                                value.value_type().ir(),
                            ),
                        )
                    }
                    fn static_value_type_opt(ctx: #crate_path::context::ContextRef<#ctx_lifetime>) -> ::core::option::Option<#crate_path::values::ValueType<#ctx_lifetime, Self>> {
                        let value_type = <#crate_path::values::UInt<#needed_bits> as #crate_path::values::Value<#ctx_lifetime>>::static_value_type_opt(ctx)?;
                        ::core::option::Option::Some(#crate_path::values::ValueType::from_ir_unchecked(
                            ctx,
                            value_type.ir(),
                        ))
                    }
                }
            })
        }
    }
    fn derive_fixed_type_value(self, common: ValueImplCommon) -> syn::Result<TokenStream> {
        let Self {
            get_value_match_arms,
            needed_bits: _,
        } = self;
        let ValueImplCommon {
            crate_path,
            original_generics,
            generics_with_added_lifetimes,
            ctx_lifetime,
            scope_lifetime,
            name,
        } = common;
        let impl_generics = generics_with_added_lifetimes.split_for_impl().0;
        let (_, ty_generics, where_clause) = original_generics.split_for_impl();
        if get_value_match_arms.is_empty() {
            Ok(quote! {
                #[automatically_derived]
                impl #impl_generics #crate_path::values::FixedTypeValue<#ctx_lifetime> for #name #ty_generics #where_clause {}
            })
        } else {
            Ok(quote! {
                #[automatically_derived]
                impl #impl_generics #crate_path::values::FixedTypeValue<#ctx_lifetime> for #name #ty_generics #where_clause {}
            })
        }
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
    scope_lifetime: Lifetime,
    name: Ident,
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

#[proc_macro_derive(Value, attributes(rust_hdl))]
pub fn derive_value(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    match derive_value_impl(ast) {
        Ok(retval) => retval,
        Err(e) => e.into_compile_error(),
    }
    .into()
}

#[proc_macro_derive(FixedTypeValue, attributes(rust_hdl))]
pub fn derive_fixed_type_value(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    match derive_fixed_type_value_impl(ast) {
        Ok(retval) => retval,
        Err(e) => e.into_compile_error(),
    }
    .into()
}

#[proc_macro_derive(IO, attributes(rust_hdl))]
pub fn derive_io(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    match derive_io_impl(ast) {
        Ok(retval) => retval,
        Err(e) => e.into_compile_error(),
    }
    .into()
}

#[proc_macro_derive(PlainIO, attributes(rust_hdl))]
pub fn derive_plain_io(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    match derive_plain_io_impl(ast) {
        Ok(retval) => retval,
        Err(e) => e.into_compile_error(),
    }
    .into()
}
