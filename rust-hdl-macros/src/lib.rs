// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    Attribute, Data, DataEnum, DataStruct, DeriveInput, Error, Field, Fields, FieldsNamed,
    FieldsUnnamed, GenericParam, Generics, Index, Lifetime, LifetimeDef, Path, Token, Variant,
    VisRestricted, Visibility, WhereClause, WherePredicate,
};

#[macro_use]
mod macros;
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

fn get_or_add_lifetime_to_generics(
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

struct GenericsWithCtx {
    generics: Generics,
    ctx_lifetime: Lifetime,
}

impl GenericsWithCtx {
    fn new(generics: Generics) -> Self {
        let (generics, ctx_lifetime) = get_or_add_lifetime_to_generics(generics, "ctx", []);
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

fn item_vis_in_mod(vis_outside_mod: Visibility) -> syn::Result<Visibility> {
    match vis_outside_mod {
        Visibility::Public(_) | Visibility::Crate(_) => Ok(vis_outside_mod),
        Visibility::Restricted(VisRestricted {
            pub_token: _,
            paren_token: _,
            in_token: None,
            ref path,
        }) if path.is_ident("crate") => Ok(vis_outside_mod),
        Visibility::Inherited => Ok(parse_quote! { pub(super) }),
        Visibility::Restricted(_) => {
            return Err(Error::new_spanned(
                vis_outside_mod,
                "unsupported visibility on item with #[derive(Value)], use `pub(crate)` instead",
            ));
        }
    }
}

fn struct_field_vis_in_mod(vis_outside_mod: Visibility) -> syn::Result<Visibility> {
    match vis_outside_mod {
        Visibility::Public(_) | Visibility::Crate(_) => Ok(vis_outside_mod),
        Visibility::Restricted(VisRestricted {
            pub_token: _,
            paren_token: _,
            in_token: None,
            ref path,
        }) if path.is_ident("crate") => Ok(vis_outside_mod),
        Visibility::Inherited => Ok(parse_quote! { pub(super) }),
        Visibility::Restricted(_) => {
            return Err(Error::new_spanned(
                vis_outside_mod,
                "unsupported visibility on struct field with #[derive(Value)], use `pub(crate)` instead",
            ));
        }
    }
}

fn assert_enum_field_visibility_is_inherited(vis: Visibility) -> syn::Result<()> {
    if let Visibility::Inherited = vis {
        Ok(())
    } else {
        Err(Error::new_spanned(
            vis,
            "#[derive(Value)] requires enum fields to have default (inherited) visibility",
        ))
    }
}

struct ValueImplFirstStep<Data> {
    crate_path: Path,
    value_mod: Ident,
    fixed_type_value_mod: Ident,
    original_generics: Generics,
    generics_with_ctx: Generics,
    ctx_lifetime: Lifetime,
    name: Ident,
    where_clause_with_value_bound: WhereClause,
    where_clause_with_fixed_type_value_bound: WhereClause,
    item_vis_in_mod: Visibility,
    data: Data,
}

struct ValueImpl {
    crate_path: Path,
    value_mod: Ident,
    fixed_type_value_mod: Ident,
    original_generics: Generics,
    generics_with_ctx: Generics,
    ctx_lifetime: Lifetime,
    name: Ident,
    value_items: Vec<TokenStream>,
    fixed_type_value_items: Vec<TokenStream>,
    where_clause_with_value_bound: WhereClause,
    where_clause_with_fixed_type_value_bound: WhereClause,
    discriminant_shape: TokenStream,
    struct_of_variant_values_body: TokenStream,
    visit_variants_body: TokenStream,
    item_vis_in_mod: Visibility,
}

impl ValueImpl {
    fn new_struct(first_step: ValueImplFirstStep<DataStruct>) -> syn::Result<Self> {
        let ValueImplFirstStep {
            crate_path,
            value_mod,
            fixed_type_value_mod,
            original_generics,
            generics_with_ctx,
            ctx_lifetime,
            name,
            where_clause_with_value_bound,
            where_clause_with_fixed_type_value_bound,
            item_vis_in_mod,
            data,
        } = first_step;
        let (_, original_ty_generics, _) = original_generics.split_for_impl();
        let (impl_generics_with_ctx, ty_generics_with_ctx, _) = generics_with_ctx.split_for_impl();
        let discriminant_shape = quote! { #crate_path::values::integer::UIntShape<0> };
        let struct_of_variant_values_body;
        let visit_variants_body;
        let mut value_items = Vec::new();
        let mut fixed_type_value_items = Vec::new();
        match data.fields {
            Fields::Named(fields) => {
                let mut struct_of_variant_values_body_fields = Vec::new();
                let mut visit_variants_body_fields = Vec::new();
                let mut struct_of_variant_values_fields = Vec::new();
                let mut active_variant_ref_visit_fields = Vec::new();
                let mut variant_value_visit_fields = Vec::new();
                let mut variant_value_visit_field_types = Vec::new();
                let mut variant_fixed_type_value_visit_field_fixed_types = Vec::new();
                for Field {
                    attrs: field_attrs,
                    vis: field_vis,
                    ident: field_name,
                    colon_token: _,
                    ty: field_type,
                } in fields.named
                {
                    let RustHdlFieldAttributes { ignored } =
                        RustHdlFieldAttributes::parse(&field_attrs)?;
                    if ignored {
                        continue;
                    }
                    let field_vis_in_mod = struct_field_vis_in_mod(field_vis)?;
                    let field_name = field_name.unwrap();
                    let field_name_str = field_name.to_string();
                    let field_index = struct_of_variant_values_body_fields.len();
                    struct_of_variant_values_body_fields.push(quote! {
                        #field_name: #crate_path::values::ops::extract_aggregate_field_unchecked(aggregate, 0, #field_index),
                    });
                    visit_variants_body_fields.push(quote! {
                        #field_name: #crate_path::values::Value::get_value(&self.#field_name, ctx),
                    });
                    struct_of_variant_values_fields.push(quote! {
                        #field_vis_in_mod #field_name: #crate_path::values::Val<#ctx_lifetime, #field_type>,
                    });
                    active_variant_ref_visit_fields.push(quote! {
                        let visitor = visitor.visit(#field_name_str, self.#field_name)?;
                    });
                    variant_value_visit_fields.push(quote! {
                        let visitor = visitor.visit(#field_name_str, self.#field_name)?;
                    });
                    variant_value_visit_field_types.push(quote! {
                        let visitor = visitor.visit::<#field_type>(#field_name_str)?;
                    });
                    variant_fixed_type_value_visit_field_fixed_types.push(quote! {
                        let visitor = visitor.visit::<#field_type>(#field_name_str)?;
                    });
                }
                struct_of_variant_values_body = quote! {
                    __StructOfVariantValues::#ty_generics_with_ctx {
                        #(#struct_of_variant_values_body_fields)*
                        __aggregate_phantom: ::core::marker::PhantomData,
                    }
                };
                visit_variants_body = quote! {
                    visitor.visit_active_variant(#crate_path::values::aggregate::Variant {
                        name: "",
                        value: __StructOfVariantValues::#ty_generics_with_ctx {
                            #(#visit_variants_body_fields)*
                            __aggregate_phantom: ::core::marker::PhantomData,
                        },
                    })
                };
                value_items.push(quote! {
                    #item_vis_in_mod struct __StructOfVariantValues #generics_with_ctx #where_clause_with_value_bound {
                        #(#struct_of_variant_values_fields)*
                        #item_vis_in_mod __aggregate_phantom: ::core::marker::PhantomData<(#name #original_ty_generics, &#ctx_lifetime ())>,
                    }
                });
                value_items.push(quote! {
                    #[automatically_derived]
                    impl #impl_generics_with_ctx #crate_path::values::aggregate::ActiveVariantRef<#ctx_lifetime> for __StructOfVariantValues #ty_generics_with_ctx #where_clause_with_value_bound {
                        type Aggregate = #name #original_ty_generics;
                        type DiscriminantShape = #discriminant_shape;
                        fn discriminant() -> #crate_path::values::Int<Self::DiscriminantShape> {
                            #crate_path::values::Int::unchecked_new(0)
                        }
                        fn visit_fields<Visitor: #crate_path::values::aggregate::ActiveFieldVisitor<#ctx_lifetime, Self>>(
                            self,
                            visitor: Visitor,
                        ) -> ::core::result::Result<Visitor, Visitor::BreakType> {
                            #(#active_variant_ref_visit_fields)*
                            ::core::result::Result::Ok(visitor)
                        }
                    }
                });
                value_items.push(quote! {
                    #[automatically_derived]
                    impl #impl_generics_with_ctx #crate_path::values::aggregate::VariantValue<#ctx_lifetime> for __StructOfVariantValues #ty_generics_with_ctx #where_clause_with_value_bound {
                        type Aggregate = #name #original_ty_generics;
                        type DiscriminantShape = #discriminant_shape;
                        fn discriminant() -> #crate_path::values::Int<Self::DiscriminantShape> {
                            #crate_path::values::Int::unchecked_new(0)
                        }
                        fn visit_fields<Visitor: #crate_path::values::aggregate::FieldValuesVisitor<#ctx_lifetime, Self>>(
                            self,
                            visitor: Visitor,
                        ) -> ::core::result::Result<Visitor, Visitor::BreakType> {
                            #(#variant_value_visit_fields)*
                            ::core::result::Result::Ok(visitor)
                        }
                        fn visit_field_types<Visitor: #crate_path::values::aggregate::FieldValueTypesVisitor<#ctx_lifetime, Self>>(
                            visitor: Visitor,
                        ) -> ::core::result::Result<Visitor, Visitor::BreakType> {
                            #(#variant_value_visit_field_types)*
                            ::core::result::Result::Ok(visitor)
                        }
                        fn visit_variants_with_self_as_active_variant<Visitor: #crate_path::values::aggregate::VariantVisitor<#ctx_lifetime, Self::Aggregate>>(
                            self,
                            visitor: Visitor,
                        ) -> ::core::result::Result<Visitor::AfterActiveVariant, Visitor::BreakType> {
                            visitor.visit_active_variant(#crate_path::values::aggregate::Variant { name: "", value: self })
                        }
                    }
                });
                fixed_type_value_items.push(quote! {
                    #[automatically_derived]
                    impl #impl_generics_with_ctx #crate_path::values::aggregate::VariantFixedTypeValue<#ctx_lifetime> for __StructOfVariantValues #ty_generics_with_ctx #where_clause_with_fixed_type_value_bound {
                        type Aggregate = #name #original_ty_generics;
                        fn visit_field_fixed_types<Visitor: #crate_path::values::aggregate::FieldValueFixedTypesVisitor<#ctx_lifetime, Self>>(
                            visitor: Visitor,
                        ) -> ::core::result::Result<Visitor, Visitor::BreakType> {
                            #(#variant_fixed_type_value_visit_field_fixed_types)*
                            ::core::result::Result::Ok(visitor)
                        }
                    }
                });
            }
            Fields::Unnamed(fields) => {
                let mut struct_of_variant_values_body_fields = Vec::new();
                let mut visit_variants_body_fields = Vec::new();
                let mut struct_of_variant_values_fields = Vec::new();
                let mut active_variant_ref_visit_fields = Vec::new();
                let mut variant_value_visit_fields = Vec::new();
                let mut variant_value_visit_field_types = Vec::new();
                let mut variant_fixed_type_value_visit_field_fixed_types = Vec::new();
                let mut unignored_field_index = 0usize;
                for (
                    field_name,
                    Field {
                        attrs: field_attrs,
                        vis: field_vis,
                        ident: _,
                        colon_token: _,
                        ty: field_type,
                    },
                ) in fields.unnamed.into_iter().enumerate()
                {
                    let RustHdlFieldAttributes { ignored } =
                        RustHdlFieldAttributes::parse(&field_attrs)?;
                    let field_name_str = field_name.to_string();
                    let field_name = Index::from(field_name);
                    if ignored {
                        visit_variants_body_fields.push(quote! {
                            (),
                        });
                        struct_of_variant_values_body_fields.push(quote! {
                            (),
                        });
                        struct_of_variant_values_fields.push(quote! {
                            #item_vis_in_mod (),
                        });
                    } else {
                        let field_vis_in_mod = struct_field_vis_in_mod(field_vis)?;
                        visit_variants_body_fields.push(quote! {
                            #crate_path::values::Value::get_value(&self.#field_name, ctx),
                        });
                        struct_of_variant_values_body_fields.push(quote! {
                            #crate_path::values::ops::extract_aggregate_field_unchecked(aggregate, 0, #unignored_field_index),
                        });
                        struct_of_variant_values_fields.push(quote! {
                            #field_vis_in_mod #crate_path::values::Val<#ctx_lifetime, #field_type>,
                        });
                        active_variant_ref_visit_fields.push(quote! {
                            let visitor = visitor.visit(#field_name_str, self.#field_name)?;
                        });
                        variant_value_visit_fields.push(quote! {
                            let visitor = visitor.visit(#field_name_str, self.#field_name)?;
                        });
                        variant_value_visit_field_types.push(quote! {
                            let visitor = visitor.visit::<#field_type>(#field_name_str)?;
                        });
                        variant_fixed_type_value_visit_field_fixed_types.push(quote! {
                            let visitor = visitor.visit::<#field_type>(#field_name_str)?;
                        });
                        unignored_field_index += 1;
                    }
                }
                struct_of_variant_values_body = quote! {
                    __StructOfVariantValues::#ty_generics_with_ctx(
                        #(#struct_of_variant_values_body_fields)*
                        ::core::marker::PhantomData,
                    )
                };
                visit_variants_body = quote! {
                    visitor.visit_active_variant(#crate_path::values::aggregate::Variant {
                        name: "",
                        value: __StructOfVariantValues::#ty_generics_with_ctx(
                            #(#visit_variants_body_fields)*
                            ::core::marker::PhantomData,
                        ),
                    })
                };
                value_items.push(quote! {
                    #item_vis_in_mod struct __StructOfVariantValues #generics_with_ctx(
                        #(#struct_of_variant_values_fields)*
                        #item_vis_in_mod ::core::marker::PhantomData<(#name #original_ty_generics, &#ctx_lifetime ())>,
                    ) #where_clause_with_value_bound;
                });
                value_items.push(quote! {
                    #[automatically_derived]
                    impl #impl_generics_with_ctx #crate_path::values::aggregate::ActiveVariantRef<#ctx_lifetime> for __StructOfVariantValues #ty_generics_with_ctx #where_clause_with_value_bound {
                        type Aggregate = #name #original_ty_generics;
                        type DiscriminantShape = #discriminant_shape;
                        fn discriminant() -> #crate_path::values::Int<Self::DiscriminantShape> {
                            #crate_path::values::Int::unchecked_new(0)
                        }
                        fn visit_fields<Visitor: #crate_path::values::aggregate::ActiveFieldVisitor<#ctx_lifetime, Self>>(
                            self,
                            visitor: Visitor,
                        ) -> ::core::result::Result<Visitor, Visitor::BreakType> {
                            #(#active_variant_ref_visit_fields)*
                            ::core::result::Result::Ok(visitor)
                        }
                    }
                });
                value_items.push(quote! {
                    #[automatically_derived]
                    impl #impl_generics_with_ctx #crate_path::values::aggregate::VariantValue<#ctx_lifetime> for __StructOfVariantValues #ty_generics_with_ctx #where_clause_with_value_bound {
                        type Aggregate = #name #original_ty_generics;
                        type DiscriminantShape = #discriminant_shape;
                        fn discriminant() -> #crate_path::values::Int<Self::DiscriminantShape> {
                            #crate_path::values::Int::unchecked_new(0)
                        }
                        fn visit_fields<Visitor: #crate_path::values::aggregate::FieldValuesVisitor<#ctx_lifetime, Self>>(
                            self,
                            visitor: Visitor,
                        ) -> ::core::result::Result<Visitor, Visitor::BreakType> {
                            #(#variant_value_visit_fields)*
                            ::core::result::Result::Ok(visitor)
                        }
                        fn visit_field_types<Visitor: #crate_path::values::aggregate::FieldValueTypesVisitor<#ctx_lifetime, Self>>(
                            visitor: Visitor,
                        ) -> ::core::result::Result<Visitor, Visitor::BreakType> {
                            #(#variant_value_visit_field_types)*
                            ::core::result::Result::Ok(visitor)
                        }
                        fn visit_variants_with_self_as_active_variant<Visitor: #crate_path::values::aggregate::VariantVisitor<#ctx_lifetime, Self::Aggregate>>(
                            self,
                            visitor: Visitor,
                        ) -> ::core::result::Result<Visitor::AfterActiveVariant, Visitor::BreakType> {
                            visitor.visit_active_variant(#crate_path::values::aggregate::Variant { name: "", value: self })
                        }
                    }
                });
                fixed_type_value_items.push(quote! {
                    #[automatically_derived]
                    impl #impl_generics_with_ctx #crate_path::values::aggregate::VariantFixedTypeValue<#ctx_lifetime> for __StructOfVariantValues #ty_generics_with_ctx #where_clause_with_fixed_type_value_bound {
                        type Aggregate = #name #original_ty_generics;
                        fn visit_field_fixed_types<Visitor: #crate_path::values::aggregate::FieldValueFixedTypesVisitor<#ctx_lifetime, Self>>(
                            visitor: Visitor,
                        ) -> ::core::result::Result<Visitor, Visitor::BreakType> {
                            #(#variant_fixed_type_value_visit_field_fixed_types)*
                            ::core::result::Result::Ok(visitor)
                        }
                    }
                });
            }
            Fields::Unit => {
                struct_of_variant_values_body = quote! {
                    __StructOfVariantValues::#ty_generics_with_ctx(::core::marker::PhantomData)
                };
                visit_variants_body = quote! {
                    visitor.visit_active_variant(#crate_path::values::aggregate::Variant {
                        name: "",
                        value: __StructOfVariantValues::#ty_generics_with_ctx(::core::marker::PhantomData),
                    })
                };
                value_items.push(quote! {
                    #item_vis_in_mod struct __StructOfVariantValues #generics_with_ctx(
                        #item_vis_in_mod ::core::marker::PhantomData<(#name #original_ty_generics, &#ctx_lifetime ())>,
                    ) #where_clause_with_value_bound;
                });
                value_items.push(quote! {
                    #[automatically_derived]
                    impl #impl_generics_with_ctx #crate_path::values::aggregate::ActiveVariantRef<#ctx_lifetime> for __StructOfVariantValues #ty_generics_with_ctx #where_clause_with_value_bound {
                        type Aggregate = #name #original_ty_generics;
                        type DiscriminantShape = #discriminant_shape;
                        fn discriminant() -> #crate_path::values::Int<Self::DiscriminantShape> {
                            #crate_path::values::Int::unchecked_new(0)
                        }
                        fn visit_fields<Visitor: #crate_path::values::aggregate::ActiveFieldVisitor<#ctx_lifetime, Self>>(
                            self,
                            visitor: Visitor,
                        ) -> ::core::result::Result<Visitor, Visitor::BreakType> {
                            ::core::result::Result::Ok(visitor)
                        }
                    }
                });
                value_items.push(quote! {
                    #[automatically_derived]
                    impl #impl_generics_with_ctx #crate_path::values::aggregate::VariantValue<#ctx_lifetime> for __StructOfVariantValues #ty_generics_with_ctx #where_clause_with_value_bound {
                        type Aggregate = #name #original_ty_generics;
                        type DiscriminantShape = #discriminant_shape;
                        fn discriminant() -> #crate_path::values::Int<Self::DiscriminantShape> {
                            #crate_path::values::Int::unchecked_new(0)
                        }
                        fn visit_fields<Visitor: #crate_path::values::aggregate::FieldValuesVisitor<#ctx_lifetime, Self>>(
                            self,
                            visitor: Visitor,
                        ) -> ::core::result::Result<Visitor, Visitor::BreakType> {
                            ::core::result::Result::Ok(visitor)
                        }
                        fn visit_field_types<Visitor: #crate_path::values::aggregate::FieldValueTypesVisitor<#ctx_lifetime, Self>>(
                            visitor: Visitor,
                        ) -> ::core::result::Result<Visitor, Visitor::BreakType> {
                            ::core::result::Result::Ok(visitor)
                        }
                        fn visit_variants_with_self_as_active_variant<Visitor: #crate_path::values::aggregate::VariantVisitor<#ctx_lifetime, Self::Aggregate>>(
                            self,
                            visitor: Visitor,
                        ) -> ::core::result::Result<Visitor::AfterActiveVariant, Visitor::BreakType> {
                            visitor.visit_active_variant(#crate_path::values::aggregate::Variant { name: "", value: self })
                        }
                    }
                });
                fixed_type_value_items.push(quote! {
                    #[automatically_derived]
                    impl #impl_generics_with_ctx #crate_path::values::aggregate::VariantFixedTypeValue<#ctx_lifetime> for __StructOfVariantValues #ty_generics_with_ctx #where_clause_with_fixed_type_value_bound {
                        type Aggregate = #name #original_ty_generics;
                        fn visit_field_fixed_types<Visitor: #crate_path::values::aggregate::FieldValueFixedTypesVisitor<#ctx_lifetime, Self>>(
                            visitor: Visitor,
                        ) -> ::core::result::Result<Visitor, Visitor::BreakType> {
                            ::core::result::Result::Ok(visitor)
                        }
                    }
                });
            }
        }
        value_items.push(quote! {
            #[automatically_derived]
            impl #impl_generics_with_ctx ::core::clone::Clone for __StructOfVariantValues #ty_generics_with_ctx #where_clause_with_value_bound {
                fn clone(&self) -> Self {
                    *self
                }
            }
        });
        value_items.push(quote! {
            #[automatically_derived]
            impl #impl_generics_with_ctx ::core::marker::Copy for __StructOfVariantValues #ty_generics_with_ctx #where_clause_with_value_bound {}
        });
        value_items.push(quote! {
            #[automatically_derived]
            impl #impl_generics_with_ctx #crate_path::values::aggregate::StructOfVariantValues<#ctx_lifetime> for __StructOfVariantValues #ty_generics_with_ctx #where_clause_with_value_bound {
                type Aggregate = #name #original_ty_generics;
                fn visit_variant_values<Visitor: #crate_path::values::aggregate::VariantValuesVisitor<#ctx_lifetime, Self>>(
                    self,
                    visitor: Visitor,
                ) -> ::core::result::Result<Visitor, Visitor::BreakType> {
                    visitor.visit(#crate_path::values::aggregate::Variant { name: "", value: self })
                }
                fn visit_variant_types<Visitor: #crate_path::values::aggregate::VariantValueTypesVisitor<#ctx_lifetime, Self>>(
                    visitor: Visitor,
                ) -> ::core::result::Result<Visitor, Visitor::BreakType> {
                    visitor.visit::<Self>(#crate_path::values::aggregate::Variant { name: "", value: () })
                }
            }
        });
        fixed_type_value_items.push(quote! {
            #[automatically_derived]
            impl #impl_generics_with_ctx #crate_path::values::aggregate::StructOfVariantFixedTypeValues<#ctx_lifetime> for __StructOfVariantValues #ty_generics_with_ctx #where_clause_with_fixed_type_value_bound {
                type Aggregate = #name #original_ty_generics;
                fn visit_variant_fixed_types<Visitor: #crate_path::values::aggregate::VariantValueFixedTypesVisitor<#ctx_lifetime, Self>>(
                    visitor: Visitor,
                ) -> ::core::result::Result<Visitor, Visitor::BreakType> {
                    visitor.visit::<Self>(#crate_path::values::aggregate::Variant { name: "", value: () })
                }
            }
        });
        Ok(Self {
            crate_path,
            value_mod,
            fixed_type_value_mod,
            original_generics,
            generics_with_ctx,
            ctx_lifetime,
            name,
            value_items,
            fixed_type_value_items,
            where_clause_with_value_bound,
            where_clause_with_fixed_type_value_bound,
            discriminant_shape,
            struct_of_variant_values_body,
            visit_variants_body,
            item_vis_in_mod,
        })
    }
    fn new_enum(first_step: ValueImplFirstStep<DataEnum>) -> syn::Result<Self> {
        let ValueImplFirstStep {
            crate_path,
            value_mod,
            fixed_type_value_mod,
            original_generics,
            generics_with_ctx,
            ctx_lifetime,
            name,
            where_clause_with_value_bound,
            where_clause_with_fixed_type_value_bound,
            item_vis_in_mod,
            data,
        } = first_step;
        let value_items = todo_err!(name);
        let fixed_type_value_items = todo_err!(name);
        let discriminant_shape = todo_err!(name);
        let struct_of_variant_values_body = todo_err!(name);
        let visit_variants_body = todo_err!(name);
        Ok(Self {
            crate_path,
            value_mod,
            fixed_type_value_mod,
            original_generics,
            generics_with_ctx,
            ctx_lifetime,
            name,
            value_items,
            fixed_type_value_items,
            where_clause_with_value_bound,
            where_clause_with_fixed_type_value_bound,
            discriminant_shape,
            struct_of_variant_values_body,
            visit_variants_body,
            item_vis_in_mod,
        })
    }
    fn new(ast: DeriveInput) -> syn::Result<Self> {
        let DeriveInput {
            attrs,
            vis: item_vis,
            ident,
            generics: original_generics,
            data,
        } = ast;
        let RustHdlAttributes {
            crate_path,
            real_type_name,
        } = RustHdlAttributes::parse(&attrs, AttributesFor::Derive)?;
        let name = real_type_name.unwrap_or(ident);
        let GenericsWithCtx {
            generics: generics_with_ctx,
            ctx_lifetime,
        } = GenericsWithCtx::new(original_generics.clone());
        let where_clause_with_value_bound = where_clause_with_bound_on_generics(
            &original_generics,
            |ident| parse_quote! { #ident: #crate_path::values::Value<#ctx_lifetime> },
        );
        let where_clause_with_fixed_type_value_bound = where_clause_with_bound_on_generics(
            &original_generics,
            |ident| parse_quote! { #ident: #crate_path::values::FixedTypeValue<#ctx_lifetime> },
        );
        let value_mod = format_ident!("__{}__impl_Value", name);
        let fixed_type_value_mod = format_ident!("__{}__impl_FixedTypeValue", name);
        let item_vis_in_mod = item_vis_in_mod(item_vis)?;
        match data {
            Data::Struct(data) => Self::new_struct(ValueImplFirstStep {
                crate_path,
                value_mod,
                fixed_type_value_mod,
                original_generics,
                generics_with_ctx,
                ctx_lifetime,
                name,
                where_clause_with_value_bound,
                where_clause_with_fixed_type_value_bound,
                item_vis_in_mod,
                data,
            }),
            Data::Enum(data) => Self::new_enum(ValueImplFirstStep {
                crate_path,
                value_mod,
                fixed_type_value_mod,
                original_generics,
                generics_with_ctx,
                ctx_lifetime,
                name,
                where_clause_with_value_bound,
                where_clause_with_fixed_type_value_bound,
                item_vis_in_mod,
                data,
            }),
            Data::Union(_) => {
                return Err(Error::new_spanned(
                    name,
                    "#[derive(Value)] can't be used on unions",
                ));
            }
        }
    }
    fn derive_value(self) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            value_mod,
            fixed_type_value_mod: _,
            original_generics,
            generics_with_ctx,
            ctx_lifetime,
            name,
            value_items,
            fixed_type_value_items: _,
            where_clause_with_value_bound,
            where_clause_with_fixed_type_value_bound: _,
            discriminant_shape,
            struct_of_variant_values_body,
            visit_variants_body,
            item_vis_in_mod,
        } = self;
        let (_, original_ty_generics, _) = original_generics.split_for_impl();
        let (impl_generics_with_ctx, ty_generics_with_ctx, _) = generics_with_ctx.split_for_impl();
        let source_location = quote_spanned! {name.span()=>
            fn source_location() -> #crate_path::ir::SourceLocation<'static> {
                #crate_path::ir::SourceLocation::caller()
            }
        };
        Ok(quote! {
            #[allow(non_snake_case)]
            mod #value_mod {
                #![no_implicit_prelude]
                use super::*;
                #(#value_items)*
                #[automatically_derived]
                impl #impl_generics_with_ctx #crate_path::values::aggregate::AggregateValue<#ctx_lifetime> for #name #original_ty_generics #where_clause_with_value_bound {
                    type DiscriminantShape = #discriminant_shape;
                    type StructOfVariantValues = __StructOfVariantValues #ty_generics_with_ctx;
                    #source_location
                    fn struct_of_variant_values(aggregate: #crate_path::values::Val<#ctx_lifetime, Self>) -> Self::StructOfVariantValues {
                        #struct_of_variant_values_body
                    }
                    fn visit_variants<
                        Ctx: #crate_path::context::AsContext<#ctx_lifetime>,
                        Visitor: #crate_path::values::aggregate::VariantVisitor<#ctx_lifetime, Self>,
                    >(
                        &self,
                        ctx: Ctx,
                        visitor: Visitor,
                    ) -> ::core::result::Result<Visitor::AfterActiveVariant, Visitor::BreakType> {
                        let ctx = ctx.ctx();
                        #visit_variants_body
                    }
                }
            }
        })
    }
    fn derive_fixed_type_value(self) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            value_mod,
            fixed_type_value_mod,
            original_generics,
            generics_with_ctx,
            ctx_lifetime,
            name,
            value_items: _,
            fixed_type_value_items,
            where_clause_with_value_bound: _,
            where_clause_with_fixed_type_value_bound,
            discriminant_shape: _,
            struct_of_variant_values_body: _,
            visit_variants_body: _,
            item_vis_in_mod: _,
        } = self;
        let (_, original_ty_generics, _) = original_generics.split_for_impl();
        let (impl_generics_with_ctx, ty_generics_with_ctx, _) = generics_with_ctx.split_for_impl();
        Ok(quote! {
            #[allow(non_snake_case)]
            mod #fixed_type_value_mod {
                #![no_implicit_prelude]
                use super::{*, #value_mod::*};
                #(#fixed_type_value_items)*
                #[automatically_derived]
                impl #impl_generics_with_ctx #crate_path::values::aggregate::FixedTypeAggregateValue<#ctx_lifetime> for #name #original_ty_generics #where_clause_with_fixed_type_value_bound {
                    type StructOfVariantValues = __StructOfVariantValues #ty_generics_with_ctx;
                }
            }
        })
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
    let GenericsWithCtx {
        generics,
        ctx_lifetime,
    } = GenericsWithCtx::new(ast.generics.clone());
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
                let name = Index::from(name);
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
    let GenericsWithCtx {
        generics,
        ctx_lifetime,
    } = GenericsWithCtx::new(ast.generics.clone());
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
