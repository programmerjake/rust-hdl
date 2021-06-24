// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use proc_macro2::{Ident, Literal, Span, TokenStream};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    Attribute, Data, DeriveInput, Error, Fields, GenericParam, Lifetime, LifetimeDef, Path, Token,
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
}

fn derive_value_impl(mut ast: DeriveInput) -> syn::Result<TokenStream> {
    let RustHdlAttributes { crate_path } = RustHdlAttributes::parse(&ast.attrs)?;
    let crate_path = crate_path.unwrap_or_else(|| Ident::new("rust_hdl", Span::call_site()).into());
    let ctx_lifetime = ast.generics.lifetimes().find_map(|l| {
        if l.lifetime.ident == "ctx" {
            Some(l.lifetime.clone())
        } else {
            None
        }
    });
    let old_generics = ast.generics.clone();
    let (_, ty_generics, where_clause) = old_generics.split_for_impl();
    let ctx_lifetime = match ctx_lifetime {
        Some(v) => v,
        None => {
            let ctx_lifetime = Lifetime::new("'ctx", Span::call_site());
            ast.generics
                .params
                .push(GenericParam::Lifetime(LifetimeDef::new(
                    ctx_lifetime.clone(),
                )));
            ctx_lifetime
        }
    };
    let name = ast.ident;
    let data_struct = match ast.data {
        Data::Struct(v) => v,
        _ => {
            return Err(Error::new_spanned(
                name,
                "#[derive(Value)] only valid on structs",
            ))
        }
    };
    let impl_generics = ast.generics.split_for_impl().0;
    let enum_def;
    let fields_const;
    let mut field_value_ir_matches = Vec::new();
    let mut field_static_value_type_ir_matches = Vec::new();
    match data_struct.fields {
        Fields::Named(fields) => {
            let enum_fields: Vec<_> = fields
                .named
                .iter()
                .map(|field| field.ident.clone())
                .collect();
            enum_def = quote! {
                #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
                #[allow(non_camel_case_types)]
                #[repr(usize)]
                pub enum __FieldEnum {
                    #(#enum_fields),*
                }

                impl ::core::convert::From<__FieldEnum> for usize {
                    fn from(v: __FieldEnum) -> Self {
                        v as usize
                    }
                }
            };
            let mut field_descriptors = Vec::new();
            for field in &fields.named {
                let name = field.ident.as_ref().unwrap();
                let name_str = name.to_string();
                field_descriptors.push(quote! {
                    #crate_path::values::StructFieldDescriptor {
                        name: #name_str,
                        field: __FieldEnum::#name,
                    }
                });
                let field_ty = &field.ty;
                field_value_ir_matches.push(quote! {
                    __FieldEnum::#name => #crate_path::values::Value::get_value(&self.#name, ctx).ir(),
                });
                field_static_value_type_ir_matches.push(quote! {
                    __FieldEnum::#name => ::core::option::Option::Some(<#field_ty as #crate_path::values::Value>::static_value_type(ctx)?.ir()),
                });
            }
            fields_const = quote! {&[#(#field_descriptors),*]};
        }
        Fields::Unnamed(fields) => {
            enum_def = quote! {
                #[allow(non_camel_case_types)]
                pub type __FieldEnum = usize;
            };
            let mut field_descriptors = Vec::new();
            for (name, field) in fields.unnamed.iter().enumerate() {
                let name_str = name.to_string();
                let name = Literal::usize_unsuffixed(name);
                field_descriptors.push(quote! {
                    #crate_path::values::StructFieldDescriptor {
                        name: #name_str,
                        field: #name,
                    }
                });
                let field_ty = &field.ty;
                field_value_ir_matches.push(quote! {
                    #name => #crate_path::values::Value::get_value(&self.#name, ctx).ir(),
                });
                field_static_value_type_ir_matches.push(quote! {
                    #name => ::core::option::Option::Some(<#field_ty as #crate_path::values::Value>::static_value_type(ctx)?.ir()),
                });
            }
            let fallback_match_arm = quote! {
                _ => ::core::unreachable!("field index out of bounds"),
            };
            field_value_ir_matches.push(fallback_match_arm.clone());
            field_static_value_type_ir_matches.push(fallback_match_arm);
            fields_const = quote! {&[#(#field_descriptors),*]};
        }
        Fields::Unit => {
            enum_def = quote! {
                #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
                #[allow(non_camel_case_types)]
                pub enum __FieldEnum {}

                impl ::core::convert::From<__FieldEnum> for usize {
                    fn from(v: __FieldEnum) -> Self {
                        match v {}
                    }
                }
            };
            fields_const = quote! { &[] };
        }
    }
    Ok(quote! {
        const _: () = {
            #enum_def
            #[automatically_derived]
            impl #impl_generics #crate_path::values::StructValue<#ctx_lifetime> for #name #ty_generics #where_clause {
                type FieldEnum = __FieldEnum;
                const FIELDS: &'static [#crate_path::values::StructFieldDescriptor<'static, Self::FieldEnum>] = #fields_const;
                fn field_value_ir(&self, ctx: #crate_path::context::ContextRef<'ctx>, field: Self::FieldEnum) -> #crate_path::ir::values::IrValueRef<'ctx> {
                    match field {
                        #(#field_value_ir_matches)*
                    }
                }
                fn field_static_value_type_ir(
                    ctx: #crate_path::context::ContextRef<'ctx>,
                    field: Self::FieldEnum,
                ) -> Option<#crate_path::ir::types::IrValueTypeRef<'ctx>> {
                    match field {
                        #(#field_static_value_type_ir_matches)*
                    }
                }
            }
        };
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
