// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use proc_macro2::{Ident, Literal, Span, TokenStream};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    Attribute, Data, DeriveInput, Error, Fields, GenericParam, Generics, Lifetime, LifetimeDef,
    Path, Token,
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

fn get_or_add_ctx_lifetime(mut generics: Generics) -> (Generics, Lifetime) {
    let ctx_lifetime = generics.lifetimes().find_map(|l| {
        if l.lifetime.ident == "ctx" {
            Some(l.lifetime.clone())
        } else {
            None
        }
    });
    let ctx_lifetime = match ctx_lifetime {
        Some(v) => v,
        None => {
            let ctx_lifetime = Lifetime::new("'ctx", Span::call_site());
            generics
                .params
                .push(GenericParam::Lifetime(LifetimeDef::new(
                    ctx_lifetime.clone(),
                )));
            ctx_lifetime
        }
    };
    (generics, ctx_lifetime)
}

fn derive_value_impl(ast: DeriveInput) -> syn::Result<TokenStream> {
    let rust_hdl_attributes = RustHdlAttributes::parse(&ast.attrs)?;
    let crate_path = rust_hdl_attributes.get_crate_path();
    let (_, ty_generics, where_clause) = ast.generics.split_for_impl();
    let (generics, ctx_lifetime) = get_or_add_ctx_lifetime(ast.generics.clone());
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
    let impl_generics = generics.split_for_impl().0;
    let enum_def;
    let fields_const;
    let mut field_value_ir_matches = Vec::new();
    let mut field_static_value_type_ir_matches = Vec::new();
    let struct_of_field_enums_const;
    match data_struct.fields {
        Fields::Named(fields) => {
            let mut enum_fields = Vec::new();
            let mut field_descriptors = Vec::new();
            let mut struct_of_field_enums_fields = Vec::new();
            let mut struct_of_field_enums_const_fields = Vec::new();
            for field in &fields.named {
                let name = field.ident.as_ref().unwrap();
                let vis = &field.vis;
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
                enum_fields.push(quote! {#name,});
                struct_of_field_enums_fields.push(quote! {
                    #vis #name: __FieldEnum,
                });
                struct_of_field_enums_const_fields.push(quote! {
                    #name: __FieldEnum::#name,
                });
            }
            fields_const = quote! {&[#(#field_descriptors),*]};
            struct_of_field_enums_const = quote! {
                __StructOfFieldEnums {
                    #(#struct_of_field_enums_const_fields)*
                }
            };
            enum_def = quote! {
                #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
                #[allow(non_camel_case_types)]
                #[repr(usize)]
                pub enum __FieldEnum {
                    #(#enum_fields)*
                }

                impl ::core::convert::From<__FieldEnum> for usize {
                    fn from(v: __FieldEnum) -> Self {
                        v as usize
                    }
                }

                #[allow(non_snake_case)]
                pub struct __StructOfFieldEnums {
                    #(#struct_of_field_enums_fields)*
                }
            };
        }
        Fields::Unnamed(fields) => {
            let mut field_descriptors = Vec::new();
            let mut struct_of_field_enums_fields = Vec::new();
            let mut struct_of_field_enums_const_fields = Vec::new();
            for (name, field) in fields.unnamed.iter().enumerate() {
                let name_str = name.to_string();
                let name = Literal::usize_unsuffixed(name);
                let vis = &field.vis;
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
                struct_of_field_enums_fields.push(quote! {
                    #vis __FieldEnum,
                });
                struct_of_field_enums_const_fields.push(quote! {
                    #name,
                });
            }
            let fallback_match_arm = quote! {
                _ => ::core::panic!("field index out of bounds"),
            };
            field_value_ir_matches.push(fallback_match_arm.clone());
            field_static_value_type_ir_matches.push(fallback_match_arm);
            fields_const = quote! {&[#(#field_descriptors),*]};
            enum_def = quote! {
                #[allow(non_camel_case_types)]
                pub type __FieldEnum = usize;

                #[allow(non_snake_case)]
                pub struct __StructOfFieldEnums(
                    #(#struct_of_field_enums_fields)*
                );
            };
            struct_of_field_enums_const = quote! {
                __StructOfFieldEnums(
                    #(#struct_of_field_enums_const_fields)*
                )
            };
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

                pub struct __StructOfFieldEnums;
            };
            fields_const = quote! { &[] };
            struct_of_field_enums_const = quote! { __StructOfFieldEnums };
        }
    }
    Ok(quote! {
        const _: () = {
            #enum_def
            #[automatically_derived]
            impl #impl_generics #crate_path::values::StructValue<#ctx_lifetime> for #name #ty_generics #where_clause {
                type FieldEnum = __FieldEnum;
                type StructOfFieldEnums = __StructOfFieldEnums;
                const STRUCT_OF_FIELD_ENUMS: Self::StructOfFieldEnums = #struct_of_field_enums_const;
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

fn derive_io_impl(ast: DeriveInput) -> syn::Result<TokenStream> {
    let rust_hdl_attributes = RustHdlAttributes::parse(&ast.attrs)?;
    let crate_path = rust_hdl_attributes.get_crate_path();
    let (_, ty_generics, where_clause) = ast.generics.split_for_impl();
    let (generics, ctx_lifetime) = get_or_add_ctx_lifetime(ast.generics.clone());
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

#[proc_macro_derive(Value, attributes(rust_hdl))]
pub fn derive_value(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    match derive_value_impl(ast) {
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
