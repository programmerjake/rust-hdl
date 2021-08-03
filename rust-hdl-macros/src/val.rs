// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{AttributesFor, RustHdlAttributes};
use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;
use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use rust_hdl_int::{Int, IntShape};
use std::{
    cmp::Ordering,
    collections::{
        btree_map::{self, Entry},
        BTreeMap,
    },
    convert::TryInto,
    iter,
};
use syn::{
    parse::{Parse, ParseStream},
    parse_quote,
    punctuated::{Pair, Punctuated},
    spanned::Spanned,
    Arm, Attribute, BinOp, Block, Error, Expr, ExprArray, ExprBinary, ExprBlock, ExprField,
    ExprGroup, ExprIf, ExprLit, ExprMatch, ExprUnary, Index, Lit, LitBool, LitByte, LitByteStr,
    LitInt, Local, Pat, PatIdent, PatLit, PatOr, PatPath, PatRange, PatTuple, PatTupleStruct,
    PatWild, Path, QSelf, RangeLimits, Stmt, Token, TypePath, UnOp,
};

#[derive(Clone)]
struct IntLiteral {
    span: Span,
    value: BigInt,
    shape: Option<IntShape>,
}

impl IntLiteral {
    fn parse(sign: Option<&Token![-]>, lit: &LitInt) -> syn::Result<Self> {
        let span = lit.span();
        let mut value: BigInt = lit.base10_parse()?;
        if sign.is_some() {
            value = -value;
        }
        if lit.suffix().is_empty() {
            return Ok(IntLiteral {
                span,
                value,
                shape: None,
            });
        }
        let shape: IntShape = lit
            .suffix()
            .parse()
            .map_err(|e| Error::new_spanned(&lit, format!("invalid literal type: {}", e)))?;
        let mut adjusted_shape = shape;
        let adjusted_shape = loop {
            if value == Int::wrapping_with_shape(value.clone(), adjusted_shape).into_value() {
                break Some(adjusted_shape);
            }
            if let Some(bit_count) = adjusted_shape.bit_count.checked_add(1) {
                // avoid many iterations by using bits() to get a value close to the correct answer
                if let Ok(bits) = value.bits().try_into() {
                    adjusted_shape.bit_count = bit_count.max(bits);
                    continue;
                }
            }
            break None;
        };
        if adjusted_shape != Some(shape) {
            Err(Error::new_spanned(
                quote! {#sign #lit},
                if let Some(adjusted_shape) = adjusted_shape {
                    format!(
                        "literal out of range for `{}`, consider using `{}` instead",
                        shape, adjusted_shape,
                    )
                } else {
                    format!("literal out of range for `{}`", shape)
                },
            ))
        } else {
            Ok(IntLiteral {
                span,
                value,
                shape: Some(shape),
            })
        }
    }
    fn to_tokens<'a>(&'a self, crate_path: &'a Path) -> IntLiteralTokens<'a> {
        IntLiteralTokens {
            int_literal: self,
            crate_path,
        }
    }
}

struct IntLiteralTokens<'a> {
    int_literal: &'a IntLiteral,
    crate_path: &'a Path,
}

impl ToTokens for IntLiteralTokens<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.to_token_stream().to_tokens(tokens);
    }
    fn to_token_stream(&self) -> TokenStream {
        let Self {
            int_literal,
            crate_path,
        } = *self;
        let IntLiteral { span, value, shape } = int_literal.clone();
        let value = if let Some(value) = value.to_i128() {
            quote_spanned! {span=>
                #value
            }
        } else {
            let (sign, magnitude) = value.into_parts();
            let sign = match sign {
                Sign::Minus => quote_spanned! {span=>
                    #crate_path::bigint::Sign::Minus
                },
                Sign::NoSign => quote_spanned! {span=>
                    #crate_path::bigint::Sign::NoSign
                },
                Sign::Plus => quote_spanned! {span=>
                    #crate_path::bigint::Sign::Plus
                },
            };
            let digits = magnitude.to_u32_digits();
            quote_spanned! {span=>
                #crate_path::bigint::BigInt::from_slice(#sign, &[#(#digits,)*])
            }
        };
        match shape {
            None => quote_spanned! {span=>
                #crate_path::values::integer::Int::new(#value).expect("literal out of range")
            },
            Some(IntShape { bit_count, signed }) => quote_spanned! {span=>
                #crate_path::values::integer::Int::<#crate_path::values::integer::ConstIntShape<#bit_count, #signed>>::new(#value).expect("literal out of range")
            },
        }
    }
}

fn assert_no_attrs(attrs: impl AsRef<[Attribute]>) -> syn::Result<()> {
    if let [attr, ..] = attrs.as_ref() {
        Err(Error::new_spanned(attr, "attributes not supported here"))
    } else {
        Ok(())
    }
}

#[derive(Default)]
struct TempNameMaker {
    next_index: usize,
}

impl TempNameMaker {
    fn new() -> Self {
        Self::default()
    }
    fn make_temp_name(&mut self, span: Span) -> Ident {
        let index = self.next_index;
        self.next_index += 1;
        Ident::new(&format!("__temp{}", index), span)
    }
}

#[derive(Clone, Debug)]
enum MatchCondition {
    Static(bool),
    Dynamic(Ident),
}

struct PatternMatcher<'a> {
    val_translator: &'a ValTranslator,
    tokens: TokenStream,
    temp_name_maker: TempNameMaker,
}

impl<'a> PatternMatcher<'a> {
    fn new(val_translator: &'a ValTranslator) -> Self {
        Self {
            val_translator,
            tokens: quote! {},
            temp_name_maker: TempNameMaker::new(),
        }
    }
}

struct PatternDefinedNames(BTreeMap<Ident, Ident>);

impl IntoIterator for PatternDefinedNames {
    type Item = (Ident, Ident);
    type IntoIter = btree_map::IntoIter<Ident, Ident>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a PatternDefinedNames {
    type Item = (&'a Ident, &'a Ident);
    type IntoIter = btree_map::Iter<'a, Ident, Ident>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a> IntoIterator for &'a mut PatternDefinedNames {
    type Item = (&'a Ident, &'a mut Ident);
    type IntoIter = btree_map::IterMut<'a, Ident, Ident>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl PatternDefinedNames {
    fn new() -> Self {
        Self(BTreeMap::new())
    }
    fn define_name(&mut self, name: Ident, value: Ident) -> syn::Result<&mut Ident> {
        match self.0.entry(name) {
            Entry::Occupied(entry) => Err(Error::new_spanned(
                entry.key(),
                format_args!(
                    "identifier `{}` is bound more than once in the same pattern",
                    entry.key()
                ),
            )),
            Entry::Vacant(entry) => Ok(entry.insert(value)),
        }
    }
    fn define_names(&mut self, names: impl IntoIterator<Item = (Ident, Ident)>) -> syn::Result<()> {
        names.into_iter().try_for_each(|(name, value)| {
            self.define_name(name, value)?;
            Ok(())
        })
    }
    fn check_same_names<'a, E>(
        &'a self,
        later_defined_names: &'a Self,
        handle_earlier_without_later: impl FnOnce(&'a Ident) -> E,
        handle_later_without_earlier: impl FnOnce(&'a Ident) -> E,
    ) -> Result<(), E> {
        let mut earlier_iter = self.0.keys();
        let mut later_iter = later_defined_names.0.keys();
        loop {
            match (earlier_iter.next(), later_iter.next()) {
                (None, None) => break,
                (None, Some(later)) => return Err(handle_later_without_earlier(later)),
                (Some(earlier), None) => return Err(handle_earlier_without_later(earlier)),
                (Some(earlier), Some(later)) => match earlier.cmp(later) {
                    Ordering::Less => return Err(handle_earlier_without_later(earlier)),
                    Ordering::Equal => {}
                    Ordering::Greater => return Err(handle_later_without_earlier(later)),
                },
            }
        }
        Ok(())
    }
}

impl Default for PatternDefinedNames {
    fn default() -> Self {
        Self::new()
    }
}

struct PatternMatchResult {
    condition: MatchCondition,
    defined_names: PatternDefinedNames,
    verification_match: Pat,
    verification_match_needs_if: bool,
}

enum TypePathKind {
    EnumVariant {
        span: Span,
        enum_type: TypePath,
        variant_path: PatPath,
    },
    Struct {
        span: Span,
        struct_type: TypePath,
    },
}

impl TypePathKind {
    fn get(qself: Option<QSelf>, path: Path) -> Self {
        match PatPathKind::get_impl(qself, path, false) {
            PatPathKind::Type(retval) => retval,
            PatPathKind::Variable { .. } => unreachable!(),
        }
    }
}

enum PatPathKind {
    Type(TypePathKind),
    Variable { name: Ident },
}

impl PatPathKind {
    fn get(qself: Option<QSelf>, path: Path) -> Self {
        Self::get_impl(qself, path, true)
    }
    fn get_impl(qself: Option<QSelf>, path: Path, can_be_variable: bool) -> Self {
        let span = path
            .segments
            .last()
            .expect("parser won't return empty path")
            .span();
        if path.segments.len() >= 2 {
            let mut enum_type = path.clone();
            enum_type.segments.pop();
            // work around https://github.com/dtolnay/syn/issues/1043
            let enum_type_name = enum_type.segments.pop().unwrap().into_value();
            enum_type.segments.push(enum_type_name);
            let variant_path = PatPath {
                attrs: Vec::new(),
                qself: qself.clone(),
                path,
            };
            let enum_type = TypePath {
                qself,
                path: enum_type,
            };
            Self::Type(TypePathKind::EnumVariant {
                span,
                enum_type,
                variant_path,
            })
        } else if qself.is_some() {
            Self::Type(TypePathKind::Struct {
                span,
                struct_type: TypePath { qself, path },
            })
        } else if path.is_ident("None") || path.is_ident("Some") {
            Self::Type(TypePathKind::EnumVariant {
                span,
                enum_type: parse_quote! { ::core::option::Option },
                variant_path: PatPath {
                    attrs: Vec::new(),
                    qself,
                    path,
                },
            })
        } else if path.is_ident("Ok") || path.is_ident("Err") {
            Self::Type(TypePathKind::EnumVariant {
                span,
                enum_type: parse_quote! { ::core::result::Result },
                variant_path: PatPath {
                    attrs: Vec::new(),
                    qself,
                    path,
                },
            })
        } else if let Some(ident) = path.get_ident() {
            if can_be_variable
                && ident.to_string().chars().next().map(char::is_uppercase) != Some(true)
            {
                Self::Variable {
                    name: ident.clone(),
                }
            } else {
                Self::Type(TypePathKind::Struct {
                    span,
                    struct_type: TypePath { qself, path },
                })
            }
        } else {
            Self::Type(TypePathKind::Struct {
                span,
                struct_type: TypePath { qself, path },
            })
        }
    }
}

enum PatternLiteral {
    Int(IntLiteral),
    ByteStr(LitByteStr),
    Byte(LitByte),
    Bool(LitBool),
}

impl PatternMatcher<'_> {
    fn condition_or(
        &mut self,
        span: Span,
        conditions: impl IntoIterator<Item = MatchCondition>,
    ) -> MatchCondition {
        conditions
            .into_iter()
            .fold(MatchCondition::Static(false), |lhs, rhs| match (lhs, rhs) {
                (MatchCondition::Static(true), _) => MatchCondition::Static(true),
                (_, MatchCondition::Static(true)) => MatchCondition::Static(true),
                (MatchCondition::Static(false), v) => v,
                (v, MatchCondition::Static(false)) => v,
                (MatchCondition::Dynamic(lhs), MatchCondition::Dynamic(rhs)) => {
                    let temp = self.temp_name_maker.make_temp_name(span);
                    let crate_path = &self.val_translator.crate_path;
                    self.tokens.extend(quote_spanned! {span=>
                        let #temp = #crate_path::values::ops::HdlOr::or(#lhs, #rhs);
                    });
                    MatchCondition::Dynamic(temp)
                }
            })
    }
    fn condition_and(
        &mut self,
        span: Span,
        conditions: impl IntoIterator<Item = MatchCondition>,
    ) -> MatchCondition {
        conditions
            .into_iter()
            .fold(MatchCondition::Static(true), |lhs, rhs| match (lhs, rhs) {
                (MatchCondition::Static(false), _) => MatchCondition::Static(false),
                (_, MatchCondition::Static(false)) => MatchCondition::Static(false),
                (MatchCondition::Static(true), v) => v,
                (v, MatchCondition::Static(true)) => v,
                (MatchCondition::Dynamic(lhs), MatchCondition::Dynamic(rhs)) => {
                    let temp = self.temp_name_maker.make_temp_name(span);
                    let crate_path = &self.val_translator.crate_path;
                    self.tokens.extend(quote_spanned! {span=>
                        let #temp = #crate_path::values::ops::HdlAnd::and(#lhs, #rhs);
                    });
                    MatchCondition::Dynamic(temp)
                }
            })
    }
    fn pat_lit_expr_lit(
        &mut self,
        expr_lit: &ExprLit,
        sign: Option<&Token![-]>,
    ) -> syn::Result<PatternLiteral> {
        let ExprLit { attrs, lit } = expr_lit;
        assert_no_attrs(attrs)?;
        match lit {
            Lit::Int(lit) => Ok(PatternLiteral::Int(IntLiteral::parse(sign, lit)?)),
            Lit::Float(lit) => Err(Error::new_spanned(lit, "float literals are not supported")),
            _ if sign.is_some() => Err(Error::new_spanned(sign, "literal not supported")),
            Lit::ByteStr(lit) => Ok(PatternLiteral::ByteStr(lit.clone())),
            Lit::Byte(lit) => Ok(PatternLiteral::Byte(lit.clone())),
            Lit::Bool(lit) => Ok(PatternLiteral::Bool(lit.clone())),
            Lit::Str(lit) => Err(Error::new_spanned(
                lit,
                "string literals are not supported, \
                did you mean to use byte strings instead? (`b\"...\"`)",
            )),
            Lit::Char(lit) => Err(Error::new_spanned(
                lit,
                "char literals are not supported, \
                did you mean to use byte literals instead? (`b\'...\'`)",
            )),
            Lit::Verbatim(lit) => Err(Error::new_spanned(lit, "literal not supported")),
        }
    }
    fn pat_lit_expr(
        &mut self,
        expr: &Expr,
        sign: Option<&Token![-]>,
    ) -> syn::Result<PatternLiteral> {
        match (expr, sign) {
            (Expr::Lit(expr_lit), sign) => self.pat_lit_expr_lit(expr_lit, sign),
            (
                Expr::Group(ExprGroup {
                    attrs,
                    group_token: _,
                    expr,
                }),
                sign,
            ) => {
                assert_no_attrs(attrs)?;
                self.pat_lit_expr(expr, sign)
            }
            (
                Expr::Unary(ExprUnary {
                    attrs,
                    op: UnOp::Neg(sign),
                    expr,
                }),
                None,
            ) => {
                assert_no_attrs(attrs)?;
                self.pat_lit_expr(expr, Some(sign))
            }
            _ => Err(Error::new_spanned(
                sign.map_or_else(|| expr as &dyn ToTokens, |sign| sign),
                "unsupported match pattern",
            )),
        }
    }
    fn pat_path(&mut self, pat_path: PatPath, value: &Ident) -> syn::Result<PatternMatchResult> {
        let ValTranslator {
            crate_path,
            module,
            scope: _,
        } = self.val_translator;
        let PatPath { attrs, qself, path } = pat_path;
        assert_no_attrs(attrs)?;
        match PatPathKind::get(qself, path) {
            PatPathKind::Type(TypePathKind::EnumVariant {
                span,
                enum_type,
                variant_path,
            }) => {
                let condition = self.temp_name_maker.make_temp_name(span);
                self.tokens.extend(quote_spanned! {span=>
                    #crate_path::values::ops::assert_variant_is_empty::<_, #enum_type>(#module, &#variant_path);
                    let __variant_index = #crate_path::values::ops::get_aggregate_variant_index::<_, #enum_type>(#module, &#variant_path);
                    let #condition = #crate_path::values::ops::is_aggregate_variant(#value, __variant_index);
                });
                Ok(PatternMatchResult {
                    condition: MatchCondition::Dynamic(condition),
                    defined_names: PatternDefinedNames::new(),
                    verification_match: Pat::Path(variant_path),
                    verification_match_needs_if: false,
                })
            }
            PatPathKind::Type(TypePathKind::Struct { span, struct_type }) => {
                self.tokens.extend(quote_spanned! {span=>
                    #crate_path::values::ops::assert_type_is_aggregate::<#struct_type>();
                });
                let TypePath { qself, path } = struct_type;
                Ok(PatternMatchResult {
                    condition: MatchCondition::Static(true),
                    defined_names: PatternDefinedNames::new(),
                    verification_match: Pat::Path(PatPath {
                        attrs: Vec::new(),
                        qself,
                        path,
                    }),
                    verification_match_needs_if: false,
                })
            }
            PatPathKind::Variable { name } => {
                let mut defined_names = PatternDefinedNames::new();
                defined_names.define_name(name.clone(), value.clone())?;
                Ok(PatternMatchResult {
                    condition: MatchCondition::Static(true),
                    defined_names,
                    verification_match: parse_quote! {#name},
                    verification_match_needs_if: false,
                })
            }
        }
    }
    fn pat_or(&mut self, pat_or: &PatOr, value: &Ident) -> syn::Result<PatternMatchResult> {
        let crate_path = &self.val_translator.crate_path;
        let PatOr {
            attrs,
            leading_vert,
            cases,
        } = pat_or;
        assert_no_attrs(attrs)?;
        if cases.len() == 1 {
            return self.pat(&cases[0], value);
        }
        let mut verification_match_needs_if = false;
        let mut verification_match_cases = Punctuated::new();
        struct ParsedCase {
            condition: MatchCondition,
            defined_names: PatternDefinedNames,
            prev_or: Token![|],
        }
        let mut parsed_cases = Vec::<ParsedCase>::new();
        let mut prev_or = None;
        for (case, case_or) in cases.pairs().map(Pair::into_tuple) {
            let PatternMatchResult {
                condition,
                defined_names,
                verification_match,
                verification_match_needs_if: needs_if,
            } = self.pat(case, value)?;
            if let Some(last_case) = parsed_cases.last() {
                last_case.defined_names.check_same_names(
                    &defined_names,
                    |earlier| {
                        Error::new_spanned(
                            case,
                            format_args!(
                                "variable `{}` is not bound in this pattern even \
                                though it is bound in previous patterns",
                                earlier
                            ),
                        )
                    },
                    |later| {
                        Error::new_spanned(
                            case,
                            format_args!(
                                "variable `{}` is not bound in this pattern even \
                                though it is bound in later patterns",
                                later
                            ),
                        )
                    },
                )?;
            }
            parsed_cases.push(ParsedCase {
                condition,
                defined_names,
                prev_or: prev_or.unwrap_or_default(),
            });
            prev_or = case_or.copied();
            verification_match_needs_if |= needs_if;
            verification_match_cases
                .extend(iter::once(Pair::new(verification_match, case_or.copied())));
        }
        let ParsedCase {
            mut condition,
            mut defined_names,
            prev_or: _,
        } = parsed_cases
            .pop()
            .expect("PatOr always has at least one case");
        for ParsedCase {
            condition: case_condition,
            defined_names: case_defined_names,
            prev_or,
        } in parsed_cases.into_iter().rev()
        {
            match case_condition {
                MatchCondition::Static(false) => continue,
                MatchCondition::Static(true) => {
                    defined_names = case_defined_names;
                    condition = case_condition;
                }
                MatchCondition::Dynamic(case_condition) => {
                    for (name, value) in defined_names.0.iter_mut() {
                        let new_value = self.temp_name_maker.make_temp_name(prev_or.span);
                        let lhs_value = case_defined_names
                            .0
                            .get(name)
                            .expect("already checked for same names above");
                        self.tokens.extend(quote_spanned! {prev_or.span=>
                            let #new_value = #crate_path::values::ops::mux(#case_condition, #lhs_value, #value);
                        });
                        *value = new_value;
                    }
                    condition = self.condition_or(
                        prev_or.span,
                        [condition, MatchCondition::Dynamic(case_condition)],
                    );
                }
            }
        }
        Ok(PatternMatchResult {
            condition,
            defined_names,
            verification_match: Pat::Or(PatOr {
                attrs: Vec::new(),
                leading_vert: leading_vert.clone(),
                cases: verification_match_cases,
            }),
            verification_match_needs_if,
        })
    }
    fn pat_tuple(
        &mut self,
        type_path_kind: Option<TypePathKind>,
        pat_tuple: &PatTuple,
        value: &Ident,
    ) -> syn::Result<PatternMatchResult> {
        let crate_path = &self.val_translator.crate_path;
        todo_err!(pat_tuple);
        let PatTuple {
            attrs,
            paren_token,
            elems,
        } = pat_tuple;
        assert_no_attrs(attrs)?;
        let fields = self.temp_name_maker.make_temp_name(paren_token.span);
        match type_path_kind {
            None|Some(TypePathKind::Struct{..})=>
            self.tokens.extend(quote_spanned! {paren_token.span=>
                let #fields = #crate_path::values::aggregate::StructValue::get_field_values(#value);
            }),
            Some(TypePathKind::EnumVariant{variant_path,..})=>todo_err!(variant_path, "enum variant match"),
        }
        let mut verification_match_elems = Punctuated::new();
        let mut last_separator_span = paren_token.span;
        let mut defined_names = PatternDefinedNames::new();
        let mut verification_match_needs_if = false;
        let mut condition = MatchCondition::Static(true);
        for (index, field) in elems.pairs().enumerate() {
            let field_variable = self.temp_name_maker.make_temp_name(last_separator_span);
            let index = Index::from(index);
            self.tokens.extend(quote_spanned! {paren_token.span=>
                let #field_variable = #fields.#index;
            });
            let PatternMatchResult {
                condition: field_condition,
                defined_names: field_defined_names,
                verification_match: field_verification_match,
                verification_match_needs_if: field_verification_match_needs_if,
            } = self.pat(field.value(), &field_variable)?;
            condition = self.condition_and(last_separator_span, [condition, field_condition]);
            defined_names.define_names(field_defined_names)?;
            verification_match_elems.extend(iter::once(Pair::new(
                field_verification_match,
                field.punct().copied().copied(),
            )));
            verification_match_needs_if |= field_verification_match_needs_if;
            last_separator_span = field.punct().map_or(paren_token.span, |v| v.span);
        }
        let pat_tuple = PatTuple {
            attrs: Vec::new(),
            paren_token: *paren_token,
            elems: verification_match_elems,
        };
        let verification_match = match type_path_kind {
            None => Pat::Tuple(pat_tuple),
            Some(TypePathKind::Struct {
                span,
                struct_type:
                    struct_type
                    @
                    TypePath {
                        qself: Some(_),
                        path: _,
                    },
            }) => todo_err!(struct_type, "qualified self"),
            Some(TypePathKind::Struct {
                span: _,
                struct_type: TypePath { qself: None, path },
            }) => Pat::TupleStruct(PatTupleStruct {
                attrs: Vec::new(),
                path,
                pat: pat_tuple,
            }),
            Some(TypePathKind::EnumVariant {
                span,
                enum_type,
                variant_path,
            }) => todo_err!(variant_path, "enum variant match"),
        };
        Ok(PatternMatchResult {
            condition,
            defined_names,
            verification_match,
            verification_match_needs_if,
        })
    }
    fn pat(&mut self, pat: &Pat, value: &Ident) -> syn::Result<PatternMatchResult> {
        let crate_path = &self.val_translator.crate_path;
        match pat {
            Pat::Ident(PatIdent {
                attrs,
                by_ref,
                mutability,
                ident,
                subpat,
            }) => {
                assert_no_attrs(attrs)?;
                if let Some(by_ref) = by_ref {
                    return Err(Error::new_spanned(by_ref, "`ref` not supported"));
                }
                if let Some(mutability) = mutability {
                    return Err(Error::new_spanned(mutability, "`mut` not supported"));
                }
                if let Some((_, subpat)) = subpat {
                    let mut retval = self.pat(subpat, value)?;
                    retval
                        .defined_names
                        .define_name(ident.clone(), value.clone())?;
                    Ok(retval)
                } else {
                    self.pat_path(
                        PatPath {
                            attrs: Vec::new(),
                            qself: None,
                            path: ident.clone().into(),
                        },
                        value,
                    )
                }
            }
            Pat::Lit(PatLit { attrs, expr }) => {
                assert_no_attrs(attrs)?;
                let span;
                let lit_value = match self.pat_lit_expr(expr, None)? {
                    PatternLiteral::Int(lit_value) => {
                        span = lit_value.span;
                        lit_value.to_tokens(crate_path).into_token_stream()
                    }
                    PatternLiteral::ByteStr(_value) => todo_err!(pat),
                    PatternLiteral::Byte(value) => {
                        span = value.span();
                        quote_spanned! {span=>
                            <#crate_path::values::integer::UInt8 as ::core::convert::From>::from(#value)
                        }
                    }
                    PatternLiteral::Bool(value) => {
                        span = value.span();
                        quote! { #value }
                    }
                };
                let condition = self.temp_name_maker.make_temp_name(span);
                self.tokens.extend(quote_spanned! {span=>
                    let #condition = #crate_path::values::ops::match_eq(#value, &#lit_value);
                });
                let mut underscore_token = <Token![_]>::default();
                underscore_token.span = span;
                Ok(PatternMatchResult {
                    condition: MatchCondition::Dynamic(condition),
                    defined_names: PatternDefinedNames::new(),
                    verification_match: Pat::Wild(PatWild {
                        attrs: Vec::new(),
                        underscore_token,
                    }),
                    verification_match_needs_if: true,
                })
            }
            Pat::Or(pat_or) => self.pat_or(pat_or, value),
            Pat::Path(pat_path) => self.pat_path(pat_path.clone(), value),
            Pat::Range(PatRange {
                attrs,
                lo,
                limits,
                hi,
            }) => {
                assert_no_attrs(attrs)?;
                let lo = self.pat_lit_expr(lo, None)?;
                let hi = self.pat_lit_expr(hi, None)?;
                let get_endpoint_tokens = |endpoint: PatternLiteral| -> syn::Result<TokenStream> {
                    match endpoint {
                        PatternLiteral::Int(endpoint) => {
                            Ok(endpoint.to_tokens(crate_path).into_token_stream())
                        }
                        PatternLiteral::ByteStr(endpoint) => todo_err!(endpoint),
                        PatternLiteral::Byte(endpoint) => Ok(quote_spanned! {endpoint.span()=>
                            <#crate_path::values::integer::UInt8 as ::core::convert::From>::from(#endpoint)
                        }),
                        PatternLiteral::Bool(endpoint) => Ok(quote! { #endpoint }),
                    }
                };
                let lo = get_endpoint_tokens(lo)?;
                let hi = get_endpoint_tokens(hi)?;
                let (span, condition_expr) = match limits {
                    RangeLimits::HalfOpen(range) => (
                        range.spans[0],
                        quote_spanned! {range.spans[0]=>
                            #crate_path::values::ops::match_range(value, &#lo #range &#hi)
                        },
                    ),
                    RangeLimits::Closed(range) => (
                        range.spans[0],
                        quote_spanned! {range.spans[0]=>
                            #crate_path::values::ops::match_range_inclusive(value, &#lo #range &#hi)
                        },
                    ),
                };
                let condition = self.temp_name_maker.make_temp_name(span);
                self.tokens.extend(quote_spanned! {span=>
                    let #condition = #condition_expr;
                });
                let mut underscore_token = <Token![_]>::default();
                underscore_token.span = span;
                Ok(PatternMatchResult {
                    condition: MatchCondition::Dynamic(condition),
                    defined_names: PatternDefinedNames::new(),
                    verification_match: Pat::Wild(PatWild {
                        attrs: Vec::new(),
                        underscore_token,
                    }),
                    verification_match_needs_if: true,
                })
            }
            Pat::Slice(pat) => todo_err!(pat),
            Pat::Struct(pat) => todo_err!(pat),
            Pat::Tuple(pat_tuple) => self.pat_tuple(None, pat_tuple, value),
            Pat::TupleStruct(PatTupleStruct { attrs, path, pat }) => {
                assert_no_attrs(attrs)?;
                self.pat_tuple(Some(TypePathKind::get(None, path.clone())), pat, value)
            }
            Pat::Wild(PatWild {
                attrs,
                underscore_token: _,
            }) => {
                assert_no_attrs(attrs)?;
                Ok(PatternMatchResult {
                    condition: MatchCondition::Static(true),
                    defined_names: PatternDefinedNames::new(),
                    verification_match: pat.clone(),
                    verification_match_needs_if: false,
                })
            }
            Pat::Rest(pat) => todo_err!(pat),
            _ => Err(Error::new_spanned(pat, "unsupported match pattern")),
        }
    }
}

struct ValTranslator {
    crate_path: Path,
    module: Ident,
    scope: Ident,
}

impl ValTranslator {
    fn expr_match(&self, expr_match: &ExprMatch) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            module,
            scope,
        } = self;
        let ExprMatch {
            attrs,
            match_token,
            expr,
            brace_token: _,
            arms,
        } = expr_match;
        assert_no_attrs(attrs)?;
        let expr = self.expr(expr)?;
        let mut pattern_matcher = PatternMatcher::new(self);
        let value = pattern_matcher
            .temp_name_maker
            .make_temp_name(match_token.span);
        pattern_matcher
            .tokens
            .extend(quote_spanned! {match_token.span=>
                let #value = #crate_path::values::ops::shrink_scope(#expr, #scope);
            });
        let mut verification_match_arms = Vec::new();
        struct MatchArmOutput {
            value: Ident,
            matched: Ident,
        }
        let mut match_arm_outputs = Vec::new();
        for Arm {
            attrs,
            pat,
            guard,
            fat_arrow_token,
            body,
            comma: _,
        } in arms
        {
            assert_no_attrs(attrs)?;
            let PatternMatchResult {
                condition,
                defined_names,
                verification_match,
                verification_match_needs_if,
            } = pattern_matcher.pat(pat, &value)?;
            let verification_match_if = if verification_match_needs_if || guard.is_some() {
                quote! { if __condition() }
            } else {
                quote! {}
            };
            verification_match_arms.push(quote_spanned! {fat_arrow_token.spans[0]=>
                #verification_match #verification_match_if => {}
            });
            let condition = if let Some((if_kw, guard_expr)) = guard {
                let guard_expr = self.expr(guard_expr)?;
                let guard_expr = quote_spanned! {if_kw.span=>
                    #crate_path::values::ops::shrink_scope::<'_, bool>(#guard_expr, #scope)
                };
                match condition {
                    MatchCondition::Static(true) => guard_expr,
                    MatchCondition::Static(false) => quote_spanned! {if_kw.span=>
                        {
                            let _ = #guard_expr;
                            #crate_path::values::Value::get_value(&false, #module)
                        }
                    },
                    MatchCondition::Dynamic(condition) => quote_spanned! {if_kw.span=>
                        <bool as #crate_path::values::ops::HdlAnd<'_, bool>>::and(#condition, #guard_expr)
                    },
                }
            } else {
                match condition {
                    MatchCondition::Static(condition) => quote_spanned! {fat_arrow_token.spans[0]=>
                        #crate_path::values::Value::get_value(&#condition, #module)
                    },
                    MatchCondition::Dynamic(condition) => quote! { #condition },
                }
            };
            let define_names: Vec<_> = defined_names
                .0
                .iter()
                .map(|(name, value)| {
                    quote_spanned! {name.span()=>
                        let #name = #crate_path::values::ops::shrink_scope(#value, #scope);
                    }
                })
                .collect();
            let body = self.expr(body)?;
            let value = pattern_matcher
                .temp_name_maker
                .make_temp_name(fat_arrow_token.spans[0]);
            let matched = pattern_matcher
                .temp_name_maker
                .make_temp_name(fat_arrow_token.spans[0]);
            pattern_matcher
                .tokens
                .extend(quote_spanned! {fat_arrow_token.spans[0]=>
                    let #value;
                    let #matched = {
                        let __match_scope = #scope;
                        let #scope = #crate_path::ir::scope::Scope::new(__match_scope, #crate_path::ir::SourceLocation::caller());
                        #(#define_names)*
                        #value = #crate_path::values::ops::expand_scope(#body, #scope, __match_scope);
                        #crate_path::values::ops::expand_scope(#condition, #scope, __match_scope)
                    };
                });
            match_arm_outputs.push(MatchArmOutput { value, matched });
        }
        let result = if let Some(MatchArmOutput {
            value: last_value,
            matched: last_matched,
        }) = match_arm_outputs.pop()
        {
            let mut muxes = Vec::new();
            for MatchArmOutput { value, matched } in match_arm_outputs.into_iter().rev() {
                muxes.push(quote_spanned! {value.span()=>
                    let __result = #crate_path::values::ops::mux(#matched, #value, __result);
                });
            }
            quote_spanned! {match_token.span=>
                let _ = #last_matched;
                let __result = #last_value;
                #(#muxes)*
                __result
            }
        } else {
            todo_err!(match_token, "match of uninhabited value")
        };
        let tokens = pattern_matcher.tokens;
        Ok(quote_spanned! {match_token.span=>
            {
                #tokens
                #crate_path::values::ops::check_val_type(#value, |__value, _| match __value {
                    #(#verification_match_arms)*
                });
                #result
            }
        })
    }

    fn expr_lit(&self, neg: Option<&Token![-]>, lit: &ExprLit) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            module,
            scope: _,
        } = self;
        let ExprLit { attrs, lit } = lit;
        assert_no_attrs(attrs)?;
        match lit {
            Lit::ByteStr(lit) => todo_err!(lit),
            Lit::Byte(lit) => Ok(quote_spanned! {lit.span()=>
                #crate_path::values::Value::get_value(&<#crate_path::values::integer::UInt8 as ::core::convert::From>::from(#lit), #module)
            }),
            Lit::Bool(lit) => Ok(quote_spanned! {lit.span()=>
                #crate_path::values::Value::get_value(&#lit, #module)
            }),
            Lit::Int(lit) => {
                let int_literal = IntLiteral::parse(neg, lit)?;
                let int_literal = int_literal.to_tokens(crate_path);
                Ok(quote_spanned! {lit.span()=>
                    #crate_path::values::Value::get_value(&#int_literal, #module)
                })
            }
            Lit::Str(lit) => Err(Error::new_spanned(
                lit,
                "string literals are not supported, \
                did you mean to use byte strings instead? (`b\"...\"`)",
            )),
            Lit::Char(lit) => Err(Error::new_spanned(
                lit,
                "char literals are not supported, \
                did you mean to use byte literals instead? (`b\'...\'`)",
            )),
            Lit::Float(lit) => Err(Error::new_spanned(lit, "float literals are not supported")),
            Lit::Verbatim(lit) => Err(Error::new_spanned(lit, "literal not supported")),
        }
    }

    fn stmt(&self, stmt: &Stmt) -> syn::Result<TokenStream> {
        match stmt {
            Stmt::Local(Local {
                attrs,
                let_token,
                pat: _,
                init: _,
                semi_token: _,
            }) => {
                assert_no_attrs(attrs)?;
                todo_err!(let_token)
            }
            Stmt::Item(item) => Err(Error::new_spanned(item, "items are not supported")),
            Stmt::Expr(expr) => self.expr(expr),
            Stmt::Semi(expr, semi) => {
                let expr = self.expr(expr)?;
                Ok(quote! {#expr #semi})
            }
        }
    }

    fn block_interpreted(&self, block: &Block) -> syn::Result<TokenStream> {
        let Block { brace_token, stmts } = block;
        let stmts = stmts
            .iter()
            .map(|stmt| self.stmt(stmt))
            .collect::<syn::Result<Vec<_>>>()?;
        Ok(quote_spanned! {brace_token.span=>
            {
                let () = (); // useless let to shut-up #[warn(unused_braces)]
                #(#stmts)*
            }
        })
    }

    fn block_uninterpreted(&self, block: &Block) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            module,
            scope: _,
        } = self;
        let mut block = block.clone();
        // add useless let to shut-up #[warn(unused_braces)]
        block.stmts.insert(0, parse_quote! {let () = ();});
        Ok(quote_spanned! {block.brace_token.span=>
            #crate_path::values::ToVal::to_val(&#block, #module)
        })
    }

    fn expr_if(&self, expr_if: &ExprIf) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            module: _,
            scope: _,
        } = self;
        let ExprIf {
            attrs,
            if_token,
            cond,
            then_branch,
            else_branch,
        } = expr_if;
        assert_no_attrs(attrs)?;
        let cond = self.expr(cond)?;
        let then_branch = self.block_interpreted(then_branch)?;
        let else_expr = &*else_branch
            .as_ref()
            .ok_or_else(|| Error::new_spanned(if_token, "`if` expression must have `else` branch"))?
            .1;
        let else_branch = match else_expr {
            Expr::If(expr_if) => self.expr_if(expr_if)?,
            Expr::Block(ExprBlock {
                attrs,
                label,
                block,
            }) => {
                assert_no_attrs(attrs)?;
                if let Some(label) = label {
                    return Err(Error::new_spanned(label, "block labels are not supported"));
                }
                self.block_interpreted(block)?
            }
            _ => unreachable!(
                "the parser should produce only an if expression or a block expression"
            ),
        };
        Ok(quote_spanned! {if_token.span=>
            #crate_path::values::ops::mux(#cond, #then_branch, #else_branch)
        })
    }

    fn expr(&self, expr: &Expr) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            module,
            scope: _,
        } = self;
        match expr {
            Expr::Array(ExprArray {
                attrs,
                bracket_token,
                elems,
            }) => {
                assert_no_attrs(&attrs)?;
                todo_err!(expr)
            }
            Expr::Binary(ExprBinary {
                attrs,
                left,
                op,
                right,
            }) => {
                assert_no_attrs(attrs)?;
                let left = self.expr(left)?;
                let right = self.expr(right)?;
                match op {
                    BinOp::Add(op) => Ok(quote_spanned! {op.spans[0]=>
                        #crate_path::values::ops::HdlAdd::add(#left, #right)
                    }),
                    BinOp::Sub(op) => Ok(quote_spanned! {op.spans[0]=>
                        #crate_path::values::ops::HdlSub::sub(#left, #right)
                    }),
                    BinOp::Mul(op) => Ok(quote_spanned! {op.spans[0]=>
                        #crate_path::values::ops::HdlMul::mul(#left, #right)
                    }),
                    BinOp::And(op) => Ok(quote_spanned! {op.spans[0]=>
                        <bool as #crate_path::values::ops::HdlAnd<'_, bool>>::and(#left, #right)
                    }),
                    BinOp::Or(op) => Ok(quote_spanned! {op.spans[0]=>
                        <bool as #crate_path::values::ops::HdlOr<'_, bool>>::or(#left, #right)
                    }),
                    BinOp::BitXor(op) => Ok(quote_spanned! {op.spans[0]=>
                        #crate_path::values::ops::HdlXor::xor(#left, #right)
                    }),
                    BinOp::BitAnd(op) => Ok(quote_spanned! {op.spans[0]=>
                        #crate_path::values::ops::HdlAnd::and(#left, #right)
                    }),
                    BinOp::BitOr(op) => Ok(quote_spanned! {op.spans[0]=>
                        #crate_path::values::ops::HdlOr::or(#left, #right)
                    }),
                    BinOp::Shl(op) => Ok(quote_spanned! {op.spans[0]=>
                        #crate_path::values::ops::HdlShiftLeft::shift_left(#left, #right)
                    }),
                    BinOp::Shr(op) => Ok(quote_spanned! {op.spans[0]=>
                        #crate_path::values::ops::HdlShiftRight::shift_right(#left, #right)
                    }),
                    BinOp::Eq(op) => Ok(quote_spanned! {op.spans[0]=>
                        #crate_path::values::ops::HdlCompareEqual::compare_equal(#left, #right)
                    }),
                    BinOp::Lt(op) => Ok(quote_spanned! {op.spans[0]=>
                        #crate_path::values::ops::HdlCompareLess::compare_less(#left, #right)
                    }),
                    BinOp::Le(op) => Ok(quote_spanned! {op.spans[0]=>
                        #crate_path::values::ops::HdlCompareLessEqual::compare_less_equal(#left, #right)
                    }),
                    BinOp::Ne(op) => Ok(quote_spanned! {op.spans[0]=>
                        #crate_path::values::ops::HdlCompareNotEqual::compare_not_equal(#left, #right)
                    }),
                    BinOp::Ge(op) => Ok(quote_spanned! {op.spans[0]=>
                        #crate_path::values::ops::HdlCompareGreaterEqual::compare_greater_equal(#left, #right)
                    }),
                    BinOp::Gt(op) => Ok(quote_spanned! {op.spans[0]=>
                        #crate_path::values::ops::HdlCompareGreater::compare_greater(#left, #right)
                    }),
                    op => Err(Error::new_spanned(op, "unsupported binary op")),
                }
            }
            Expr::Call(expr) => todo_err!(expr),
            Expr::Cast(expr) => todo_err!(expr),
            Expr::Field(ExprField {
                attrs,
                base,
                dot_token,
                member,
            }) => {
                assert_no_attrs(attrs)?;
                let base = self.expr(base)?;
                Ok(quote_spanned! {dot_token.span=>
                    #crate_path::values::ops::assert_arg_is_val(#crate_path::values::aggregate::AggregateValue::struct_of_variant_values(#base) #dot_token #member)
                })
            }
            Expr::Group(ExprGroup {
                attrs,
                group_token: _,
                expr,
            }) => {
                assert_no_attrs(attrs)?;
                self.expr(expr)
            }
            Expr::If(expr_if) => self.expr_if(expr_if),
            Expr::Index(expr) => todo_err!(expr),
            Expr::Let(expr) => todo_err!(expr),
            Expr::Lit(expr) => self.expr_lit(None, expr),
            Expr::Match(expr_match) => self.expr_match(expr_match),
            Expr::MethodCall(expr) => todo_err!(expr),
            Expr::Paren(expr) => {
                if let Expr::Block(ExprBlock {
                    attrs,
                    label,
                    block,
                }) = &*expr.expr
                {
                    assert_no_attrs(attrs)?;
                    if let Some(label) = label {
                        return Err(Error::new_spanned(label, "block labels are not supported"));
                    }
                    self.block_uninterpreted(block)
                } else {
                    self.expr(&expr.expr)
                }
            }
            Expr::Path(expr) => {
                assert_no_attrs(&expr.attrs)?;
                let span = expr.path.segments.last().unwrap().ident.span();
                Ok(quote_spanned! {span=>
                    #crate_path::values::ToVal::to_val(&#expr, #module)
                })
            }
            Expr::Repeat(expr) => todo_err!(expr),
            Expr::Struct(expr) => todo_err!(expr),
            Expr::Tuple(expr) => todo_err!(expr),
            Expr::Unary(expr) => {
                if let UnOp::Neg(op) = &expr.op {
                    if let Expr::Lit(expr) = &*expr.expr {
                        return self.expr_lit(Some(op), expr);
                    }
                }
                let input = self.expr(&expr.expr)?;
                match expr.op {
                    UnOp::Not(op) => Ok(quote_spanned! {op.spans[0]=>
                        #crate_path::values::ops::HdlNot::not(#input)
                    }),
                    UnOp::Neg(op) => Ok(quote_spanned! {op.spans[0]=>
                        #crate_path::values::ops::HdlNeg::neg(#input)
                    }),
                    _ => Err(Error::new_spanned(expr.op, "unsupported unary op")),
                }
            }
            Expr::Block(ExprBlock {
                attrs,
                label,
                block,
            }) => {
                assert_no_attrs(attrs)?;
                if let Some(label) = label {
                    return Err(Error::new_spanned(label, "block labels are not supported"));
                }
                Err(Error::new_spanned(block, "uninterpreted blocks must be surrounded with both parenthesis and curly braces, like so: `({ uninterpreted() })`"))
            }
            _ => Err(Error::new_spanned(expr, "unsupported expression")),
        }
    }
}

pub(crate) struct ValInput {
    attrs: Vec<Attribute>,
    module_expr: Expr,
    comma: Token![,],
    expr: Expr,
}

impl Parse for ValInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            attrs: Attribute::parse_inner(input)?,
            module_expr: input.parse()?,
            comma: input.parse()?,
            expr: input.parse()?,
        })
    }
}

impl ToTokens for ValInput {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            attrs,
            module_expr,
            comma,
            expr,
        } = self;
        attrs.iter().for_each(|attr| attr.to_tokens(tokens));
        module_expr.to_tokens(tokens);
        comma.to_tokens(tokens);
        expr.to_tokens(tokens);
    }
}

pub(crate) fn val_impl(ast: ValInput) -> syn::Result<TokenStream> {
    let ValInput {
        attrs,
        module_expr,
        comma,
        expr,
    } = ast;
    let _ = comma;
    let RustHdlAttributes {
        crate_path,
        real_type_name,
    } = RustHdlAttributes::parse(&attrs, AttributesFor::Val)?;
    assert!(real_type_name.is_none());
    let module = parse_quote! { __module };
    let scope = parse_quote! { __scope };
    let init = quote! {
        let #module = #crate_path::module::AsIrModule::as_ir_module(&#module_expr);
        let #scope = #module.scope();
    };
    let expr = ValTranslator {
        crate_path,
        module,
        scope,
    }
    .expr(&expr)?;
    Ok(quote! {
        {
            #init
            #expr
        }
    })
}
