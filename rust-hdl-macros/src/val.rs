// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{AttributesFor, RustHdlAttributes, VariantField};
use core::fmt;
use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;
use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, quote_spanned, ToTokens};
use rust_hdl_int::{Int, IntShape};
use std::{
    borrow::Cow,
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
    Arm, Attribute, BinOp, Block, Error, Expr, ExprArray, ExprBinary, ExprBlock, ExprCall,
    ExprCast, ExprField, ExprGroup, ExprIf, ExprIndex, ExprLet, ExprLit, ExprMatch, ExprMethodCall,
    ExprParen, ExprPath, ExprRepeat, ExprStruct, ExprTuple, ExprUnary, FieldPat, FieldValue,
    GenericArgument, GenericParam, Generics, ItemType, Lit, LitBool, LitByte, LitByteStr, LitInt,
    Local, Member, Pat, PatIdent, PatLit, PatOr, PatPath, PatRange, PatRest, PatStruct, PatTuple,
    PatTupleStruct, PatWild, Path, PathSegment, QSelf, RangeLimits, Stmt, Token, Type, TypeArray,
    TypeBareFn, TypeGroup, TypeImplTrait, TypeInfer, TypeMacro, TypeNever, TypeParen, TypePath,
    TypePtr, TypeReference, TypeSlice, TypeTraitObject, TypeTuple, UnOp,
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

fn unwrap_expr_groups(mut expr: &Expr) -> syn::Result<&Expr> {
    while let Expr::Group(ExprGroup {
        attrs,
        group_token: _,
        expr: inner_expr,
    }) = expr
    {
        assert_no_attrs(attrs)?;
        expr = inner_expr;
    }
    Ok(expr)
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

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum NameCasingStyle {
    /// SCREAMING_SNAKE_CASE: e.g. `const MY_NAME: usize = 0`
    ConstStyle,
    /// UpperCamelCase: e.g. `type MyName = ();`
    TypeStyle,
    /// snake_case: e.g. `let my_name = 1;`
    VarStyle,
}

impl NameCasingStyle {
    fn get_impl<ErrorFn: FnOnce(fmt::Arguments<'_>) -> E, E>(
        name: &str,
        error_fn: ErrorFn,
    ) -> Result<Self, E> {
        if !name.is_ascii() {
            return Err(error_fn(format_args!(
                "non-ASCII identifiers not yet supported -- please create a bug report if you need them"
            )));
        }
        if !name.contains(|c: char| c.is_ascii_uppercase()) {
            return Ok(Self::VarStyle);
        }
        if name.contains('_') {
            Ok(Self::ConstStyle)
        } else if name.contains(|c: char| c.is_ascii_lowercase()) {
            Ok(Self::TypeStyle)
        } else if name.len() <= 1 {
            Ok(Self::TypeStyle)
        } else {
            Ok(Self::ConstStyle)
        }
    }
    fn get(name: &Ident) -> syn::Result<Self> {
        Self::get_impl(&name.to_string(), |message| {
            Error::new_spanned(name, message)
        })
    }
}

enum PathKind {
    EnumVariant {
        span: Span,
        enum_type: TypePath,
        variant_path: PatPath,
        variant_name: Ident,
    },
    Type {
        span: Span,
        path: TypePath,
    },
    Const {
        span: Span,
        path: ExprPath,
    },
    VarOrFn {
        name: Ident,
    },
    Function {
        span: Span,
        path: ExprPath,
    },
}

impl PathKind {
    fn get_enum_type_for_well_known_variant(path: &Path) -> Option<TypePath> {
        if path.is_ident("None") || path.is_ident("Some") {
            Some(parse_quote! { ::core::option::Option })
        } else if path.is_ident("Ok") || path.is_ident("Err") {
            Some(parse_quote! { ::core::result::Result })
        } else {
            None
        }
    }
    fn is_well_known_type(path: &Path) -> bool {
        if let Some(ident) = path.get_ident() {
            match &*ident.to_string() {
                "bool" | "char" | "f32" | "f64" | "i128" | "i16" | "i32" | "i64" | "i8"
                | "isize" | "str" | "u128" | "u16" | "u32" | "u64" | "u8" | "usize" => true,
                _ => false,
            }
        } else {
            false
        }
    }
    fn get(qself: Option<QSelf>, path: Path) -> syn::Result<Self> {
        let last_path_segment = path
            .segments
            .last()
            .expect("parser won't return empty path");
        let span = last_path_segment.span();
        let name_casing = path
            .segments
            .iter()
            .map(|path_segment| NameCasingStyle::get(&path_segment.ident))
            .collect::<syn::Result<Vec<_>>>()?;
        if let Some(qself) = qself {
            todo_err!(qself.as_token)
        } else if let Some(enum_type) = Self::get_enum_type_for_well_known_variant(&path) {
            let variant_name = last_path_segment.ident.clone();
            Ok(Self::EnumVariant {
                span,
                enum_type,
                variant_path: PatPath {
                    attrs: Vec::new(),
                    qself,
                    path,
                },
                variant_name,
            })
        } else if Self::is_well_known_type(&path) {
            Ok(Self::Type {
                span,
                path: TypePath { qself, path },
            })
        } else {
            match name_casing[..] {
                [.., NameCasingStyle::ConstStyle] => Ok(Self::Const {
                    span,
                    path: ExprPath {
                        attrs: Vec::new(),
                        qself,
                        path,
                    },
                }),
                [.., NameCasingStyle::TypeStyle, NameCasingStyle::TypeStyle] => {
                    let mut enum_type = path.clone();
                    let variant_name = enum_type.segments.pop().unwrap().into_value().ident;
                    // work around https://github.com/dtolnay/syn/issues/1043
                    let enum_type_name = enum_type.segments.pop().unwrap().into_value();
                    enum_type.segments.push(enum_type_name);
                    let enum_type = TypePath {
                        qself: qself.clone(),
                        path: enum_type,
                    };
                    Ok(Self::EnumVariant {
                        span,
                        enum_type,
                        variant_path: PatPath {
                            attrs: Vec::new(),
                            qself,
                            path,
                        },
                        variant_name,
                    })
                }
                [.., NameCasingStyle::TypeStyle] => Ok(Self::Type {
                    span,
                    path: TypePath { qself, path },
                }),
                [.., NameCasingStyle::VarStyle] => {
                    if let Some(name) = path.get_ident().cloned() {
                        Ok(Self::VarOrFn { name })
                    } else {
                        Ok(Self::Function {
                            span,
                            path: ExprPath {
                                attrs: Vec::new(),
                                qself,
                                path,
                            },
                        })
                    }
                }
                [] => unreachable!(),
            }
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
            module: _,
            scope: _,
        } = self.val_translator;
        let PatPath { attrs, qself, path } = pat_path;
        assert_no_attrs(attrs)?;
        match PathKind::get(qself, path)? {
            PathKind::EnumVariant {
                span,
                enum_type: _,
                variant_path,
                variant_name,
            } => {
                let condition = self.temp_name_maker.make_temp_name(span);
                self.tokens.extend(quote_spanned! {span=>
                    let __fields = #crate_path::values::aggregate::AggregateValue::struct_of_variant_values(#value).#variant_name;
                    let __variant_index = #crate_path::values::ops::get_aggregate_variant_index_from_variant_value(#value, &__fields);
                    let #condition = #crate_path::values::ops::is_aggregate_variant(#value, __variant_index);
                });
                Ok(PatternMatchResult {
                    condition: MatchCondition::Dynamic(condition),
                    defined_names: PatternDefinedNames::new(),
                    verification_match: Pat::Path(variant_path),
                    verification_match_needs_if: false,
                })
            }
            PathKind::Type { span, path } => {
                self.tokens.extend(quote_spanned! {span=>
                    #crate_path::values::ops::assert_type_is_aggregate::<#path>();
                });
                let TypePath { qself, path } = path;
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
            PathKind::VarOrFn { name } => {
                let mut defined_names = PatternDefinedNames::new();
                defined_names.define_name(name.clone(), value.clone())?;
                Ok(PatternMatchResult {
                    condition: MatchCondition::Static(true),
                    defined_names,
                    verification_match: Pat::Wild(PatWild {
                        attrs: Vec::new(),
                        underscore_token: Token![_](name.span()),
                    }),
                    verification_match_needs_if: false,
                })
            }
            PathKind::Const { span: _, path } => {
                todo_err!(path, "using constants as patterns is not yet implemented")
            }
            PathKind::Function { span: _, path } => Err(Error::new_spanned(
                path,
                "functions/methods can't be used as patterns",
            )),
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
        path_kind: Option<PathKind>,
        pat_tuple: &PatTuple,
        value: &Ident,
    ) -> syn::Result<PatternMatchResult> {
        let crate_path = &self.val_translator.crate_path;
        let PatTuple {
            attrs,
            paren_token,
            elems,
        } = pat_tuple;
        assert_no_attrs(attrs)?;
        if path_kind.is_none()
            && !elems.trailing_punct()
            && elems.len() == 1
            && !matches!(elems[0], Pat::Rest(_))
        {
            // the pattern is actually parenthesis like `(pat)` and not a tuple pattern
            let PatternMatchResult {
                condition,
                defined_names,
                verification_match,
                verification_match_needs_if,
            } = self.pat(&elems[0], value)?;
            return Ok(PatternMatchResult {
                condition,
                defined_names,
                verification_match: Pat::Tuple(PatTuple {
                    attrs: Vec::new(),
                    paren_token: *paren_token,
                    elems: iter::once(verification_match).collect(),
                }),
                verification_match_needs_if,
            });
        }
        let fields = self.temp_name_maker.make_temp_name(paren_token.span);
        let mut condition = match path_kind {
            None | Some(PathKind::Type { .. }) => {
                self.tokens.extend(quote_spanned! {paren_token.span=>
                    let #fields = #crate_path::values::aggregate::AggregateValue::struct_of_variant_values(#value);
                });
                MatchCondition::Static(true)
            }
            Some(PathKind::EnumVariant {
                span,
                ref variant_name,
                ..
            }) => {
                let condition = self.temp_name_maker.make_temp_name(span);
                self.tokens.extend(quote_spanned! {span=>
                    let #fields = #crate_path::values::aggregate::AggregateValue::struct_of_variant_values(#value).#variant_name;
                    let __variant_index = #crate_path::values::ops::get_aggregate_variant_index_from_variant_value(#value, &#fields);
                    let #condition = #crate_path::values::ops::is_aggregate_variant(#value, __variant_index);
                });
                MatchCondition::Dynamic(condition)
            }
            Some(PathKind::Const { span: _, path }) => {
                return Err(Error::new_spanned(
                    path,
                    "a constant is not a valid tuple struct type",
                ))
            }
            Some(PathKind::VarOrFn { name }) => {
                return Err(Error::new_spanned(
                    name,
                    "a variable/function is not a valid tuple struct type",
                ))
            }
            Some(PathKind::Function { span: _, path }) => {
                return Err(Error::new_spanned(
                    path,
                    "a function is not a valid tuple struct type",
                ))
            }
        };
        let mut verification_match_elems = Punctuated::new();
        let mut last_separator_span = paren_token.span;
        let mut defined_names = PatternDefinedNames::new();
        let mut verification_match_needs_if = false;
        let mut rest_index = None;
        for (index, field) in elems.pairs().enumerate() {
            if rest_index.is_some() {
                todo_err!(field.value(), "tuple fields after `..`");
            }
            if let Pat::Rest(PatRest {
                attrs,
                dot2_token: _,
            }) = field.value()
            {
                assert_no_attrs(attrs)?;
                rest_index = Some(index);
                verification_match_elems.extend(iter::once(Pair::new(
                    (*field.value()).clone(),
                    field.punct().copied().copied(),
                )));
                continue;
            }
            let field_variable = self.temp_name_maker.make_temp_name(last_separator_span);
            let generated_name = VariantField::generate_name(index.into());
            self.tokens.extend(quote_spanned! {paren_token.span=>
                let #field_variable = #fields.#generated_name;
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
        let verification_match = match path_kind {
            None => Pat::Tuple(pat_tuple),
            Some(PathKind::Type {
                span: _,
                path: TypePath { qself, path },
            }) => {
                if qself.is_some() {
                    todo_err!(TypePath { qself, path }, "qualified self");
                }
                Pat::TupleStruct(PatTupleStruct {
                    attrs: Vec::new(),
                    path,
                    pat: pat_tuple,
                })
            }
            Some(PathKind::EnumVariant {
                variant_path:
                    PatPath {
                        attrs: _,
                        qself,
                        path,
                    },
                ..
            }) => {
                if qself.is_some() {
                    todo_err!(TypePath { qself, path }, "qualified self");
                }
                Pat::TupleStruct(PatTupleStruct {
                    attrs: Vec::new(),
                    path,
                    pat: pat_tuple,
                })
            }
            Some(PathKind::Const { .. })
            | Some(PathKind::VarOrFn { .. })
            | Some(PathKind::Function { .. }) => unreachable!(),
        };
        Ok(PatternMatchResult {
            condition,
            defined_names,
            verification_match,
            verification_match_needs_if,
        })
    }
    fn pat_range(
        &mut self,
        pat_range: &PatRange,
        value: &Ident,
    ) -> syn::Result<PatternMatchResult> {
        let crate_path = &self.val_translator.crate_path;
        let PatRange {
            attrs,
            lo,
            limits,
            hi,
        } = pat_range;
        assert_no_attrs(attrs)?;
        let lo = self.pat_lit_expr(lo, None)?;
        let hi = self.pat_lit_expr(hi, None)?;
        let get_endpoint_tokens = |endpoint: PatternLiteral| -> syn::Result<TokenStream> {
            match endpoint {
                PatternLiteral::Int(endpoint) => {
                    Ok(endpoint.to_tokens(crate_path).into_token_stream())
                }
                PatternLiteral::ByteStr(endpoint) => Err(Error::new_spanned(
                    endpoint,
                    "byte strings are not allowed in range patterns",
                )),
                PatternLiteral::Byte(endpoint) => Ok(quote_spanned! {endpoint.span()=>
                    <#crate_path::values::integer::UInt8 as ::core::convert::From>::from(#endpoint)
                }),
                PatternLiteral::Bool(endpoint) => Err(Error::new_spanned(
                    endpoint,
                    "`bool` type is not allowed in range patterns",
                )),
            }
        };
        let lo = get_endpoint_tokens(lo)?;
        let hi = get_endpoint_tokens(hi)?;
        let (span, condition_expr) = match limits {
            RangeLimits::HalfOpen(range) => (
                range.spans[0],
                quote_spanned! {range.spans[0]=>
                    #crate_path::values::ops::match_range(#value, &#lo #range &#hi)
                },
            ),
            RangeLimits::Closed(range) => (
                range.spans[0],
                quote_spanned! {range.spans[0]=>
                    #crate_path::values::ops::match_range_inclusive(#value, &#lo #range &#hi)
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
    fn pat_lit(&mut self, pat_lit: &PatLit, value: &Ident) -> syn::Result<PatternMatchResult> {
        let crate_path = &self.val_translator.crate_path;
        let PatLit { attrs, expr } = pat_lit;
        assert_no_attrs(attrs)?;
        let span;
        let lit_value = match self.pat_lit_expr(expr, None)? {
            PatternLiteral::Int(lit_value) => {
                span = lit_value.span;
                lit_value.to_tokens(crate_path).into_token_stream()
            }
            PatternLiteral::ByteStr(_value) => todo_err!(pat_lit),
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
    fn pat_ident(
        &mut self,
        pat_ident: &PatIdent,
        value: &Ident,
    ) -> syn::Result<PatternMatchResult> {
        let PatIdent {
            attrs,
            by_ref,
            mutability,
            ident,
            subpat,
        } = pat_ident;
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
    fn pat_wild(&mut self, pat_wild: &PatWild) -> syn::Result<PatternMatchResult> {
        let PatWild {
            attrs,
            underscore_token: _,
        } = pat_wild;
        assert_no_attrs(attrs)?;
        Ok(PatternMatchResult {
            condition: MatchCondition::Static(true),
            defined_names: PatternDefinedNames::new(),
            verification_match: Pat::Wild(pat_wild.clone()),
            verification_match_needs_if: false,
        })
    }
    fn pat_tuple_struct(
        &mut self,
        pat_tuple_struct: &PatTupleStruct,
        value: &Ident,
    ) -> syn::Result<PatternMatchResult> {
        let PatTupleStruct { attrs, path, pat } = pat_tuple_struct;
        assert_no_attrs(attrs)?;
        self.pat_tuple(Some(PathKind::get(None, path.clone())?), pat, value)
    }
    fn pat_struct(
        &mut self,
        pat_struct: &PatStruct,
        value: &Ident,
    ) -> syn::Result<PatternMatchResult> {
        let crate_path = &self.val_translator.crate_path;
        let PatStruct {
            attrs,
            path,
            brace_token,
            fields: struct_fields,
            dot2_token,
        } = pat_struct;
        assert_no_attrs(attrs)?;
        let path_kind = PathKind::get(None, path.clone())?;
        let fields_var = self.temp_name_maker.make_temp_name(brace_token.span);
        let mut condition = match path_kind {
            PathKind::Type { .. } => {
                self.tokens.extend(quote_spanned! {brace_token.span=>
                    let #fields_var = #crate_path::values::aggregate::AggregateValue::struct_of_variant_values(#value);
                });
                MatchCondition::Static(true)
            }
            PathKind::EnumVariant {
                span,
                ref variant_name,
                ..
            } => {
                let condition = self.temp_name_maker.make_temp_name(span);
                self.tokens.extend(quote_spanned! {brace_token.span=>
                    let #fields_var = #crate_path::values::aggregate::AggregateValue::struct_of_variant_values(#value).#variant_name;
                    let __variant_index = #crate_path::values::ops::get_aggregate_variant_index_from_variant_value(#value, &#fields_var);
                    let #condition = #crate_path::values::ops::is_aggregate_variant(#value, __variant_index);
                });
                MatchCondition::Dynamic(condition)
            }
            PathKind::Const { span: _, path } => {
                return Err(Error::new_spanned(
                    path,
                    "a constant is not a valid struct type",
                ))
            }
            PathKind::VarOrFn { name } => {
                return Err(Error::new_spanned(
                    name,
                    "a variable/function is not a valid struct type",
                ))
            }
            PathKind::Function { span: _, path } => {
                return Err(Error::new_spanned(
                    path,
                    "a function is not a valid struct type",
                ))
            }
        };
        let mut verification_match_fields = Punctuated::new();
        let mut last_separator_span = brace_token.span;
        let mut defined_names = PatternDefinedNames::new();
        let mut verification_match_needs_if = false;
        for field in struct_fields.pairs() {
            let FieldPat {
                attrs,
                member,
                colon_token,
                pat,
            } = field.value();
            assert_no_attrs(attrs)?;
            let field_variable = self.temp_name_maker.make_temp_name(member.span());
            self.tokens.extend(quote_spanned! {member.span()=>
                let #field_variable = #fields_var.#member;
            });
            let PatternMatchResult {
                condition: field_condition,
                defined_names: field_defined_names,
                verification_match: field_verification_match,
                verification_match_needs_if: field_verification_match_needs_if,
            } = self.pat(pat, &field_variable)?;
            condition = self.condition_and(last_separator_span, [condition, field_condition]);
            defined_names.define_names(field_defined_names)?;
            verification_match_fields.extend(iter::once(Pair::new(
                FieldPat {
                    attrs: Vec::new(),
                    member: member.clone(),
                    colon_token: Some(colon_token.unwrap_or_else(|| Token![:](member.span()))),
                    pat: Box::new(field_verification_match),
                },
                field.punct().copied().copied(),
            )));
            verification_match_needs_if |= field_verification_match_needs_if;
            last_separator_span = field.punct().map_or(brace_token.span, |v| v.span);
        }
        let verification_match = Pat::Struct(PatStruct {
            attrs: Vec::new(),
            path: path.clone(),
            brace_token: *brace_token,
            fields: verification_match_fields,
            dot2_token: *dot2_token,
        });
        Ok(PatternMatchResult {
            condition,
            defined_names,
            verification_match,
            verification_match_needs_if,
        })
    }
    fn pat(&mut self, pat: &Pat, value: &Ident) -> syn::Result<PatternMatchResult> {
        match pat {
            Pat::Ident(pat_ident) => self.pat_ident(pat_ident, value),
            Pat::Lit(pat_lit) => self.pat_lit(pat_lit, value),
            Pat::Or(pat_or) => self.pat_or(pat_or, value),
            Pat::Path(pat_path) => self.pat_path(pat_path.clone(), value),
            Pat::Range(pat_range) => self.pat_range(pat_range, value),
            Pat::Slice(pat_slice) => todo_err!(pat_slice),
            Pat::Struct(pat_struct) => self.pat_struct(pat_struct, value),
            Pat::Tuple(pat_tuple) => self.pat_tuple(None, pat_tuple, value),
            Pat::TupleStruct(pat_tuple_struct) => self.pat_tuple_struct(pat_tuple_struct, value),
            Pat::Wild(pat_wild) => self.pat_wild(pat_wild),
            Pat::Rest(pat_rest) => Err(Error::new_spanned(
                pat_rest,
                "`..` patterns aren't allowed here",
            )),
            _ => Err(Error::new_spanned(pat, "unsupported match pattern")),
        }
    }
}

struct IfOrMatchArm<'a> {
    span: Span,
    pat: Cow<'a, Pat>,
    guard: Option<(Token![if], &'a Expr)>,
    translated_body: TokenStream,
}

struct ValTranslator {
    crate_path: Path,
    module: Ident,
    scope: Ident,
}

impl ValTranslator {
    fn expr_if_or_match<'a>(
        &self,
        if_or_match_token_span: Span,
        expr: &Expr,
        arms: impl IntoIterator<Item = IfOrMatchArm<'a>>,
    ) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            module,
            scope,
        } = self;
        let expr = self.expr(expr)?;
        let mut pattern_matcher = PatternMatcher::new(self);
        let value = pattern_matcher
            .temp_name_maker
            .make_temp_name(if_or_match_token_span);
        pattern_matcher
            .tokens
            .extend(quote_spanned! {if_or_match_token_span=>
                let #value = #crate_path::values::ops::shrink_scope(#expr, #scope);
            });
        let mut verification_match_arms = Vec::new();
        struct MatchArmOutput {
            value: Ident,
            matched: Ident,
        }
        let mut match_arm_outputs = Vec::new();
        for IfOrMatchArm {
            span: arm_span,
            pat,
            guard,
            translated_body,
        } in arms
        {
            let PatternMatchResult {
                condition,
                defined_names,
                verification_match,
                verification_match_needs_if,
            } = pattern_matcher.pat(&pat, &value)?;
            let verification_match_if = if verification_match_needs_if || guard.is_some() {
                quote! { if #crate_path::values::ops::match_fake_condition() }
            } else {
                quote! {}
            };
            verification_match_arms.push(quote_spanned! {arm_span=>
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
                    MatchCondition::Static(condition) => quote_spanned! {arm_span=>
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
            let value = pattern_matcher.temp_name_maker.make_temp_name(arm_span);
            let matched = pattern_matcher.temp_name_maker.make_temp_name(arm_span);
            pattern_matcher
                .tokens
                .extend(quote_spanned! {arm_span=>
                    let #value;
                    let #matched = {
                        let __match_scope = #scope;
                        let #scope = #crate_path::ir::scope::Scope::new(__match_scope, #crate_path::ir::SourceLocation::caller());
                        #(#define_names)*
                        #value = #crate_path::values::ops::expand_scope(#translated_body, #scope, __match_scope);
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
            quote_spanned! {if_or_match_token_span=>
                let _ = #last_matched;
                let __result = #last_value;
                #(#muxes)*
                __result
            }
        } else {
            todo_err!(
                Token![match](if_or_match_token_span),
                "match of uninhabited value"
            )
        };
        let tokens = pattern_matcher.tokens;
        Ok(quote_spanned! {if_or_match_token_span=>
            {
                #tokens
                #crate_path::values::ops::check_val_type(#value, |__value, _| match __value {
                    #(#verification_match_arms)*
                });
                #result
            }
        })
    }
    fn expr_match(&self, expr_match: &ExprMatch) -> syn::Result<TokenStream> {
        let ExprMatch {
            attrs,
            match_token,
            expr,
            brace_token: _,
            arms,
        } = expr_match;
        assert_no_attrs(attrs)?;
        let arms = arms
            .iter()
            .map(
                |Arm {
                     attrs,
                     pat,
                     guard,
                     fat_arrow_token,
                     body,
                     comma: _,
                 }| {
                    assert_no_attrs(attrs)?;
                    let translated_body =
                        self.expr_or_block(body, |block| self.block_interpreted(block, true))?;
                    Ok(IfOrMatchArm {
                        span: fat_arrow_token.span(),
                        pat: Cow::Borrowed(pat),
                        guard: guard.as_ref().map(|(if_token, expr)| (*if_token, &**expr)),
                        translated_body,
                    })
                },
            )
            .collect::<syn::Result<Vec<_>>>()?;
        return self.expr_if_or_match(match_token.span, expr, arms);
    }

    fn expr_or_block(
        &self,
        expr: &Expr,
        block_fn: impl FnOnce(&Block) -> syn::Result<TokenStream>,
    ) -> syn::Result<TokenStream> {
        let expr = unwrap_expr_groups(expr)?;
        if let Expr::Block(expr_block) = expr {
            self.expr_block(expr_block, block_fn)
        } else {
            self.expr(expr)
        }
    }

    fn expr_block(
        &self,
        expr_block: &ExprBlock,
        block_fn: impl FnOnce(&Block) -> syn::Result<TokenStream>,
    ) -> syn::Result<TokenStream> {
        let ExprBlock {
            attrs,
            label,
            block,
        } = expr_block;
        assert_no_attrs(attrs)?;
        if let Some(label) = label {
            return Err(Error::new_spanned(label, "block labels are not supported"));
        }
        block_fn(block)
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
            Lit::ByteStr(lit) => Ok(quote_spanned! {lit.span()=>
                #crate_path::values::ops::literal_byte_array(#module, #lit)
            }),
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

    fn block_interpreted(
        &self,
        block: &Block,
        warn_on_unused_braces: bool,
    ) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            module,
            scope: _,
        } = self;
        let Block { brace_token, stmts } = block;
        if stmts.is_empty() {
            return Ok(quote_spanned! {brace_token.span=>
                #crate_path::values::ToVal::to_val(&(), #module)
            });
        }
        let mut translated_statements = Vec::with_capacity(1 + stmts.len());
        if !warn_on_unused_braces {
            translated_statements.push(quote_spanned! {brace_token.span=>
                let () = (); // useless let to shut-up #[warn(unused_braces)]
            });
        }
        let mut need_empty_return = true;
        for stmt in stmts {
            let stmt_need_empty_return;
            translated_statements.push(match stmt {
                Stmt::Local(Local {
                    attrs,
                    let_token,
                    pat: _,
                    init: _,
                    semi_token: _,
                }) => {
                    assert_no_attrs(attrs)?;
                    stmt_need_empty_return = true;
                    let _ = stmt_need_empty_return;
                    todo_err!(let_token)
                }
                Stmt::Item(item) => {
                    return Err(Error::new_spanned(item, "items are not supported"))
                }
                Stmt::Expr(expr) => {
                    stmt_need_empty_return = false;
                    self.expr(expr)?
                }
                Stmt::Semi(expr, semi) => {
                    stmt_need_empty_return = true;
                    let expr = self.expr(expr)?;
                    quote! {#expr #semi}
                }
            });
            need_empty_return = stmt_need_empty_return;
        }
        if need_empty_return {
            translated_statements.push(quote_spanned! {brace_token.span=>
                #crate_path::values::ToVal::to_val(&(), #module)
            });
        }
        Ok(quote_spanned! {brace_token.span=>
            {
                #(#translated_statements)*
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
        let ExprIf {
            attrs,
            if_token,
            cond,
            then_branch,
            else_branch,
        } = expr_if;
        assert_no_attrs(attrs)?;
        let cond = unwrap_expr_groups(cond)?;
        let then_branch = self.block_interpreted(then_branch, false)?;
        let match_expr: Cow<Expr>;
        let then_arm = if let Expr::Let(ExprLet {
            attrs,
            let_token,
            pat,
            eq_token: _,
            expr,
        }) = cond
        {
            assert_no_attrs(attrs)?;
            match_expr = Cow::Borrowed(expr);
            IfOrMatchArm {
                span: let_token.span,
                pat: Cow::Borrowed(pat),
                guard: None,
                translated_body: then_branch,
            }
        } else {
            match_expr = Cow::Owned(parse_quote! { () });
            IfOrMatchArm {
                span: if_token.span,
                pat: Cow::Owned(Pat::Wild(PatWild {
                    attrs: Vec::new(),
                    underscore_token: Token![_](if_token.span),
                })),
                guard: Some((*if_token, cond)),
                translated_body: then_branch,
            }
        };
        let (else_token, ref else_expr) = *else_branch.as_ref().ok_or_else(|| {
            Error::new_spanned(if_token, "`if` expression must have `else` branch")
        })?;
        let else_branch = match &**else_expr {
            Expr::If(expr_if) => self.expr_if(expr_if)?,
            Expr::Block(expr_block) => {
                self.expr_block(expr_block, |block| self.block_interpreted(block, false))?
            }
            _ => unreachable!(
                "the parser should produce only an if expression or a block expression"
            ),
        };
        let else_arm = IfOrMatchArm {
            span: else_token.span,
            pat: Cow::Owned(Pat::Wild(PatWild {
                attrs: Vec::new(),
                underscore_token: Token![_](else_token.span),
            })),
            guard: None,
            translated_body: else_branch,
        };
        self.expr_if_or_match(if_token.span, &match_expr, [then_arm, else_arm])
    }

    fn expr_tuple(&self, expr_tuple: &ExprTuple) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            module,
            scope: _,
        } = self;
        let ExprTuple {
            attrs,
            paren_token,
            elems,
        } = expr_tuple;
        assert_no_attrs(attrs)?;
        if elems.is_empty() {
            return Ok(quote_spanned! {paren_token.span=>
                #crate_path::values::ToVal::to_val(&(), #module)
            });
        }
        let elems = elems
            .iter()
            .enumerate()
            .map(|(index, expr)| {
                let expr = self.expr(expr)?;
                let generated_name = VariantField::generate_name(index.into());
                Ok(quote_spanned! {paren_token.span=>
                    #generated_name: #expr,
                })
            })
            .collect::<syn::Result<Vec<_>>>()?;
        let type_params: Vec<_> = elems
            .iter()
            .enumerate()
            .map(|(index, _)| format_ident!("__T{}", index, span = paren_token.span))
            .collect();
        Ok(quote_spanned! {paren_token.span=>
            #crate_path::values::aggregate::get_aggregate_of_variants_value(
                #module,
                {
                    type __T<'__ctx, #(#type_params,)*> = <(#(#type_params,)*) as #crate_path::values::aggregate::AggregateValue<'__ctx>>::AggregateOfVariantValues;
                    __T { #(#elems)* __aggregate_phantom: ::core::marker::PhantomData }
                }
            )
        })
    }

    fn expr_array(&self, expr_array: &ExprArray) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            module,
            scope: _,
        } = self;
        let ExprArray {
            attrs,
            bracket_token,
            elems,
        } = expr_array;
        assert_no_attrs(attrs)?;
        let elements = elems
            .iter()
            .map(|e| self.expr(e))
            .collect::<syn::Result<Vec<_>>>()?;
        if elements.is_empty() {
            Ok(quote_spanned! {bracket_token.span=>
                #crate_path::values::ops::literal_empty_array(#module)
            })
        } else {
            Ok(quote_spanned! {bracket_token.span=>
                #crate_path::values::ops::literal_nonempty_array([#(#elements,)*])
            })
        }
    }

    fn expr_repeat(&self, expr_repeat: &ExprRepeat) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            module: _,
            scope: _,
        } = self;
        let ExprRepeat {
            attrs,
            bracket_token,
            expr,
            semi_token: _,
            len,
        } = expr_repeat;
        assert_no_attrs(attrs)?;
        let expr = self.expr(expr)?;
        Ok(quote_spanned! {bracket_token.span=>
            #crate_path::values::ops::literal_array_repeat::<_, { #len }>(#expr)
        })
    }

    fn expr_struct(&self, expr_struct: &ExprStruct) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            module,
            scope: _,
        } = self;
        let ExprStruct {
            attrs,
            path,
            brace_token,
            fields,
            dot2_token,
            rest,
        } = expr_struct;
        assert_no_attrs(attrs)?;
        let path_kind = PathKind::get(None, path.clone())?;
        let variant_of_values_type;
        let phantom_data_field;
        match path_kind {
            PathKind::EnumVariant {
                span,
                enum_type,
                variant_path: _,
                variant_name,
            } => {
                if rest.is_some() {
                    return Err(Error::new_spanned(
                        dot2_token,
                        "can't use `..` in enum variant literal",
                    ));
                }
                variant_of_values_type = quote_spanned! {span=>
                    __Type::<#enum_type>::#variant_name
                };
                phantom_data_field = quote! {};
            }
            PathKind::Type { span, path } => {
                variant_of_values_type = quote_spanned! {span=>
                    __Type::<#path>
                };
                phantom_data_field = quote_spanned! {span=>
                    __aggregate_phantom: ::core::marker::PhantomData,
                };
            }
            PathKind::Const { span: _, path } => {
                return Err(Error::new_spanned(
                    path,
                    "a constant is not a valid struct type",
                ))
            }
            PathKind::VarOrFn { name } => {
                return Err(Error::new_spanned(
                    name,
                    "a variable/function is not a valid struct type",
                ))
            }
            PathKind::Function { span: _, path } => {
                return Err(Error::new_spanned(
                    path,
                    "a function is not a valid struct type",
                ))
            }
        }
        let mut field_tokens = Vec::new();
        for FieldValue {
            attrs,
            member,
            colon_token: _,
            expr,
        } in fields
        {
            assert_no_attrs(attrs)?;
            let generated_name = VariantField::generate_name(member.clone());
            let expr = self.expr(expr)?;
            field_tokens.push(quote_spanned! {member.span()=>
                #generated_name: #expr,
            });
        }
        if let Some(rest) = rest {
            let rest = self.expr(rest)?;
            field_tokens.push(quote_spanned! {brace_token.span=>
                ..#crate_path::values::aggregate::AggregateValue::struct_of_variant_values(#rest)
            });
        } else {
            field_tokens.push(phantom_data_field);
        }
        Ok(quote_spanned! {brace_token.span=>
            {
                type __Type<'ctx, T> = <T as #crate_path::values::aggregate::AggregateValue<'ctx>>::AggregateOfVariantValues;
                #crate_path::values::aggregate::get_aggregate_of_variants_value(
                    #module,
                    #variant_of_values_type {
                        #(#field_tokens)*
                    },
                )
            }
        })
    }

    fn expr_call(&self, expr_call: &ExprCall) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            module: _,
            scope: _,
        } = self;
        let ExprCall {
            attrs,
            func,
            paren_token,
            args,
        } = expr_call;
        assert_no_attrs(attrs)?;
        let func = unwrap_expr_groups(func)?;
        let ExprPath { attrs, qself, path } = match func {
            Expr::Path(v) => v,
            _ => return Err(Error::new_spanned(func, "called function must be a path")),
        };
        assert_no_attrs(attrs)?;
        let path_kind = PathKind::get(qself.clone(), path.clone())?;
        let args = args
            .pairs()
            .map(|pair| {
                Ok(Pair::new(
                    self.expr(pair.value())?,
                    pair.punct().copied().copied(),
                ))
            })
            .collect::<syn::Result<Punctuated<TokenStream, _>>>()?;
        match path_kind {
            PathKind::EnumVariant {
                span,
                enum_type,
                variant_path,
                variant_name,
            } => todo_err!(variant_path, "enum literals"),
            PathKind::Type { span, path } => todo_err!(path, "literal structs"),
            PathKind::Const { span: _, path } => {
                Err(Error::new_spanned(path, "can't call constants"))
            }
            PathKind::VarOrFn { name } => todo_err!(name, "function calls"),
            PathKind::Function { span, path } => todo_err!(path, "function calls"),
        }
    }

    fn expr_cast(&self, expr_cast: &ExprCast) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            module: _,
            scope: _,
        } = self;
        let ExprCast {
            attrs,
            expr,
            as_token,
            ty,
        } = expr_cast;
        assert_no_attrs(attrs)?;
        todo_err!(expr_cast);
    }

    fn expr_index(&self, expr_index: &ExprIndex) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            module: _,
            scope: _,
        } = self;
        let ExprIndex {
            attrs,
            expr,
            bracket_token,
            index,
        } = expr_index;
        assert_no_attrs(attrs)?;
        todo_err!(expr_index);
    }

    fn expr_method_call(&self, expr_method_call: &ExprMethodCall) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            module: _,
            scope: _,
        } = self;
        let ExprMethodCall {
            attrs,
            receiver,
            dot_token,
            method,
            turbofish,
            paren_token,
            args,
        } = expr_method_call;
        assert_no_attrs(attrs)?;
        todo_err!(expr_method_call);
    }

    fn expr(&self, expr: &Expr) -> syn::Result<TokenStream> {
        let Self {
            crate_path,
            module,
            scope: _,
        } = self;
        let expr = unwrap_expr_groups(expr)?;
        match expr {
            Expr::Array(expr_array) => self.expr_array(expr_array),
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
            Expr::Call(expr_call) => self.expr_call(expr_call),
            Expr::Cast(expr_cast) => self.expr_cast(expr_cast),
            Expr::Field(ExprField {
                attrs,
                base,
                dot_token,
                member,
            }) => {
                assert_no_attrs(attrs)?;
                let base = self.expr(base)?;
                let generated_name = VariantField::generate_name(member.clone());
                Ok(quote_spanned! {dot_token.span=>
                    #crate_path::values::ops::assert_arg_is_val(#crate_path::values::aggregate::AggregateValue::struct_of_variant_values(#base) #dot_token #generated_name)
                })
            }
            Expr::Group(_) => unreachable!(),
            Expr::If(expr_if) => self.expr_if(expr_if),
            Expr::Index(expr_index) => self.expr_index(expr_index),
            Expr::Let(expr) => Err(Error::new_spanned(expr, "let expressions not allowed here")),
            Expr::Lit(expr) => self.expr_lit(None, expr),
            Expr::Match(expr_match) => self.expr_match(expr_match),
            Expr::MethodCall(expr_method_call) => self.expr_method_call(expr_method_call),
            Expr::Paren(ExprParen {
                attrs,
                paren_token: _,
                expr,
            }) => {
                assert_no_attrs(attrs)?;
                self.expr_or_block(expr, |block| self.block_uninterpreted(block))
            }
            Expr::Path(expr) => {
                assert_no_attrs(&expr.attrs)?;
                Ok(quote_spanned! {expr.span()=>
                    #crate_path::values::ToVal::to_val(&#expr, #module)
                })
            }
            Expr::Repeat(expr_repeat) => self.expr_repeat(expr_repeat),
            Expr::Struct(expr_struct) => self.expr_struct(expr_struct),
            Expr::Tuple(expr_tuple) => self.expr_tuple(expr_tuple),
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
            Expr::Block(expr_block) => self.expr_block(expr_block, |block| {
                Err(Error::new_spanned(
                    block,
                    "uninterpreted blocks must be surrounded with both parenthesis \
                    and curly braces, like so: `({ uninterpreted() })`",
                ))
            }),
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
