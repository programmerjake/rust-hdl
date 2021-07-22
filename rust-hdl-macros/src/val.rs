// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;
use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use rust_hdl_int::{Int, IntShape};
use std::{
    collections::{hash_map::Entry, HashMap},
    convert::TryInto,
    fmt,
    panic::Location,
};
use syn::{
    parse::{Parse, ParseStream},
    parse_quote, Arm, Attribute, BinOp, Block, Error, Expr, ExprArray, ExprBinary, ExprBlock,
    ExprGroup, ExprIf, ExprLit, ExprMatch, ExprUnary, Lit, LitInt, Local, Member, Pat, PatIdent,
    PatLit, PatRange, PatRest, PatWild, Path, Stmt, Token, UnOp,
};

use crate::{AttributesFor, RustHdlAttributes};

fn parse_int_literal(
    sign: Option<&Token![-]>,
    lit: &LitInt,
) -> syn::Result<(BigInt, Option<IntShape>)> {
    let mut value: BigInt = lit.base10_parse()?;
    if sign.is_some() {
        value = -value;
    }
    if lit.suffix().is_empty() {
        return Ok((value, None));
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
        Ok((value, Some(shape)))
    }
}

#[allow(dead_code)]
#[track_caller]
fn make_todo_error(tokens: impl ToTokens, arg: Option<fmt::Arguments<'_>>) -> syn::Error {
    match arg {
        Some(arg) => Error::new_spanned(
            tokens,
            format_args!("not yet implemented: {}\nat {}", arg, Location::caller()),
        ),
        None => Error::new_spanned(
            tokens,
            format_args!("not yet implemented\nat {}", Location::caller()),
        ),
    }
}

macro_rules! todo_err {
    ($tokens:expr, $format_string:literal $($args:tt)*) => {
        return Err(make_todo_error($tokens, Some(format_args!($format_string $($args)*))))
    };
    ($tokens:expr) => {
        return Err(make_todo_error($tokens, None))
    };
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

struct PatternDefinedNames(HashMap<Ident, Ident>);

impl PatternDefinedNames {
    fn new() -> Self {
        Self(HashMap::new())
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

enum PatternName {
    EnumVariant { enum_type: Path, variant_path: Path },
    Struct { struct_type: Path },
    Variable { name: Ident },
}

impl PatternName {
    fn get(path: &Path) -> Self {
        if path.is_ident("Some")
            || path.is_ident("None")
            || path.is_ident("Ok")
            || path.is_ident("Err")
        {
            todo!("Self::EnumVariant")
        } else {
            todo!()
        }
    }
}

struct PatternLiteral {
    // TODO
}

impl PatternMatcher<'_> {
    fn condition_or(
        &mut self,
        span: &Span,
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
                    let temp = self.temp_name_maker.make_temp_name(span.clone());
                    let crate_path = &self.val_translator.crate_path;
                    self.tokens.extend(quote_spanned! {span.clone()=>
                        let #temp = #crate_path::values::ops::HdlOr::or(#lhs, #rhs);
                    });
                    MatchCondition::Dynamic(temp)
                }
            })
    }
    fn condition_and(
        &mut self,
        span: &Span,
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
                    let temp = self.temp_name_maker.make_temp_name(span.clone());
                    let crate_path = &self.val_translator.crate_path;
                    self.tokens.extend(quote_spanned! {span.clone()=>
                        let #temp = #crate_path::values::ops::HdlAnd::and(#lhs, #rhs);
                    });
                    MatchCondition::Dynamic(temp)
                }
            })
    }
    fn pat_lit_expr(
        &mut self,
        expr: &Expr,
        neg: Option<&Token![-]>,
        value: &Ident,
    ) -> syn::Result<PatternLiteral> {
        match (expr, neg) {
            (Expr::Lit(lit), neg) => todo_err!(lit),
            (
                Expr::Group(ExprGroup {
                    attrs,
                    group_token,
                    expr,
                }),
                neg,
            ) => {
                assert_no_attrs(attrs)?;
                self.pat_lit_expr(expr, neg, value)
            }
            (
                Expr::Unary(ExprUnary {
                    attrs,
                    op: UnOp::Neg(neg),
                    expr,
                }),
                None,
            ) => {
                assert_no_attrs(attrs)?;
                self.pat_lit_expr(expr, Some(neg), value)
            }
            _ => Err(Error::new_spanned(
                neg.map_or_else(|| expr as &dyn ToTokens, |neg| neg),
                "unsupported match pattern",
            )),
        }
    }
    fn pat(&mut self, pat: &Pat, value: &Ident) -> syn::Result<PatternMatchResult> {
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
                    let mut defined_names = PatternDefinedNames::new();
                    defined_names.define_name(ident.clone(), value.clone())?;
                    Ok(PatternMatchResult {
                        condition: MatchCondition::Static(true),
                        defined_names,
                        verification_match: pat.clone(),
                        verification_match_needs_if: false,
                    })
                }
            }
            Pat::Lit(PatLit { attrs, expr }) => {
                assert_no_attrs(attrs)?;
                let expr = self.pat_lit_expr(expr, None, value)?;
                todo_err!(pat)
            }
            Pat::Or(pat) => todo_err!(pat),
            Pat::Path(pat) => todo_err!(pat),
            Pat::Range(PatRange {
                attrs,
                lo,
                limits,
                hi,
            }) => {
                assert_no_attrs(attrs)?;
                let lo = self.pat_lit_expr(lo, None, value)?;
                let hi = self.pat_lit_expr(hi, None, value)?;
                todo_err!(pat)
            }
            Pat::Slice(pat) => todo_err!(pat),
            Pat::Struct(pat) => todo_err!(pat),
            Pat::Tuple(pat) => todo_err!(pat),
            Pat::TupleStruct(pat) => todo_err!(pat),
            Pat::Wild(PatWild {
                attrs,
                underscore_token,
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

struct MatchArmPatResult {
    match_cond: MatchCondition,
    tokens: TokenStream,
}

struct MatchArmResult {}

struct ValTranslator {
    crate_path: Path,
    module: Ident,
    scope: Ident,
}

impl ValTranslator {
    fn match_arm(&self, arm: &Arm) -> syn::Result<MatchArmResult> {
        let Arm {
            attrs,
            pat,
            guard,
            fat_arrow_token,
            body,
            comma,
        } = arm;
        assert_no_attrs(attrs)?;
        let mut pattern_matcher = PatternMatcher {
            val_translator: self,
            temp_name_maker: TempNameMaker::new(),
            tokens: quote! {},
        };
        let PatternMatchResult {
            condition,
            defined_names,
            verification_match,
            verification_match_needs_if,
        } = pattern_matcher.pat(pat, todo_err!(pat))?;
        if let Some(guard) = guard {
            todo_err!(guard.0);
        }
        todo_err!(fat_arrow_token)
    }

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
            brace_token,
            arms,
        } = expr_match;
        assert_no_attrs(attrs)?;
        let expr = self.expr(expr)?;
        let generated_arms = arms
            .into_iter()
            .map(|arm| {
                self.match_arm(arm)?;
                todo_err!(arm)
            })
            .collect::<syn::Result<Vec<i32>>>()?;
        Ok(quote_spanned! {match_token.span=>
            #crate_path::values::ops::match_aggregate_value(
                #module,
                #expr,
                #scope,
                |__arg| {
                    let #crate_path::values::aggregate::AggregateValueMatchArm {
                        value: __value,
                        match_arm_scope: #scope,
                    } = __arg;
                    ::core::result::Result::<_, ::core::convert::Infallible>::Ok(match __value {
                        #(#generated_arms)*
                    })
                },
            ).unwrap()
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
                let (value, shape) = parse_int_literal(neg, lit)?;
                let value = if let Some(value) = value.to_i128() {
                    quote_spanned! {lit.span()=>
                        #value
                    }
                } else {
                    let (sign, magnitude) = value.into_parts();
                    let sign = match sign {
                        Sign::Minus => quote_spanned! {lit.span()=>
                            #crate_path::bigint::Sign::Minus
                        },
                        Sign::NoSign => quote_spanned! {lit.span()=>
                            #crate_path::bigint::Sign::NoSign
                        },
                        Sign::Plus => quote_spanned! {lit.span()=>
                            #crate_path::bigint::Sign::Plus
                        },
                    };
                    let digits = magnitude.to_u32_digits();
                    quote_spanned! {lit.span()=>
                        #crate_path::bigint::BigInt::from_slice(#sign, &[#(#digits,)*])
                    }
                };
                Ok(match shape {
                    None => quote_spanned! {lit.span()=>
                        #crate_path::values::Value::get_value(&#crate_path::values::integer::Int::new(#value).expect("literal out of range"), #module)
                    },
                    Some(IntShape { bit_count, signed }) => quote_spanned! {lit.span()=>
                        #crate_path::values::Value::get_value(&#crate_path::values::integer::Int::<#crate_path::values::integer::ConstIntShape<#bit_count, #signed>>::new(#value).expect("literal out of range"), #module)
                    },
                })
            }
            Lit::Str(lit) => Err(Error::new_spanned(lit, "string literals are not supported, did you mean to use byte strings instead? (`b\"...\"`)")),
            Lit::Char(lit) => Err(Error::new_spanned(lit, "char literals are not supported, did you mean to use byte literals instead? (`b\'...\'`)")),
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
            Expr::Field(expr) => todo_err!(expr),
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
