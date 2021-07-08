// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
#![no_std]
#![deny(elided_lifetimes_in_paths)]
#![deny(unsafe_op_in_unsafe_fn)]

extern crate alloc;
#[cfg(any(test, feature = "std"))]
extern crate std;

pub mod clocking;
pub mod context;
pub mod export;
mod fmt_utils;
pub mod io;
pub mod ir;
pub mod logic;
pub mod module;
pub mod prelude;
pub mod values;

#[macro_export]
macro_rules! named {
    {
        let $name:ident$(: $ty:ty)? = $mod_or_ctx:ident.$method:ident($($args:expr),*)
    } => {
        let $name $(: $ty)? = $mod_or_ctx.$method(stringify!($name), $($args),*);
    };
    {
        let ($name:ident, $io:pat)$(: $ty:ty)? = $mod_or_ctx:ident.$method:ident($($args:expr),*)
    } => {
        let ($name, $io)$(: $ty)? = $mod_or_ctx.$method(stringify!($name), $($args),*);
    };
}

/// Get the field enumerant for a field of a struct.
/// The struct must be decorated with [`#[derive(Value)]`](rust_hdl_macros::Value).
///
/// Example:
/// ```
/// # use rust_hdl::{field_enum, prelude::*};
/// #[derive(Value)]
/// struct MyStruct {
///     a: Int32,
///     b: UInt16,
/// }
/// #[derive(Value)]
/// struct MyTupleStruct(bool, bool);
/// println!("fields: {:?} {:?}", field_enum!(MyStruct::a), field_enum!(MyTupleStruct::1));
/// ```
///
/// If the named field is invalid, it will fail at compile time:
/// ```compile_fail
/// # use rust_hdl::{field_enum, prelude::*};
/// # #[derive(Value)]
/// # struct MyStruct {
/// #     a: Int32,
/// #     b: UInt16,
/// # }
/// field_enum!(MyStruct::into);
/// ```
///
/// ```compile_fail
/// # use rust_hdl::{field_enum, prelude::*};
/// # #[derive(Value)]
/// # struct MyTupleStruct(bool, bool);
/// field_enum!(MyTupleStruct::2);
/// ```
#[macro_export]
macro_rules! field_enum {
    (<$ty:ty>::$field:ident) => {{
        const VALUE: <$ty as $crate::values::aggregate::StructValue>::FieldEnum =
            <<$ty as $crate::values::aggregate::StructValue>::FieldEnum>::$field;
        VALUE
    }};
    (<$ty:ty>::$field:literal) => {{
        const VALUE: <$ty as $crate::values::aggregate::StructValue>::FieldEnum = {
            let index: usize = $field;
            let _ = [(); <$ty as $crate::values::aggregate::StructValue>::FIELD_COUNT][index];
            index
        };
        VALUE
    }};
    ($ty:ident::$field:tt) => {
        $crate::field_enum!(<$ty>::$field)
    };
}

/// Get the field of a struct wrapped in [`Val`].
///
/// You can use it to get a field:
/// `field!((val_expr).field)`
/// You can also use it to get a field's field (or a field of a field's field, and so on):
/// or `field!((val_expr).field1.subfield2)`
///
/// [`Val`]: crate::prelude::Val
///
/// Example:
/// ```
/// # use rust_hdl::prelude::*;
/// #[derive(Value)]
/// struct MyStruct {
///     a: MyTupleStruct,
/// }
/// #[derive(Value)]
/// struct MyTupleStruct(bool, bool);
///
/// fn get_field<'ctx: 'scope, 'scope>(v: Val<'ctx, 'scope, MyStruct>) -> Val<'ctx, 'scope, bool> {
///     field!((v).a.1)
/// }
/// ```
#[macro_export]
macro_rules! field {
    (($val:expr).$first_field:tt$(.$rest:tt)+) => {
        $crate::field!(($crate::field!(($val).$first_field))$(.$rest)+)
    };
    (($val:expr).$field:tt) => {
        {
            let val: $crate::values::Val<'_, '_, _> = $val;
            val.extract_field_unchecked_macro_helper(|opt, struct_of_field_enums| (struct_of_field_enums.$field, opt.map(|v| (&v.0.$field, v.1))))
        }
    };
}
