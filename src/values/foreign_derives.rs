// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::prelude::*;
use core::option::Option;

#[cfg(todo)]
#[derive(Value, FixedTypeValue)]
#[rust_hdl(crate = crate, real_type_name = Option)]
#[allow(dead_code)]
pub enum MyOption<T> {
    None,
    Some(T),
}

pub type Tuple0 = ();
#[cfg(todo)]
#[derive(Value, FixedTypeValue)]
#[rust_hdl(crate = crate, real_type_name = Tuple0)]
#[allow(dead_code)]
pub struct MyTuple0();

pub type Tuple1<T1> = (T1,);
#[cfg(todo)]
#[derive(Value, FixedTypeValue)]
#[rust_hdl(crate = crate, real_type_name = Tuple1)]
#[allow(dead_code)]
pub struct MyTuple1<T1>(T1);

pub type Tuple2<T1, T2> = (T1, T2);
#[cfg(todo)]
#[derive(Value, FixedTypeValue)]
#[rust_hdl(crate = crate, real_type_name = Tuple2)]
#[allow(dead_code)]
pub struct MyTuple2<T1, T2>(T1, T2);

pub type Tuple3<T1, T2, T3> = (T1, T2, T3);
#[cfg(todo)]
#[derive(Value, FixedTypeValue)]
#[rust_hdl(crate = crate, real_type_name = Tuple3)]
#[allow(dead_code)]
pub struct MyTuple3<T1, T2, T3>(T1, T2, T3);

pub type Tuple4<T1, T2, T3, T4> = (T1, T2, T3, T4);
#[cfg(todo)]
#[derive(Value, FixedTypeValue)]
#[rust_hdl(crate = crate, real_type_name = Tuple4)]
#[allow(dead_code)]
pub struct MyTuple4<T1, T2, T3, T4>(T1, T2, T3, T4);

pub type Tuple5<T1, T2, T3, T4, T5> = (T1, T2, T3, T4, T5);
#[cfg(todo)]
#[derive(Value, FixedTypeValue)]
#[rust_hdl(crate = crate, real_type_name = Tuple5)]
#[allow(dead_code)]
pub struct MyTuple5<T1, T2, T3, T4, T5>(T1, T2, T3, T4, T5);

pub type Tuple6<T1, T2, T3, T4, T5, T6> = (T1, T2, T3, T4, T5, T6);
#[cfg(todo)]
#[derive(Value, FixedTypeValue)]
#[rust_hdl(crate = crate, real_type_name = Tuple6)]
#[allow(dead_code)]
pub struct MyTuple6<T1, T2, T3, T4, T5, T6>(T1, T2, T3, T4, T5, T6);

pub type Tuple7<T1, T2, T3, T4, T5, T6, T7> = (T1, T2, T3, T4, T5, T6, T7);
#[cfg(todo)]
#[derive(Value, FixedTypeValue)]
#[rust_hdl(crate = crate, real_type_name = Tuple7)]
#[allow(dead_code)]
pub struct MyTuple7<T1, T2, T3, T4, T5, T6, T7>(T1, T2, T3, T4, T5, T6, T7);

pub type Tuple8<T1, T2, T3, T4, T5, T6, T7, T8> = (T1, T2, T3, T4, T5, T6, T7, T8);
#[cfg(todo)]
#[derive(Value, FixedTypeValue)]
#[rust_hdl(crate = crate, real_type_name = Tuple8)]
#[allow(dead_code)]
pub struct MyTuple8<T1, T2, T3, T4, T5, T6, T7, T8>(T1, T2, T3, T4, T5, T6, T7, T8);

pub type Tuple9<T1, T2, T3, T4, T5, T6, T7, T8, T9> = (T1, T2, T3, T4, T5, T6, T7, T8, T9);
#[cfg(todo)]
#[derive(Value, FixedTypeValue)]
#[rust_hdl(crate = crate, real_type_name = Tuple9)]
#[allow(dead_code)]
pub struct MyTuple9<T1, T2, T3, T4, T5, T6, T7, T8, T9>(T1, T2, T3, T4, T5, T6, T7, T8, T9);

pub type Tuple10<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10> =
    (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
#[cfg(todo)]
#[derive(Value, FixedTypeValue)]
#[rust_hdl(crate = crate, real_type_name = Tuple10)]
#[allow(dead_code)]
pub struct MyTuple10<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10>(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
);

pub type Tuple11<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11> =
    (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
#[cfg(todo)]
#[derive(Value, FixedTypeValue)]
#[rust_hdl(crate = crate, real_type_name = Tuple11)]
#[allow(dead_code)]
pub struct MyTuple11<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11>(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
);

pub type Tuple12<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12> =
    (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);
#[cfg(todo)]
#[derive(Value, FixedTypeValue)]
#[rust_hdl(crate = crate, real_type_name = Tuple12)]
#[allow(dead_code)]
pub struct MyTuple12<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12>(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
);
