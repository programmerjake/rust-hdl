// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
use rust_hdl::prelude::*;
#[macro_use]
mod common;

#[derive(Value, IO, Default)]
struct EmptyType;

#[derive(Value, Default)]
struct Struct {
    a: bool,
    b: EmptyType,
    c: TupleStruct,
}

#[derive(IO)]
struct IOStruct<'ctx> {
    a: Input<'ctx, bool>,
    b: Output<'ctx, bool>,
    c: EmptyType,
    d: IOTupleStruct<'ctx>,
}

#[derive(Value, Default)]
struct TupleStruct(bool, Int8);

#[derive(IO)]
struct IOTupleStruct<'ctx>(Output<'ctx, Struct>);

#[test]
fn test_structs() {
    Context::with(|ctx| {
        named!(let (top, ()) = ctx.top_module());
        named!(let _wire: Wire<Struct> = top.wire());
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    source_location: SourceLocation($FILE, $LINE-2, 9),
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {
        "_wire": IrWire {
            source_location: SourceLocation($FILE, $LINE-1, 9),
            value_type: IrStructType {
                fields: [
                    IrStructFieldType {
                        name: "a",
                        ty: BitVector {
                            bit_count: 1,
                        },
                    },
                    IrStructFieldType {
                        name: "b",
                        ty: IrStructType {
                            fields: [],
                        },
                    },
                    IrStructFieldType {
                        name: "c",
                        ty: IrStructType {
                            fields: [
                                IrStructFieldType {
                                    name: "0",
                                    ty: BitVector {
                                        bit_count: 1,
                                    },
                                },
                                IrStructFieldType {
                                    name: "1",
                                    ty: BitVector {
                                        bit_count: 8,
                                    },
                                },
                            ],
                        },
                    },
                ],
            },
            assigned_value: <None>,
            ..
        },
    },
    registers: {},
    ..
}"#
        );
        let io = IOStruct {
            a: false.get_value(ctx).into(),
            b: top.output(),
            c: EmptyType,
            d: IOTupleStruct(top.output()),
        };
        named!(let (submodule, io) = top.submodule(io));
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    source_location: SourceLocation($FILE, $LINE-64, 9),
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {
        "_wire": IrWire {
            source_location: SourceLocation($FILE, $LINE-63, 9),
            value_type: IrStructType {
                fields: [
                    IrStructFieldType {
                        name: "a",
                        ty: BitVector {
                            bit_count: 1,
                        },
                    },
                    IrStructFieldType {
                        name: "b",
                        ty: IrStructType {
                            fields: [],
                        },
                    },
                    IrStructFieldType {
                        name: "c",
                        ty: IrStructType {
                            fields: [
                                IrStructFieldType {
                                    name: "0",
                                    ty: BitVector {
                                        bit_count: 1,
                                    },
                                },
                                IrStructFieldType {
                                    name: "1",
                                    ty: BitVector {
                                        bit_count: 8,
                                    },
                                },
                            ],
                        },
                    },
                ],
            },
            assigned_value: <None>,
            ..
        },
    },
    registers: {},
    ..
}"#
        );
        assert_formats_to!(
            submodule,
            r#"
IrModule {
    path: "top"."submodule",
    source_location: SourceLocation($FILE, $LINE-56, 9),
    parent: "top",
    interface_types: [
        Input(
            BitVector {
                bit_count: 1,
            },
        ),
        Output(
            BitVector {
                bit_count: 1,
            },
        ),
        Output(
            IrStructType {
                fields: [
                    IrStructFieldType {
                        name: "a",
                        ty: BitVector {
                            bit_count: 1,
                        },
                    },
                    IrStructFieldType {
                        name: "b",
                        ty: IrStructType {
                            fields: [],
                        },
                    },
                    IrStructFieldType {
                        name: "c",
                        ty: IrStructType {
                            fields: [
                                IrStructFieldType {
                                    name: "0",
                                    ty: BitVector {
                                        bit_count: 1,
                                    },
                                },
                                IrStructFieldType {
                                    name: "1",
                                    ty: BitVector {
                                        bit_count: 8,
                                    },
                                },
                            ],
                        },
                    },
                ],
            },
        ),
    ],
    interface_write_ends: [
        Input(
            Input {
                value: LiteralBits {
                    bit_count: 1,
                    value: 0x0,
                },
            },
        ),
        Output(
            IrWire {
                path: "top"."submodule"."io.b",
                source_location: SourceLocation($FILE, $LINE-56, 9),
                value_type: BitVector {
                    bit_count: 1,
                },
                assigned_value: <None>,
                ..
            },
        ),
        Output(
            IrWire {
                path: "top"."submodule"."io.d.0",
                source_location: SourceLocation($FILE, $LINE-56, 9),
                value_type: IrStructType {
                    fields: [
                        IrStructFieldType {
                            name: "a",
                            ty: BitVector {
                                bit_count: 1,
                            },
                        },
                        IrStructFieldType {
                            name: "b",
                            ty: IrStructType {
                                fields: [],
                            },
                        },
                        IrStructFieldType {
                            name: "c",
                            ty: IrStructType {
                                fields: [
                                    IrStructFieldType {
                                        name: "0",
                                        ty: BitVector {
                                            bit_count: 1,
                                        },
                                    },
                                    IrStructFieldType {
                                        name: "1",
                                        ty: BitVector {
                                            bit_count: 8,
                                        },
                                    },
                                ],
                            },
                        },
                    ],
                },
                assigned_value: <None>,
                ..
            },
        ),
    ],
    wires: {
        "io.b": IrWire {
            source_location: SourceLocation($FILE, $LINE-56, 9),
            value_type: BitVector {
                bit_count: 1,
            },
            assigned_value: <None>,
            ..
        },
        "io.d.0": IrWire {
            source_location: SourceLocation($FILE, $LINE-56, 9),
            value_type: IrStructType {
                fields: [
                    IrStructFieldType {
                        name: "a",
                        ty: BitVector {
                            bit_count: 1,
                        },
                    },
                    IrStructFieldType {
                        name: "b",
                        ty: IrStructType {
                            fields: [],
                        },
                    },
                    IrStructFieldType {
                        name: "c",
                        ty: IrStructType {
                            fields: [
                                IrStructFieldType {
                                    name: "0",
                                    ty: BitVector {
                                        bit_count: 1,
                                    },
                                },
                                IrStructFieldType {
                                    name: "1",
                                    ty: BitVector {
                                        bit_count: 8,
                                    },
                                },
                            ],
                        },
                    },
                ],
            },
            assigned_value: <None>,
            ..
        },
    },
    registers: {},
    ..
}"#
        );
        let field = field!((io.d.0.read()).c.1);
        assert_formats_to!(
            field,
            r#"
Val {
    ir: ExtractStructField {
        struct_value: ExtractStructField {
            struct_value: IrWireRead {
                path: "top"."submodule"."io.d.0",
                value_type: IrStructType {
                    fields: [
                        IrStructFieldType {
                            name: "a",
                            ty: BitVector {
                                bit_count: 1,
                            },
                        },
                        IrStructFieldType {
                            name: "b",
                            ty: IrStructType {
                                fields: [],
                            },
                        },
                        IrStructFieldType {
                            name: "c",
                            ty: IrStructType {
                                fields: [
                                    IrStructFieldType {
                                        name: "0",
                                        ty: BitVector {
                                            bit_count: 1,
                                        },
                                    },
                                    IrStructFieldType {
                                        name: "1",
                                        ty: BitVector {
                                            bit_count: 8,
                                        },
                                    },
                                ],
                            },
                        },
                    ],
                },
                ..
            },
            struct_field_type: IrStructFieldType {
                name: "c",
                ty: IrStructType {
                    fields: [
                        IrStructFieldType {
                            name: "0",
                            ty: BitVector {
                                bit_count: 1,
                            },
                        },
                        IrStructFieldType {
                            name: "1",
                            ty: BitVector {
                                bit_count: 8,
                            },
                        },
                    ],
                },
            },
            field_index: 2,
            ..
        },
        struct_field_type: IrStructFieldType {
            name: "1",
            ty: BitVector {
                bit_count: 8,
            },
        },
        field_index: 1,
        ..
    },
    value_type: ValueType(
        BitVector {
            bit_count: 8,
        },
    ),
}"#
        );
    });
}

#[test]
fn test1() {
    Context::with(|ctx: ContextRef<'_>| {
        named!(let (top, ()) = ctx.top_module());
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    source_location: SourceLocation($FILE, $LINE-1, 9),
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    registers: {},
    ..
}"#
        );
        named!(let wire: Wire<[bool; 4]> = top.wire());
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    source_location: SourceLocation($FILE, $LINE-16, 9),
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {
        "wire": IrWire {
            source_location: SourceLocation($FILE, $LINE-1, 9),
            value_type: Array {
                element: BitVector {
                    bit_count: 1,
                },
                length: 4,
            },
            assigned_value: <None>,
            ..
        },
    },
    registers: {},
    ..
}"#
        );
        wire.assign([true, false, true, true].get_value(ctx));
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    source_location: SourceLocation($FILE, $LINE-43, 9),
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {
        "wire": IrWire {
            source_location: SourceLocation($FILE, $LINE-28, 9),
            value_type: Array {
                element: BitVector {
                    bit_count: 1,
                },
                length: 4,
            },
            assigned_value: LiteralArray {
                element_type: BitVector {
                    bit_count: 1,
                },
                owning_module: None,
                elements: [
                    LiteralBits {
                        bit_count: 1,
                        value: 0x1,
                    },
                    LiteralBits {
                        bit_count: 1,
                        value: 0x0,
                    },
                    LiteralBits {
                        bit_count: 1,
                        value: 0x1,
                    },
                    LiteralBits {
                        bit_count: 1,
                        value: 0x1,
                    },
                ],
            },
            ..
        },
    },
    registers: {},
    ..
}"#
        );
    });
}

#[test]
fn test_submodule() {
    Context::with(|ctx: ContextRef<'_>| {
        named!(let (top, ()) = ctx.top_module());
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    source_location: SourceLocation($FILE, $LINE-1, 9),
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    registers: {},
    ..
}"#
        );
        let submodule_io: (Input<bool>, Output<UInt32>, Input<[Int8; 1]>) = (
            true.get_value(ctx).into(),
            top.output(),
            [Int8::wrapping_new(0x23)].get_value(ctx).into(),
        );
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    source_location: SourceLocation($FILE, $LINE-20, 9),
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    registers: {},
    ..
}"#
        );
        named!(let (submodule, submodule_io) = top.submodule(submodule_io));
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    source_location: SourceLocation($FILE, $LINE-35, 9),
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    registers: {},
    ..
}"#
        );
        assert_formats_to!(
            submodule,
            r#"
IrModule {
    path: "top"."submodule",
    source_location: SourceLocation($FILE, $LINE-15, 9),
    parent: "top",
    interface_types: [
        Input(
            BitVector {
                bit_count: 1,
            },
        ),
        Output(
            BitVector {
                bit_count: 32,
            },
        ),
        Input(
            Array {
                element: BitVector {
                    bit_count: 8,
                },
                length: 1,
            },
        ),
    ],
    interface_write_ends: [
        Input(
            Input {
                value: LiteralBits {
                    bit_count: 1,
                    value: 0x1,
                },
            },
        ),
        Output(
            IrWire {
                path: "top"."submodule"."io.1",
                source_location: SourceLocation($FILE, $LINE-15, 9),
                value_type: BitVector {
                    bit_count: 32,
                },
                assigned_value: <None>,
                ..
            },
        ),
        Input(
            Input {
                value: LiteralArray {
                    element_type: BitVector {
                        bit_count: 8,
                    },
                    owning_module: None,
                    elements: [
                        LiteralBits {
                            bit_count: 8,
                            value: 0x23,
                        },
                    ],
                },
            },
        ),
    ],
    wires: {
        "io.1": IrWire {
            source_location: SourceLocation($FILE, $LINE-15, 9),
            value_type: BitVector {
                bit_count: 32,
            },
            assigned_value: <None>,
            ..
        },
    },
    registers: {},
    ..
}"#
        );
        let (first_input, output, last_input) = submodule_io;
        assert_formats_to!(
            first_input,
            r#"
Input {
    ir: Input {
        value: IrModuleInput {
            module: "top"."submodule",
            index: 0,
            path: "io.0",
            value_type: BitVector {
                bit_count: 1,
            },
        },
    },
    value_type: ValueType(
        BitVector {
            bit_count: 1,
        },
    ),
}"#
        );
        assert_formats_to!(
            output,
            r#"
Output {
    ir: WriteEnd(
        IrWire {
            path: "top"."submodule"."io.1",
            source_location: SourceLocation($FILE, $LINE-115, 9),
            value_type: BitVector {
                bit_count: 32,
            },
            assigned_value: <None>,
            ..
        },
    ),
    ..
}"#
        );
        assert_formats_to!(
            last_input,
            r#"
Input {
    ir: Input {
        value: IrModuleInput {
            module: "top"."submodule",
            index: 2,
            path: "io.2",
            value_type: Array {
                element: BitVector {
                    bit_count: 8,
                },
                length: 1,
            },
        },
    },
    value_type: ValueType(
        Array {
            element: BitVector {
                bit_count: 8,
            },
            length: 1,
        },
    ),
}"#
        );
        output.assign(UInt32::wrapping_new(0xDEADBEEFu32).get_value(ctx));
        assert_formats_to!(
            submodule,
            r#"
IrModule {
    path: "top"."submodule",
    source_location: SourceLocation($FILE, $LINE-161, 9),
    parent: "top",
    interface_types: [
        Input(
            BitVector {
                bit_count: 1,
            },
        ),
        Output(
            BitVector {
                bit_count: 32,
            },
        ),
        Input(
            Array {
                element: BitVector {
                    bit_count: 8,
                },
                length: 1,
            },
        ),
    ],
    interface_write_ends: [
        Input(
            Input {
                value: LiteralBits {
                    bit_count: 1,
                    value: 0x1,
                },
            },
        ),
        Output(
            IrWire {
                path: "top"."submodule"."io.1",
                source_location: SourceLocation($FILE, $LINE-161, 9),
                value_type: BitVector {
                    bit_count: 32,
                },
                assigned_value: LiteralBits {
                    bit_count: 32,
                    value: 0xdeadbeef,
                },
                ..
            },
        ),
        Input(
            Input {
                value: LiteralArray {
                    element_type: BitVector {
                        bit_count: 8,
                    },
                    owning_module: None,
                    elements: [
                        LiteralBits {
                            bit_count: 8,
                            value: 0x23,
                        },
                    ],
                },
            },
        ),
    ],
    wires: {
        "io.1": IrWire {
            source_location: SourceLocation($FILE, $LINE-161, 9),
            value_type: BitVector {
                bit_count: 32,
            },
            assigned_value: LiteralBits {
                bit_count: 32,
                value: 0xdeadbeef,
            },
            ..
        },
    },
    registers: {},
    ..
}"#
        );
    });
}

#[test]
fn test_sub_submodule() {
    Context::with(|ctx| {
        named!(let (top, ()) = ctx.top_module());
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    source_location: SourceLocation($FILE, $LINE-1, 9),
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    registers: {},
    ..
}"#
        );
        let submodule_io: (Input<bool>, Output<bool>) = (true.get_value(ctx).into(), top.output());
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    source_location: SourceLocation($FILE, $LINE-16, 9),
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    registers: {},
    ..
}"#
        );
        named!(let (submodule, submodule_io) = top.submodule(submodule_io));
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    source_location: SourceLocation($FILE, $LINE-31, 9),
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    registers: {},
    ..
}"#
        );
        assert_formats_to!(
            submodule,
            r#"
IrModule {
    path: "top"."submodule",
    source_location: SourceLocation($FILE, $LINE-15, 9),
    parent: "top",
    interface_types: [
        Input(
            BitVector {
                bit_count: 1,
            },
        ),
        Output(
            BitVector {
                bit_count: 1,
            },
        ),
    ],
    interface_write_ends: [
        Input(
            Input {
                value: LiteralBits {
                    bit_count: 1,
                    value: 0x1,
                },
            },
        ),
        Output(
            IrWire {
                path: "top"."submodule"."io.1",
                source_location: SourceLocation($FILE, $LINE-15, 9),
                value_type: BitVector {
                    bit_count: 1,
                },
                assigned_value: <None>,
                ..
            },
        ),
    ],
    wires: {
        "io.1": IrWire {
            source_location: SourceLocation($FILE, $LINE-15, 9),
            value_type: BitVector {
                bit_count: 1,
            },
            assigned_value: <None>,
            ..
        },
    },
    registers: {},
    ..
}"#
        );
        named!(let (sub_submodule, sub_submodule_io) = submodule.submodule(submodule_io));
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    source_location: SourceLocation($FILE, $LINE-100, 9),
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    registers: {},
    ..
}"#
        );
        assert_formats_to!(
            submodule,
            r#"
IrModule {
    path: "top"."submodule",
    source_location: SourceLocation($FILE, $LINE-84, 9),
    parent: "top",
    interface_types: [
        Input(
            BitVector {
                bit_count: 1,
            },
        ),
        Output(
            BitVector {
                bit_count: 1,
            },
        ),
    ],
    interface_write_ends: [
        Input(
            Input {
                value: LiteralBits {
                    bit_count: 1,
                    value: 0x1,
                },
            },
        ),
        Output(
            IrWire {
                path: "top"."submodule"."io.1",
                source_location: SourceLocation($FILE, $LINE-84, 9),
                value_type: BitVector {
                    bit_count: 1,
                },
                assigned_value: IrOutputRead(
                    IrOutputReadData {
                        module: "top"."submodule",
                        value_type: BitVector {
                            bit_count: 1,
                        },
                        write_data: IrOutputWriteData {
                            writing_module: "top"."submodule"."sub_submodule",
                            index: 1,
                        },
                    },
                ),
                ..
            },
        ),
    ],
    wires: {
        "io.1": IrWire {
            source_location: SourceLocation($FILE, $LINE-84, 9),
            value_type: BitVector {
                bit_count: 1,
            },
            assigned_value: IrOutputRead(
                IrOutputReadData {
                    module: "top"."submodule",
                    value_type: BitVector {
                        bit_count: 1,
                    },
                    write_data: IrOutputWriteData {
                        writing_module: "top"."submodule"."sub_submodule",
                        index: 1,
                    },
                },
            ),
            ..
        },
    },
    registers: {},
    ..
}"#
        );
        assert_formats_to!(
            sub_submodule,
            r#"
IrModule {
    path: "top"."submodule"."sub_submodule",
    source_location: SourceLocation($FILE, $LINE-91, 9),
    parent: "top"."submodule",
    interface_types: [
        Input(
            BitVector {
                bit_count: 1,
            },
        ),
        Output(
            BitVector {
                bit_count: 1,
            },
        ),
    ],
    interface_write_ends: [
        Input(
            Input {
                value: IrModuleInput {
                    module: "top"."submodule",
                    index: 0,
                    path: "io.0",
                    value_type: BitVector {
                        bit_count: 1,
                    },
                },
            },
        ),
        Output(
            IrWire {
                path: "top"."submodule"."sub_submodule"."io.1",
                source_location: SourceLocation($FILE, $LINE-91, 9),
                value_type: BitVector {
                    bit_count: 1,
                },
                assigned_value: <None>,
                ..
            },
        ),
    ],
    wires: {
        "io.1": IrWire {
            source_location: SourceLocation($FILE, $LINE-91, 9),
            value_type: BitVector {
                bit_count: 1,
            },
            assigned_value: <None>,
            ..
        },
    },
    registers: {},
    ..
}"#
        );
        sub_submodule_io.1.assign(false.get_value(ctx));
        assert_formats_to!(
            sub_submodule,
            r#"
IrModule {
    path: "top"."submodule"."sub_submodule",
    source_location: SourceLocation($FILE, $LINE-150, 9),
    parent: "top"."submodule",
    interface_types: [
        Input(
            BitVector {
                bit_count: 1,
            },
        ),
        Output(
            BitVector {
                bit_count: 1,
            },
        ),
    ],
    interface_write_ends: [
        Input(
            Input {
                value: IrModuleInput {
                    module: "top"."submodule",
                    index: 0,
                    path: "io.0",
                    value_type: BitVector {
                        bit_count: 1,
                    },
                },
            },
        ),
        Output(
            IrWire {
                path: "top"."submodule"."sub_submodule"."io.1",
                source_location: SourceLocation($FILE, $LINE-150, 9),
                value_type: BitVector {
                    bit_count: 1,
                },
                assigned_value: LiteralBits {
                    bit_count: 1,
                    value: 0x0,
                },
                ..
            },
        ),
    ],
    wires: {
        "io.1": IrWire {
            source_location: SourceLocation($FILE, $LINE-150, 9),
            value_type: BitVector {
                bit_count: 1,
            },
            assigned_value: LiteralBits {
                bit_count: 1,
                value: 0x0,
            },
            ..
        },
    },
    registers: {},
    ..
}"#
        );
    });
}
