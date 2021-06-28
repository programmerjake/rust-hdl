// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
use rust_hdl::prelude::*;
#[macro_use]
mod common;

#[derive(IO, PlainIO)]
struct TopIO<'ctx> {
    cd: Input<'ctx, ClockDomain>,
    output: Output<'ctx, bool>,
    input: Input<'ctx, bool>,
}

#[test]
fn test_reg() {
    Context::with(|ctx: ContextRef| {
        named!(let (top, io): (_, TopIO) = ctx.top_module());
        named!(let reg = top.reg(io.cd.get(), bool::default()));
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    source_location: SourceLocation($FILE, $LINE-2, 9),
    parent: <None>,
    interface_types: [
        Input(
            IrStructType {
                fields: [
                    IrStructFieldType {
                        name: "clk",
                        ty: BitVector {
                            bit_count: 1,
                        },
                    },
                    IrStructFieldType {
                        name: "rst",
                        ty: BitVector {
                            bit_count: 1,
                        },
                    },
                ],
            },
        ),
        Output(
            BitVector {
                bit_count: 1,
            },
        ),
        Input(
            BitVector {
                bit_count: 1,
            },
        ),
    ],
    interface: [
        Input(
            IrModuleInputData {
                external_value: ExternalInput {
                    value_type: IrStructType {
                        fields: [
                            IrStructFieldType {
                                name: "clk",
                                ty: BitVector {
                                    bit_count: 1,
                                },
                            },
                            IrStructFieldType {
                                name: "rst",
                                ty: BitVector {
                                    bit_count: 1,
                                },
                            },
                        ],
                    },
                },
                path: "io.cd",
            },
        ),
        Output(
            IrWire {
                path: "top"."io.output",
                source_location: SourceLocation($FILE, $LINE-2, 9),
                value_type: BitVector {
                    bit_count: 1,
                },
                assigned_value: <None>,
                ..
            },
        ),
        Input(
            IrModuleInputData {
                external_value: ExternalInput {
                    value_type: BitVector {
                        bit_count: 1,
                    },
                },
                path: "io.input",
            },
        ),
    ],
    wires: {
        "io.output": IrWire {
            source_location: SourceLocation($FILE, $LINE-2, 9),
            value_type: BitVector {
                bit_count: 1,
            },
            assigned_value: <None>,
            ..
        },
    },
    registers: {
        "reg": IrReg {
            source_location: SourceLocation($FILE, $LINE-1, 9),
            value_type: BitVector {
                bit_count: 1,
            },
            data_in: <None>,
            clk: ExtractStructField {
                struct_value: IrModuleInput {
                    module: "top",
                    index: 0,
                    path: "io.cd",
                    value_type: IrStructType {
                        fields: [
                            IrStructFieldType {
                                name: "clk",
                                ty: BitVector {
                                    bit_count: 1,
                                },
                            },
                            IrStructFieldType {
                                name: "rst",
                                ty: BitVector {
                                    bit_count: 1,
                                },
                            },
                        ],
                    },
                },
                struct_field_type: IrStructFieldType {
                    name: "clk",
                    ty: BitVector {
                        bit_count: 1,
                    },
                },
                field_index: 0,
                ..
            },
            reset_enable: ExtractStructField {
                struct_value: IrModuleInput {
                    module: "top",
                    index: 0,
                    path: "io.cd",
                    value_type: IrStructType {
                        fields: [
                            IrStructFieldType {
                                name: "clk",
                                ty: BitVector {
                                    bit_count: 1,
                                },
                            },
                            IrStructFieldType {
                                name: "rst",
                                ty: BitVector {
                                    bit_count: 1,
                                },
                            },
                        ],
                    },
                },
                struct_field_type: IrStructFieldType {
                    name: "rst",
                    ty: BitVector {
                        bit_count: 1,
                    },
                },
                field_index: 1,
                ..
            },
            reset_value: LiteralBits {
                bit_count: 1,
                value: 0x0,
            },
            ..
        },
    },
    ..
}"#
        );
        assert_formats_to!(
            reg,
            r#"
Reg {
    ir: IrReg {
        path: "top"."reg",
        source_location: SourceLocation($FILE, $LINE-173, 9),
        value_type: BitVector {
            bit_count: 1,
        },
        data_in: <None>,
        clk: ExtractStructField {
            struct_value: IrModuleInput {
                module: "top",
                index: 0,
                path: "io.cd",
                value_type: IrStructType {
                    fields: [
                        IrStructFieldType {
                            name: "clk",
                            ty: BitVector {
                                bit_count: 1,
                            },
                        },
                        IrStructFieldType {
                            name: "rst",
                            ty: BitVector {
                                bit_count: 1,
                            },
                        },
                    ],
                },
            },
            struct_field_type: IrStructFieldType {
                name: "clk",
                ty: BitVector {
                    bit_count: 1,
                },
            },
            field_index: 0,
            ..
        },
        reset_enable: ExtractStructField {
            struct_value: IrModuleInput {
                module: "top",
                index: 0,
                path: "io.cd",
                value_type: IrStructType {
                    fields: [
                        IrStructFieldType {
                            name: "clk",
                            ty: BitVector {
                                bit_count: 1,
                            },
                        },
                        IrStructFieldType {
                            name: "rst",
                            ty: BitVector {
                                bit_count: 1,
                            },
                        },
                    ],
                },
            },
            struct_field_type: IrStructFieldType {
                name: "rst",
                ty: BitVector {
                    bit_count: 1,
                },
            },
            field_index: 1,
            ..
        },
        reset_value: LiteralBits {
            bit_count: 1,
            value: 0x0,
        },
        ..
    },
    ..
}"#
        );
        io.output.assign(reg.output());
        let reg = reg.assign_data_in(io.input.get());
        assert_formats_to!(
            reg.output(),
            r#"
Val {
    ir: IrRegOutput {
        path: "top"."reg",
        value_type: BitVector {
            bit_count: 1,
        },
        ..
    },
    value_type: ValueType(
        BitVector {
            bit_count: 1,
        },
    ),
}"#
        );
        assert_formats_to!(
            reg,
            r#"
Reg {
    ir: IrReg {
        path: "top"."reg",
        source_location: SourceLocation($FILE, $LINE-275, 9),
        value_type: BitVector {
            bit_count: 1,
        },
        data_in: IrModuleInput {
            module: "top",
            index: 2,
            path: "io.input",
            value_type: BitVector {
                bit_count: 1,
            },
        },
        clk: ExtractStructField {
            struct_value: IrModuleInput {
                module: "top",
                index: 0,
                path: "io.cd",
                value_type: IrStructType {
                    fields: [
                        IrStructFieldType {
                            name: "clk",
                            ty: BitVector {
                                bit_count: 1,
                            },
                        },
                        IrStructFieldType {
                            name: "rst",
                            ty: BitVector {
                                bit_count: 1,
                            },
                        },
                    ],
                },
            },
            struct_field_type: IrStructFieldType {
                name: "clk",
                ty: BitVector {
                    bit_count: 1,
                },
            },
            field_index: 0,
            ..
        },
        reset_enable: ExtractStructField {
            struct_value: IrModuleInput {
                module: "top",
                index: 0,
                path: "io.cd",
                value_type: IrStructType {
                    fields: [
                        IrStructFieldType {
                            name: "clk",
                            ty: BitVector {
                                bit_count: 1,
                            },
                        },
                        IrStructFieldType {
                            name: "rst",
                            ty: BitVector {
                                bit_count: 1,
                            },
                        },
                    ],
                },
            },
            struct_field_type: IrStructFieldType {
                name: "rst",
                ty: BitVector {
                    bit_count: 1,
                },
            },
            field_index: 1,
            ..
        },
        reset_value: LiteralBits {
            bit_count: 1,
            value: 0x0,
        },
        ..
    },
    ..
}"#
        );
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    source_location: SourceLocation($FILE, $LINE-365, 9),
    parent: <None>,
    interface_types: [
        Input(
            IrStructType {
                fields: [
                    IrStructFieldType {
                        name: "clk",
                        ty: BitVector {
                            bit_count: 1,
                        },
                    },
                    IrStructFieldType {
                        name: "rst",
                        ty: BitVector {
                            bit_count: 1,
                        },
                    },
                ],
            },
        ),
        Output(
            BitVector {
                bit_count: 1,
            },
        ),
        Input(
            BitVector {
                bit_count: 1,
            },
        ),
    ],
    interface: [
        Input(
            IrModuleInputData {
                external_value: ExternalInput {
                    value_type: IrStructType {
                        fields: [
                            IrStructFieldType {
                                name: "clk",
                                ty: BitVector {
                                    bit_count: 1,
                                },
                            },
                            IrStructFieldType {
                                name: "rst",
                                ty: BitVector {
                                    bit_count: 1,
                                },
                            },
                        ],
                    },
                },
                path: "io.cd",
            },
        ),
        Output(
            IrWire {
                path: "top"."io.output",
                source_location: SourceLocation($FILE, $LINE-365, 9),
                value_type: BitVector {
                    bit_count: 1,
                },
                assigned_value: IrRegOutput {
                    path: "top"."reg",
                    value_type: BitVector {
                        bit_count: 1,
                    },
                    ..
                },
                ..
            },
        ),
        Input(
            IrModuleInputData {
                external_value: ExternalInput {
                    value_type: BitVector {
                        bit_count: 1,
                    },
                },
                path: "io.input",
            },
        ),
    ],
    wires: {
        "io.output": IrWire {
            source_location: SourceLocation($FILE, $LINE-365, 9),
            value_type: BitVector {
                bit_count: 1,
            },
            assigned_value: IrRegOutput {
                path: "top"."reg",
                value_type: BitVector {
                    bit_count: 1,
                },
                ..
            },
            ..
        },
    },
    registers: {
        "reg": IrReg {
            source_location: SourceLocation($FILE, $LINE-364, 9),
            value_type: BitVector {
                bit_count: 1,
            },
            data_in: IrModuleInput {
                module: "top",
                index: 2,
                path: "io.input",
                value_type: BitVector {
                    bit_count: 1,
                },
            },
            clk: ExtractStructField {
                struct_value: IrModuleInput {
                    module: "top",
                    index: 0,
                    path: "io.cd",
                    value_type: IrStructType {
                        fields: [
                            IrStructFieldType {
                                name: "clk",
                                ty: BitVector {
                                    bit_count: 1,
                                },
                            },
                            IrStructFieldType {
                                name: "rst",
                                ty: BitVector {
                                    bit_count: 1,
                                },
                            },
                        ],
                    },
                },
                struct_field_type: IrStructFieldType {
                    name: "clk",
                    ty: BitVector {
                        bit_count: 1,
                    },
                },
                field_index: 0,
                ..
            },
            reset_enable: ExtractStructField {
                struct_value: IrModuleInput {
                    module: "top",
                    index: 0,
                    path: "io.cd",
                    value_type: IrStructType {
                        fields: [
                            IrStructFieldType {
                                name: "clk",
                                ty: BitVector {
                                    bit_count: 1,
                                },
                            },
                            IrStructFieldType {
                                name: "rst",
                                ty: BitVector {
                                    bit_count: 1,
                                },
                            },
                        ],
                    },
                },
                struct_field_type: IrStructFieldType {
                    name: "rst",
                    ty: BitVector {
                        bit_count: 1,
                    },
                },
                field_index: 1,
                ..
            },
            reset_value: LiteralBits {
                bit_count: 1,
                value: 0x0,
            },
            ..
        },
    },
    ..
}"#
        );
    });
}
