// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
use rust_hdl::prelude::*;

macro_rules! assert_formats_to {
    ($value:expr, $expected:literal) => {{
        let value = format!("\n{:#?}", $value);
        let expected: &str = $expected;
        assert!(value == expected, "doesn't match expected. value:{}", value);
    }};
}

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
    interface_write_ends: [
        Input(
            ExternalInput {
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
        ),
        Output(
            IrWire {
                path: "top"."io.output",
                value_type: BitVector {
                    bit_count: 1,
                },
                assigned_value: <None>,
                ..
            },
        ),
        Input(
            ExternalInput {
                value_type: BitVector {
                    bit_count: 1,
                },
            },
        ),
    ],
    wires: {
        "io.output": IrWire {
            value_type: BitVector {
                bit_count: 1,
            },
            assigned_value: <None>,
            ..
        },
    },
    registers: {
        "reg": IrReg {
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
        let reg_output = reg.output();
        io.output.assign(reg_output);
        reg.assign_data_in(io.input.get());
        assert_formats_to!(
            reg_output,
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
            top,
            r#"
IrModule {
    path: "top",
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
    interface_write_ends: [
        Input(
            ExternalInput {
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
        ),
        Output(
            IrWire {
                path: "top"."io.output",
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
            ExternalInput {
                value_type: BitVector {
                    bit_count: 1,
                },
            },
        ),
    ],
    wires: {
        "io.output": IrWire {
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
