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

#[test]
fn test_reg() {
    Context::with(|ctx: ContextRef| {
        named!(let (top, cd) = ctx.top_module(ClockDomain::default().get_input(ctx)));
        named!(let reg = top.reg(cd.get(), UInt32::default()));
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
    ],
    interface_write_ends: [
        Input(
            LiteralStruct {
                ty: IrStructType {
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
                owning_module: None,
                fields: [
                    LiteralStructField {
                        name: "clk",
                        value: LiteralBits {
                            bit_count: 1,
                            value: 0x0,
                        },
                    },
                    LiteralStructField {
                        name: "rst",
                        value: LiteralBits {
                            bit_count: 1,
                            value: 0x0,
                        },
                    },
                ],
            },
        ),
    ],
    wires: {},
    registers: {
        "reg": IrReg {
            value_type: BitVector {
                bit_count: 32,
            },
            data_in: <None>,
            clk: ExtractStructField {
                struct_value: IrModuleInput {
                    module: "top",
                    index: 0,
                    path: "io",
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
                    path: "io",
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
                bit_count: 32,
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
            bit_count: 32,
        },
        data_in: <None>,
        clk: ExtractStructField {
            struct_value: IrModuleInput {
                module: "top",
                index: 0,
                path: "io",
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
                path: "io",
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
            bit_count: 32,
            value: 0x0,
        },
        ..
    },
    ..
}"#
        );
        let reg_output = reg.output();
        reg.assign_data_in(UInt32::wrapping_new(0x1234).get_value(ctx));
        assert_formats_to!(
            reg_output,
            r#"
Val {
    ir: IrRegOutput {
        path: "top"."reg",
        value_type: BitVector {
            bit_count: 32,
        },
        ..
    },
    value_type: ValueType(
        BitVector {
            bit_count: 32,
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
    ],
    interface_write_ends: [
        Input(
            LiteralStruct {
                ty: IrStructType {
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
                owning_module: None,
                fields: [
                    LiteralStructField {
                        name: "clk",
                        value: LiteralBits {
                            bit_count: 1,
                            value: 0x0,
                        },
                    },
                    LiteralStructField {
                        name: "rst",
                        value: LiteralBits {
                            bit_count: 1,
                            value: 0x0,
                        },
                    },
                ],
            },
        ),
    ],
    wires: {},
    registers: {
        "reg": IrReg {
            value_type: BitVector {
                bit_count: 32,
            },
            data_in: LiteralBits {
                bit_count: 32,
                value: 0x1234,
            },
            clk: ExtractStructField {
                struct_value: IrModuleInput {
                    module: "top",
                    index: 0,
                    path: "io",
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
                    path: "io",
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
                bit_count: 32,
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
