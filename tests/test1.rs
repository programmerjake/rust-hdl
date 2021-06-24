use rust_hdl::{
    context::Context,
    io::{Input, Output},
    logic::Wire,
    module::Module,
    values::{Int8, UInt32, Value},
};

macro_rules! assert_formats_to {
    ($value:expr, $expected:literal) => {{
        let value = format!("\n{:#?}", $value);
        let expected: &str = $expected;
        assert!(value == expected, "doesn't match expected. value:{}", value);
    }};
}

#[test]
fn test1() {
    Context::with(|ctx| {
        let top = Module::top(ctx, "top");
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    ..
}"#
        );
        let wire: Wire<[bool; 4]> = top.wire("wire");
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {
        "wire": IrWire {
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
    ..
}"#
        );
        wire.assign([true, false, true, true].get_value(ctx));
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {
        "wire": IrWire {
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
    ..
}"#
        );
    });
}

#[test]
fn test_submodule() {
    Context::with(|ctx| {
        let top = Module::top(ctx, "top");
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
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
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    ..
}"#
        );
        let (submodule, submodule_io) = top.submodule("submodule", submodule_io);
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    ..
}"#
        );
        assert_formats_to!(
            submodule,
            r#"
IrModule {
    path: "top"."submodule",
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
            LiteralBits {
                bit_count: 1,
                value: 0x1,
            },
        ),
        Output(
            IrWire {
                path: "top"."submodule"."io.1",
                value_type: BitVector {
                    bit_count: 32,
                },
                assigned_value: <None>,
                ..
            },
        ),
        Input(
            LiteralArray {
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
        ),
    ],
    wires: {
        "io.1": IrWire {
            value_type: BitVector {
                bit_count: 32,
            },
            assigned_value: <None>,
            ..
        },
    },
    ..
}"#
        );
        let (first_input, output, last_input) = submodule_io;
        assert_formats_to!(
            first_input,
            r#"
Input {
    ir: IrInput {
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
    ir: IrInput {
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
            LiteralBits {
                bit_count: 1,
                value: 0x1,
            },
        ),
        Output(
            IrWire {
                path: "top"."submodule"."io.1",
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
            LiteralArray {
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
        ),
    ],
    wires: {
        "io.1": IrWire {
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
    ..
}"#
        );
    });
}

#[test]
fn test_sub_submodule() {
    Context::with(|ctx| {
        let top = Module::top(ctx, "top");
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    ..
}"#
        );
        let submodule_io: (Input<bool>, Output<bool>) = (true.get_value(ctx).into(), top.output());
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    ..
}"#
        );
        let (submodule, submodule_io) = top.submodule("submodule", submodule_io);
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    ..
}"#
        );
        assert_formats_to!(
            submodule,
            r#"
IrModule {
    path: "top"."submodule",
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
            LiteralBits {
                bit_count: 1,
                value: 0x1,
            },
        ),
        Output(
            IrWire {
                path: "top"."submodule"."io.1",
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
            value_type: BitVector {
                bit_count: 1,
            },
            assigned_value: <None>,
            ..
        },
    },
    ..
}"#
        );
        let (sub_submodule, sub_submodule_io) = submodule.submodule("sub_submodule", submodule_io);
        assert_formats_to!(
            top,
            r#"
IrModule {
    path: "top",
    parent: <None>,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    ..
}"#
        );
        assert_formats_to!(
            submodule,
            r#"
IrModule {
    path: "top"."submodule",
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
            LiteralBits {
                bit_count: 1,
                value: 0x1,
            },
        ),
        Output(
            IrWire {
                path: "top"."submodule"."io.1",
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
    ..
}"#
        );
        assert_formats_to!(
            sub_submodule,
            r#"
IrModule {
    path: "top"."submodule"."sub_submodule",
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
            IrModuleInput {
                module: "top"."submodule",
                index: 0,
                path: "io.0",
                value_type: BitVector {
                    bit_count: 1,
                },
            },
        ),
        Output(
            IrWire {
                path: "top"."submodule"."sub_submodule"."io.1",
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
            value_type: BitVector {
                bit_count: 1,
            },
            assigned_value: <None>,
            ..
        },
    },
    ..
}"#
        );
        sub_submodule_io.1.assign(false.get_value(ctx));
        assert_formats_to!(
            sub_submodule,
            r#"
IrModule {
    path: "top"."submodule"."sub_submodule",
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
            IrModuleInput {
                module: "top"."submodule",
                index: 0,
                path: "io.0",
                value_type: BitVector {
                    bit_count: 1,
                },
            },
        ),
        Output(
            IrWire {
                path: "top"."submodule"."sub_submodule"."io.1",
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
    ..
}"#
        );
    });
}
