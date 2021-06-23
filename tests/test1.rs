use rust_hdl::{
    context::{Context, IrModule},
    io::{Input, Output},
    logic::Wire,
    values::{Int8, UInt32, ValueTrait},
};

macro_rules! assert_formats_to {
    ($value:expr, $expected:literal) => {{
        let value = format!("\n{:#?}", $value);
        let expected: &str = $expected;
        assert!(value == expected, "value:{}\nexpected:{}", value, expected);
    }};
}

#[test]
fn test1() {
    Context::with(|ctx| {
        let mod0 = IrModule::new(ctx, |_, _| {}, &mut ());
        assert_formats_to!(
            mod0,
            r"
IrModule {
    id: 0,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    ..
}"
        );
        let wire = Wire::<[bool; 4]>::new(mod0);
        assert_formats_to!(
            mod0,
            r"
IrModule {
    id: 0,
    interface_types: [],
    interface_write_ends: [],
    wires: {
        0.0: IrWire {
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
}"
        );
        wire.assign([true, false, true, true].get_value(ctx));
        assert_formats_to!(
            mod0,
            r"
IrModule {
    id: 0,
    interface_types: [],
    interface_write_ends: [],
    wires: {
        0.0: IrWire {
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
}"
        );
    });
}

#[test]
fn test_submodule() {
    Context::with(|ctx| {
        let top = IrModule::new(ctx, |_, _| {}, &mut ());
        assert_formats_to!(
            top,
            r"
IrModule {
    id: 0,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    ..
}"
        );
        let mut submodule_interface = (
            Input::from(true.get_value(ctx)),
            Output::<UInt32>::new(top),
            Input::from([Int8::wrapping_new(0x23)].get_value(ctx)),
        );
        assert_formats_to!(
            top,
            r"
IrModule {
    id: 0,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    ..
}"
        );
        let submodule = IrModule::new(ctx, |_, _| {}, &mut submodule_interface);
        assert_formats_to!(
            top,
            r"
IrModule {
    id: 0,
    interface_types: [],
    interface_write_ends: [],
    wires: {},
    ..
}"
        );
        assert_formats_to!(
            submodule,
            r"
IrModule {
    id: 1,
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
                id: 1.0,
                value_type: BitVector {
                    bit_count: 32,
                },
                assigned_value: OnceCell(Uninit),
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
        1.0: IrWire {
            value_type: BitVector {
                bit_count: 32,
            },
            assigned_value: <None>,
            ..
        },
    },
    ..
}"
        );
        let (first_input, output, last_input) = submodule_interface;
        assert_formats_to!(
            first_input,
            r"
Input {
    ir_input: IrInput {
        value: IrModuleInput {
            module: 1,
            index: 0,
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
}"
        );
        assert_formats_to!(
            output,
            r"
Output {
    ir: WriteEnd(
        IrWire {
            id: 1.0,
            value_type: BitVector {
                bit_count: 32,
            },
            assigned_value: OnceCell(Uninit),
            ..
        },
    ),
    ..
}"
        );
        assert_formats_to!(
            last_input,
            r"
Input {
    ir_input: IrInput {
        value: IrModuleInput {
            module: 1,
            index: 2,
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
}"
        );
        output.assign(UInt32::wrapping_new(0xDEADBEEFu32).get_value(ctx));
        assert_formats_to!(
            submodule,
            r"
IrModule {
    id: 1,
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
                id: 1.0,
                value_type: BitVector {
                    bit_count: 32,
                },
                assigned_value: OnceCell(
                    LiteralBits {
                        bit_count: 32,
                        value: 0xdeadbeef,
                    },
                ),
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
        1.0: IrWire {
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
}"
        );
    });
}
