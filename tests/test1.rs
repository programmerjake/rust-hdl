use rust_hdl::{
    context::{Context, Module},
    logic::Wire,
    values::ValueTrait,
};

#[test]
fn test1() {
    Context::with(|ctx| {
        let mod0 = Module::new(ctx);
        assert_eq!(
            format!("\n{:#?}", mod0),
            r"
Module {
    id: 0,
    wires: {},
    ..
}"
        );
        let wire = Wire::<[bool; 4]>::new(mod0);
        assert_eq!(
            format!("\n{:#?}", mod0),
            r"
Module {
    id: 0,
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
        assert_eq!(
            format!("\n{:#?}", mod0),
            r"
Module {
    id: 0,
    wires: {
        0.0: IrWire {
            value_type: Array {
                element: BitVector {
                    bit_count: 1,
                },
                length: 4,
            },
            assigned_value: LiteralArray(
                LiteralArray {
                    element_type: BitVector {
                        bit_count: 1,
                    },
                    elements: [
                        LiteralBits(
                            LiteralBits {
                                bit_count: 1,
                                value: 0x1,
                            },
                        ),
                        LiteralBits(
                            LiteralBits {
                                bit_count: 1,
                                value: 0x0,
                            },
                        ),
                        LiteralBits(
                            LiteralBits {
                                bit_count: 1,
                                value: 0x1,
                            },
                        ),
                        LiteralBits(
                            LiteralBits {
                                bit_count: 1,
                                value: 0x1,
                            },
                        ),
                    ],
                },
            ),
            ..
        },
    },
    ..
}"
        );
    });
}
