// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{ContextRef, Intern},
    fmt_utils::{debug_format_option_as_value_or_invalid, debug_format_option_as_value_or_none},
    ir::{
        logic::{IrWire, IrWireRef},
        module::IrModuleRef,
        symbols::IrSymbol,
        types::IrValueTypeRef,
        values::{IrValue, IrValueRef},
    },
};
use alloc::borrow::Cow;
use core::{
    fmt,
    hash::{Hash, Hasher},
    mem, ptr,
};
use once_cell::unsync::OnceCell;

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct IrModuleInput<'ctx> {
    module: IrModuleRef<'ctx>,
    index: usize,
    path: IrSymbol<'ctx>,
}

impl fmt::Debug for IrModuleInput<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IrModuleInput")
            .field("module", &self.module().path())
            .field("index", &self.index())
            .field("path", &self.path())
            .field(
                "value_type",
                debug_format_option_as_value_or_invalid(self.try_value_type().as_ref()),
            )
            .finish()
    }
}

impl<'ctx> IrModuleInput<'ctx> {
    pub fn index(self) -> usize {
        self.index
    }
    pub fn value_type(self) -> IrValueTypeRef<'ctx> {
        self.module.interface_types()[self.index]
            .input()
            .expect("expected input")
    }
    fn try_value_type(self) -> Option<IrValueTypeRef<'ctx>> {
        self.module.interface_types().get(self.index)?.input()
    }
    pub fn module(self) -> IrModuleRef<'ctx> {
        self.module
    }
    pub fn path(self) -> IrSymbol<'ctx> {
        self.path
    }
}

#[derive(Debug, Clone)]
pub struct IrInput<'ctx> {
    value: IrValueRef<'ctx>,
}

impl<'ctx> From<IrValueRef<'ctx>> for IrInput<'ctx> {
    fn from(value: IrValueRef<'ctx>) -> Self {
        Self { value }
    }
}

impl<'ctx> IrInput<'ctx> {
    pub fn get_wrapped_value(&self) -> IrValueRef<'ctx> {
        self.value
    }
    #[track_caller]
    pub fn assert_is_module_input(&self) {
        if let IrValue::Input(_) = *self.value {
        } else {
            panic!("can't read from a module's input outside that module")
        }
    }
    #[track_caller]
    pub fn get(&self) -> IrValueRef<'ctx> {
        self.assert_is_module_input();
        self.value
    }
    /// returns the write end (the value from the external module)
    pub(crate) fn map_to_module_internal(
        &mut self,
        parent_module: Option<IrModuleRef<'ctx>>,
        module: IrModuleRef<'ctx>,
        index: usize,
        path: &str,
    ) -> IrValueRef<'ctx> {
        let module_input = IrModuleInput {
            module,
            index,
            path: module.symbol_table().insert_uniquified(module.ctx(), path),
        };
        assert_eq!(self.value.get_type(module.ctx()), module_input.value_type());
        if let Some(owning_module) = self.value.owning_module() {
            assert_eq!(Some(owning_module), parent_module);
        }
        mem::replace(
            &mut self.value,
            IrValue::Input(module_input).intern(module.ctx()),
        )
    }
}

pub struct IrOutputWriteData<'ctx> {
    writing_module: IrModuleRef<'ctx>,
    index: usize,
}

impl<'ctx> IrOutputWriteData<'ctx> {
    pub fn writing_module(&self) -> IrModuleRef<'ctx> {
        self.writing_module
    }
    pub fn index(&self) -> usize {
        self.index
    }
}

impl fmt::Debug for IrOutputWriteData<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IrOutputWriteData")
            .field("writing_module", &self.writing_module().path())
            .field("index", &self.index)
            .finish()
    }
}

pub struct IrOutputReadData<'ctx> {
    module: IrModuleRef<'ctx>,
    value_type: IrValueTypeRef<'ctx>,
    write_data: OnceCell<IrOutputWriteData<'ctx>>,
}

impl fmt::Debug for IrOutputReadData<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IrOutputReadData")
            .field("module", &self.module().path())
            .field("value_type", &self.value_type())
            .field(
                "write_data",
                debug_format_option_as_value_or_none(self.write_data.get()),
            )
            .finish()
    }
}

impl<'ctx> IrOutputReadData<'ctx> {
    pub fn write_data(&self) -> Option<&IrOutputWriteData<'ctx>> {
        self.write_data.get()
    }
    pub fn value_type(&self) -> IrValueTypeRef<'ctx> {
        self.value_type
    }
    pub fn module(&self) -> IrModuleRef<'ctx> {
        self.module
    }
    pub fn read(&'ctx self) -> IrValueRef<'ctx> {
        IrValue::OutputRead(IrOutputRead(self)).intern(self.module().ctx())
    }
}

pub type IrOutputReadDataRef<'ctx> = &'ctx IrOutputReadData<'ctx>;

#[derive(Clone, Copy, Debug)]
pub struct IrOutputRead<'ctx>(pub IrOutputReadDataRef<'ctx>);

impl<'ctx> IrOutputRead<'ctx> {
    pub fn read(self) -> IrValueRef<'ctx> {
        self.0.read()
    }
    pub fn new(module: IrModuleRef<'ctx>, value_type: IrValueTypeRef<'ctx>) -> Self {
        IrOutputRead(module.ctx().output_read_data_arena.alloc(IrOutputReadData {
            module,
            value_type,
            write_data: OnceCell::new(),
        }))
    }
}

impl<'ctx> Eq for IrOutputRead<'ctx> {}

impl<'ctx> PartialEq for IrOutputRead<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<'ctx> Hash for IrOutputRead<'ctx> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.0 as *const IrOutputReadData<'ctx>).hash(state)
    }
}

#[derive(Debug, Clone)]
pub enum IrOutput<'ctx> {
    WriteEnd(IrWireRef<'ctx>),
    ReadEnd(IrOutputRead<'ctx>),
}

impl<'ctx> IrOutput<'ctx> {
    pub fn new(module: IrModuleRef<'ctx>, value_type: IrValueTypeRef<'ctx>) -> Self {
        IrOutput::ReadEnd(IrOutputRead::new(module, value_type))
    }
    #[track_caller]
    pub fn assign(self, value: IrValueRef<'ctx>) {
        match self {
            IrOutput::WriteEnd(wire) => wire.assign(value),
            IrOutput::ReadEnd(_) => {
                panic!("can't assign to output outside of the module to be called")
            }
        }
    }
    pub fn read(&self) -> IrValueRef<'ctx> {
        match self {
            IrOutput::WriteEnd(v) => v.read(),
            IrOutput::ReadEnd(v) => v.read(),
        }
    }
    pub fn ctx(&self) -> ContextRef<'ctx> {
        match self {
            IrOutput::WriteEnd(v) => v.module().ctx(),
            IrOutput::ReadEnd(v) => v.0.module.ctx(),
        }
    }
    pub fn value_type(&self) -> IrValueTypeRef<'ctx> {
        match self {
            IrOutput::WriteEnd(v) => v.value_type(),
            IrOutput::ReadEnd(v) => v.0.value_type(),
        }
    }
    /// returns the write end (the wire in the internal module)
    pub(crate) fn map_to_module_internal(
        &mut self,
        parent_module: IrModuleRef<'ctx>,
        module: IrModuleRef<'ctx>,
        index: usize,
        path: &str,
    ) -> IrWireRef<'ctx> {
        let expected_value_type = module.interface_types()[index]
            .output()
            .expect("expected output");
        assert_eq!(self.value_type(), expected_value_type);
        let read_end = match self {
            IrOutput::ReadEnd(read_end) => *read_end,
            IrOutput::WriteEnd(write_end) => {
                // we're passing an IrOutput from our grandparent module directly
                // through parent_module to module, adapt
                // by creating a IrOutputRead in parent_module and
                // writing it to the wire there.
                assert_eq!(parent_module, write_end.module());
                let read_end = IrOutputRead::new(parent_module, write_end.value_type());
                write_end.assign(read_end.read());
                read_end
            }
        };
        assert_eq!(read_end.0.module(), parent_module);
        let was_empty = read_end
            .0
            .write_data
            .set(IrOutputWriteData {
                writing_module: module,
                index,
            })
            .is_ok();
        assert!(was_empty);
        let wire = IrWire::new(module, Cow::Borrowed(path), read_end.0.value_type());
        *self = IrOutput::WriteEnd(wire);
        wire
    }
}

pub type IrIOMutRef<'a, 'ctx> = InOrOut<&'a mut IrInput<'ctx>, &'a mut IrOutput<'ctx>>;

pub trait IrIOCallback<'ctx> {
    fn callback(&mut self, io: IrIOMutRef<'_, 'ctx>, path: &str) -> Result<(), ()>;
}

impl<'ctx, T: FnMut(IrIOMutRef<'_, 'ctx>, &str) -> Result<(), ()>> IrIOCallback<'ctx> for T {
    fn callback(&mut self, io: IrIOMutRef<'_, 'ctx>, path: &str) -> Result<(), ()> {
        self(io, path)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InOrOut<I, O> {
    Input(I),
    Output(O),
}

impl<I, O> InOrOut<I, O> {
    pub fn input(self) -> Option<I> {
        if let Self::Input(v) = self {
            Some(v)
        } else {
            None
        }
    }
    pub fn output(self) -> Option<O> {
        if let Self::Output(v) = self {
            Some(v)
        } else {
            None
        }
    }
    pub fn as_ref(&self) -> InOrOut<&I, &O> {
        match self {
            Self::Input(v) => InOrOut::Input(v),
            Self::Output(v) => InOrOut::Output(v),
        }
    }
    pub fn as_mut(&mut self) -> InOrOut<&mut I, &mut O> {
        match self {
            Self::Input(v) => InOrOut::Input(v),
            Self::Output(v) => InOrOut::Output(v),
        }
    }
    pub fn map<I2, O2, IFn: FnOnce(I) -> I2, OFn: FnOnce(O) -> O2>(
        self,
        i_fn: IFn,
        o_fn: OFn,
    ) -> InOrOut<I2, O2> {
        match self {
            Self::Input(v) => InOrOut::Input(i_fn(v)),
            Self::Output(v) => InOrOut::Output(o_fn(v)),
        }
    }
    pub fn try_map<I2, O2, E, IFn: FnOnce(I) -> Result<I2, E>, OFn: FnOnce(O) -> Result<O2, E>>(
        self,
        i_fn: IFn,
        o_fn: OFn,
    ) -> Result<InOrOut<I2, O2>, E> {
        Ok(match self {
            Self::Input(v) => InOrOut::Input(i_fn(v)?),
            Self::Output(v) => InOrOut::Output(o_fn(v)?),
        })
    }
}
