// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{AsContext, ContextRef, Intern},
    fmt_utils::{debug_format_option_as_value_or_invalid, debug_format_option_as_value_or_none},
    ir::{
        logic::{IrWire, IrWireRef},
        module::{
            combine_owning_modules, IrModule, IrModuleInputData, IrModuleOutputData, IrModuleRef,
            OwningModule,
        },
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

impl<'ctx> OwningModule<'ctx> for IrModuleInput<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        Some(self.module)
    }
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
pub enum IrInput<'ctx> {
    Input { value: IrValueRef<'ctx> },
    ExternalInput { value_type: IrValueTypeRef<'ctx> },
}

impl<'ctx> From<IrValueRef<'ctx>> for IrInput<'ctx> {
    fn from(value: IrValueRef<'ctx>) -> Self {
        Self::Input { value }
    }
}

impl<'ctx> IrInput<'ctx> {
    pub fn get_wrapped_value(&self) -> Option<IrValueRef<'ctx>> {
        if let Self::Input { value } = *self {
            Some(value)
        } else {
            None
        }
    }
    pub fn value_type(&self, ctx: impl AsContext<'ctx>) -> IrValueTypeRef<'ctx> {
        match *self {
            IrInput::Input { value } => value.get_type(ctx.ctx()),
            IrInput::ExternalInput { value_type } => value_type,
        }
    }
    pub fn external(ctx: impl AsContext<'ctx>, value_type: IrValueTypeRef<'ctx>) -> Self {
        let _ = ctx;
        Self::ExternalInput { value_type }
    }
    #[track_caller]
    pub fn get(&self) -> IrValueRef<'ctx> {
        if let IrInput::Input { value } = *self {
            if let IrValue::Input(_) = *value {
                return value;
            }
        }
        panic!("can't read from a module's input outside that module")
    }
    pub(crate) fn map_to_module_internal(
        &mut self,
        parent_module: Option<IrModuleRef<'ctx>>,
        module: IrModuleRef<'ctx>,
        index: usize,
        path: &str,
    ) -> IrModuleInputData<'ctx> {
        let path = module.symbol_table().insert_uniquified(module.ctx(), path);
        let module_input = IrModuleInput {
            module,
            index,
            path,
        };
        assert_eq!(self.value_type(module.ctx()), module_input.value_type());
        match (&*self, parent_module) {
            (IrInput::Input { value }, Some(parent_module)) => {
                combine_owning_modules([Some(parent_module), value.owning_module()]);
            }
            (IrInput::Input { .. }, None) => {
                panic!("input to top module must be an external input")
            }
            (IrInput::ExternalInput { .. }, Some(_)) => {
                panic!("input to submodule must not be an external input")
            }
            (IrInput::ExternalInput { .. }, None) => {}
        }
        IrModuleInputData::new(
            mem::replace(
                self,
                IrInput::Input {
                    value: IrValue::Input(module_input).intern(module.ctx()),
                },
            ),
            module_input,
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

#[derive(Clone, Copy)]
enum ModuleOrContext<'ctx> {
    Module(IrModuleRef<'ctx>),
    Context(ContextRef<'ctx>),
}

impl<'ctx> AsContext<'ctx> for ModuleOrContext<'ctx> {
    fn ctx(&self) -> ContextRef<'ctx> {
        match *self {
            Self::Module(v) => v.ctx(),
            Self::Context(v) => v,
        }
    }
}

impl<'ctx> ModuleOrContext<'ctx> {
    pub fn module(self) -> Option<IrModuleRef<'ctx>> {
        match self {
            ModuleOrContext::Module(v) => Some(v),
            ModuleOrContext::Context(_) => None,
        }
    }
}

pub struct IrOutputReadData<'ctx> {
    module_or_context: ModuleOrContext<'ctx>,
    value_type: IrValueTypeRef<'ctx>,
    write_data: OnceCell<IrOutputWriteData<'ctx>>,
}

impl fmt::Debug for IrOutputReadData<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IrOutputReadData")
            .field(
                "module",
                debug_format_option_as_value_or_none(self.module().map(IrModule::path).as_ref()),
            )
            .field("value_type", &self.value_type())
            .field(
                "write_data",
                debug_format_option_as_value_or_none(self.write_data.get()),
            )
            .finish()
    }
}

impl<'ctx> AsContext<'ctx> for IrOutputReadData<'ctx> {
    fn ctx(&self) -> ContextRef<'ctx> {
        self.module_or_context.ctx()
    }
}

impl<'ctx> IrOutputReadData<'ctx> {
    pub fn write_data(&self) -> Option<&IrOutputWriteData<'ctx>> {
        self.write_data.get()
    }
    pub fn value_type(&self) -> IrValueTypeRef<'ctx> {
        self.value_type
    }
    pub fn module(&self) -> Option<IrModuleRef<'ctx>> {
        self.module_or_context.module()
    }
    pub fn read(&'ctx self) -> IrValueRef<'ctx> {
        IrValue::OutputRead(IrOutputRead(self)).intern(
            self.module()
                .expect("can't read from an external output")
                .ctx(),
        )
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
            module_or_context: ModuleOrContext::Module(module),
            value_type,
            write_data: OnceCell::new(),
        }))
    }
    pub fn external(ctx: impl AsContext<'ctx>, value_type: IrValueTypeRef<'ctx>) -> Self {
        let ctx = ctx.ctx();
        IrOutputRead(ctx.output_read_data_arena.alloc(IrOutputReadData {
            module_or_context: ModuleOrContext::Context(ctx),
            value_type,
            write_data: OnceCell::new(),
        }))
    }
}

impl<'ctx> OwningModule<'ctx> for IrOutputRead<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        self.0.module()
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

impl<'ctx> AsContext<'ctx> for IrOutputRead<'ctx> {
    fn ctx(&self) -> ContextRef<'ctx> {
        self.0.ctx()
    }
}

#[derive(Debug, Clone)]
pub enum IrOutput<'ctx> {
    WriteEnd(IrWireRef<'ctx>),
    ReadEnd(IrOutputRead<'ctx>),
}

impl<'ctx> AsContext<'ctx> for IrOutput<'ctx> {
    fn ctx(&self) -> ContextRef<'ctx> {
        match self {
            IrOutput::WriteEnd(v) => v.ctx(),
            IrOutput::ReadEnd(v) => v.ctx(),
        }
    }
}

impl<'ctx> IrOutput<'ctx> {
    pub fn new(module: IrModuleRef<'ctx>, value_type: IrValueTypeRef<'ctx>) -> Self {
        IrOutput::ReadEnd(IrOutputRead::new(module, value_type))
    }
    pub fn external(ctx: impl AsContext<'ctx>, value_type: IrValueTypeRef<'ctx>) -> Self {
        IrOutput::ReadEnd(IrOutputRead::external(ctx, value_type))
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
    pub fn value_type(&self) -> IrValueTypeRef<'ctx> {
        match self {
            IrOutput::WriteEnd(v) => v.value_type(),
            IrOutput::ReadEnd(v) => v.0.value_type(),
        }
    }
    pub(crate) fn map_to_module_internal(
        &mut self,
        parent_module: Option<IrModuleRef<'ctx>>,
        module: IrModuleRef<'ctx>,
        index: usize,
        path: &str,
    ) -> IrModuleOutputData<'ctx> {
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
                assert_eq!(parent_module, Some(write_end.module()));
                let read_end = IrOutputRead::new(parent_module.unwrap(), write_end.value_type());
                write_end.assign(read_end.read());
                read_end
            }
        };
        match (read_end.0.module(), parent_module) {
            (Some(read_end_module), Some(parent_module)) => {
                assert_eq!(read_end_module, parent_module);
            }
            (None, Some(_)) => panic!("an external output can only be the output of a top module"),
            (Some(_), None) => panic!("the output(s) of a top module must be external outputs"),
            (None, None) => {}
        }
        let was_empty = read_end
            .0
            .write_data
            .set(IrOutputWriteData {
                writing_module: module,
                index,
            })
            .is_ok();
        assert!(was_empty);
        let wire = IrWire::new(
            module,
            module.source_location(),
            Cow::Borrowed(path),
            read_end.0.value_type(),
        );
        *self = IrOutput::WriteEnd(wire);
        IrModuleOutputData::new(read_end, wire)
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
