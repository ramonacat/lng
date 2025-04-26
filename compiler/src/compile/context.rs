use inkwell::{AddressSpace, builder::Builder, context::Context, types::BasicType};

use crate::types;

use super::value::StructHandle;

pub struct Builtins<'ctx> {
    pub rc_handle: StructHandle<'ctx>,
    pub string_handle: StructHandle<'ctx>,
}

pub struct CompilerContext<'ctx> {
    pub llvm_context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub builtins: Builtins<'ctx>,
}

impl<'ctx> CompilerContext<'ctx> {
    pub fn type_to_llvm(&self, type_: &types::Type) -> Box<dyn BasicType<'ctx> + 'ctx> {
        match type_ {
            // TODO use the actual void type for void
            types::Type::U8 | types::Type::Void => Box::new(self.llvm_context.i8_type()),
            types::Type::StructDescriptor(_, _) => todo!(),
            types::Type::U64 => Box::new(self.llvm_context.i64_type()),
            // TODO arrays should be structs that have bounds, not just ptrs
            types::Type::Callable { .. }
            | types::Type::Pointer(_)
            | types::Type::Array(_)
            | types::Type::Object(_) => {
                Box::new(self.llvm_context.ptr_type(AddressSpace::default()))
            }
        }
    }
}
