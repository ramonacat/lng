use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    types::{BasicType, FunctionType},
};

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
    // TODO get rid of this function, and instead have higher-level constructs (make_function_type,
    // make_struct_type, etc.)
    pub fn type_to_llvm(&self, type_: &types::Type) -> Box<dyn BasicType<'ctx> + 'ctx> {
        match type_ {
            types::Type::Void => panic!("void type cannot be converted with type_to_llvm"),
            types::Type::U8 => Box::new(self.llvm_context.i8_type()),
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

    pub fn make_function_type(
        &self,
        arguments: &[types::Argument],
        return_type: &types::Type,
    ) -> FunctionType<'ctx> {
        let arguments = arguments
            .iter()
            .map(|arg| self.type_to_llvm(&arg.type_).as_basic_type_enum().into())
            .collect::<Vec<_>>();

        match return_type {
            types::Type::Void => self.llvm_context.void_type().fn_type(&arguments[..], false),
            _ => self
                .type_to_llvm(return_type)
                .fn_type(&arguments[..], false),
        }
    }
}
