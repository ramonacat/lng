use rand::Rng;
use std::collections::HashMap;

use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    types::{BasicType, BasicTypeEnum, FunctionType, StructType},
    values::PointerValue,
};

use crate::types::{self, FieldPath, Identifier, ModulePath};

use super::{scope::GlobalScope, value::StructHandle};

pub struct Builtins<'ctx> {
    pub rc_handle: StructHandle<'ctx>,
    pub string_handle: StructHandle<'ctx>,
}

pub struct CompilerContext<'ctx> {
    pub llvm_context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub builtins: Builtins<'ctx>,
    pub global_scope: GlobalScope<'ctx>,
}

impl<'ctx> CompilerContext<'ctx> {
    pub fn get_std_type(&self, name: &str) -> Option<StructHandle<'ctx>> {
        self.global_scope
            .get_value(&types::ItemPath::new(
                ModulePath::parse("std"),
                Identifier::parse(name),
            ))
            .map(|x| x.as_struct().unwrap())
    }

    fn type_to_llvm(&self, type_: &types::Type) -> Box<dyn BasicType<'ctx> + 'ctx> {
        match type_ {
            types::Type::Unit | types::Type::U8 => Box::new(self.llvm_context.i8_type()),
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
            types::Type::Unit => self.llvm_context.void_type().fn_type(&arguments[..], false),
            _ => self
                .type_to_llvm(return_type)
                .fn_type(&arguments[..], false),
        }
    }

    pub fn make_struct_type(&self, fields: &[types::StructField]) -> StructTypeHandle<'ctx> {
        let mut field_types = vec![];
        let mut field_indices = HashMap::new();

        for (index, field) in fields.iter().enumerate() {
            field_types.push(self.type_to_llvm(&field.type_).as_basic_type_enum());
            field_indices.insert(field.name.clone(), u32::try_from(index).unwrap());
        }

        StructTypeHandle {
            llvm_type: self.llvm_context.struct_type(&field_types, false),
            field_indices,
        }
    }
}

pub struct StructTypeHandle<'ctx> {
    llvm_type: StructType<'ctx>,
    field_indices: HashMap<FieldPath, u32>,
}

impl<'ctx> StructTypeHandle<'ctx> {
    // TODO return Result (error if there's no such field)
    pub fn field_pointer(
        &self,
        field: &FieldPath,
        instance: PointerValue<'ctx>,
        context: &CompilerContext<'ctx>,
    ) -> (BasicTypeEnum<'ctx>, PointerValue<'ctx>) {
        let index = self.field_indices.get(field).unwrap();

        // TODO why does const_gep not work here?
        let pointer = unsafe {
            context.builder.build_gep(
                self.llvm_type,
                instance,
                &[
                    context.llvm_context.i32_type().const_int(0, false),
                    context
                        .llvm_context
                        .i32_type()
                        .const_int(u64::from(*index), false),
                ],
                &format!(
                    "{}_{}",
                    field.clone().into_mangled().as_str(),
                    rand::rng()
                        .sample_iter(rand::distr::Alphanumeric)
                        .take(8)
                        .map(char::from)
                        .collect::<String>()
                ),
            )
        }
        .unwrap();

        (
            self.llvm_type.get_field_type_at_index(*index).unwrap(),
            pointer,
        )
    }

    pub(crate) const fn as_llvm_type(&self) -> StructType<'ctx> {
        self.llvm_type
    }
}
