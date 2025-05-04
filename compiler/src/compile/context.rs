use std::collections::HashMap;

use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    types::{BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{IntValue, PointerValue},
};

use crate::types;

use super::{
    CompileError, CompileErrorDescription, scope::GlobalScope, unique_name, value::StructHandle,
};

pub struct Builtins {
    pub rc_handle: types::Struct,
    pub string_handle: types::Struct,
}

pub struct CompilerContext<'ctx> {
    pub llvm_context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub builtins: Builtins,
    pub global_scope: GlobalScope<'ctx>,
}

impl<'ctx> CompilerContext<'ctx> {
    pub fn const_u64(&self, value: u64) -> IntValue<'ctx> {
        self.llvm_context.i64_type().const_int(value, false)
    }

    fn const_u32(&self, value: u32) -> IntValue<'ctx> {
        self.llvm_context
            .i32_type()
            .const_int(u64::from(value), false)
    }

    pub fn get_std_type(&self, name: &str) -> Option<StructHandle<'ctx>> {
        self.global_scope
            .get_value(types::FQName::parse("std").with_part(types::Identifier::parse(name)))
            .map(|x| x.as_struct().unwrap())
            .map(StructHandle::new)
    }

    fn type_to_llvm(&self, type_: &types::Type) -> Box<dyn BasicType<'ctx> + 'ctx> {
        match &type_.kind() {
            types::TypeKind::Unit | types::TypeKind::U8 => Box::new(self.llvm_context.i8_type()),
            types::TypeKind::StructDescriptor(_) => todo!(),
            types::TypeKind::U64 => Box::new(self.llvm_context.i64_type()),
            types::TypeKind::Callable { .. }
            | types::TypeKind::Pointer(_)
            | types::TypeKind::Array { .. }
            | types::TypeKind::Object { .. } => {
                Box::new(self.llvm_context.ptr_type(AddressSpace::default()))
            }
            types::TypeKind::Generic(_) => todo!(),
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

        match return_type.kind() {
            types::TypeKind::Unit => self.llvm_context.void_type().fn_type(&arguments[..], false),
            _ => self
                .type_to_llvm(return_type)
                .fn_type(&arguments[..], false),
        }
    }

    pub fn make_struct_type(
        &self,
        struct_name: types::FQName,
        fields: &[types::StructField],
    ) -> CompiledStruct<'ctx> {
        let mut field_types = vec![];
        let mut field_indices = HashMap::new();

        for (index, field) in fields.iter().enumerate() {
            field_types.push(self.type_to_llvm(&field.type_).as_basic_type_enum());
            field_indices.insert(field.name, u32::try_from(index).unwrap());
        }

        CompiledStruct {
            llvm_type: self.llvm_context.struct_type(&field_types, false),
            field_indices,
            struct_name,
        }
    }

    pub(crate) fn make_object_type(&self, item_type: &types::Type) -> BasicTypeEnum<'ctx> {
        self.type_to_llvm(item_type).as_basic_type_enum()
    }
}

pub struct CompiledStruct<'ctx> {
    struct_name: types::FQName,
    llvm_type: StructType<'ctx>,
    field_indices: HashMap<types::Identifier, u32>,
}

impl<'ctx> CompiledStruct<'ctx> {
    pub fn field_pointer(
        &self,
        field: types::Identifier,
        instance: PointerValue<'ctx>,
        context: &CompilerContext<'ctx>,
    ) -> Result<(BasicTypeEnum<'ctx>, PointerValue<'ctx>), CompileError> {
        let index = self.field_indices.get(&field).ok_or_else(|| {
            CompileErrorDescription::FieldNotFound(self.struct_name, field).at_indeterminate()
        })?;

        let pointer = unsafe {
            context.builder.build_gep(
                self.llvm_type,
                instance,
                &[context.const_u32(0), context.const_u32(*index)],
                &unique_name(&[&field.raw()]),
            )
        }
        .unwrap();

        Ok((
            self.llvm_type.get_field_type_at_index(*index).unwrap(),
            pointer,
        ))
    }

    pub(crate) const fn as_llvm_type(&self) -> StructType<'ctx> {
        self.llvm_type
    }
}
