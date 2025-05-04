use std::{collections::HashMap, sync::RwLock};

use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    types::{BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{IntValue, PointerValue},
};

use crate::types::{self, StructId};

use super::{
    CompileError, CompileErrorDescription, scope::GlobalScope, unique_name,
    value::InstantiatedStructType,
};

pub struct Builtins {
    pub rc_handle: types::Struct,
}

pub struct AllStructs<'ctx> {
    structs: HashMap<types::StructId, types::Struct>,
    instantiated_structs:
        RwLock<HashMap<types::InstantiatedStructId, InstantiatedStructType<'ctx>>>,
}

// TODO rename the methdos to something less verbose
impl<'ctx> AllStructs<'ctx> {
    pub(crate) fn new(structs: HashMap<StructId, types::Struct>) -> Self {
        Self {
            structs,
            instantiated_structs: RwLock::new(HashMap::new()),
        }
    }

    pub(crate) fn inspect_instantiated_struct<T>(
        &self,
        handle: &types::InstantiatedStructId,
        inspect: impl FnOnce(Option<&InstantiatedStructType<'ctx>>) -> T,
    ) -> T {
        self.instantiate_struct(handle.0, &handle.1);

        inspect(self.instantiated_structs.read().unwrap().get(handle))
    }

    // TODO deal with setting the static fields here
    // TODO this method should probably be private?
    pub fn instantiate_struct(
        &self,
        struct_: types::StructId,
        type_argument_values: &types::TypeArgumentValues,
    ) -> types::InstantiatedStructId {
        let id = types::InstantiatedStructId(struct_, type_argument_values.clone());

        self.instantiated_structs
            .write()
            .unwrap()
            .entry(id.clone())
            .or_insert_with(|| {
                dbg!(&self.structs, struct_);
                let instantiated = self
                    .structs
                    .get(&struct_)
                    .unwrap()
                    .instantiate(type_argument_values);

                InstantiatedStructType::new(instantiated)
            });

        id
    }
}

pub struct CompilerContext<'ctx> {
    pub llvm_context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub builtins: Builtins,
    pub global_scope: GlobalScope<'ctx>,
}

impl<'ctx> CompilerContext<'ctx> {
    pub fn instantiate_struct(
        &self,
        id: StructId,
        type_argument_values: &types::TypeArgumentValues,
    ) -> types::InstantiatedStructId {
        self.global_scope
            .structs
            .instantiate_struct(id, type_argument_values)
    }

    pub fn const_u64(&self, value: u64) -> IntValue<'ctx> {
        self.llvm_context.i64_type().const_int(value, false)
    }

    fn const_u32(&self, value: u32) -> IntValue<'ctx> {
        self.llvm_context
            .i32_type()
            .const_int(u64::from(value), false)
    }

    pub fn get_std_type(&self, name: &str) -> types::InstantiatedStructId {
        // TODO verify the type exists and return an error if not
        // TODO what about std types that have type arguments?
        self.instantiate_struct(
            types::StructId::FQName(
                types::FQName::parse("std").with_part(types::Identifier::parse(name)),
            ),
            &types::TypeArgumentValues::new_empty(),
        )
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
