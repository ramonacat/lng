use std::{collections::HashMap, sync::RwLock};

use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    types::{BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{IntValue, PointerValue},
};

use crate::{
    identifier::{FQName, Identifier},
    types,
};

use super::{
    scope::GlobalScope,
    unique_name,
    value::{InstantiatedFunctionType, InstantiatedStructType},
};

pub struct Builtins {
    pub rc_handle: types::structs::Struct,
}

// TODO rename -> AllItems or something
pub struct AllStructs<'ctx> {
    structs: HashMap<types::structs::StructId, types::structs::Struct>,
    instantiated_structs:
        RwLock<HashMap<types::structs::InstantiatedStructId, InstantiatedStructType<'ctx>>>,
    functions: HashMap<types::functions::FunctionId, types::functions::Function>,
    instantiated_functions:
        RwLock<HashMap<types::functions::InstantiatedFunctionId, InstantiatedFunctionType>>,
}

impl<'ctx> AllStructs<'ctx> {
    pub(crate) fn new(
        structs: HashMap<types::structs::StructId, types::structs::Struct>,
        functions: HashMap<types::functions::FunctionId, types::functions::Function>,
    ) -> Self {
        Self {
            structs,
            instantiated_structs: RwLock::new(HashMap::new()),
            functions,
            instantiated_functions: RwLock::new(HashMap::new()),
        }
    }

    pub(crate) fn inspect_instantiated<T>(
        &self,
        handle: &types::structs::InstantiatedStructId,
        inspect: impl FnOnce(Option<&InstantiatedStructType<'ctx>>) -> T,
    ) -> T {
        self.instantiate(handle);

        inspect(self.instantiated_structs.read().unwrap().get(handle))
    }

    // TODO deal with setting the static fields here
    fn instantiate(&self, id: &types::structs::InstantiatedStructId) {
        self.instantiated_structs
            .write()
            .unwrap()
            .entry(id.clone())
            .or_insert_with(|| {
                let instantiated = self.structs.get(&id.0).unwrap().instantiate(&id.1);

                InstantiatedStructType::new(instantiated)
            });
    }

    pub(crate) fn inspect_instantiated_function<T>(
        &self,
        id: &types::functions::InstantiatedFunctionId,
        inspect: impl FnOnce(Option<&InstantiatedFunctionType>) -> T,
    ) -> T {
        self.instantiate_function(id);

        inspect(self.instantiated_functions.read().unwrap().get(id))
    }

    fn instantiate_function(&self, id: &types::functions::InstantiatedFunctionId) {
        self.instantiated_functions
            .write()
            .unwrap()
            .entry(id.clone())
            .or_insert_with(|| {
                let instantiated = self.functions.get(&id.0).unwrap().instantiate(&id.1);

                InstantiatedFunctionType::new(instantiated)
            });
    }
}

impl std::fmt::Debug for AllStructs<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "structs: {:?}", self.structs.keys())?;
        writeln!(f, "functions: {:?}", self.functions.keys())
    }
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

    pub fn get_std_type(name: &str) -> types::structs::InstantiatedStructId {
        // TODO what about std types that have type arguments?
        types::structs::InstantiatedStructId(
            types::structs::StructId::FQName(
                FQName::parse("std").with_part(Identifier::parse(name)),
            ),
            types::TypeArgumentValues::new_empty(),
        )
    }

    fn type_to_llvm(&self, type_: &types::Type) -> Box<dyn BasicType<'ctx> + 'ctx> {
        match &type_.kind() {
            types::TypeKind::Unit | types::TypeKind::U8 => Box::new(self.llvm_context.i8_type()),
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
        arguments: &[types::functions::Argument],
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

    pub fn make_struct_type(&self, fields: &[types::structs::StructField]) -> CompiledStruct<'ctx> {
        let mut field_types = vec![];
        let mut field_indices = HashMap::new();

        for (index, field) in fields.iter().enumerate() {
            field_types.push(self.type_to_llvm(&field.type_).as_basic_type_enum());
            field_indices.insert(field.name, u32::try_from(index).unwrap());
        }

        CompiledStruct {
            llvm_type: self.llvm_context.struct_type(&field_types, false),
            field_indices,
        }
    }

    pub(crate) fn make_object_type(&self, item_type: &types::Type) -> BasicTypeEnum<'ctx> {
        self.type_to_llvm(item_type).as_basic_type_enum()
    }
}

pub struct CompiledStruct<'ctx> {
    llvm_type: StructType<'ctx>,
    field_indices: HashMap<Identifier, u32>,
}

impl<'ctx> CompiledStruct<'ctx> {
    pub fn field_pointer(
        &self,
        field: Identifier,
        instance: PointerValue<'ctx>,
        context: &CompilerContext<'ctx>,
    ) -> (BasicTypeEnum<'ctx>, PointerValue<'ctx>) {
        let index = self.field_indices.get(&field).expect("field should exist");

        let pointer = unsafe {
            context.builder.build_gep(
                self.llvm_type,
                instance,
                &[context.const_u32(0), context.const_u32(*index)],
                &unique_name(&[&field.raw()]),
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
