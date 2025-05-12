use std::collections::HashMap;

use dashmap::DashMap;
use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    types::{BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{IntValue, PointerValue},
};

use crate::{identifier::Identifier, types};

use super::{unique_name, value::InstantiatedStructType};

pub struct Builtins {
    pub rc_handle: types::structs::Struct<types::GenericType>,
}

// TODO do we really need DashMap here? this is single-threaded anyway
pub struct AllItems<'ctx> {
    structs: HashMap<types::structs::StructId, types::structs::Struct<types::GenericType>>,
    instantiated_structs:
        DashMap<types::structs::InstantiatedStructId, InstantiatedStructType<'ctx>>,

    functions:
        HashMap<types::functions::FunctionId, types::functions::Function<types::GenericType>>,
    instantiated_functions: DashMap<
        types::functions::InstantiatedFunctionId,
        types::functions::Function<types::InstantiatedType>,
    >,
}

impl<'ctx> AllItems<'ctx> {
    pub(crate) fn new(
        structs: HashMap<types::structs::StructId, types::structs::Struct<types::GenericType>>,
        functions: HashMap<
            types::functions::FunctionId,
            types::functions::Function<types::GenericType>,
        >,
    ) -> Self {
        Self {
            structs,
            instantiated_structs: DashMap::new(),
            functions,
            instantiated_functions: DashMap::new(),
        }
    }

    pub(crate) fn inspect_instantiated<T>(
        &self,
        handle: &types::structs::InstantiatedStructId,
        inspect: impl FnOnce(Option<&InstantiatedStructType<'ctx>>) -> T,
    ) -> T {
        self.instantiate(handle);

        inspect(self.instantiated_structs.get(handle).as_deref())
    }

    // TODO deal with setting the static fields here
    fn instantiate(&self, id: &types::structs::InstantiatedStructId) {
        self.instantiated_structs
            .entry(id.clone())
            .or_insert_with(|| {
                let instantiated = self
                    .structs
                    .get(&id.id())
                    .unwrap()
                    .instantiate(id.argument_values())
                    .unwrap();

                InstantiatedStructType::new(instantiated, HashMap::new())
            });
    }

    pub(crate) fn inspect_instantiated_function<T>(
        &self,
        id: &types::functions::InstantiatedFunctionId,
        inspect: impl FnOnce(Option<&types::functions::Function<types::InstantiatedType>>) -> T,
    ) -> T {
        if !self.functions.contains_key(&id.id()) {
            return inspect(None);
        }

        self.instantiate_function(id);

        inspect(self.instantiated_functions.get(id).as_deref())
    }

    fn instantiate_function(&self, id: &types::functions::InstantiatedFunctionId) {
        self.instantiated_functions
            .entry(id.clone())
            .or_insert_with(|| {
                let instantiated = self
                    .functions
                    .get(&id.id())
                    .unwrap()
                    .instantiate(id.argument_values())
                    .unwrap();

                instantiated
            });
    }

    pub(crate) fn get_function(
        &self,
        id: types::functions::FunctionId,
    ) -> Option<&types::functions::Function<types::GenericType>> {
        self.functions.get(&id)
    }
}

impl std::fmt::Debug for AllItems<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "structs: {:?}", self.structs.keys())?;
        writeln!(f, "functions: {:?}", self.functions.keys())
    }
}

pub struct CompilerContext<'ctx> {
    pub llvm_context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub builtins: Builtins,
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

    pub fn get_std_type(name: &str) -> types::structs::StructId {
        types::structs::StructId::InModule(
            types::modules::ModuleId::parse("std"),
            Identifier::parse(name),
        )
    }

    fn type_to_llvm(&self, type_: &types::InstantiatedType) -> Box<dyn BasicType<'ctx> + 'ctx> {
        match &type_.kind() {
            types::InstantiatedTypeKind::Unit | types::InstantiatedTypeKind::U8 => {
                Box::new(self.llvm_context.i8_type())
            }
            types::InstantiatedTypeKind::U64 => Box::new(self.llvm_context.i64_type()),
            types::InstantiatedTypeKind::Callable { .. }
            | types::InstantiatedTypeKind::Pointer(_)
            | types::InstantiatedTypeKind::Array { .. }
            | types::InstantiatedTypeKind::Object { .. } => {
                Box::new(self.llvm_context.ptr_type(AddressSpace::default()))
            }
            types::InstantiatedTypeKind::Struct(_) => todo!(),
            types::InstantiatedTypeKind::Function(_) => todo!(),
            types::InstantiatedTypeKind::IndirectCallable(_, _) => todo!(),
            types::InstantiatedTypeKind::InterfaceObject { .. } => todo!(),
        }
    }

    pub fn make_function_type(
        &self,
        arguments: &[types::functions::Argument<types::InstantiatedType>],
        return_type: &types::InstantiatedType,
    ) -> FunctionType<'ctx> {
        let arguments = arguments
            .iter()
            .map(|arg| self.type_to_llvm(&arg.type_).as_basic_type_enum().into())
            .collect::<Vec<_>>();

        match return_type.kind() {
            types::InstantiatedTypeKind::Unit => {
                self.llvm_context.void_type().fn_type(&arguments[..], false)
            }
            _ => self
                .type_to_llvm(return_type)
                .fn_type(&arguments[..], false),
        }
    }

    pub fn make_struct_type(
        &self,
        fields: &[types::structs::StructField<types::InstantiatedType>],
    ) -> CompiledStruct<'ctx> {
        let mut field_types = vec![];
        let mut field_indices = HashMap::new();

        for (index, field) in fields.iter().filter(|f| !f.static_).enumerate() {
            field_types.push(self.type_to_llvm(&field.type_).as_basic_type_enum());
            field_indices.insert(field.name, u32::try_from(index).unwrap());
        }

        CompiledStruct {
            llvm_type: self.llvm_context.struct_type(&field_types, false),
            field_indices,
        }
    }

    pub(crate) fn make_object_type(
        &self,
        item_type: &types::InstantiatedType,
    ) -> BasicTypeEnum<'ctx> {
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
                &unique_name(&[&field.raw(), "gep"]),
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
