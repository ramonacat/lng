use std::collections::HashMap;

use inkwell::{
    AddressSpace,
    context::Context,
    types::{BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{IntValue, PointerValue},
};

use crate::{identifier::Identifier, types};

use super::{CompiledFunction, unique_name, value::InstantiatedStructType};

pub struct Builtins {
    pub rc_handle: types::structs::Struct,
}

pub struct AllItems<'ctx> {
    structs: HashMap<types::structs::StructId, types::structs::Struct>,
    instantiated_structs:
        HashMap<types::structs::InstantiatedStructId, InstantiatedStructType<'ctx>>,

    functions: HashMap<types::functions::FunctionId, types::functions::Function>,
    instantiated_functions:
        HashMap<types::functions::InstantiatedFunctionId, types::functions::Function>,
}

impl<'ctx> AllItems<'ctx> {
    pub(crate) fn new(
        structs: HashMap<types::structs::StructId, types::structs::Struct>,
        functions: HashMap<types::functions::FunctionId, types::functions::Function>,
    ) -> Self {
        Self {
            structs,
            instantiated_structs: HashMap::new(),
            functions,
            instantiated_functions: HashMap::new(),
        }
    }

    pub(crate) fn get_struct(
        &self,
        handle: &types::structs::InstantiatedStructId,
    ) -> Option<&InstantiatedStructType<'ctx>> {
        self.instantiated_structs.get(handle)
    }

    pub(crate) fn get_or_instantiate_struct(
        &mut self,
        handle: &types::structs::InstantiatedStructId,
        types: &mut dyn types::store::TypeStore,
    ) -> Option<&InstantiatedStructType<'ctx>> {
        self.instantiate_struct(handle, types);

        self.instantiated_structs.get(handle)
    }

    // TODO deal with setting the static fields here
    fn instantiate_struct(
        &mut self,
        id: &types::structs::InstantiatedStructId,
        types: &mut dyn types::store::TypeStore,
    ) {
        let rc_id = types::structs::StructId::InModule(
            types::modules::ModuleId::parse("std"),
            Identifier::parse("rc"),
        );

        let item_id = self
            .instantiated_structs
            .entry(id.clone())
            .or_insert_with(|| {
                let instantiated = self.structs.get(&id.id()).unwrap().with_type_arguments(
                    id.argument_values().iter().map(|x| x.unwrap()).collect(),
                    types,
                );

                InstantiatedStructType::new(instantiated, HashMap::new())
            })
            .definition
            .instance_type;

        if id.id() != rc_id {
            let tav = vec![types::generics::TypeArgument::new_value(
                Identifier::parse("TPointee"),
                item_id,
            )];
            self.instantiated_structs
                .entry(types::structs::InstantiatedStructId::new(
                    rc_id,
                    types::generics::TypeArguments::new(tav),
                ))
                .or_insert_with(|| {
                    let instantiated = self.structs.get(&rc_id).unwrap().with_type_arguments(
                        id.argument_values().iter().map(|x| x.unwrap()).collect(),
                        types,
                    );

                    InstantiatedStructType::new(instantiated, HashMap::new())
                });
        }
    }

    pub(crate) fn get_or_instantiate_function(
        &mut self,
        id: &types::functions::InstantiatedFunctionId,
        types: &mut dyn types::store::TypeStore,
    ) -> Option<&types::functions::Function> {
        if !self.functions.contains_key(&id.id()) {
            return None;
        }

        self.instantiate_function(id, types);

        self.instantiated_functions.get(id)
    }

    fn instantiate_function(
        &mut self,
        id: &types::functions::InstantiatedFunctionId,
        types: &mut dyn types::store::TypeStore,
    ) {
        self.instantiated_functions
            .entry(id.clone())
            .or_insert_with(|| {
                self.functions.get(&id.id()).unwrap().with_type_arguments(
                    id.argument_values().iter().map(|x| x.unwrap()).collect(),
                    types,
                )
            });
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
            types::TypeKind::Struct(_) => todo!(),
            types::TypeKind::IndirectCallable(_, _) => todo!(),
            types::TypeKind::InterfaceObject { .. } => todo!(),
            types::TypeKind::Generic(_) => todo!(),
            types::TypeKind::Interface(_) => todo!(),
        }
    }

    pub fn make_function_type(
        &self,
        arguments: &[types::store::TypeId],
        return_type: &types::Type,
        types: &dyn types::store::TypeStore,
    ) -> FunctionType<'ctx> {
        let arguments = arguments
            .iter()
            .map(|arg| {
                self.type_to_llvm(types.get(*arg))
                    .as_basic_type_enum()
                    .into()
            })
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
        fields: &[types::structs::StructField],
        types: &dyn types::store::TypeStore,
    ) -> CompiledStruct<'ctx> {
        let mut field_types = vec![];
        let mut field_indices = HashMap::new();

        for (index, field) in fields.iter().filter(|f| !f.static_).enumerate() {
            field_types.push(
                self.type_to_llvm(types.get(field.type_))
                    .as_basic_type_enum(),
            );
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
        compiled_function: &CompiledFunction<'ctx>,
        context: &CompilerContext<'ctx>,
    ) -> (BasicTypeEnum<'ctx>, PointerValue<'ctx>) {
        let index = self.field_indices.get(&field).expect("field should exist");

        let pointer = unsafe {
            compiled_function.builder.build_gep(
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
