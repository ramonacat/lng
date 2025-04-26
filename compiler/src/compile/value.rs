use std::collections::HashMap;

use inkwell::{
    types::StructType,
    values::{BasicValue as _, BasicValueEnum, FunctionValue, PointerValue},
};

use crate::{
    ast::SourceRange,
    name_mangler::MangledIdentifier,
    types::{self, Identifier},
};

use super::{context::CompilerContext, module::CompiledModule, rc_builder::RcValue};

#[derive(Debug, Clone)]
pub struct FunctionHandle {
    pub name: MangledIdentifier,
    pub location: SourceRange,
    pub arguments: Vec<types::Argument>,
    pub return_type: types::Type,
    pub export: bool,
}

impl<'ctx> FunctionHandle {
    pub fn get_or_create_in_module(
        &self,
        module: &CompiledModule<'ctx>,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        module
            .llvm_module
            .get_function(self.name.as_str())
            .unwrap_or_else(|| {
                module.declare_function(
                    self.export,
                    self.name.clone(),
                    &self.arguments,
                    &self.return_type,
                    context,
                )
            })
    }
}

#[derive(Debug, Clone)]
pub struct StructHandle<'ctx> {
    description: types::Struct,
    // TODO rename to static_values?
    // TODO ensure that this object can only be created when all values are set
    static_fields: HashMap<types::Identifier, Value<'ctx>>,
    llvm_type: StructType<'ctx>,
}

impl<'ctx> StructHandle<'ctx> {
    pub fn build_heap_instance(
        &self,
        context: &CompilerContext<'ctx>,
        binding_name: &str,
        field_values: HashMap<Identifier, BasicValueEnum<'ctx>>,
    ) -> PointerValue<'ctx> {
        let instance = context
            .builder
            .build_malloc(self.llvm_type, &(binding_name.to_string() + "_ptr"))
            .unwrap();

        for field in self.description.fields.iter().filter(|x| !x.static_) {
            let field_value = field_values.get(&field.name).unwrap();
            let (_, field_pointer) = self.field_pointer(&field.name, instance, context);

            context
                .builder
                .build_store(field_pointer, *field_value)
                .unwrap();
        }

        instance
    }

    pub fn build_field_load(
        &self,
        field: Identifier,
        instance: PointerValue<'ctx>,
        binding_name: &str,
        context: &CompilerContext<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let (field_type, field_pointer) = self.field_pointer(&field, instance, context);

        context
            .builder
            .build_load(field_type, field_pointer, binding_name)
            .unwrap()
    }

    pub fn build_field_store(
        &self,
        field_name: Identifier,
        instance: PointerValue<'ctx>,
        value: BasicValueEnum<'ctx>,
        context: &CompilerContext<'ctx>,
    ) {
        let (_, field_pointer) = self.field_pointer(&field_name, instance, context);

        context.builder.build_store(field_pointer, value).unwrap();
    }

    fn field_pointer(
        &self,
        name: &Identifier,
        instance: PointerValue<'ctx>,
        context: &CompilerContext<'ctx>,
    ) -> (inkwell::types::BasicTypeEnum<'ctx>, PointerValue<'ctx>) {
        let (field_index, field) = self
            .description
            .fields
            .iter()
            .enumerate()
            .find(|x| &x.1.name == name)
            .unwrap();

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
                        .const_int(field_index as u64, false),
                ],
                &format!("{name}_gep_{field_index}"),
            )
        }
        .unwrap();

        (
            context.type_to_llvm(&field.type_).as_basic_type_enum(),
            pointer,
        )
    }

    pub fn import(&self, importing_module: &CompiledModule<'ctx>, context: &CompilerContext<'ctx>) {
        for impl_ in self.static_fields.values() {
            match impl_ {
                Value::Primitive(_, _) => todo!(),
                Value::Reference(_) => todo!(),
                Value::Function(function_handle) => importing_module.import_function(
                    function_handle,
                    function_handle.name.clone(),
                    context,
                ),
                Value::Struct(_) => todo!(),
            };
        }

        importing_module.set_variable(self.description.name.clone(), Value::Struct(self.clone()));
    }

    pub fn read_static_field(&self, name: &types::Identifier) -> Option<Value<'ctx>> {
        self.static_fields.get(name).cloned()
    }

    pub fn read_field_value(
        &self,
        _instance: Value<'ctx>,
        name: &types::Identifier,
    ) -> Option<Value<'ctx>> {
        let field = self.description.fields.iter().find(|f| &f.name == name)?;

        if field.static_ {
            return self.read_static_field(name);
        }

        todo!("support reading non-static fields!");
    }

    // TODO remove llvm_type argument, figure out the type based on `description`
    pub(crate) fn new(description: types::Struct, llvm_type: StructType<'ctx>) -> Self {
        Self::new_with_statics(description, llvm_type, HashMap::new())
    }

    // TODO remove llvm_type argument, figure out the type based on `description`
    pub(crate) fn new_with_statics(
        description: types::Struct,
        llvm_type: StructType<'ctx>,
        static_fields: HashMap<Identifier, Value<'ctx>>,
    ) -> StructHandle<'ctx> {
        Self {
            description,
            static_fields,
            llvm_type,
        }
    }
}

// TODO The *Handle structs should be lightweight handles, and not copied with the vecs and all
// that
#[derive(Debug, Clone)]
pub enum Value<'ctx> {
    Primitive(StructHandle<'ctx>, BasicValueEnum<'ctx>),
    Reference(RcValue<'ctx>),
    Function(FunctionHandle),
    Struct(StructHandle<'ctx>),
}

impl<'ctx> Value<'ctx> {
    pub fn as_basic_value(&self) -> BasicValueEnum<'ctx> {
        match self {
            Value::Primitive(_, value) => *value,
            Value::Reference(value) => value.as_ptr().as_basic_value_enum(),
            Value::Function(_) => todo!(),
            Value::Struct(_) => todo!(),
        }
    }

    pub fn read_field_value(&self, field_name: &Identifier) -> Option<Value<'ctx>> {
        match self {
            Value::Primitive(handle, _) => handle.read_field_value(self.clone(), field_name),
            Value::Reference(ref_) => ref_.type_().read_field_value(self.clone(), field_name),
            Value::Function(_) => todo!(),
            Value::Struct(_) => todo!(),
        }
    }

    pub fn as_struct(&self) -> Option<StructHandle<'ctx>> {
        if let Value::Struct(handle) = self {
            Some(handle.clone())
        } else {
            None
        }
    }

    pub fn as_function(&self) -> Option<FunctionHandle> {
        if let Value::Function(handle) = self {
            Some(handle.clone())
        } else {
            None
        }
    }
}
