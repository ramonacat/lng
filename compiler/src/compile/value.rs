use std::{collections::HashMap, fmt::Debug};

use inkwell::values::{BasicValue as _, BasicValueEnum, FunctionValue, PointerValue};
use itertools::Itertools;

use crate::{
    ast::SourceRange,
    name_mangler::MangledIdentifier,
    types::{self, FQName, Identifier, Visibility},
};

use super::{
    context::{CompilerContext, CompiledStruct},
    module::CompiledModule,
    rc_builder::RcValue,
};

#[derive(Clone)]
pub struct FunctionHandle {
    pub name: MangledIdentifier,
    pub fqname: FQName,
    pub position: SourceRange,
    pub arguments: Vec<types::Argument>,
    pub return_type: types::Type,
    // TODO this should be handled in Scope
    pub visibility: Visibility,
}

impl Debug for FunctionHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let arguments = self
            .arguments
            .iter()
            .map(|a| format!("{}:{}", a.name, a.type_))
            .join(", ");

        write!(f, "Fn<{}({})>", self.name.as_str(), arguments)
    }
}

impl<'ctx> FunctionHandle {
    // TODO should this be moved to the module? so that it handles the creation?
    pub fn get_or_create_in_module(
        &self,
        module: &CompiledModule<'ctx>,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        module
            .get_llvm_function(self.name.as_str())
            .unwrap_or_else(|| {
                module.declare_function(
                    self.visibility == Visibility::Export,
                    &self.name,
                    &self.arguments,
                    &self.return_type,
                    context,
                )
            })
    }
}

#[derive(Clone)]
pub struct StructHandle<'ctx> {
    description: types::Struct,
    // TODO rename to static_values?
    // TODO ensure that this object can only be created when all values are set
    static_fields: HashMap<types::Identifier, Value<'ctx>>,
}

impl Debug for StructHandle<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fields = self
            .description
            .fields
            .iter()
            .map(|f| format!("{}:{}", f.name, f.type_.debug_name()))
            .join(", ");

        write!(f, "Struct<{}({})>", self.description.name, fields)
    }
}

impl<'ctx> StructHandle<'ctx> {
    pub fn build_heap_instance(
        &self,
        context: &CompilerContext<'ctx>,
        binding_name: &str,
        mut field_values: HashMap<Identifier, BasicValueEnum<'ctx>>,
    ) -> PointerValue<'ctx> {
        let llvm_type = self.llvm_type(context);

        let instance = context
            .builder
            .build_malloc(
                llvm_type.as_llvm_type(),
                &(binding_name.to_string() + "_ptr"),
            )
            .unwrap();

        for field in self.description.fields.iter().filter(|x| !x.static_) {
            let field_value = field_values.remove(&field.name).unwrap();
            let (_, field_pointer) = llvm_type.field_pointer(field.name, instance, context);

            context
                .builder
                .build_store(field_pointer, field_value)
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
        let (field_type, field_pointer) = self
            .llvm_type(context)
            .field_pointer(field, instance, context);

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
        let (_, field_pointer) = self
            .llvm_type(context)
            .field_pointer(field_name, instance, context);

        context.builder.build_store(field_pointer, value).unwrap();
    }

    pub fn import(&self, importing_module: &CompiledModule<'ctx>, context: &CompilerContext<'ctx>) {
        for impl_ in self.static_fields.values() {
            match impl_ {
                Value::Primitive(_, _) => todo!(),
                Value::Reference(_) => todo!(),
                Value::Function(function_handle) => {
                    importing_module.import_function(function_handle, context)
                }
                Value::Struct(_) => todo!(),
                Value::Empty => todo!(),
            };
        }

        importing_module.set_variable(self.description.name.last(), Value::Struct(self.clone()));
    }

    // TODO this should be integrated with build_field_load perhaps?
    pub fn read_field_value(
        &self,
        _instance: Value<'ctx>,
        name: Identifier,
    ) -> Option<Value<'ctx>> {
        let field = self.description.fields.iter().find(|f| f.name == name)?;

        if field.static_ {
            return self.static_fields.get(&name).cloned();
        }

        todo!("support reading non-static fields!");
    }

    pub(crate) fn new(description: types::Struct) -> Self {
        Self::new_with_statics(description, HashMap::new())
    }

    pub(crate) const fn new_with_statics(
        description: types::Struct,
        static_fields: HashMap<Identifier, Value<'ctx>>,
    ) -> Self {
        Self {
            description,
            static_fields,
        }
    }

    // TODO inline this???
    fn llvm_type(&self, context: &CompilerContext<'ctx>) -> CompiledStruct<'ctx> {
        context.make_struct_type(&self.description.fields)
    }
}

// TODO The *Handle structs should be lightweight handles, and not copied with the vecs and all
// that
#[derive(Clone)]
pub enum Value<'ctx> {
    Empty,
    Primitive(StructHandle<'ctx>, BasicValueEnum<'ctx>),
    Reference(RcValue<'ctx>),
    Function(FunctionHandle),
    Struct(StructHandle<'ctx>),
}

impl Debug for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Primitive(struct_handle, basic_value_enum) => {
                write!(f, "{struct_handle:?}({basic_value_enum})")
            }
            Value::Reference(rc_value) => {
                write!(f, "Rc<{:?}>({})", rc_value.type_(), rc_value.as_ptr())
            }
            Value::Function(function_handle) => write!(f, "{function_handle:?}"),
            Value::Struct(struct_handle) => write!(f, "{struct_handle:?}"),
            Value::Empty => todo!(),
        }
    }
}

impl<'ctx> Value<'ctx> {
    pub fn as_basic_value(&self) -> BasicValueEnum<'ctx> {
        match self {
            Value::Primitive(_, value) => *value,
            Value::Reference(value) => value.as_ptr().as_basic_value_enum(),
            Value::Function(_) => todo!(),
            Value::Struct(_) => todo!(),
            Value::Empty => todo!(),
        }
    }

    pub fn read_field_value(&self, field_path: Identifier) -> Option<Self> {
        match self {
            Value::Primitive(handle, _) => handle.read_field_value(self.clone(), field_path),
            Value::Reference(ref_) => ref_.type_().read_field_value(self.clone(), field_path),
            Value::Function(_) => todo!(),
            Value::Struct(_) => todo!(),
            Value::Empty => todo!(),
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
