use std::collections::HashMap;

use inkwell::{
    types::StructType,
    values::{BasicValue as _, BasicValueEnum, FunctionValue},
};

use crate::{ast::SourceRange, name_mangler::MangledIdentifier, types};

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

// TODO make the fields private
#[derive(Debug, Clone)]
pub struct StructHandle<'ctx> {
    pub description: types::Struct,
    // TODO static_fileds should be in types::Struct really
    pub static_fields: HashMap<types::Identifier, Value<'ctx>>,
    pub llvm_type: StructType<'ctx>,
}

impl<'ctx> StructHandle<'ctx> {
    pub fn read_field_value(
        &self,
        _instance: Value<'ctx>,
        name: &types::Identifier,
    ) -> Option<Value<'ctx>> {
        let field = self.description.fields.iter().find(|f| &f.name == name)?;

        if field.static_ {
            return Some(self.static_fields.get(name).unwrap().clone());
        }

        todo!("support reading non-static fields!");
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
    pub fn to_basic_value(&self) -> BasicValueEnum<'ctx> {
        match self {
            Value::Primitive(_, value) => *value,
            Value::Reference(value) => value.as_ptr().as_basic_value_enum(),
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
