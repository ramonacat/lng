use std::{collections::HashMap, fmt::Debug};

use inkwell::values::{BasicValue as _, BasicValueEnum, PointerValue, StructValue};
use itertools::Itertools;

use crate::{identifier::Identifier, types};

use super::{
    builtins::rc::RcValue,
    context::{AllItems, CompilerContext},
    unique_name,
};

pub struct InstantiatedStructType<'ctx> {
    pub(crate) definition: types::structs::Struct,
    pub(crate) static_field_values: HashMap<Identifier, Value<'ctx>>,
}

pub struct StructInstance<'ctx>(PointerValue<'ctx>, types::store::TypeId);

impl<'ctx> StructInstance<'ctx> {
    pub(crate) const fn value(&self) -> PointerValue<'ctx> {
        self.0
    }

    pub(crate) const fn new(pointer: PointerValue<'ctx>, type_: types::store::TypeId) -> Self {
        Self(pointer, type_)
    }

    pub(crate) const fn type_id(&self) -> types::store::TypeId {
        self.1
    }
}

impl Debug for InstantiatedStructType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fields = self
            .definition
            .fields
            .iter()
            .map(|f| format!("{}:{}", f.name, f.type_))
            .join(", ");

        write!(f, "Struct({}){{{}}}>", self.definition.id, fields)
    }
}

impl<'ctx> InstantiatedStructType<'ctx> {
    pub fn build_value(
        &self,
        mut field_values: HashMap<Identifier, BasicValueEnum<'ctx>>,
        types: &dyn types::store::TypeStore,
        context: &CompilerContext<'ctx>,
    ) -> StructValue<'ctx> {
        let llvm_type = context.make_struct_type(&self.definition.fields, types);

        dbg!(&field_values, self.definition.id);

        let mut ordered_field_values = vec![];
        for field in self.definition.fields.iter().filter(|x| !x.static_) {
            ordered_field_values.push(field_values.remove(&field.name).unwrap());
        }
        assert!(field_values.is_empty());

        llvm_type
            .as_llvm_type()
            .const_named_struct(&ordered_field_values)
    }

    pub fn build_heap_instance(
        &self,
        context: &CompilerContext<'ctx>,
        binding_name: &str,
        mut field_values: HashMap<Identifier, BasicValueEnum<'ctx>>,
        types: &dyn types::store::TypeStore,
    ) -> PointerValue<'ctx> {
        let llvm_type = context.make_struct_type(&self.definition.fields, types);

        let instance = context
            .builder
            .build_malloc(
                llvm_type.as_llvm_type(),
                &(binding_name.to_string() + "_ptr"),
            )
            .unwrap();

        dbg!(self.definition.id, &field_values);
        for field in self.definition.fields.iter().filter(|x| !x.static_) {
            let field_value = field_values.remove(&field.name).unwrap();
            let (_, field_pointer) = llvm_type.field_pointer(field.name, instance, context);

            context
                .builder
                .build_store(field_pointer, field_value)
                .unwrap();
        }
        assert!(field_values.is_empty());

        instance
    }

    pub fn build_field_load(
        &self,
        field: Identifier,
        instance: PointerValue<'ctx>,
        binding_name: &str,
        context: &CompilerContext<'ctx>,
        types: &dyn types::store::TypeStore,
    ) -> BasicValueEnum<'ctx> {
        let (field_type, field_pointer) = context
            .make_struct_type(&self.definition.fields, types)
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
        types: &dyn types::store::TypeStore,
    ) {
        let (_, field_pointer) = context
            .make_struct_type(&self.definition.fields, types)
            .field_pointer(field_name, instance, context);

        context.builder.build_store(field_pointer, value).unwrap();
    }

    // TODO this should be integrated with build_field_load perhaps?
    pub(crate) fn read_field_value(
        &self,
        instance: Value<'ctx>,
        name: Identifier,
        context: &CompilerContext<'ctx>,
        structs: &AllItems<'ctx>,
        types: &dyn types::store::TypeStore,
    ) -> Option<Value<'ctx>> {
        let field = self.definition.fields.iter().find(|f| f.name == name)?;

        if field.static_ {
            return self.static_field_values.get(&name).cloned();
        }

        let instance_ptr = match instance {
            Value::Empty => todo!(),
            Value::Primitive(_, _) => todo!(),
            Value::Reference(rc_value) => rc_value.pointee(context, structs, types),
            Value::Function(_) => todo!(),
            Value::InstantiatedStruct(_) => todo!(),
        };

        let raw_result = self.build_field_load(
            field.name,
            instance_ptr,
            &unique_name(&["field_read", &field.name.raw()]),
            context,
            types,
        );
        let field_type = types.get(field.type_);
        let result = match field_type.kind() {
            types::TypeKind::Unit => todo!(),
            types::TypeKind::Object { .. } => Value::Reference(RcValue::from_pointer(
                raw_result.into_pointer_value(),
                field.type_,
            )),
            types::TypeKind::Array { .. } => todo!(),
            types::TypeKind::Callable(_) => todo!(),
            types::TypeKind::U64 => todo!(),
            types::TypeKind::U8 => todo!(),
            types::TypeKind::Pointer(_) => todo!(),
            types::TypeKind::Struct(_) => todo!(),
            types::TypeKind::IndirectCallable(_, _) => todo!(),
            types::TypeKind::InterfaceObject { .. } => todo!(),
            types::TypeKind::Generic(_) => todo!(),
            types::TypeKind::Interface(_) => todo!(),
        };

        Some(result)
    }

    pub(crate) fn new(
        description: types::structs::Struct,
        mut static_fields: HashMap<Identifier, Value<'ctx>>,
    ) -> Self {
        let default_static_fields: Vec<_> = description
            .impls
            .iter()
            .map(|id| (id.local(), Value::Function(*id)))
            .collect();

        for (name, value) in default_static_fields {
            static_fields.insert(name, value);
        }

        Self {
            definition: description,
            static_field_values: static_fields,
        }
    }
}

// TODO this should be Copy, once InstantiatedStructId is
#[derive(Clone)]
pub enum Value<'ctx> {
    Empty,
    Primitive(types::structs::InstantiatedStructId, BasicValueEnum<'ctx>),
    Reference(RcValue<'ctx>),
    // TODO this must be InstantiatedFunctionId!
    Function(types::functions::FunctionId),
    InstantiatedStruct(types::structs::InstantiatedStructId),
}

impl Debug for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Primitive(struct_id, basic_value_enum) => {
                write!(f, "{struct_id}({basic_value_enum})")
            }
            Value::Reference(rc_value) => {
                write!(f, "Rc<{:?}>({})", rc_value.type_(), rc_value.as_ptr())
            }
            Value::Function(function_handle) => write!(f, "{function_handle:?}"),
            Value::Empty => todo!(),
            Value::InstantiatedStruct(_) => todo!(),
        }
    }
}

impl<'ctx> Value<'ctx> {
    pub fn as_basic_value(&self) -> BasicValueEnum<'ctx> {
        match self {
            Value::Primitive(_, value) => *value,
            Value::Reference(value) => value.as_ptr().as_basic_value_enum(),
            Value::Function(_) => todo!(),
            Value::Empty => todo!(),
            Value::InstantiatedStruct(_) => todo!(),
        }
    }

    pub fn read_field_value(
        &self,
        field_path: Identifier,
        context: &CompilerContext<'ctx>,
        structs: &AllItems<'ctx>,
        types: &dyn types::store::TypeStore,
    ) -> Option<Self> {
        match self {
            Value::Primitive(struct_id, _) => structs
                .get_struct(struct_id)
                .unwrap()
                .read_field_value(self.clone(), field_path, context, structs, types),
            Value::Reference(ref_) => {
                let ref_type = types.get(ref_.type_());
                let instantiated_struct_id = match ref_type.kind() {
                    types::TypeKind::Unit => todo!(),
                    types::TypeKind::Object(instantiated_struct_id) => instantiated_struct_id,
                    types::TypeKind::Array { .. } => todo!(),
                    types::TypeKind::Callable(_) => todo!(),
                    types::TypeKind::U64 => todo!(),
                    types::TypeKind::U8 => todo!(),
                    types::TypeKind::Pointer(_) => todo!(),
                    types::TypeKind::Struct(_) => todo!(),
                    types::TypeKind::IndirectCallable(_, _) => {
                        todo!()
                    }
                    types::TypeKind::InterfaceObject { .. } => todo!(),
                    types::TypeKind::Generic(_) => todo!(),
                    types::TypeKind::Interface(_) => todo!(),
                };
                structs
                    .get_struct(instantiated_struct_id)
                    .unwrap()
                    .read_field_value(self.clone(), field_path, context, structs, types)
            }
            Value::Function(_) => todo!(),
            Value::Empty => todo!(),
            Value::InstantiatedStruct(_) => todo!(),
        }
    }
}
