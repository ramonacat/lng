use std::{collections::HashMap, fmt::Debug};

use inkwell::{
    module::Linkage,
    values::{BasicValue as _, BasicValueEnum, PointerValue},
};
use itertools::Itertools;

use crate::{ast::SourceRange, name_mangler::MangledIdentifier, types};

use super::{builtins::rc::RcValue, context::CompilerContext};

#[derive(Clone)]
pub struct FunctionHandle {
    pub name: MangledIdentifier,
    pub fqname: types::FQName,
    pub module_name: types::FQName,
    pub position: SourceRange,
    // TODO do we need arguments, if they're defined in the definition already???
    pub arguments: Vec<types::Argument>,
    pub return_type: types::Type,
    pub linkage: Linkage,
    pub definition: types::FunctionDefinition,
}

impl FunctionHandle {
    // TODO this method should not be exist, FunctionHandle should be a lightweight pointer to some
    // table with all the declared functions, and this struct should only be created for
    // already instantiated functions
    pub(crate) fn instantiate(
        &self,
        type_argument_values: &types::TypeArgumentValues,
        object_lookup: &impl Fn(types::FQName) -> types::Type,
    ) -> Self {
        let arguments = self
            .arguments
            .iter()
            .map(|x| x.instantiate(type_argument_values, object_lookup))
            .collect();

        let return_type = self
            .return_type
            .instantiate(type_argument_values, object_lookup);

        Self {
            name: self.name.clone(),
            fqname: self.fqname,
            module_name: self.module_name,
            position: self.position,
            arguments,
            return_type,
            linkage: self.linkage,
            definition: self
                .definition
                .instantiate(type_argument_values, object_lookup),
        }
    }
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

pub struct InstantiatedStructType<'ctx> {
    definition: types::Struct,
    static_field_values: HashMap<types::Identifier, Value<'ctx>>,
}

pub struct StructInstance<'ctx>(PointerValue<'ctx>, types::InstantiatedStructId);

impl<'ctx> StructInstance<'ctx> {
    pub(crate) const fn value(&self) -> PointerValue<'ctx> {
        self.0
    }

    pub(crate) fn id(&self) -> types::InstantiatedStructId {
        self.1.clone()
    }

    pub(crate) const fn new(
        pointer: PointerValue<'ctx>,
        type_: types::InstantiatedStructId,
    ) -> Self {
        Self(pointer, type_)
    }
}

impl Debug for InstantiatedStructType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fields = self
            .definition
            .fields
            .iter()
            .map(|f| format!("{}:{}", f.name, f.type_.debug_name()))
            .join(", ");

        write!(f, "Struct({}){{{}}}>", self.definition.name, fields)
    }
}

impl<'ctx> InstantiatedStructType<'ctx> {
    pub fn build_heap_instance(
        &self,
        context: &CompilerContext<'ctx>,
        binding_name: &str,
        mut field_values: HashMap<types::Identifier, BasicValueEnum<'ctx>>,
    ) -> PointerValue<'ctx> {
        // TODO ensure the type has all the type arguments filled in here
        let llvm_type = context.make_struct_type(self.definition.name, &self.definition.fields);

        let instance = context
            .builder
            .build_malloc(
                llvm_type.as_llvm_type(),
                &(binding_name.to_string() + "_ptr"),
            )
            .unwrap();

        for field in self.definition.fields.iter().filter(|x| !x.static_) {
            let field_value = field_values.remove(&field.name).unwrap();
            let (_, field_pointer) = llvm_type
                .field_pointer(field.name, instance, context)
                .unwrap();

            context
                .builder
                .build_store(field_pointer, field_value)
                .unwrap();
        }

        instance
    }

    // TODO perhaps have a type for instance that can hold both the pointer and type_argument
    // values?
    pub fn build_field_load(
        &self,
        field: types::Identifier,
        instance: PointerValue<'ctx>,
        binding_name: &str,
        context: &CompilerContext<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let (field_type, field_pointer) = context
            .make_struct_type(self.definition.name, &self.definition.fields)
            .field_pointer(field, instance, context)
            .unwrap();

        context
            .builder
            .build_load(field_type, field_pointer, binding_name)
            .unwrap()
    }

    pub fn build_field_store(
        &self,
        field_name: types::Identifier,
        instance: PointerValue<'ctx>,
        value: BasicValueEnum<'ctx>,
        context: &CompilerContext<'ctx>,
    ) {
        let (_, field_pointer) = context
            .make_struct_type(self.definition.name, &self.definition.fields)
            .field_pointer(field_name, instance, context)
            .unwrap();

        context.builder.build_store(field_pointer, value).unwrap();
    }

    // TODO this should be integrated with build_field_load perhaps?
    pub(crate) fn read_field_value(
        &self,
        _instance: Value<'ctx>,
        name: types::Identifier,
    ) -> Option<Value<'ctx>> {
        let field = self.definition.fields.iter().find(|f| f.name == name)?;

        if field.static_ {
            return self.static_field_values.get(&name).cloned();
        }

        todo!("support reading non-static fields!");
    }

    // TODO remove this, new_with_statics should be the only constructor
    // TODO we should take the generic arguments here, this handle should be one per the set of
    // types
    // TODO the constructor should not be called willy nilly, but instead this should be
    // constructed through CompilerScope, so there's only one instance per set of type arguments
    pub(crate) fn new(description: types::Struct) -> Self {
        Self::new_with_statics(description, HashMap::new())
    }

    pub(crate) fn new_with_statics(
        description: types::Struct,
        mut static_fields: HashMap<types::Identifier, Value<'ctx>>,
    ) -> Self {
        let default_static_fields: Vec<_> = description
            .impls
            .iter()
            .map(|(name, impl_)| {
                let handle = FunctionHandle {
                    definition: impl_.definition.clone(),
                    fqname: description.name.with_part(*name),
                    linkage: if impl_.visibility == types::Visibility::Export {
                        Linkage::External
                    } else {
                        Linkage::Internal
                    },
                    name: impl_.mangled_name(),
                    return_type: impl_.definition.return_type.clone(),
                    arguments: impl_.definition.arguments.clone(),
                    position: impl_.definition.position,
                    module_name: description.name.without_last(),
                };
                (*name, Value::Function(handle))
            })
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

// TODO The *Handle structs should be lightweight handles, and not copied with the vecs and all
// that
#[derive(Clone)]
pub enum Value<'ctx> {
    Empty,
    Primitive(types::InstantiatedStructId, BasicValueEnum<'ctx>),
    Reference(RcValue<'ctx>),
    // TODO remove callable, this is supposed to be a function instad
    #[allow(unused)]
    Callable(FunctionHandle, types::TypeArgumentValues),
    // TODO functions should be refered to by id
    Function(FunctionHandle),
    Struct(types::Struct),
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
            Value::Callable(function_handle, type_argument_values) => {
                write!(f, "{function_handle:?}{type_argument_values:?}")
            }
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
            Value::Callable(_, _) => todo!(),
        }
    }

    pub fn read_field_value(
        &self,
        field_path: types::Identifier,
        context: &CompilerContext<'ctx>,
    ) -> Option<Self> {
        match self {
            Value::Primitive(handle, _) => context
                .instantiated_structs
                .inspect_instantiated_struct(handle, |struct_| {
                    struct_.unwrap().read_field_value(self.clone(), field_path)
                }),
            Value::Reference(ref_) => context
                .instantiated_structs
                .inspect_instantiated_struct(&ref_.type_(), |struct_| {
                    struct_.unwrap().read_field_value(self.clone(), field_path)
                }),
            Value::Function(_) => todo!(),
            Value::Struct(_) => todo!(),
            Value::Empty => todo!(),
            Value::Callable(_, _) => todo!(),
        }
    }

    pub fn as_struct(&self) -> Option<types::Struct> {
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
