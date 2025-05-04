use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    sync::{LazyLock, RwLock},
};

use itertools::Itertools;
use string_interner::{StringInterner, backend::StringBackend, symbol::SymbolU32};

use crate::{
    ast::{self, SourceRange},
    name_mangler::{MangledIdentifier, mangle_fq_name, nomangle_identifier},
    std::{TYPE_NAME_U64, TYPE_NAME_UNIT},
};

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Identifier(SymbolU32);

impl std::fmt::Debug for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Identifier({self})")
    }
}

static IDENTIFIERS: LazyLock<RwLock<StringInterner<StringBackend>>> =
    LazyLock::new(|| RwLock::new(StringInterner::default()));

impl Identifier {
    pub fn parse(raw: &str) -> Self {
        let symbol = IDENTIFIERS.write().unwrap().get_or_intern(raw);
        Self(symbol)
    }

    pub(crate) fn raw(self) -> String {
        IDENTIFIERS
            .read()
            .unwrap()
            .resolve(self.0)
            .unwrap()
            .to_string()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw())
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct FQName(SymbolU32);

impl std::fmt::Debug for FQName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "FQName({self})")
    }
}

impl Display for FQName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.parts().iter().map(|x| x.raw()).join("."))
    }
}

impl FQName {
    // TODO add from_identifier and check all usages of parse&from_parts that can be simplified
    pub fn parse(raw: &str) -> Self {
        let interned = IDENTIFIERS.write().unwrap().get_or_intern(raw);

        Self(interned)
    }

    pub fn from_parts(path: impl Iterator<Item = impl Into<String>>) -> Self {
        Self::parse(&path.map(Into::<String>::into).join("."))
    }

    pub fn with_part(self, new_part: Identifier) -> Self {
        let raw = IDENTIFIERS
            .read()
            .unwrap()
            .resolve(self.0)
            .unwrap()
            .to_string();

        if raw.is_empty() {
            Self::parse(&format!("{new_part}"))
        } else {
            Self::parse(&format!("{raw}.{new_part}"))
        }
    }

    pub fn parts(self) -> Vec<Identifier> {
        let raw = IDENTIFIERS
            .read()
            .unwrap()
            .resolve(self.0)
            .unwrap()
            .to_string();

        raw.split('.').map(Identifier::parse).collect()
    }

    pub(crate) fn into_mangled(self) -> MangledIdentifier {
        mangle_fq_name(self)
    }

    pub fn last(self) -> Identifier {
        *self.parts().last().unwrap()
    }

    pub fn without_last(self) -> Self {
        let parts = self.parts();
        let len = parts.len();
        let parts = parts.into_iter().map(Identifier::raw);

        Self::from_parts(parts.take(len - 1))
    }

    pub(crate) fn split_first(self) -> (Identifier, Self) {
        let parts = self.parts();
        let (first, rest) = parts.split_first().unwrap();

        (*first, Self::from_parts(rest.iter().map(|x| x.raw())))
    }

    pub(crate) fn len(self) -> usize {
        self.parts().len()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeArguments(Vec<TypeArgument>);
impl TypeArguments {
    pub(crate) const fn new_empty() -> Self {
        Self(vec![])
    }

    pub(crate) const fn new(arguments: Vec<TypeArgument>) -> Self {
        Self(arguments)
    }
}

impl std::fmt::Display for TypeArguments {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            return Ok(());
        }

        write!(f, "{}", self.0.iter().map(ToString::to_string).join(","))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDescriptorType {
    pub instance_id: Option<InstantiatedStructId>,
    pub name: FQName,
    // the fields are a Vec<_>, so that the order is well defined
    pub fields: Vec<StructField>,
}

impl StructDescriptorType {
    fn instance_type(&self) -> Type {
        // TODO can we avoid special-casing the types here? perhaps take the object type as an
        // argument?
        if self.name == *TYPE_NAME_U64 {
            return Type::new_not_generic(TypeKind::U64);
        }

        if self.name == *TYPE_NAME_UNIT {
            return Type::new_not_generic(TypeKind::Unit);
        }

        for field in &self.fields {
            // TODO instead of assert! return an error here if there are still type arguments that
            // need values
            assert!(field.type_.arguments.0.is_empty());
        }

        // TODO we should ensure that the object type has the correct type_arguments, it does not
        // have to be not generic
        //
        self.instance_id.as_ref().map_or_else(
            || {
                Type::new_not_generic(TypeKind::UninstantiatedObject {
                    type_name: self.name,
                })
            },
            |instance_id| {
                Type::new_not_generic(TypeKind::Object {
                    // TODO return Err() in case there's no instance_id
                    type_name: instance_id.clone(),
                })
            },
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeArgument(Identifier);

impl TypeArgument {
    pub const fn new(name: Identifier) -> Self {
        Self(name)
    }
}

impl std::fmt::Display for TypeArgument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeArgumentValues(HashMap<TypeArgument, Type>);

impl std::hash::Hash for TypeArgumentValues {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.iter().collect_vec().hash(state);
    }
}

impl TypeArgumentValues {
    pub(crate) fn new_empty() -> Self {
        Self(HashMap::new())
    }

    fn get(&self, type_argument: TypeArgument) -> Option<&Type> {
        self.0.get(&type_argument)
    }

    pub(crate) const fn new(tav: HashMap<TypeArgument, Type>) -> Self {
        Self(tav)
    }
}

impl std::fmt::Display for TypeArgumentValues {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            return Ok(());
        }

        write!(
            f,
            "<{}>",
            self.0
                .iter()
                .map(|(name, value)| format!("{name}={value}"))
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstantiatedStructId(pub Struct, pub TypeArgumentValues);

impl Display for InstantiatedStructId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Generic(TypeArgument),
    Unit,
    Object {
        type_name: InstantiatedStructId,
    },
    Array {
        element_type: Box<Type>,
    },
    // TODO this should probably be simply an object, just of StructDescriptor<TargetStruct> type
    StructDescriptor(StructDescriptorType),
    // TODO this should be an object with special properties
    Callable {
        arguments: Vec<Argument>,
        return_type: Box<Type>,
    },
    // TODO add u128,u32,u16,u8 and signed counterparts
    // TODO add bool
    // TODO add float
    U64,
    U8,
    Pointer(Box<Type>),
    // TODO we should stop refering to structs by FQName, as dynamically created ones won't have
    // it, they should instead have an ID created for them at declaration time
    UninstantiatedObject {
        type_name: FQName,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    kind: TypeKind,
    arguments: TypeArguments,
    argument_values: TypeArgumentValues,
}

impl Type {
    pub(crate) fn debug_name(&self) -> String {
        let Self {
            kind,
            arguments: _,
            argument_values: type_argument_values,
        } = self;
        match kind {
            TypeKind::Unit => "void".to_string(),
            TypeKind::Object {
                type_name: item_path,
            } => format!("{item_path}{type_argument_values}"),
            TypeKind::StructDescriptor(StructDescriptorType {
                name: item_path,
                fields: _,
                instance_id: _,
            }) => format!("Struct{type_argument_values}({item_path})"),
            TypeKind::Array {
                element_type: inner,
            } => format!("{}[]", inner.debug_name()),
            TypeKind::Callable {
                arguments,
                return_type,
            } => format!(
                "Callable<({}):{}>",
                arguments
                    .iter()
                    .map(|x| format!("{}:{}", x.name, x.type_))
                    .join(", "),
                return_type
            ),
            TypeKind::U64 => "u64".to_string(),
            TypeKind::U8 => "u8".to_string(),
            TypeKind::Pointer(inner) => format!("*{}", inner.debug_name()),
            TypeKind::Generic(type_argument) => format!("Generic({type_argument})"),
            TypeKind::UninstantiatedObject { type_name } => {
                format!("UninstantiatedObject({type_name})")
            }
        }
    }

    // TODO we should really not use FQName to refer to structs, as those could be created
    // dynamically, we should only allow getting an instance id
    pub(crate) fn name(&self) -> FQName {
        match &self.kind {
            TypeKind::Unit => todo!(),
            TypeKind::Object {
                type_name: item_path,
            } => item_path.0.name,
            TypeKind::Array { .. } => todo!(),
            TypeKind::StructDescriptor(_) => todo!(),
            TypeKind::Callable { .. } => todo!(),
            TypeKind::U64 => *TYPE_NAME_U64,
            TypeKind::U8 => todo!(),
            TypeKind::Pointer(_) => todo!(),
            TypeKind::Generic(_) => todo!(),
            TypeKind::UninstantiatedObject { type_name } => *type_name,
        }
    }

    pub(crate) fn instance_type(&self) -> Self {
        // TODO return a Result<> here, error if there are still unresolved type arguments
        assert!(self.arguments.0.is_empty());

        // TODO check for type arguments values here and pass them to the instance as needed
        match &self.kind {
            TypeKind::Unit => todo!(),
            TypeKind::Object { .. } => todo!(),
            // TODO arrays cannot be instantiated, this case is wrong!
            TypeKind::Array {
                element_type: inner,
            } => Self {
                kind: TypeKind::Array {
                    element_type: Box::new(inner.instance_type()),
                },
                arguments: TypeArguments::new_empty(),
                argument_values: TypeArgumentValues::new_empty(),
            },
            TypeKind::StructDescriptor(struct_descriptor_type) => {
                struct_descriptor_type.instance_type()
            }
            TypeKind::Callable { .. } => todo!(),
            TypeKind::U64 => todo!(),
            TypeKind::U8 => todo!(),
            TypeKind::Pointer(_) => todo!(),
            TypeKind::Generic(_) => todo!(),
            TypeKind::UninstantiatedObject { .. } => todo!(),
        }
    }

    pub(crate) fn new_not_generic(kind: TypeKind) -> Self {
        Self {
            kind,
            argument_values: TypeArgumentValues::new_empty(),
            arguments: TypeArguments::new_empty(),
        }
    }

    pub(crate) fn new_generic(kind: TypeKind, type_arguments: Vec<TypeArgument>) -> Self {
        Self {
            kind,
            arguments: TypeArguments::new(type_arguments),
            argument_values: TypeArgumentValues::new_empty(),
        }
    }

    pub(crate) fn u8() -> Self {
        Self::new_not_generic(TypeKind::U8)
    }

    pub(crate) fn u64() -> Self {
        Self::new_not_generic(TypeKind::U64)
    }

    pub(crate) const fn kind(&self) -> &TypeKind {
        &self.kind
    }

    pub(crate) fn instantiate(
        &self,
        type_argument_values: &TypeArgumentValues,
        object_lookup: &impl Fn(FQName) -> Self,
    ) -> Self {
        let kind = match &self.kind {
            // TODO assert that the type_argument is fully instantiated
            TypeKind::Generic(type_argument) => type_argument_values
                .get(*type_argument)
                .unwrap()
                .kind()
                .clone(),
            TypeKind::Unit => TypeKind::Unit,
            TypeKind::Object { type_name } => TypeKind::Object {
                type_name: type_name.clone(),
            },
            TypeKind::Array { element_type } => TypeKind::Array {
                element_type: Box::new(
                    element_type.instantiate(type_argument_values, object_lookup),
                ),
            },
            TypeKind::StructDescriptor(_) => todo!(),
            TypeKind::Callable {
                arguments,
                return_type,
            } => {
                let arguments = arguments
                    .iter()
                    .map(|x| x.instantiate(type_argument_values, object_lookup))
                    .collect();
                let return_type =
                    Box::new(return_type.instantiate(type_argument_values, object_lookup));

                TypeKind::Callable {
                    arguments,
                    return_type,
                }
            }
            TypeKind::U64 => TypeKind::U64,
            TypeKind::U8 => TypeKind::U8,
            TypeKind::Pointer(target) => TypeKind::Pointer(Box::new(
                target.instantiate(type_argument_values, object_lookup),
            )),
            TypeKind::UninstantiatedObject { type_name } => {
                object_lookup(*type_name)
                    .instantiate(type_argument_values, object_lookup)
                    .kind
            }
        };

        Self {
            kind,
            arguments: TypeArguments::new_empty(),
            argument_values: TypeArgumentValues::new_empty(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TypeKind::Unit => write!(f, "void"),
            TypeKind::Object {
                type_name: identifier,
            } => write!(f, "{identifier}{}", self.argument_values),
            TypeKind::Array {
                element_type: inner,
            } => write!(f, "{inner}[]"),
            TypeKind::StructDescriptor(StructDescriptorType {
                name,
                fields: _,
                instance_id: _,
            }) => {
                write!(f, "StructDescriptor{}<{name}>", self.argument_values)
            }
            TypeKind::Callable {
                arguments,
                return_type,
            } => write!(
                f,
                "Callable({}): {return_type}",
                arguments
                    .iter()
                    .map(|a| format!("{a}"))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            TypeKind::U8 => write!(f, "u8"),
            TypeKind::U64 => write!(f, "u64"),
            TypeKind::Pointer(to) => write!(f, "*{to}"),
            TypeKind::Generic(name) => write!(f, "{name}"),
            TypeKind::UninstantiatedObject { .. } => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: Identifier,
    pub type_: Type,
    pub position: SourceRange,
}
impl Argument {
    pub(crate) fn instantiate(
        &self,
        type_argument_values: &TypeArgumentValues,
        object_lookup: &impl Fn(FQName) -> Type,
    ) -> Self {
        Self {
            name: self.name,
            type_: self.type_.instantiate(type_argument_values, object_lookup),
            position: self.position,
        }
    }
}

impl std::hash::Hash for Argument {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.name, &self.type_).hash(state);
    }
}

impl Eq for Argument {}
impl PartialEq for Argument {
    fn eq(&self, other: &Self) -> bool {
        (&self.name, &self.type_) == (&other.name, &other.type_)
    }
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.type_)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub arguments: Vec<Argument>,
    pub return_type: Type,
    pub body: FunctionBody,
    pub position: ast::SourceRange,
}
impl FunctionDefinition {
    pub(crate) fn instantiate(
        &self,
        type_argument_values: &TypeArgumentValues,
        object_lookup: &impl Fn(FQName) -> Type,
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
            arguments,
            return_type,
            // TODO body probably needs to be instantiated as well, as there could be references to
            // type arguments eg. in let statements
            body: self.body.clone(),
            position: self.position,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: FQName,
    pub definition: FunctionDefinition,
}

#[derive(Debug, Clone)]
pub struct AssociatedFunction {
    pub struct_: FQName,
    pub name: Identifier,
    pub definition: FunctionDefinition,
    pub visibility: Visibility,
}
impl AssociatedFunction {
    pub(crate) fn type_(&self) -> Type {
        // TODO the function may actually have type arguments, so we need to consider that case
        // here
        Type {
            kind: TypeKind::Callable {
                arguments: self.definition.arguments.clone(),
                return_type: Box::new(self.definition.return_type.clone()),
            },
            arguments: TypeArguments::new_empty(),
            argument_values: TypeArgumentValues::new_empty(),
        }
    }

    pub(crate) fn mangled_name(&self) -> MangledIdentifier {
        self.struct_.with_part(self.name).into_mangled()
    }
}

impl Function {
    pub(crate) fn type_(&self) -> Type {
        // TODO the function may actually have type arguments, so we need to consider that case
        // here
        Type {
            kind: TypeKind::Callable {
                arguments: self.definition.arguments.clone(),
                return_type: Box::new(self.definition.return_type.clone()),
            },
            argument_values: TypeArgumentValues::new_empty(),
            arguments: TypeArguments::new_empty(),
        }
    }

    pub(crate) fn mangled_name(&self) -> MangledIdentifier {
        match &self.definition.body {
            FunctionBody::Extern(foreign_name) => nomangle_identifier(*foreign_name),
            FunctionBody::Statements(_) => self.name.into_mangled(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionBody {
    Extern(Identifier),
    Statements(Vec<Statement>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Let(LetStatement),
    Return(Expression),
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    UnsignedInteger(u64),
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Call {
        target: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Literal(Literal),
    VariableAccess(Identifier),
    StructConstructor(Identifier),
    FieldAccess {
        target: Box<Expression>,
        field: Identifier,
    },
    SelfAccess,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub position: SourceRange,
    pub type_: Type,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub binding: Identifier,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub imported_item: FQName,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub struct_name: FQName,
    pub name: Identifier,
    pub type_: Type,
    pub static_: bool,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: FQName,
    #[allow(unused)] // TODO this will be needed once we have a syntax for type instantiation
    pub type_arguments: TypeArguments,
    pub fields: Vec<StructField>,
    pub impls: HashMap<Identifier, AssociatedFunction>,
}

// TODO the Hash, Eq, PartialEq implementations are questionable, get rid of them (but for that we
// need to have some globally identifiable id for structs (and FQName isn't it, because that won't
// handle runtime-defined ones))
impl std::hash::Hash for Struct {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl Eq for Struct {}

impl PartialEq for Struct {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Struct {
    pub(crate) fn instantiate(
        &self,
        _type_argument_values: &TypeArgumentValues,
        _object_lookup: &impl Fn(FQName) -> Type,
    ) -> Self {
        // TODO support actually instantiating the impls and fields, but this can't be done yet, as the
        // self argument will lead to infinite recursion the way things are handled rn
        let fields = self.fields.clone();
        // .map(|f| f.instantiate(type_argument_values, object_lookup))

        let impls = self.impls.clone();
        //.map(|(id, impl_)| (*id, impl_.instantiate(type_argument_values, object_lookup)))

        Self {
            name: self.name,
            type_arguments: TypeArguments::new_empty(),
            fields,
            impls,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Function(Function),
    Struct(Struct),
    Import(Import),
    Module(Module),
}

#[derive(Clone)]
pub struct Item {
    pub kind: ItemKind,
    pub visibility: Visibility,
}

impl std::fmt::Debug for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ItemKind::Function(function) => write!(f, "Function({})", function.name),
            ItemKind::Struct(struct_) => write!(f, "Struct({})", struct_.name),
            ItemKind::Import(import) => write!(f, "Import({})", import.imported_item),
            ItemKind::Module(module) => write!(f, "{module:?}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Export,
    Internal,
}

pub struct AppModule {
    main: FQName,
    module: Module,
}
pub struct LibraryModule {
    module: Module,
}

pub enum RootModule {
    App(AppModule),
    Library(LibraryModule),
}

impl RootModule {
    pub(crate) const fn main(&self) -> Option<FQName> {
        match self {
            Self::App(app_module) => Some(app_module.main),
            Self::Library(_) => None,
        }
    }

    pub(crate) const fn root_module(&self) -> &Module {
        match self {
            Self::App(app_module) => &app_module.module,
            Self::Library(library_module) => &library_module.module,
        }
    }

    pub(crate) const fn new_app(main: FQName, root_module: Module) -> Self {
        Self::App(AppModule {
            main,
            module: root_module,
        })
    }

    pub(crate) const fn new_library(root_module: Module) -> Self {
        Self::Library(LibraryModule {
            module: root_module,
        })
    }
}

#[derive(Clone)]
pub struct Module {
    items: HashMap<Identifier, Item>,
}

impl std::fmt::Debug for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut struct_ = f.debug_struct("module");
        for item in &self.items {
            struct_.field(&item.0.raw(), item.1);
        }
        struct_.finish()
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

impl Module {
    pub fn new() -> Self {
        Self {
            items: HashMap::new(),
        }
    }

    pub(crate) fn declare_item(&mut self, name: Identifier, item: Item) {
        self.items.insert(name, item);
    }

    pub(crate) fn items(&self) -> impl Iterator<Item = (Identifier, &Item)> {
        self.items.iter().map(|(k, v)| (*k, v))
    }

    pub(crate) fn get_item(&self, item_name: FQName) -> Option<&Item> {
        if item_name.len() == 1 {
            return self.items.get(&item_name.last());
        }

        let (first, rest) = item_name.split_first();

        // TODO check visibility (but for that we need an argument to tell us whether we should,
        // and what the accessing module is)
        let Item {
            kind: ItemKind::Module(module),
            visibility: _,
        } = self.items.get(&first).unwrap()
        else {
            todo!();
        };

        module.get_item(rest)
    }
}
