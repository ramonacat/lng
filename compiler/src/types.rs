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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeArguments(Vec<TypeArgument>);
impl TypeArguments {
    pub(crate) const fn new_empty() -> Self {
        Self(vec![])
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDescriptorType {
    pub name: FQName,
    // the fields are a Vec<_>, so that the order is well defined
    pub fields: Vec<StructField>,
    pub type_arguments: TypeArguments,
}

impl StructDescriptorType {
    pub fn instance_type(&self, type_argument_values: TypeArgumentValues) -> Type {
        // TODO verify that the type_argument_values actually match the type_arguments declared
        // TODO can we avoid special-casing the types here? perhaps take the object type as an
        // argument?
        if self.name == *TYPE_NAME_U64 {
            return Type::U64;
        }

        if self.name == *TYPE_NAME_UNIT {
            return Type::Unit;
        }

        Type::Object {
            type_name: self.name,
            type_argument_values,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeArgument(Identifier);

impl std::fmt::Display for TypeArgument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeArgumentValues(HashMap<TypeArgument, Type>);
impl TypeArgumentValues {
    pub(crate) fn new_empty() -> Self {
        Self(HashMap::new())
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

// TODO support generics
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Object {
        type_name: FQName,
        type_argument_values: TypeArgumentValues,
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
}
impl Type {
    pub(crate) fn debug_name(&self) -> String {
        match &self {
            Self::Unit => "void".to_string(),
            Self::Object {
                type_name: item_path,
                type_argument_values,
            } => format!("{item_path}{type_argument_values}"),
            Self::StructDescriptor(StructDescriptorType {
                name: item_path,
                fields: _,
                type_arguments,
            }) => format!("Struct{type_arguments}({item_path})"),
            Self::Array {
                element_type: inner,
            } => format!("{}[]", inner.debug_name()),
            Self::Callable {
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
            Self::U64 => "u64".to_string(),
            Self::U8 => "u8".to_string(),
            Self::Pointer(inner) => format!("*{}", inner.debug_name()),
        }
    }

    // TODO all types should have a struct descriptor defined for them in std
    pub(crate) fn name(&self) -> FQName {
        match &self {
            Self::Unit => todo!(),
            Self::Object {
                type_name: item_path,
                type_argument_values: _,
            } => *item_path,
            Self::Array { .. } => todo!(),
            Self::StructDescriptor(_) => todo!(),
            Self::Callable { .. } => todo!(),
            Self::U64 => *TYPE_NAME_U64,
            Self::U8 => todo!(),
            Self::Pointer(_) => todo!(),
        }
    }

    pub(crate) fn instance_type(&self, type_argument_values: TypeArgumentValues) -> Self {
        match &self {
            Self::Unit => todo!(),
            Self::Object { .. } => todo!(),
            // TODO arrays cannot be instantiated, this case is wrong!
            Self::Array {
                element_type: inner,
            } => Self::Array {
                element_type: Box::new(inner.instance_type(type_argument_values)),
            },
            Self::StructDescriptor(struct_descriptor_type) => {
                struct_descriptor_type.instance_type(type_argument_values)
            }
            Self::Callable { .. } => todo!(),
            Self::U64 => todo!(),
            Self::U8 => todo!(),
            Self::Pointer(_) => todo!(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "void"),
            Self::Object {
                type_name: identifier,
                type_argument_values,
            } => write!(f, "{identifier}{type_argument_values}"),
            Self::Array {
                element_type: inner,
            } => write!(f, "{inner}[]"),
            Self::StructDescriptor(StructDescriptorType {
                name,
                fields: _,
                type_arguments,
            }) => {
                write!(f, "StructDescriptor{type_arguments}<{name}>")
            }
            Self::Callable {
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
            Self::U8 => write!(f, "u8"),
            Self::U64 => write!(f, "u64"),
            Self::Pointer(to) => write!(f, "*{to}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: Identifier,
    pub type_: Type,
    pub position: SourceRange,
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
        Type::Callable {
            arguments: self.definition.arguments.clone(),
            return_type: Box::new(self.definition.return_type.clone()),
        }
    }

    pub(crate) fn mangled_name(&self) -> MangledIdentifier {
        self.struct_.with_part(self.name).into_mangled()
    }
}

impl Function {
    pub(crate) fn type_(&self) -> Type {
        Type::Callable {
            arguments: self.definition.arguments.clone(),
            return_type: Box::new(self.definition.return_type.clone()),
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
    pub position: ast::SourceRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub struct_name: FQName,
    pub name: Identifier,
    pub type_: Type,
    pub static_: bool,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: FQName,
    pub fields: Vec<StructField>,
    pub impls: HashMap<Identifier, AssociatedFunction>,
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
