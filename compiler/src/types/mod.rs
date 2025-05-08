pub mod functions;
pub mod structs;

use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    sync::{LazyLock, RwLock},
};

use functions::{Function, FunctionId};
use itertools::Itertools;
use string_interner::{StringInterner, backend::StringBackend, symbol::SymbolU32};
use structs::{InstantiatedStructId, Struct, StructId};

use crate::{
    ast,
    name_mangler::{MangledIdentifier, mangle_fq_name},
    std::TYPE_NAME_U64,
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
pub enum TypeKind {
    Generic(TypeArgument),
    Unit,
    Object { type_name: InstantiatedStructId },
    Array { element_type: Box<Type> },
    // TODO this should be an object with special properties
    Callable(FunctionId),
    // TODO add u128,u32,u16,u8 and signed counterparts
    // TODO add bool
    // TODO add float
    U64,
    U8,
    Pointer(Box<Type>),
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
            TypeKind::Array {
                element_type: inner,
            } => format!("{}[]", inner.debug_name()),
            TypeKind::Callable(function_id) => format!("Callable({function_id})",),
            TypeKind::U64 => "u64".to_string(),
            TypeKind::U8 => "u8".to_string(),
            TypeKind::Pointer(inner) => format!("*{}", inner.debug_name()),
            TypeKind::Generic(type_argument) => format!("Generic({type_argument})"),
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

    pub(crate) fn u64() -> Self {
        Self::new_not_generic(TypeKind::U64)
    }

    pub(crate) fn unit() -> Self {
        Self::new_not_generic(TypeKind::Unit)
    }

    pub(crate) const fn kind(&self) -> &TypeKind {
        &self.kind
    }

    pub(crate) fn instantiate(&self, type_argument_values: &TypeArgumentValues) -> Self {
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
                element_type: Box::new(element_type.instantiate(type_argument_values)),
            },
            TypeKind::Callable(function_id) => {
                // TODO we should actually go to the function table and instantiate it there
                TypeKind::Callable(*function_id)
            }
            TypeKind::U64 => TypeKind::U64,
            TypeKind::U8 => TypeKind::U8,
            TypeKind::Pointer(target) => {
                TypeKind::Pointer(Box::new(target.instantiate(type_argument_values)))
            }
        };

        Self {
            kind,
            arguments: TypeArguments::new_empty(),
            argument_values: TypeArgumentValues::new_empty(),
        }
    }

    pub(crate) fn struct_name(&self) -> InstantiatedStructId {
        match &self.kind {
            TypeKind::Generic(_) => todo!(),
            TypeKind::Unit => todo!(),
            TypeKind::Object { type_name } => type_name.clone(),
            TypeKind::Array { .. } => todo!(),
            TypeKind::Callable { .. } => todo!(),
            TypeKind::U64 => InstantiatedStructId(
                StructId::FQName(*TYPE_NAME_U64),
                TypeArgumentValues::new_empty(),
            ),
            TypeKind::U8 => todo!(),
            TypeKind::Pointer(_) => todo!(),
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
            TypeKind::Callable(function_id) => write!(f, "Callable({function_id})"),
            TypeKind::U8 => write!(f, "u8"),
            TypeKind::U64 => write!(f, "u64"),
            TypeKind::Pointer(to) => write!(f, "*{to}"),
            TypeKind::Generic(name) => write!(f, "{name}"),
        }
    }
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
    StructConstructor(InstantiatedStructId),
    FieldAccess {
        target: Box<Expression>,
        field: Identifier,
    },
    SelfAccess,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub position: ast::SourceRange,
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

#[derive(Debug, Clone)]
pub enum ItemKind {
    Function(FunctionId),
    Struct(StructId),
    Import(Import),
    Module(Module),
    // TODO is this needed at all?
    StructImport(StructId),
}

#[derive(Clone)]
pub struct Item {
    pub kind: ItemKind,
    pub visibility: Visibility,
}

impl std::fmt::Debug for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ItemKind::Function(function) => write!(f, "Function({function})"),
            ItemKind::Struct(struct_) => write!(f, "Struct({struct_})"),
            ItemKind::Import(import) => write!(f, "Import({})", import.imported_item),
            ItemKind::Module(module) => write!(f, "{module:?}"),
            ItemKind::StructImport(struct_id) => write!(f, "StructImport({struct_id})"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Export,
    Internal,
}

pub struct AppModule {
    main: FunctionId,
    module: Module,
    structs: HashMap<StructId, Struct>,
    functions: HashMap<FunctionId, Function>,
}
pub struct LibraryModule {
    module: Module,
    structs: HashMap<StructId, Struct>,
    functions: HashMap<FunctionId, Function>,
}

pub enum RootModule {
    App(AppModule),
    Library(LibraryModule),
}

impl RootModule {
    pub(crate) const fn main(&self) -> Option<FunctionId> {
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

    pub(crate) const fn structs(&self) -> &HashMap<StructId, Struct> {
        match self {
            Self::App(app_module) => &app_module.structs,
            Self::Library(library_module) => &library_module.structs,
        }
    }

    pub(crate) const fn functions(&self) -> &HashMap<FunctionId, Function> {
        match self {
            Self::App(app_module) => &app_module.functions,
            Self::Library(library_module) => &library_module.functions,
        }
    }

    pub(crate) const fn new_app(
        main: FunctionId,
        root_module: Module,
        structs: HashMap<StructId, Struct>,
        functions: HashMap<FunctionId, Function>,
    ) -> Self {
        Self::App(AppModule {
            main,
            module: root_module,
            structs,
            functions,
        })
    }

    pub(crate) const fn new_library(
        root_module: Module,
        structs: HashMap<StructId, Struct>,
        functions: HashMap<FunctionId, Function>,
    ) -> Self {
        Self::Library(LibraryModule {
            module: root_module,
            structs,
            functions,
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
