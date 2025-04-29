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

        Self::parse(&format!("{raw}.{new_part}"))
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
pub struct StructDescriptorType {
    pub name: FQName,
    // the fields are a Vec<_>, so that the order is well defined
    pub fields: Vec<StructField>,
}

impl StructDescriptorType {
    pub fn object_type(&self) -> Type {
        // TODO can we avoid special-casing the types here? perhaps take the object type as an
        // argument?
        if self.name == *TYPE_NAME_U64 {
            return Type::U64;
        }

        Type::Object(self.name)
    }
}

// TODO support generics
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Object(FQName),
    Array(Box<Type>),
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
    // TODO do we want this exposed to userland in an unsafe mode?
    Pointer(Box<Type>),
}
impl Type {
    pub(crate) fn debug_name(&self) -> String {
        match &self {
            Self::Unit => "void".to_string(),
            Self::Object(item_path) => format!("{item_path}"),
            Self::StructDescriptor(StructDescriptorType {
                name: item_path,
                fields: _,
            }) => format!("Struct<{item_path}>"),
            Self::Array(inner) => format!("{}[]", inner.debug_name()),
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
            Self::Object(item_path) => *item_path,
            Self::Array(_) => todo!(),
            Self::StructDescriptor(_) => todo!(),
            Self::Callable { .. } => todo!(),
            Self::U64 => *TYPE_NAME_U64,
            Self::U8 => todo!(),
            Self::Pointer(_) => todo!(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "void"),
            Self::Object(identifier) => write!(f, "{identifier}"),
            Self::Array(inner) => write!(f, "{inner}[]"),
            Self::StructDescriptor(StructDescriptorType { name, fields: _ }) => {
                write!(f, "StructDescriptor<{name}>")
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

// TODO is AssociatedFunction really a separate entity from Function?
#[derive(Debug, Clone)]
pub struct AssociatedFunction {
    pub struct_: FQName,
    pub name: Identifier,
    pub definition: FunctionDefinition,
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

    fn get_item_mut(&mut self, imported_item: FQName) -> Option<&mut Item> {
        if imported_item.len() == 1 {
            return self.items.get_mut(&imported_item.last());
        }

        let (first, rest) = imported_item.split_first();

        // TODO check visibility?
        let Item {
            kind: ItemKind::Module(module),
            visibility: _,
        } = self.items.get_mut(&first).unwrap()
        else {
            todo!();
        };

        module.get_item_mut(rest)
    }

    pub(crate) fn declare(&mut self, name: FQName, item: Item) -> &mut Item {
        let found_module = if name.len() > 1 {
            let Some(Item {
                kind: ItemKind::Module(module),
                visibility: _,
            }) = self.get_item_mut(name.without_last())
            else {
                todo!();
            };

            module
        } else {
            self
        };

        found_module
            .items
            .entry(name.last())
            .or_insert_with(|| item)
    }

    // TODO -> into_all (consuming self)
    pub(crate) fn all(&self, root_path: Option<FQName>) -> HashMap<FQName, Item> {
        let mut result = HashMap::new();

        for (item_name, item) in &self.items {
            let item_path = root_path.map_or_else(
                || FQName::parse(&item_name.raw()),
                |x| x.with_part(*item_name),
            );

            match &item.kind {
                ItemKind::Function(_) | ItemKind::Struct(_) | ItemKind::Import(_) => {
                    result.insert(item_path, item.clone());
                }
                ItemKind::Module(module) => {
                    result.insert(item_path, item.clone());

                    for (item_path, item) in module.all(Some(item_path)) {
                        result.insert(item_path, item);
                    }
                }
            }
        }

        result
    }

    pub(crate) fn get_item(&self, imported_item: FQName) -> Option<&Item> {
        if imported_item.len() == 1 {
            return self.items.get(&imported_item.last());
        }

        let (first, rest) = imported_item.split_first();

        // TODO check visibility?
        // TODO perhaps don't create modules on the fly and actually let the user figure it out, by
        // declaring modules in the code?
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
