use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    str,
};

use itertools::Itertools as _;

use crate::{
    ast::{self, SourceRange},
    name_mangler::{MangledIdentifier, mangle_field, mangle_item, mangle_module, nomangle_item},
};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Identifier(String);

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
// TODO support interning
pub struct ItemPath {
    pub module: ModulePath,
    pub item: Identifier,
}

impl ItemPath {
    pub fn into_mangled(self) -> MangledIdentifier {
        mangle_item(self)
    }

    pub(crate) const fn new(module: ModulePath, item: Identifier) -> Self {
        Self { module, item }
    }
}

impl Display for ItemPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.module, self.item)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FieldPath {
    pub struct_: ItemPath,
    pub field: Identifier, // TODO support nesting?
}

impl FieldPath {
    pub fn into_mangled(self) -> MangledIdentifier {
        mangle_field(self)
    }

    pub(crate) const fn new(struct_: ItemPath, field: Identifier) -> Self {
        Self { struct_, field }
    }
}

impl Display for FieldPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.struct_, self.field)
    }
}

impl Identifier {
    pub fn parse(raw: &str) -> Self {
        Self(raw.to_string())
    }

    pub(crate) fn raw(&self) -> &str {
        self.0.as_str()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = &self.0;

        write!(f, "{name}")
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ModulePath(Vec<Identifier>);

impl ModulePath {
    pub fn parse(raw: &str) -> Self {
        Self(raw.split('.').map(Identifier::parse).collect())
    }

    pub fn from_parts<'a>(path: impl Iterator<Item = &'a str>) -> Self {
        Self(path.map(Identifier::parse).collect())
    }

    pub fn parts(&self) -> &[Identifier] {
        self.0.as_slice()
    }

    pub(crate) fn into_mangled(self) -> MangledIdentifier {
        mangle_module(self)
    }
}

impl Display for ModulePath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = self.0.iter().map(Identifier::raw).join(".");

        write!(f, "{name}")
    }
}

// TODO support generics
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Object(ItemPath),
    Array(Box<Type>),
    // TODO this should probably be simply an object, just of StructDescriptor<TargetStruct> type
    StructDescriptor(ItemPath, Vec<StructField>),
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
            Self::StructDescriptor(item_path, _) => format!("Struct<{item_path}>"),
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

    pub(crate) fn name(&self) -> ItemPath {
        match &self {
            Self::Unit => todo!(),
            Self::Object(item_path) => item_path.clone(),
            Self::Array(_) => todo!(),
            Self::StructDescriptor(_, _) => todo!(),
            Self::Callable { .. } => todo!(),
            Self::U64 => ItemPath::new(ModulePath::parse("std"), Identifier::parse("u64")),
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
            Self::StructDescriptor(name, _) => write!(f, "StructDescriptor<{name}>"),
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
    pub name: ItemPath,
    pub definition: FunctionDefinition,
}

#[derive(Debug, Clone)]
pub struct AssociatedFunction {
    pub name: FieldPath,
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
        self.name.clone().into_mangled()
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
            FunctionBody::Extern(foreign_name) => nomangle_item(foreign_name.clone()),
            FunctionBody::Statements(_) => self.name.clone().into_mangled(),
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
    FunctionCall {
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
    pub imported_item: ItemPath,
    pub position: ast::SourceRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub name: FieldPath,
    pub type_: Type,
    pub static_: bool,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: ItemPath,
    pub fields: Vec<StructField>,
    pub impls: HashMap<FieldPath, AssociatedFunction>,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Function(Function),
    Struct(Struct),
    Import(Import),
}

#[derive(Debug, Clone)]
pub struct Item {
    pub kind: ItemKind,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Export,
    Internal,
}

#[derive(Debug)]
pub struct Module {
    pub items: HashMap<ItemPath, Item>,
}

#[derive(Debug)]
// this should have a different name because this can be a program OR a library
pub struct Program {
    pub modules: HashMap<ModulePath, Module>,
}
