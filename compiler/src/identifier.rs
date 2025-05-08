use std::fmt::Display;
use std::fmt::Formatter;
use std::sync::LazyLock;
use std::sync::RwLock;
use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

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
