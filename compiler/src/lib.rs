#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
// TODO get rid of unneccessary panics, and then document and remove the allow
#![allow(clippy::missing_errors_doc, clippy::missing_panics_doc)]

mod ast;
pub mod compile;
mod name_mangler;
pub mod parse;
pub mod runtime;
pub mod std;
pub mod type_check;
mod types;
