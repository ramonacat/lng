#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
// TODO get rid of unneccessary panics, and then document and remove the allow
#![allow(clippy::missing_errors_doc, clippy::missing_panics_doc)]

mod ast;
mod errors;
mod name_mangler;
mod types;

pub mod compile;
pub mod parser;
pub mod std;
pub mod type_check;
