use lalrpop_util::lalrpop_mod;

use crate::ast;

lalrpop_mod!(
    #[allow(
        clippy::redundant_pub_crate,
        clippy::unicode_not_nfc,
        clippy::uninlined_format_args,
        clippy::no_effect_underscore_binding,
        clippy::cast_sign_loss,
        clippy::option_if_let_else,
        clippy::use_self,
        clippy::missing_const_for_fn,
        clippy::unnested_or_patterns,
        clippy::trivially_copy_pass_by_ref,
        clippy::unnecessary_wraps,
        clippy::cloned_instead_of_copied,
        clippy::match_same_arms,
        clippy::too_many_lines
    )]
    grammar
);

#[must_use]
pub fn parse_file(name: &str, contents: &str) -> ast::SourceFile {
    let ast = grammar::SourceFileParser::new().parse(contents);

    // TODO Actual error handling
    match ast {
        Ok(mut ast) => {
            ast.name = name.to_string();

            ast
        }
        Err(e) => match e {
            lalrpop_util::ParseError::InvalidToken { location } => {
                todo!("invalid token: {}", &contents[location..])
            }
            lalrpop_util::ParseError::UnrecognizedEof { .. } => todo!(),
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                todo!(
                    "unrecognized token: {token:?}, expected {expected:?}, code: {}",
                    &contents[token.0..]
                )
            }
            lalrpop_util::ParseError::ExtraToken { .. } => todo!(),
            lalrpop_util::ParseError::User { .. } => todo!(),
        },
    }
}
