use std::fmt::Display;

use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use thiserror::Error;

use crate::ast::{
    Argument, Declaration, Expression, Function, FunctionBody, Import, Literal, SourceFile,
    SourceRange, Statement, Struct, StructField, TypeDescription,
};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct LNGParser;

#[derive(Debug)]
#[allow(unused)]
pub enum InternalError<'parser> {
    UnexpectedRule(Pair<'parser, Rule>),
    MissingExpectedRule(Pair<'parser, Rule>),
    MissingRootRules,
}

impl Display for InternalError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: mayhaps a more reader friendly representation
        write!(f, "{self:?}")
    }
}

#[derive(Error, Debug)]
pub enum ParseError<'parser> {
    #[error("Internal parser error: {0}")]
    InternalError(InternalError<'parser>),
    #[error("Parsing failed: {0}")]
    ParseFailed(#[from] Box<pest::error::Error<Rule>>),
}

pub fn parse_file<'parser>(
    name: &'parser str,
    source: &'parser str,
) -> Result<SourceFile, ParseError<'parser>> {
    let Some(parsed_file) = LNGParser::parse(Rule::source_file, source)
        .map_err(Box::new)?
        .next()
    else {
        return Err(ParseError::InternalError(InternalError::MissingRootRules));
    };

    let parsed_file_inner = parsed_file.clone().into_inner();

    let mut declarations = vec![];
    let mut imports = vec![];

    for item in parsed_file_inner {
        match item.as_rule() {
            Rule::declaration => {
                let Some(inner_declaration) = item.clone().into_inner().next() else {
                    return Err(ParseError::InternalError(
                        InternalError::MissingExpectedRule(item),
                    ));
                };
                declarations.push(parse_declaration(inner_declaration)?)
            }
            Rule::import => imports.push(parse_import(item)?),
            Rule::EOI => {}
            _ => {
                return Err(ParseError::InternalError(InternalError::UnexpectedRule(
                    item,
                )))
            }
        }
    }

    Ok(SourceFile {
        declarations,
        imports,
        name: name.to_string(),
    })
}

fn find_source_position(pair: &Pair<Rule>) -> SourceRange {
    let span = pair.as_span();

    SourceRange(span.start_pos().line_col(), span.end_pos().line_col())
}

fn parse_declaration(item: Pair<Rule>) -> Result<Declaration, ParseError<'_>> {
    match item.as_rule() {
        Rule::function_declaration => Ok(Declaration::Function(parse_function(item)?)),
        Rule::struct_declaration => Ok(Declaration::Struct(parse_struct(item)?)),
        _ => Err(ParseError::InternalError(InternalError::UnexpectedRule(
            item,
        ))),
    }
}

fn parse_struct(item: Pair<Rule>) -> Result<Struct, ParseError<'_>> {
    let mut name = String::new();
    let mut fields = vec![];

    for item in item.into_inner() {
        match item.as_rule() {
            Rule::identifier => name = item.as_str().to_string(),
            Rule::struct_field => {
                let mut inner = item.clone().into_inner();

                let Some(field_name) = inner.next() else {
                    return Err(ParseError::InternalError(
                        InternalError::MissingExpectedRule(item),
                    ));
                };

                let Some(field_type) = inner.next() else {
                    return Err(ParseError::InternalError(
                        InternalError::MissingExpectedRule(item),
                    ));
                };

                fields.push(StructField {
                    name: field_name.as_str().to_string(),
                    type_: parse_type(field_type.as_str()),
                });
            }
            _ => {
                return Err(ParseError::InternalError(InternalError::UnexpectedRule(
                    item,
                )))
            }
        }
    }

    Ok(Struct { name, fields })
}

fn parse_import(item: Pair<Rule>) -> Result<Import, ParseError<'_>> {
    let mut path = vec![];
    let position = find_source_position(&item);

    for element in item.into_inner() {
        match element.as_rule() {
            Rule::identifier => path.push(element.as_str().to_string()),
            _ => {
                return Err(ParseError::InternalError(InternalError::UnexpectedRule(
                    element,
                )))
            }
        }
    }

    Ok(Import { path, position })
}

fn parse_function(function: Pair<Rule>) -> Result<Function, ParseError<'_>> {
    let mut name = String::new();
    let mut type_ = String::new();
    let mut arguments = vec![];
    let mut body = None;
    let mut export = false;
    let position = find_source_position(&function);

    for element in function.into_inner() {
        match element.as_rule() {
            Rule::identifier => {
                name = element.as_str().to_string();
            }
            Rule::type_ => {
                type_ = element.as_str().to_string();
            }
            Rule::argument => {
                let position = find_source_position(&element);
                let mut argument_inner = element.clone().into_inner();
                let Some(argument_name_pair) = argument_inner.next() else {
                    return Err(ParseError::InternalError(
                        InternalError::MissingExpectedRule(element),
                    ));
                };
                let Some(argument_type_pair) = argument_inner.next() else {
                    return Err(ParseError::InternalError(
                        InternalError::MissingExpectedRule(element),
                    ));
                };

                arguments.push(Argument {
                    name: argument_name_pair.as_str().to_string(),
                    type_: parse_type(argument_type_pair.as_str()),
                    position,
                });
            }
            Rule::function_body => {
                let position = find_source_position(&element);
                let Some(inner_expression) = element.clone().into_inner().next() else {
                    return Err(ParseError::InternalError(
                        InternalError::MissingExpectedRule(element),
                    ));
                };

                if Rule::keyword_extern == inner_expression.as_rule() {
                    body = Some(FunctionBody::Extern(find_source_position(
                        &inner_expression,
                    )));
                    continue;
                }

                let Some(body_statements) = inner_expression.clone().into_inner().next() else {
                    return Err(ParseError::InternalError(
                        InternalError::MissingExpectedRule(inner_expression),
                    ));
                };

                let statement_expressions = body_statements.into_inner();

                let mut statements = vec![];

                for expression in statement_expressions {
                    let position = find_source_position(&expression);
                    statements.push(Statement::Expression(
                        parse_expression(expression)?,
                        position,
                    ));
                }

                body = Some(FunctionBody::Statements(statements, position));
            }
            Rule::keyword_export => {
                export = true;
            }
            _ => {
                return Err(ParseError::InternalError(InternalError::UnexpectedRule(
                    element,
                )))
            }
        }
    }

    Ok(Function {
        name,
        arguments,
        return_type: parse_type(type_.as_str()),
        body: body.unwrap(),
        export,
        position,
    })
}

fn parse_expression(expression: Pair<Rule>) -> Result<Expression, ParseError<'_>> {
    let position = find_source_position(&expression);

    match expression.as_rule() {
        Rule::expression_function_call => {
            let mut expression_inner = expression.clone().into_inner();
            let mut arguments = vec![];
            let Some(function_name_pair) = expression_inner.next() else {
                return Err(ParseError::InternalError(
                    InternalError::MissingExpectedRule(expression),
                ));
            };
            for argument in expression_inner {
                let Some(argument_expression) = argument.clone().into_inner().next() else {
                    return Err(ParseError::InternalError(
                        InternalError::MissingExpectedRule(argument),
                    ));
                };

                arguments.push(parse_expression(argument_expression)?);
            }

            Ok(Expression::FunctionCall {
                name: function_name_pair.as_str().to_string(),
                arguments,
                position,
            })
        }
        Rule::expression_literal => {
            let Some(expression_inner) = expression.clone().into_inner().next() else {
                return Err(ParseError::InternalError(
                    InternalError::MissingExpectedRule(expression),
                ));
            };
            match expression_inner.as_rule() {
                Rule::expression_literal_string => {
                    let value = expression_inner.as_str();

                    Ok(Expression::Literal(
                        Literal::String((value[1..value.len() - 1]).to_string(), position),
                        position,
                    ))
                }
                _ => Err(ParseError::InternalError(InternalError::UnexpectedRule(
                    expression_inner,
                ))),
            }
        }
        Rule::expression_variable_reference => {
            let Some(expression_inner) = expression.clone().into_inner().next() else {
                return Err(ParseError::InternalError(
                    InternalError::MissingExpectedRule(expression),
                ));
            };

            Ok(Expression::VariableReference(
                expression_inner.as_str().to_string(),
                find_source_position(&expression_inner),
            ))
        }
        _ => Err(ParseError::InternalError(InternalError::UnexpectedRule(
            expression,
        ))),
    }
}

fn parse_type(mut type_: &str) -> TypeDescription {
    type_ = type_.trim();

    if let Some(stripped) = type_.strip_suffix("[]") {
        return TypeDescription::Array(Box::new(parse_type(stripped)));
    }

    TypeDescription::Named(type_.to_string())
}
