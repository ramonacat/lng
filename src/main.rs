mod ast;
mod compile;
mod stdlib;
mod type_check;

use ast::{
    Argument, Expression, Function, FunctionBody, Import, Literal, SourceFile, Statement, Type,
};
use compile::compile;
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use type_check::{type_check, Program};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct LNGParser;

fn main() {
    let stdlib = "
        export fn println(value: string): void extern;
    ";

    let program = "
        import std::println;

        fn main(args:string[]): void {
            println(\"hello world\");
        }
    ";

    let program_ast = parse_file("main", program);
    let stdlib_ast = parse_file("std", stdlib);

    let program = Program(vec![stdlib_ast, program_ast]);
    let type_check_result = type_check(&program);

    println!("{type_check_result:?}");

    compile(&program);
}

fn parse_file(name: &str, source: &str) -> SourceFile {
    let parsed_file = LNGParser::parse(Rule::source_file, source)
        .expect("failed to parse")
        .next()
        .unwrap()
        .into_inner();

    let mut functions = vec![];
    let mut imports = vec![];

    for item in parsed_file {
        match item.as_rule() {
            Rule::function_definition => functions.push(parse_function(item)),
            Rule::import => imports.push(parse_import(item)),
            Rule::EOI => {}
            _ => panic!("Unexpected rule: {item:?}"),
        }
    }

    SourceFile {
        functions,
        imports,
        name: name.to_string(),
    }
}

fn parse_import(item: Pair<Rule>) -> Import {
    let mut path = vec![];

    for element in item.into_inner() {
        match element.as_rule() {
            Rule::identifier => {
                path.push(element.as_str().to_string());
            }
            _ => panic!("Unexpected rule {element:?}"),
        }
    }

    Import { path }
}

fn parse_function(function: Pair<Rule>) -> Function {
    let mut name = String::new();
    let mut type_ = String::new();
    let mut arguments = vec![];
    let mut body = FunctionBody::Intrinsic;
    let mut export = false;

    for element in function.into_inner() {
        match element.as_rule() {
            Rule::identifier => {
                name = element.as_str().to_string();
            }
            Rule::type_ => {
                type_ = element.as_str().to_string();
            }
            Rule::argument => {
                let mut argument_inner = element.into_inner();
                let argument_name = argument_inner.next().unwrap().as_str().to_string();
                let argument_type = argument_inner.next().unwrap().as_str().to_string();

                arguments.push(Argument {
                    name: argument_name,
                    type_: parse_type(argument_type.as_str()),
                });
            }
            Rule::function_body => {
                let inner_expression = element.into_inner().next().unwrap();

                if Rule::keyword_extern == inner_expression.as_rule() {
                    body = FunctionBody::Intrinsic;
                    continue;
                }

                let statement_expressions =
                    inner_expression.into_inner().next().unwrap().into_inner();

                let mut statements = vec![];

                for expression in statement_expressions {
                    statements.push(Statement::Expression(parse_expression(expression)));
                }

                body = FunctionBody::Statements(statements);
            }
            Rule::keyword_export => {
                export = true;
            }
            _ => panic!("Unexpected rule {element:?}"),
        }
    }

    Function {
        name,
        arguments,
        return_type: parse_type(type_.as_str()),
        body,
        export,
    }
}

fn parse_expression(expression: Pair<Rule>) -> Expression {
    match expression.as_rule() {
        Rule::expression_function_call => {
            let mut expression_inner = expression.into_inner();
            let mut arguments = vec![];
            let function_name = expression_inner.next().unwrap().as_str().to_string();
            for argument in expression_inner {
                arguments.push(parse_expression(argument.into_inner().next().unwrap()));
            }

            Expression::FunctionCall {
                name: function_name,
                arguments,
            }
        }
        Rule::expression_literal => {
            let expression_inner = expression.into_inner().next().unwrap();
            match expression_inner.as_rule() {
                Rule::expression_literal_string => {
                    let value = expression_inner.as_str();

                    Expression::Literal(Literal::String((value[1..value.len() - 1]).to_string()))
                }
                _ => panic!("Unexpected rule: {expression_inner:?}"),
            }
        }
        _ => panic!("Unexpected rule: {expression:?}"),
    }
}

fn parse_type(mut type_: &str) -> ast::Type {
    type_ = type_.trim();

    if let Some(stripped) = type_.strip_suffix("[]") {
        return Type::Array(Box::new(parse_type(stripped)));
    }

    match type_ {
        "void" => ast::Type::Void,
        _ => ast::Type::Named(type_.to_string()),
    }
}
