mod ast;
mod compile;
mod parse;
mod stdlib;
mod type_check;

use compile::compile;
use parse::parse_file;
use type_check::{type_check, Program};

fn main() {
    let stdlib = include_str!("../stdlib/std.lng");

    let program = "
        import std::println;

        fn main(args:string[]): void {
            println(\"hello world\");
        }
    ";

    let program_ast = parse_file("main", program);
    let stdlib_ast = parse_file("std", stdlib);

    let program = Program(vec![program_ast, stdlib_ast]);
    let type_check_result = type_check(&program);

    println!("{type_check_result:?}");

    compile(&program);
}
