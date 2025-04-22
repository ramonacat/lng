mod ast;
mod compile;
mod parse;
mod runtime;
mod type_check;
mod types;

use compile::compile;
use parse::parse_file;
use type_check::{type_check, Program};

fn main() {
    let stdlib = include_str!("../stdlib/std.lng");

    let program = "
        import std::println;

        struct Greeter {}

        impl Greeter {
            fn greet(self: Greeter, whom: string): void {
                println(\"hello\");
                println(whom);
            }
        }

        fn my_println(arg: string): void {
            println(arg);
        }

        fn main(args:string[]): void {
            let greeter:Greeter = Greeter {};
            greeter.greet(\"henlo\");

            my_println(\"hello world\");
            my_println(\"hello world\");
        }
    ";

    let program_ast = parse_file("main", program).unwrap();
    let stdlib_ast = parse_file("std", stdlib).unwrap();

    let program = Program(vec![program_ast, stdlib_ast]);
    let type_check_result = type_check(&program).unwrap();

    compile(&type_check_result).unwrap();
}
