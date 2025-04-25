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
        import main::printer::my_println;

        struct Greeter {}

        impl Greeter {
            fn greet(self: Greeter, whom: string): void {
                println(\"hello\");
                println(whom);
            }
        }

        fn main(args:string[]): void {
            let greeter:Greeter = Greeter {};
            greeter.greet(\"henlo\");

            my_println(\"hello world\");
            my_println(\"hello world\");
        }
    ";

    let printer = "
        import std::println;

        export fn my_println(arg: string): void {
            println(arg);
        }
    ";

    let program_ast = parse_file("main", program).unwrap();
    let greeter_ast = parse_file("main.printer", printer).unwrap();
    let stdlib_ast = parse_file("std", stdlib).unwrap();

    let program = Program(vec![program_ast, greeter_ast, stdlib_ast]);
    let type_check_result = type_check(&program).unwrap();

    compile(&type_check_result).unwrap();
}
