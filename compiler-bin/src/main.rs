#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
// TODO get rid of unneccessary panics, and then document and remove the allow
#![allow(clippy::missing_errors_doc, clippy::missing_panics_doc)]

use compiler::{
    compile::compile,
    parse::parse_file,
    std::type_check_std,
    type_check::{Program, type_check},
};

fn main() {
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

    let program = Program(vec![program_ast, greeter_ast]);
    let std_program = type_check_std().unwrap();
    let type_check_result = type_check(&program, Some(&std_program)).unwrap();

    compile(&type_check_result, &std_program, None).unwrap();
}
