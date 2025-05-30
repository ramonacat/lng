#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
// TODO get rid of unneccessary panics, and then document and remove the allow
#![allow(clippy::missing_errors_doc, clippy::missing_panics_doc)]

use compiler::{
    ast::SourceFileName, compile::compile, std::type_check_std, type_check::type_check,
};

fn main() {
    let program = "
        import string, println from std;
        import my_println from main.printer;

        struct Greeter {}

        impl Greeter {
            fn greet(self: Greeter, whom: string): () {
                println(\"hello\");
                println(whom);
            }
        }

        export fn main(args:string[]): () {
            let greeter:Greeter = Greeter {};
            greeter.greet(\"henlo\");

            my_println(\"hello world\");
            my_println(\"hello world\");
        }
    ";

    let printer = "
        import string, println from std;

        export fn my_println(arg: string): () {
            println(arg);
        }
    ";

    let program_ast =
        compiler::parser::parse_file(SourceFileName::new("main".to_string()), program);
    let greeter_ast =
        compiler::parser::parse_file(SourceFileName::new("main.printer".to_string()), printer);

    let program = vec![program_ast, greeter_ast];
    let std_program = type_check_std().unwrap();
    let type_check_result = type_check(&program, Some(&std_program)).unwrap();

    compile(&type_check_result, &std_program, None).unwrap();
}
