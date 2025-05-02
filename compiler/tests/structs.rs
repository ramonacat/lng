use std::collections::HashMap;

mod common;

#[test]
pub fn method_call() {
    let main = "
        import std::string;
        import std::println;

        struct Greeter {}

        impl Greeter {
            fn greet(self: Greeter, whom: string): () {
                println(whom);
            }
        }

        export fn main(): () {
            let greeter:Greeter = Greeter {};
            greeter.greet(\"hello\");
        }
    ";

    let mut program = HashMap::new();
    program.insert("main", main);
    let result = common::run(program);

    assert_eq!("hello\n", result);
}
