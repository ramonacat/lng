use std::collections::HashMap;

mod common;

#[test]
pub fn method_call() {
    let main = "
        import string, println from std;

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

#[test]
pub fn field() {
    let main = "
        import string, println from std;

        struct Greeter {
            greeting: string,
        }

        impl Greeter {
            fn greet(self: Greeter, whom: string): () {
                println(self.greeting);
                println(whom);
            }
        }

        export fn main(): () {
            let greeter:Greeter = Greeter {greeting: \"hi\"};
            greeter.greet(\"mate\");
        }
    ";

    let mut program = HashMap::new();
    program.insert("main", main);
    let result = common::run(program);

    assert_eq!("hi\nmate\n", result);
}
