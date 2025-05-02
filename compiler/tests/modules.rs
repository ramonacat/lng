use std::collections::HashMap;

mod common;

#[test]
pub fn import_function_from_nested() {
    let main = "
        import std::string;
        import main::test::my_println;

        export fn main(): () {
            my_println(\"hello\");
        }
    ";
    let main_test = "
        import std::string;
        import std::println;

        export fn my_println(text: string): () {
            println(text);
        }
    ";

    let mut program = HashMap::new();
    program.insert("main", main);
    program.insert("main.test", main_test);
    let result = common::run(program);

    assert_eq!("hello\n", result);
}

#[test]
pub fn import_aliased() {
    let main = "
        import std::string;
        import main::test::my_println as just_println;

        export fn main(): () {
            just_println(\"hello\");
        }
    ";
    let main_test = "
        import std::string;
        import std::println;

        export fn my_println(text: string): () {
            println(text);
        }
    ";

    let mut program = HashMap::new();
    program.insert("main", main);
    program.insert("main.test", main_test);
    let result = common::run(program);

    assert_eq!("hello\n", result);
}
