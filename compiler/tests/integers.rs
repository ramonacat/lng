use std::collections::HashMap;

mod common;

#[test]
pub fn integer_to_string() {
    let main = "
        import std::println;

        export fn main(): () {
            println(5.to_string());
        }
    ";

    let mut program = HashMap::new();
    program.insert("main", main);
    let result = common::run(program);

    assert_eq!("5\n", result);
}
