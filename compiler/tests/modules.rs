use std::collections::HashMap;

mod common;

#[test]
pub fn import_function_from_nested() {
    let main = "
        import string from std;
        import my_println from main.test;

        export fn main(): () {
            my_println(\"hello\");
        }
    ";
    let main_test = "
        import string, println from std;

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
        import string from std;
        import (my_println as just_println) from main.test;

        export fn main(): () {
            just_println(\"hello\");
        }
    ";
    let main_test = "
        import string, println from std;

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
