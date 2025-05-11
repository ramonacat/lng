use std::collections::HashMap;

mod common;

#[test]
pub fn interface() {
    let main = "
        import println, string from std;

        interface Blargh {
            fn blargh(name: string): string;
        }

        struct A {}

        impl A : Blargh {
            fn blargh(): string {
                return \"A\";
            }
        }

        struct B {}

        impl B : Blargh {
            fn blargh(): string {
                return \"B\";
            }
        }

        export fn main(): () {
            let a: A = A{};
            let b: B = B{};

            println(a.blargh());
            println(b.blargh());
        }
    ";

    let mut program = HashMap::new();
    program.insert("main", main);
    let result = common::run(program);

    assert_eq!("A\nB\n", result);
}

#[test]
pub fn simple_polymorphism() {
    let main = "
        import println, string from std;

        interface Blargh {
            fn blargh(): string;
        }

        struct A {}

        impl A : Blargh {
            fn blargh(): string {
                return \"A\";
            }
        }

        struct B {}

        impl B : Blargh {
            fn blargh(): string {
                return \"B\";
            }
        }

        export fn main(): () {
            let a: Blargh = A{};
            let b: Blargh = B{};

            println(a.blargh());
            println(b.blargh());
        }
    ";

    let mut program = HashMap::new();
    program.insert("main", main);
    let result = common::run(program);

    assert_eq!("A\nB\n", result);
}
