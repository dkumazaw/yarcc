use std::process::Command;

macro_rules! tests {
    ($($name:ident: ($input:tt, $expected:tt),)*) => {
        $(
            #[test]
            fn $name() {
                println!($input);
                println!($expected);
            }
        )*
    }
}

tests! {
    hoge: ("fuga", "foo"),
}
