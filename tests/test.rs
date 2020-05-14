use assert_cmd::prelude::*;
use std::process::Command;

macro_rules! tests {
    ($($name:ident: ($input:tt, $expected:tt),)*) => {
        $(
            #[test]
            fn $name() {
                let _rcc = Command::cargo_bin("rcc")
                                .unwrap()
                                .arg($input)
                                .assert()
                                .success();

                let _obj = Command::new("cc")
                                 .args(&["-o", "tmp", "tmp.s"])
                                 .output()
                                 .unwrap();

                let status = Command::new("./tmp")
                                       .status()
                                       .unwrap();
                // println!("{}", status.code().unwrap());
                assert_eq!($expected, status.code().unwrap());
            }
        )*
    }
}

tests! {
    simple0: ("0", 0),
    simple1: ("42", 42),
}
