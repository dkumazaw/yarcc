/// Expects that the outcome of the produced binary matches $expect
#[allow(unused_macros)]
macro_rules! test_succeed {
    ($($name:ident: ($input:tt, $expected:tt),)*) => {
        $(
            #[test]
            fn $name() {
                use assert_cmd::prelude::*;
                use std::process::Command;

                let _rcc = Command::cargo_bin("rcc")
                                .unwrap()
                                .arg($input)
                                .assert()
                                .success();

                let obj = Command::new("cc")
                                 .args(&["-no-pie", "-o", "tmp", "tmp.s"])
                                 .output();
                match obj {
                    Ok(_) => (),
                    Err(_) => panic!("Failed to assemble/link the generated file.")
                }

                let status = Command::new("./tmp")
                                       .status()
                                       .unwrap();
                // println!("{}", status.code().unwrap());
                assert_eq!($expected, status.code().unwrap());
            }
        )*
    }
}

/// Expects that the compilation fails
#[allow(unused_macros)]
macro_rules! test_fail {
    ($($name:ident: ($input:tt),)*) => {
        $(
            #[test]
            fn $name() {
                use assert_cmd::prelude::*;
                use std::process::Command;

                let _rcc = Command::cargo_bin("rcc")
                                .unwrap()
                                .arg($input)
                                .assert()
                                .failure();
            }
        )*
    }
}
