use assert_cmd::prelude::*;
use std::process::Command;

macro_rules! tests {
    ($($name:ident: ($input:tt, $expected:tt),)*) => {
        $(
            #[test]
            fn $name() {
                let mut rcc = Command::cargo_bin("rcc").unwrap();
                rcc.arg($input)
                    .assert()
                    .success();

                let mut obj = Command::new("cc")
                                      .args(&["-o", "tmp", "tmp.s"])
                                      .output()
                                      .unwrap();

                let mut tmp = Command::new("./tmp")
                                       .status()
                                       .unwrap();
                println!("{}", tmp);
            }
        )*
    }
}

tests! {
    hoge: ("1", "1"),
}
