macro_rules! test_succeed {
    ($($name:ident: ($input:tt, $expected:tt),)*) => {
        $(
            #[test]
            fn $name() {
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
