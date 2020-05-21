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
    simple0: ("0;", 0),
    simple1: ("42;", 42),
    simple2: (" 12 + 34 - 5 ;", 41),
    simple3: ("5+6*7;", 47),
    simple4: ("5*(9-6);", 15),
    simple5: ("(3+5)/2;", 4),
    unary0: ("-3*-5;", 15),
    unary1: ("8*(-3)+30;", 6),
    relational0: ("1 == 1;", 1),
    relational1: ("1 != 1;", 0), 
    relational2: ("2 < 0;", 0),
    relational3: ("3 > -1;", 1),
    relational4: ("5 >= -5;", 1),
    relational5: ("5 <= -5;", 0),
    lvar0: ("a = 1; a;", 1),
    lvar1: ("b = 1 + 3 * (4 - 2); b;", 7),
    lvar2: ("foo = 10 + 4; foo;", 14),
    lvar3: ("a = 1; b = 2; a + b;", 3),
    return0: ("return 1;", 1),
    return1: ("return (3 + 1) *  10;", 40),
    return2: ("a = 4 + 5; return a;", 9),
    return3: ("return 5; return 1; 2;", 5),
    if0: ("if (1 == 1) return 1; return 2;", 1),
    if1: ("if (1 != 1) return 1; return 2;", 2),
    ifelse0: ("if (1 == 1) return 1; else return 2;", 1),
    ifelse1: ("if (1 != 1) return 1; else return 2; ", 2),
    ifelse2: ("a = 4; if (a == 4) return 1; else return 5;", 1),
    ifelse3: ("boo = 19; if (boo == 1) return 10; else return 2;", 2),
}
