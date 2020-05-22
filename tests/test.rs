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
    simple0: ("main() {  0;} ", 0),
    simple1: ("main() { 42;} ", 42),
    simple2: ("main() {  12 + 34 - 5 ;} ", 41),
    simple3: ("main() { 5+6*7;} ", 47),
    simple4: ("main() { 5*(9-6);} ", 15),
    simple5: ("main() { (3+5)/2;} ", 4),
    unary0: ("main() { -3*-5;} ", 15),
    unary1: ("main() { 8*(-3)+30;} ", 6),
    relational0: ("main() { 1 == 1;} ", 1),
    relational1: ("main() { 1 != 1;} ", 0), 
    relational2: ("main() { 2 < 0;} ", 0),
    relational3: ("main() { 3 > -1;} ", 1),
    relational4: ("main() { 5 >= -5;} ", 1),
    relational5: ("main() { 5 <= -5;} ", 0),
    lvar0: ("main() { a = 1; a;} ", 1),
    lvar1: ("main() { b = 1 + 3 * (4 - 2); b;} ", 7),
    lvar2: ("main() { foo = 10 + 4; foo;} ", 14),
    lvar3: ("main() { a = 1; b = 2; a + b;} ", 3),
    return0: ("main() { return 1;} ", 1),
    return1: ("main() { return (3 + 1) *  10;} ", 40),
    return2: ("main() { a = 4 + 5; return a + 1;} ", 10),
    return3: ("main() { return 5; return 1; 2;} ", 5),
    if0: ("main() { if (1 == 1) return 1; return 2;} ", 1),
    if1: ("main() { if (1 != 1) return 1; return 2;} ", 2),
    ifelse0: ("main() { if (1 == 1) return 1; else return 2;} ", 1),
    ifelse1: ("main() { if (1 != 1) return 1; else return 2; }", 2),
    ifelse2: ("main() { a = 4; if (a == 4) return 1; else return 5;} ", 1),
    ifelse3: ("main() { boo = 19; if (boo == 1) return 10; else return 2;} ", 2),
    while0: ("main() { a = 0; while (a != 10) a = a + 1; return a;} ", 10),
    while1: ("main() { a = 0; while (a != 10) if (a == 3) return a; else a = a + 1; return a;} ", 3),
    for0: ("main() { a = 0; for (i = 0; i < 10; i = i + 1) a = a + 1; return a;} ", 10),
    for1: ("main() { a = 0; for (;a < 13;) a = a + 1; return a;} ", 13),
    block0: ("main() { a = 0; while (a != 10) { a = a + 1; } return a;} ", 10),
    block1: ("main() { for (a = 0; a <= 14; a = a +1) {} return a;} ", 15),
    block2: ("main() { a = 1 * 5; if (a == 5) {a =7; return a;} else {return 10; return 11;} }", 7),
    func0: ("main() { return foo(); } 
             foo() { return 123; }", 123),
    func1: ("bar () { 24; } 
             baz () { 46; }
             main () { 
                 a = 10; 
                 if (a < 12) { 
                     return baz(); 
                 } else {
                    return bar();
                 } 
             }", 46),
    func2: ("fib (a) {
                if (a <= 1) return a;
                return fib(a-1) + fib(a-2);
             }
             main() {
                b = 7;
                return fib(b);
             }", 13),
    func3: ("fib (a) {
                if (a <= 1) return a;
                return fib(a-1) + fib(a-2);
             }
             proxy(a, b, c, d) {
                return fib(a + b + c - d);
             }
             main() {
                b = 9;
                return proxy(1, b, 2, 3);
             }", 34),
    func4: ("fib (a) {
                if (a <= 1) return a;
                return fib(a-1) + fib(a-2);
             }
             proxy(a, b, c, d, e, f) {
                return fib(a + b + c - d + e * f);
             }
             main() {
                b = 8;
                hoooo = 100;
                return proxy(1, b, 2, 3, hoooo, 0);
             }", 21),
}
