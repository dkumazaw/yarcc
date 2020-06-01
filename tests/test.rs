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
                                 .args(&["-no-pie", "-o", "tmp", "tmp.s"])
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

// Test cases from external files
tests! {
    file_check: ("examples/test.c", 1),
    singlenum: ("examples/singlenum.c", 94),
}

// Unit tests:
tests! {
    simple0: ("int main() {  0;} ", 0),
    simple1: ("int main() { 42;} ", 42),
    simple2: ("int main() {  12 + 34 - 5 ;} ", 41),
    simple3: ("int main() { 5+6*7;} ", 47),
    simple4: ("int main() { 5*(9-6);} ", 15),
    simple5: ("int main() { (3+5)/2;} ", 4),
    comment0: ("// Hey I am just a line comment! \n int main() { return 3; }", 3),
    comment1: ("examples/comment.c", 4),
    unary0: ("int main() { -3*-5;} ", 15),
    unary1: ("int main() { 8*(-3)+30;} ", 6),
    incr0: ("int main() { int a = 5; a++; return a; }", 6),
    incr1: ("int main() { int a = 0; int i = 0; for (i=0; i < 10; i++) { a += i; } return a; }", 45),
    incr2: ("int main() { int a = 63; return a++; }", 64),
    decr0: ("int main() { int a = 5; a--; return a; }", 4),
    decr1: ("int main() { int a = 0; int i = 0; for (i=9; i > 0; i--) { a += i; } return a; }", 45),
    decr2: ("int main() { int a = 63; return a--; }", 62),
    relational0: ("int main() { 1 == 1;} ", 1),
    relational1: ("int main() { 1 != 1;} ", 0),
    relational2: ("int main() { 2 < 0;} ", 0),
    relational3: ("int main() { 3 > -1;} ", 1),
    relational4: ("int main() { 5 >= -5;} ", 1),
    relational5: ("int main() { 5 <= -5;} ", 0),
    assign0: ("int main() { int a; a = 0; a += 4; return a; }", 4),
    assign1: ("int main() { int b = 13; b -= 6; return b; }", 7),
    assign2: ("int main() { int *p; int a[2]; p = a; p += 1; *p = 23; return a[1]; }", 23),
    assign3: ("int main() { int a[20]; int *p; a[7] = 27; int c = 9; int d = 2; p = a; p += c; p -= d; return *p;}", 27),
    assign4: ("int main() { int f = 6; f *= 4; return f; }", 24),
    assign5: ("int main() { int g = 12; int h = 4; g *= h; return g; }", 48),
    assign6: ("int a; int main() { a = 50; a /= 5; return a; }", 10),
    assign7: ("int a[14]; int main() { int idx = 4; a[idx] = 128; a[idx] /= 32; return a[4]; }", 4),
    assign8: ("int a[23]; int main() { a[5]= 59; a[5] -= 4; return *(a + 5); }", 55),
    assing9: ("int main() { int a; int b; a = b = 5; return a + b; }", 10),
    assing10: ("int main() { int a = 0; int b = 3; a = b += 4; return a; }", 7),
    assing11: ("int main() { int a = 0; int b = 5; a = b *= 3; return a;  }", 15),
    assing12: ("int main() { int a = 0; int b = 9; int c; c =  a = b /= 3; return c; }", 3),
    bitwise0: ("int main() { return 0 & 1; }", 0),
    bitwise1: ("int main() { return 2 & 3 > 0; }", 0),
    bitwise2: ("int main() { return (2 & 3) > 0; }", 1),
    bitwise3: ("int main() { int a = 7; int b = 3; return a & b; }", 3),
    bitwise4: ("int main() { return 1 ^ 3; }", 2),
    bitwise5: ("int main() { return (3 ^ 1) & 5; }", 0),
    bitwise6: ("int main() { return 3 ^ 1 & 5; }", 2),
    bitwise7: ("int main() { return 4 | 3; }", 7),
    bitwise8: ("int main() { return 8 | 4 | 3; }", 15),
    bitwise9: ("int main() { return 7 | 4 & 3; }", 7),
    bitwise10: ("int main() { return (7 | 4) & 3; }", 3),
    bitwise11: ("int main() { char a = 240; a = ~a; return a; }", 15),
    bitwise12: ("int main() { int a = 340123; if (a & ~a) { return 4; } else {return 33;} }", 33),
    shift0: ("int main() { return 1 << 2; }", 4),
    shift1: ("int main() { return 17 >> 3;  }", 2),
    shift2: ("int main() { return 6 >> 2 << 2; }", 4),
    logical0: ("int main() { return 1 && 4; }", 1),
    logical1: ("int main() { int a = 4; int b = 13; if (a > 2 && b <= 22) { return 5; } else { return 1; } }", 5),
    logical2: ("int main() {  int a = 4; int b = 13; if (a <= 2 && b <= 22) { return 5; } else { return 1; } }", 1),
    logical3: ("int main() {  int a = 4; int b = 13; if (a > 2 && b > 22) { return 5; } else { return 1; } }", 1),
    logical4: ("int main() {  int a = 4; int b = 13; if (a < 2 && b > 22) { return 5; } else { return 1; } }", 1),
    logical5: ("int main() { return 0 || 123; }", 1),
    logical6: ("int main() { int a = 4; int b = 13; if (a > 2 || b <= 22) { return 5; } else { return 1; } }", 5),
    logical7: ("int main() {  int a = 4; int b = 13; if (a <= 2 || b <= 22) { return 5; } else { return 1; } }", 5),
    logical8: ("int main() {  int a = 4; int b = 13; if (a > 2 || b > 22) { return 5; } else { return 1; } }", 5),
    logical9: ("int main() {  int a = 4; int b = 13; if (a < 2 || b > 22) { return 5; } else { return 1; } }", 1),
    lvar0: ("int main() { int a; a = 1; a;} ", 1),
    lvar1: ("int main() { int b; b = 1 + 3 * (4 - 2); b;} ", 7),
    lvar2: ("int main() { int foo; foo = 10 + 4; foo;} ", 14),
    lvar3: ("int main() { int a; int b; a = 1; b = 2; a + b;} ", 3),
    char0: ("int main() { char a; a = 1; return a; }", 1),
    char1: ("int y; int main() { char a[10]; a[0] = -4; a[5] = 19; y = a[0] + a[5]; return y; }", 15),
    char2: ("char a[10]; int main() { a[3]= 4; return a[3]; }", 4),
    short0: ("int main() { short a; a = 2; return a; }", 2),
    return0: ("int main() { return 1;} ", 1),
    return1: ("int main() { return (3 + 1) *  10;} ", 40),
    return2: ("int main() { int a; a = 4 + 5; return a + 1;} ", 10),
    return3: ("int main() { return 5; return 1; 2;} ", 5),
    if0: ("int main() { if (1 == 1) return 1; return 2;} ", 1),
    if1: ("int main() { if (1 != 1) return 1; return 2;} ", 2),
    ifelse0: ("int main() { if (1 == 1) return 1; else return 2;} ", 1),
    ifelse1: ("int main() { if (1 != 1) return 1; else return 2; }", 2),
    ifelse2: ("int main() { int a; a = 4; if (a == 4) return 1; else return 5;} ", 1),
    ifelse3: ("int main() { int boo; boo = 19; if (boo == 1) return 10; else return 2;} ", 2),
    while0: ("int main() { int a; a = 0; while (a != 10) a = a + 1; return a;} ", 10),
    while1: ("int main() { int a; a = 0; while (a != 10) if (a == 3) return a; else a = a + 1; return a;} ", 3),
    for0: ("int main() { int a; a = 0;int i; for (i = 0; i < 10; i = i + 1) a = a + 1; return a;} ", 10),
    for1: ("int main() { int a; a = 0; for (;a < 13;) a = a + 1; return a;} ", 13),
    block0: ("int main() { int a; a = 0; while (a != 10) { a = a + 1; } return a;} ", 10),
    block1: ("int main() { int a; for (a = 0; a <= 14; a = a +1) {} return a;} ", 15),
    block2: ("int main() { int a; a = 1 * 5; if (a == 5) {a =7; return a;} else {return 10; return 11;} }", 7),
    sizeof0: ("int main() { int x; x = 10; return sizeof x; }", 4),
    sizeof1: ("int main() {int x; int *hoge; hoge = &x; return sizeof (hoge);}", 8),
    sizeof2: ("int main() { return sizeof (100); }", 4),
    sizeof3: ("int main() { int x; return sizeof sizeof x; }", 4),
    sizeof4: ("int main() { int *y; return sizeof (*y); }", 4),
    sizeof5: ("int main() { int x; return sizeof (&x); }", 8),
    sizeof6: ("int main() { int x; return sizeof (x + 1); }", 4),
    sizeof7: ("int main() { int array[10]; return sizeof array; } ", 40),
    sizeof8: ("int main() { int *arr[13]; return sizeof arr; }", 104),
    gvar0: ("int a; int main() { a = 1; return a; }", 1),
    gvar1: ("int a; int b; int c; int foo (int arg) { b = arg; return 7; }int main() { c = foo(2); a = c * b; return a; }", 14),
    gvar2: ("int *a; int main() { int b; int *c; c = &b; a = c; *c = 53; return *a; }", 53),
    gvar3: ("int a[4]; int main() { a[0] = 3; a[1] = a[0]+2; a[2] = a[1]*a[0]; return a[2]; }", 15),
    init0: ("int main() { int a = 4; return a; }", 4),
    init1: ("int main() { short b = 3; return b; }", 3),
    init2: ("int main() { char c = 23; return c;  }", 23),
    init3: ("int main() { int a=19; int *b = &a; return *b; }", 19),
    init4: ("int main() { int a = {11, 22, 33}; return a; }", 11),
    init5: ("int main() { int a[4] = {1, 2, 3, 5}; return a[3]; }", 5),
    array0: ("int main() { int a[10]; return 1; }", 1),
    array1: ("int main() { int arr[12]; arr[1] = 4; return arr[1];}", 4),
    array2: ("int main() { int arr[10]; int b; b = 3; arr[4 * b -10] = 21; return arr[4 - 2]; }", 21),
    array3: ("int main() { int a[100]; *(a + (1 - 3 + 2 * 4)) = 22; return a[6];}", 22),
    array4: ("int main() { int a[2]; int *p;  *a = 1; *(a + 1) = 2; p = a; return *p + *(p + 1);  }", 3),
    array5: ("int main() { int *a[5]; int c; c = 37; a[1] = &c; return *a[1]; }", 37),
    array6: ("int main() { int **a[6]; int b; int *c;b = 4; c = &b; a[b] = &c; return ***(a + b);}", 4),
    array7: ("int main() {int a[10]; a[5] = 23; return *(5 + a);}", 23),
    array8: ("int main() { int a[12]; int *p; *(a + 2) = 4; p = a; return * (4-2 + p);}", 4),
    pointer0: ("int main() { int x; int *y; y = &x; *y = 3; return x; }", 3),
    pointer1: ("int foo(int *aaa) { return *aaa; } int main() {int b; b = 120; return foo(&b); }", 120),
    pointer2: ("int main() { int x; int *xx; int **xxx; xx = &x; xxx = &xx; **xxx = 103; return x; } ", 103),
    pointer3: ("int main() { int x; int *y; y = &x; *y = 5; return *(&x); }", 5),
    pointer4: ("int foo(int **x) { **x = 32; } int main() {int a; int *b; b = &a; foo(&b); return a; } ", 32),
    func0: ("int main() { return foo(); }
             int foo() { return 123; }", 123),
    func1: ("int bar () { 24; }
             int baz () { 46; }
             int main () { 
                 int a;
                 a = 10; 
                 if (a < 12) { 
                     return baz(); 
                 } else {
                    return bar();
                 } 
             }", 46),
    func2: ("int fib (int a) {
                if (a <= 1) return a;
                return fib(a-1) + fib(a-2);
             }
             int main() {
                int b;
                b = 7;
                return fib(b);
             }", 13),
    func3: ("int fib (int a) {
                if (a <= 1) return a;
                return fib(a-1) + fib(a-2);
             }
             int proxy(int a, int b, int c, int d) {
                return fib(a + b + c - d);
             }
             int main() {
                int b;
                b = 9;
                return proxy(1, b, 2, 3);
             }", 34),
    func4: ("int fib (int a) {
                if (a <= 1) return a;
                return fib(a-1) + fib(a-2);
             }
             int proxy(int a, int b, int c, int d, int e, int f) {
                return fib(a + b + c - d + e * f);
             }
             int main() {
                int b;
                int hoooo;
                b = 8;
                hoooo = 100;
                return proxy(1, b, 2, 3, hoooo, 0);
             }", 21),
    func5: ("int fib (int a) {
                if (a <= 1) return a;
                return fib(a-1) + fib(a-2);
             }
             int main() {return fib(12);}", 144),
    func6: ("int fib (int a) {
                if (a <= 1) return a;
                return fib(a-1) + fib(a-2);
             }
             int main() {int a[30]; a[fib(7)] = 12; return a[13]; }", 12),
}
