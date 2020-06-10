// Tests related to types, locals, globals, consts, literals
use assert_cmd::prelude::*;
use std::process::Command;

#[macro_use]
mod macros;

test_succeed! {
    lvar0: ("int main() { int a; a = 1; a;} ", 1),
    lvar1: ("int main() { int b; b = 1 + 3 * (4 - 2); b;} ", 7),
    lvar2: ("int main() { int foo; foo = 10 + 4; foo;} ", 14),
    lvar3: ("int main() { int a; int b; a = 1; b = 2; a + b;} ", 3),
    gvar0: ("int a; int main() { a = 1; return a; }", 1),
    gvar1: ("int a; int b; int c; int foo (int arg) { b = arg; return 7; }int main() { c = foo(2); a = c * b; return a; }", 14),
    gvar2: ("int *a; int main() { int b; int *c; c = &b; a = c; *c = 53; return *a; }", 53),
    gvar3: ("int a[4]; int main() { a[0] = 3; a[1] = a[0]+2; a[2] = a[1]*a[0]; return a[2]; }", 15),
    char0: ("int main() { char a; a = 1; return a; }", 1),
    char1: ("int y; int main() { char a[10]; a[0] = -4; a[5] = 19; y = a[0] + a[5]; return y; }", 15),
    char2: ("char a[10]; int main() { a[3]= 4; return a[3]; }", 4),
    short0: ("int main() { short a; a = 2; return a; }", 2),
    init0: ("int main() { int a = 4; return a; }", 4),
    init1: ("int main() { short b = 3; return b; }", 3),
    init2: ("int main() { char c = 23; return c;  }", 23),
    init3: ("int main() { int a=19; int *b = &a; return *b; }", 19),
    init4: ("int main() { int a = {11, 22, 33}; return a; }", 11),
    init5: ("int main() { int a[4] = {1, 2, 3, 5}; return a[3]; }", 5),
    init6: ("int main() { int a, b=4, c=5; return b *c; }", 20),
    init7: ("int main() { int a[5] = {1,2,3,4,5}, *b, c=4; return a[3]; }", 4),
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
    literal: ("tests/ctests/literal.c", 1),
}
