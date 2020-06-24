// env
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
    inits: ("tests/ctests/init.c", 0),
    literals: ("tests/ctests/literal.c", 1),
}

test_fail! {
    no_prototype: ("int main() { return hoge(); }"),
}
