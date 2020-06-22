// types
#[macro_use]
mod macros;

test_succeed! {
    char0: ("int main() { char a; a = 1; return a; }", 1),
    char1: ("int y; int main() { char a[10]; a[0] = -4; a[5] = 19; y = a[0] + a[5]; return y; }", 15),
    char2: ("char a[10]; int main() { a[3]= 4; return a[3]; }", 4),
    short0: ("int main() { short a; a = 2; return a; }", 2),
    pointer0: ("int main() { int x; int *y; y = &x; *y = 3; return x; }", 3),
    pointer1: ("int foo(int *aaa) { return *aaa; } int main() {int b; b = 120; return foo(&b); }", 120),
    pointer2: ("int main() { int x; int *xx; int **xxx; xx = &x; xxx = &xx; **xxx = 103; return x; } ", 103),
    pointer3: ("int main() { int x; int *y; y = &x; *y = 5; return *(&x); }", 5),
    pointer4: ("int foo(int **x) { **x = 32; } int main() {int a; int *b; b = &a; foo(&b); return a; } ", 32),
    array: ("tests/ctests/array.c", 0),
    cstruct: ("tests/ctests/struct.c", 0),
    cenum: ("tests/ctests/enum.c", 0),
    typequal: ("tests/ctests/type_qual.c", 0),
    func: ("tests/ctests/func.c", 0),
}

test_fail! {
    const_assign: ("int main() { const int i = 0; i = 2; }"),
    ptr_const_assing: ("int main() { int a, b, * const i=&a; i = &b; }"),
}
