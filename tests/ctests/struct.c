// Test struct

struct simple {
    int a;
    char b;
};

int test_simple() {
    struct simple s;
    s.a = 4;
    s.b = 2;
    return s.a + s.b; // Expect: 6
}

int test_local() {
    struct local { long a; char b; short c; } l; 
    l.a = 25;
    l.b = 21;
    l.c = 1;

    return l.a + l.b - l.c; // Expect: 45
}

int test_various_decls() {
    struct local1;
    struct local1 { int a; int b; long c; };
    struct local1 l1;
    struct local2 { int e; char f; long g; } l2_1, l2_2;
    struct { int h; char i; long j; } l_no_tag; // Not tagged

    l1.a = 4;
    l1.c = 6;
    l2_1.e = 10; l2_2.f = 9;
    l_no_tag.j = 3;

    return l1.a + l1.c + l2_1.e + l2_2.f + l_no_tag.j; // Expect: 32
}

int test_same_name() {
    struct simple { int aa; int bb; int cc; };
    struct simple s;
    s.aa = 23;
    s.cc = 45;

    return s.cc - s.aa; // Expect: 22
}

int test_arrow() {
    struct a { int foo; int bar; } base, *ptr;
    base.foo = 2; 
    base.bar = 3;
    ptr = &base;

    return ptr->foo * ptr->bar; // Expect: 6
}

int test_comma_sep() {
    // Comma separated struct members
    struct hello { int a, b, c; long d, e; short f, g, h; };
    struct hello hi;
    hi.a = hi.b = 5;
    hi.d = hi.e = 3;
    return hi.a + hi.e; // Expect: 8
}

int test_array() {
    struct hoge { int a[10], b, *c[2]; } l;
    l.a[3] = 4;
    l.c[1] = &l.a[3];

    return *l.c[1]; // Expect: 4
}

/* TODO:
int test_ptr_to_self() {
    struct a {
        int val;
        struct a *ptr;
    };
    struct a foo;
    struct a bar;
    bar.val = 123456;
    foo.ptr = &bar;
    
    return foo.ptr->val; // Expect: 123456
} */

int main() {
    if (test_simple() != 6) return 1;
    if (test_local() != 45) return 2;
    if (test_various_decls() != 32) return 3;
    if (test_same_name() != 22) return 4;
    if (test_arrow() != 6) return 5;
    if (test_comma_sep() != 8) return 6;
    if (test_array() != 4) return 7;
    // TODO: if (test_ptr_to_self() != 123456) return 6;

    // Successful
    return 0;
}
