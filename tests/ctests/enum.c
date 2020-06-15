
enum simple { HOGE, FUGA };

int test_simple() {
    enum simple s;
    s = FUGA;
    return s; // Expect: 1
}

int test_local() {
    enum local { FOO, BAR, BAZ };
    enum local a = BAZ;
    return a; // Expect: 2
}

int test_various_decls() {
    enum local1;
    enum local1 { foo, bar, baz };
    enum local1 l1;
    enum local2 { a, b, c } l2_1, l2_2;
    enum { d, e, f } l_no_tag;

    l1 = bar;
    l2_1 = b;
    l2_2 = c;
    l_no_tag = f;

    return l1 + l2_1 + l2_2 + l_no_tag; // Expect: 6
}

int test_same_name() {
    enum simple { aaa, bbb, HOGE, FOO };
    enum simple s;
    s = HOGE;
    return s; // Expect: 2 (Not 0!)
}

int test_assign() {
    enum e { a, b, c = 20, d };
    return d + b; // Expect: 22
}

int main() {
    if (test_simple() != 1) return 1; 
    if (test_local() != 2) return 2;
    if (test_various_decls() != 6) return 3;
    if (test_same_name() != 2) return 4;
    if (test_assign() != 22) return 5;

    // Successful
    return 0;
}
