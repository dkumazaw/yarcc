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

int main() {
    int ret = 0;
    if (test_simple() != 6) ret |= 1;
    if (test_local() != 45) ret |= (1 << 1);
    if (test_various_decls() != 32) ret |= (1 << 2);

    // Successful if 0
    return ret;
}
