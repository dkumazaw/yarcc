// Various forms of function definitions and declarations

/* Useless declarations are permitted. */
int;
long;

/* Prototypes */
int test_useless();
int foo(int a, long b);
int (*bar())(int a, long b);
int (*(*baz(int hoge))())(int a, long b);

/* Definitions */
int test_useless() {
    short;
    return 1;
}

int foo(int a, long b) {
    return 352 + a + b;
}

int (*bar())(int a, long b) {
    return &foo;
}

int (*(*baz(int hoge))())(int a, long b) {
    return &bar;
}

int test_ptr_to_func() {
    return bar()(11, 1);
}

int test_ptr_to_ptr_to_func() {
    int input = 1;
    return baz(input)()(3, 5);
}

int main() {
    if (test_useless() != 1) return 1;
    if (test_ptr_to_func() != 364) return 2;
    if (test_ptr_to_ptr_to_func() != 360) return 3;

    // Successful 
    return 0;
}
