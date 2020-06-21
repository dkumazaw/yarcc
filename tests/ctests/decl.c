
/* Useless declarations are permitted. */
int;
long;

int test_useless() {
    short;
    return 1;
}

int foo(int a) {
    return 352 + a;
}

int (*bar())(int a) {
    return &foo;
}

int test_ptr_to_func() {
    return bar()(12);
}

int main() {
    if (test_useless() != 1) return 1;
    if (test_ptr_to_func() != 364) return 2;

    // Successful 
    return 0;
}
