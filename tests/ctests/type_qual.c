// Type qualifiers

int test_simple() {
    const int a = 1;
    volatile int b = 2;
    const volatile int c = 3;
    return a + b + c;
}

int test_ptr() {
    int a, *b;
    b = &a;
    int * const volatile * const c = &b;
    a = 5;
    return **c; // Expect: 5
}

int main() {
    if (test_simple() != 6) return 1; 
    if (test_ptr() != 5) return 2;

    // Successful
    return 0;
}
