// Type qualifiers

int test_simple() {
    const int a = 1;
    volatile int b = 2;
    const volatile int c = 3;
    return a + b + c;
}

int main() {
    if (test_simple() != 6) return 1; 

    // Successful
    return 0;
}
