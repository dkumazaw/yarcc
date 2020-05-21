
// Test functions to be executed from generated binaries
#include <stdio.h>

int foo() {
    printf("OK\n");
    return 1;
}

int foo1(int a) {
    printf("%d OK\n", a);
    return 1;
}

int foo6(int a, int b, int c, int d, int e, int f) {
    printf("%d %d %d %d %d %d OK\n", a, b, c, d, e, f);
    return 1;
}
