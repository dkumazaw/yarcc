// Test struct

struct tagged {
    int a;
    char b;
};

int test_tagged() {
    struct tagged c;
    c.a = 4;
    c.b = 2;
    return 1;
}

int main() {
    return 0;
}
