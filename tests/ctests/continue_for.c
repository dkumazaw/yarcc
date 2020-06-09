
int main() {
    int i = 0;
    int b = 0;
    for (; i < 20; i++) {
        if (i % 2 == 0) {
            continue;
        }
        b += 3;
    }
    return b; // Expect: 30
}
