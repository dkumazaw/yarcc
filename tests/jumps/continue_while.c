int main() {
    int i =0;
    int b = 0;
    while (i < 20) {
        i += 1;
        if (i %2 == 0) {
            continue;
        }
        b += 3;
    }
    return b; // Expect 30
}
