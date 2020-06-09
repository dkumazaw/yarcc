int main() {
    int i = 0;
    int b = 0;
    do {
        i++;
        if (i %2 == 0) continue;
        b += 3;
    } while (i < 20);
    return b; // Expect: 30
}
