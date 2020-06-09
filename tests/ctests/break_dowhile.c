/* Break out of dowhile loop */
int main() {
    int i = 0;
    do {
        i += 2;
        if (i == 24) break;
    } while (i < 30);
    return i; // Expect: 24
}
