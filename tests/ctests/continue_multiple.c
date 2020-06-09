/* Multiple continues in loop should be ok */
int main() {
    int i;
    int a = 0;
    for (i =0; i < 20; i++) {
        if (i % 2 == 0) continue;
        a++;
        if (i % 3 == 0) continue;
        a++;
    }
    return a; // Expect: 17
}
