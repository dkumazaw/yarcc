/* Continue in nest of loops. */
int main() {
    int i = 0;
    int a = 0;

    for (; i < 20; i++) {
        int j = 0;
        // Should be equivalent to a += 7 
        while (j < 10) {
            j++;
            if ( j % 3 == 0 ) { continue; }
            a += 1; 
        }

        if (i % 2 == 0) {
            continue;
        }
        a++;
    }

    return a; // Expect: 150
}
