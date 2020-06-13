// Test cases for continue

int cont_dowhile() {
    int i = 0;
    int b = 0;
    do {
        i++;
        if (i %2 == 0) continue;
        b += 3;
    } while (i < 20);
    return b; // Expect: 30
}

int cont_for() {
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

/* Multiple continues in loop should be ok */
int cont_multiple() {
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

/* Continue in nest of loops. */
int cont_nested() {
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

int cont_while() {
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

int main() {
    if (cont_for() != 30) return 1;
    if (cont_while() != 30) return 2;
    if (cont_dowhile() != 30) return 3;
    if (cont_nested() != 150) return 4;
    if (cont_multiple() != 17) return 5;
    
    // Successful
    return 0;
}
