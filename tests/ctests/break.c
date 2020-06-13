// Test cases for break

/* Break out of dowhile loop */
int break_dowhile() {
    int i = 0;
    do {
        i += 2;
        if (i == 24) break;
    } while (i < 30);
    return i; // Expect: 24
}

/* Break out of for loop */
int break_for() {
    int i = 0;
    for (; i < 10; i++) {
        if (i == 4) break;
    }

    return i;
}

/* Multiple breaks in loop should be ok */
int break_multiple() {
    int i;
    for (i =0; i < 20; i++) {
        if (i & 8) break;
        if (i & 4) break;
    }
    return i;
}

/* Break in nested loops should break out of the 
 * immediate loop that contains break. */
int break_nested() {
    int i = 0;
    int a = 0;

    for (; i < 20; i++) {
        int j = 0;
        while (j < 10) {
            if ( j == 3 ) { break; }
            j++;
        }
        a += j;
        if (i == 3) {
            break;
        }
    }

    return a;
}

/* Break out of while loop */
int break_while() {
    int i = 0;
    while (i <= 10) {
        if (i == 6) {
            break;
        } 
        i++;
    }
    return i;
}

int main() {
    if (break_for() != 4) return 1;
    if (break_while() != 6) return 2;
    if (break_dowhile() != 24) return 3;
    if (break_nested() != 12) return 4;
    if (break_multiple() != 4) return 5;
    
    // Successful
    return 0;
}

