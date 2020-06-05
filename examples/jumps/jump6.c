/* Break in nested loops should break out of the 
 * immediate loop that contains break. */
int main() {
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
