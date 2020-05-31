// Find which element occurs only once.
// Taken from https://leetcode.com/problems/single-number-ii/
int findnum (int *p, int len) {
    int once = 0;
    int twice = 0;

    int i;
    // Note: cannot use declaration as initialization in C89
    for (i = 0; i < len; i++) {
        once = ~twice & (once ^ *(p + i));
        twice = ~once & (twice ^ *(p + i));
    }
    return once;
}

int main() {
    int input[10] = {10, 2, 4, 4, 2, 10, 94, 2, 4, 10};
    // Expect: 94
    return findnum(input, 10);
}
