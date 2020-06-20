
/* Useless declarations are permitted. */
int;
long;

int test_useless() {
    short;
    return 1;
}

int main() {
    if (test_useless() != 1) return 1;

    // Successful 
    return 0;
}
