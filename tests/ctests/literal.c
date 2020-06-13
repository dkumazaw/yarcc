// Test cases for string literals

char *bp1;
char *bp2;

int simple1() {
    char *p1 = "Hello world";
    char *p2 = "Hello world2";

    return p1[3] == p2[3];
}

int simple2() {
    bp1 = bp2 = "foo";
    char *lp = "foo";
    
    return (bp1[1] == bp2[1]) && (bp1[1] == lp[1]);
}

int main() {
    return simple1() && simple2();
}
