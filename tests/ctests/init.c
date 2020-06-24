// Initialization
/* TODO global*/


/* local */
int simple() { int a = 4; return a; }
int simple2() { short b = 3; return b; }
int simple3() { char c = 23; return c;  }
int ptr() { int a=19; int *b = &a; return *b; }
int scalar_excessive() { int a = {11, 22, 33}; return a; }
int array_exact() { int a[4] = {1, 2, 3, 5}; return a[3]; }
int comma_sep() { int a, b=4, c=5; return b *c; }
int array_excessive() { int a[5] = {1,2,3,4,5,6,7}, *b, c=4; return a[3]; }
int scalar_nested_excessive() { int a = {{{25}, {14}}, {0}}; return a; }

int array_2d() {
    int a[2][3] = { {1, 2, 3}, {4, 5, 6} }; 
    return a[1][1] * a[0][2]; // Expect: 15
}

int array_3d() {
    int a[2][2][2] = { { {1, 2}, { 3, 4 } }, { { 5, 6 } } };
    return 1;
}

/* TODO */
//int array_missing() {
//    int a[20] =  { 1, 2, 3 };
//    return a[15]; // Expect: 0
//}

int main() {
    if (simple() != 4) return 1;
    if (simple2() != 3) return 2;
    if (simple3() != 23) return 3;
    if (ptr() != 19) return 4;
    if (scalar_excessive() != 11) return 5;
    if (array_exact() != 5) return 6;
    if (comma_sep() != 20) return 7;
    if (array_excessive() != 4) return 8;
    if (scalar_nested_excessive() != 25) return 9;
    if (array_2d() != 15) return 10;

    // Successful
    return 0;
}
