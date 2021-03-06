/* Array */

int test_simple() {
    int a[10]; 
    return 1; 
}

int test_simple2() {
    int arr[12]; 
    arr[1] = 4; 
    return arr[1]; // Expect: 4
}

int test_simple3() {
    int arr[10]; 
    int b; 
    b = 3; 
    arr[4 * b -10] = 21; 
    return arr[4 - 2]; // Expect: 21
}

int test_deref() {
    int a[100]; 
    *(a + (1 - 3 + 2 * 4)) = 22; 
    return a[6]; // Expect: 22
}

int test_deref2() {
    int a[2]; 
    int *p;  
    *a = 1; 
    *(a + 1) = 2; 
    p = a; 
    return *p + *(p + 1); // Expect: 3
}

int test_deref3() {
    int a[10]; 
    a[5] = 23; 
    return *(5 + a); // Expect: 23
}

int test_ptr() {
    int *a[5]; int c; 
    c = 37; a[1] = &c; return *a[1]; // Expect: 37
}

int test_ptr2() {
    int **a[6]; int b; int *c;
    b = 4; c = &b; a[b] = &c; return ***(a + b); // Expect: 4
}

int test_ptr3() {
    int a[12]; int *p; 
    *(a + 2) = 4; p = a; return * (4-2 + p); // Expect: 4
}

int test_simple_2d() {
    int a[5][6];
    a[2][1] = 9;
    return *(*(a + 2) + 1); // Expect: 9
}

int test_simple_2d_2() {
    int a[5][6];
    *(*(a + 1) + 5) = 129;
    return a[1][5]; // Expect: 129
}

int test_multidim() {
    int b[2][6][2], c[1][2][3][4];
    b[1][3][1] = 68; 
    *(*(*(*(c + 1))+1)+2) = 5;
    return *(*(*(b + 1) + 3 )+ 1) + c[1][0][1][2]; // Expect: 73
}

int test_addr() {
    int a[4];
    a[0] = 12;
    int *b = &a;
    return *b; // Expect: 12
}

int test_zero_inits() {
    int a[4] = { 1, 2 };
    int b[3][3] = { { 4, 8 }, { 16, 32 } };
    int c[2][2][2] = { { {64} } };

    return a[3] | b[0][2] | b[2][0] | c[1][0][0]; // Expect 0
}

int main() {
    if (test_simple() != 1) return 1;
    if (test_simple2() != 4) return 2;
    if (test_simple3() != 21) return 3;
    if (test_deref() != 22) return 4;
    if (test_deref2() != 3) return 5;
    if (test_deref3() != 23) return 6;
    if (test_ptr() != 37) return 7;
    if (test_ptr2() != 4) return 8;
    if (test_ptr3() != 4) return 9;
    if (test_simple_2d() != 9) return 10;
    if (test_simple_2d_2() != 129) return 11;
    if (test_multidim() != 73) return 12;
    if (test_addr() != 12) return 13;
    if (test_zero_inits() != 0) return 14;

    // Successful
    return 0;
}
