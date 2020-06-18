// Test cases for stmt
#[macro_use]
mod macros;

test_succeed! {
    // Optional expression stmts should be a no-op.
    optexpr0: ("int main() { ;;;;  {;; } return 1;}", 1),
    optexpr1: ("int main() { int i; for (i = 0; i < 4; i++) ; return i; }", 4),
    optexpr2: ("int main() { int i = 3; if ( i == 3 ) ; return i; }", 3),
    optexpr3: ("int main() { int i = 3; if (i < 2) ; else ; return i; }", 3),
    block0: ("int main() { int a; a = 0; while (a != 10) { a = a + 1; } return a;} ", 10),
    block1: ("int main() { int a; for (a = 0; a <= 14; a = a +1) {} return a;} ", 15),
    block2: ("int main() { int a; a = 1 * 5; if (a == 5) {a =7; return a;} else {return 10; return 11;} }", 7),
    block3: ("int main() { int a= 3; {int a = 5;} return a; }", 3),
    block4: ("int main() { int a = 4; { int a = 10; { int a = 44;  { int a = 3; { int a = 24; } }} }  return a; }", 4),
    block5: ("int main() { int a= 5, i = 0; for (; i< 5; i++) { int a = 2; a += 9; } return a; }", 5),
    block6: ("int main() { int a= 6; {int a = 56; return a;} }", 56),
}
