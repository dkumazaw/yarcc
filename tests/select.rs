// if, if else, switch

use assert_cmd::prelude::*;
use std::process::Command;

#[macro_use]
mod macros;

tests! {
    if0: ("int main() { if (1 == 1) return 1; return 2;} ", 1),
    if1: ("int main() { if (1 != 1) return 1; return 2;} ", 2),
    if2: ("int main() { int a = 1; if (a == 0 ) a = 3; return a; }", 1),
    ifelse0: ("int main() { if (1 == 1) return 1; else return 2;} ", 1),
    ifelse1: ("int main() { if (1 != 1) return 1; else return 2; }", 2),
    ifelse2: ("int main() { int a; a = 4; if (a == 4) return 1; else return 5;} ", 1),
    ifelse3: ("int main() { int boo; boo = 19; if (boo == 1) return 10; else return 2;} ", 2),
}
