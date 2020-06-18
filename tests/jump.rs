// jump stmts (goto, continue, break, return)
#[macro_use]
mod macros;

test_succeed! {
    break0: ("tests/ctests/break.c", 0),
    continue0: ("tests/ctests/continue.c", 0),
}
