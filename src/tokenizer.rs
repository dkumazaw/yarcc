use std::collections::LinkedList;

enum TokenKind {
    TK_RESERVED,
    TK_NUM,
    TK_EOF
}

impl Default for TokenKind {
    fn default() -> Self {
        TokenKind::TK_RESERVED
    }
}

#[derive(Default)]
struct Token {
    kind: TokenKind,
    val: i32,
}

pub struct Tokenizer {
    tokens: LinkedList<Token>
}

impl Token {
    fn new(kind: TokenKind) {}
}

impl Tokenizer {
    pub fn new() -> Self {
        Tokenizer {
            tokens: LinkedList::new(),
        }
    }

    pub fn tokenize(&mut self, in_str: &str) {
        for c in in_str.chars() {
            match c {
                c if c.is_whitespace() => {
                    println!("That's a whitespace!");
                    continue;
                },

                '+' | '-' => {
                    continue;
                },

                c if c.is_numeric() => {
                    println!("{}", c);
                    continue;
                },

                _ => {
                    panic!("Unexpected char.")
                }
            }
        }
    }
}
