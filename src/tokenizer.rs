use std::collections::LinkedList;

enum TokenKind {
    TK_RESERVED,
    TK_NUM,
    TK_EOF
}

struct Token {
    kind: TokenKind,
}

pub struct Tokenizer {
    tokens: LinkedList<Token>
}

impl Token {
    fn new(kind: TokenKind) {}
}

impl Tokenizer {
    pub fn new() -> Tokenizer {
        Tokenizer {
            tokens: LinkedList::new(),
        }
    }

    pub fn tokenize(&mut self, in_str: &str) {
        for c in in_str.chars() {
            if c == ' ' {
                continue;
            }

            println!("{}", c);
        }
    }
}
