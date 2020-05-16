use std::collections::LinkedList;

enum TokenKind {
    TK_RESERVED,
    TK_NUM,
    TK_EOF
}

struct Token {
    kind: TokenKind,
    val: i32,
}

pub struct Tokenizer {
    tokens: LinkedList<Token>
}

impl Token {
    fn new(kind: TokenKind) -> Self {
        // TODO: Can we have some default behavior?
        Token {
            kind: kind,
            val: 0,
        }
    }

    fn val(mut self, value: i32) -> Self {
        self.val = value; 
        self
    }
}

impl Tokenizer {
    pub fn new() -> Self {
        Tokenizer {
            tokens: LinkedList::new(),
        }
    }

    pub fn tokenize(&mut self, in_str: &str) {
        use TokenKind::*;

        for c in in_str.chars() {
            match c {
                c if c.is_whitespace() => {
                    continue;
                },

                '+' | '-' => {
                    self.tokens.push_back(Token::new(TK_RESERVED));
                    continue;
                },

                c if c.is_numeric() => {
                    self.tokens.push_back(Token::new(TK_NUM)
                                                .val(c.to_digit(10).unwrap() as i32));
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
