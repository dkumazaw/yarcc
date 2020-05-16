use std::collections::LinkedList;
use std::collections::linked_list::IterMut;

#[derive(Debug)]
enum TokenKind {
    TKRESERVED,
    TKNUM,
    TKEOF
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    val: i32,
}

pub struct Tokenizer {
    tokens: LinkedList<Token>, // Linked list of tokens
    pos: u32, // Token that is being read currently
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
    // Constructor
    pub fn new() -> Self {
        Tokenizer {
            tokens: LinkedList::new(),
            pos: 0
        }
    }

    // Tokenizes the passed str
    pub fn tokenize(&mut self, in_str: &str) -> IterMut<Token> {
        use TokenKind::*;

        for c in in_str.chars() {
            match c {
                c if c.is_whitespace() => {
                    continue;
                },

                '+' | '-' => {
                    self.tokens.push_back(Token::new(TKRESERVED));
                    continue;
                },

                c if c.is_numeric() => {
                    self.tokens.push_back(Token::new(TKNUM)
                                                .val(c.to_digit(10).unwrap() as i32));
                    continue;
                },

                _ => {
                    panic!("Unexpected char.")
                }
            }
        }

        // Finally add eof
        self.tokens.push_back(Token::new(TKEOF));

        self.tokens.iter_mut()
    }

    // Returns true if we arrieved at EOF
    pub fn at_eof(&mut self) -> bool {
        false
    }
}
