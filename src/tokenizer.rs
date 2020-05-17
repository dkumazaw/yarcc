use std::collections::LinkedList;
use std::collections::linked_list::Iter;
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    TKRESERVED,
    TKNUM,
    TKEOF
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    string: Option<String>,
    val: i32,
}

pub struct Tokenizer {
    tokens: LinkedList<Token>, // Linked list of tokens
}

pub struct TokenIter<'a> {
    iter: Peekable<Iter<'a, Token>>,    
}

impl Token {
    fn new(kind: TokenKind) -> Self {
        // TODO: Can we have some default behavior?
        Token {
            kind: kind,
            string: None,
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
        }
    }

    // Tokenizes the passed str
    pub fn tokenize(&mut self, in_str: &str) -> Iter<Token> {
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

        // Finally add tof
        self.tokens.push_back(Token::new(TKEOF));

        self.tokens.iter()
    }
}

impl<'a> TokenIter<'a> {
    pub fn new(baseiter: Iter<'a, Token>) -> Self {
        TokenIter {
            iter: baseiter.peekable()
        }
    }

    pub fn expect(&mut self, s: &str) {
        let t = self.peek();
        if t.kind != TokenKind::TKRESERVED {
            panic!("TokenIter: Expected reserved token {}", s);
        }
        if let Some(ref tkstr) = t.string {
            if tkstr != s {
                panic!("hogehoge!")
            }
        } else {
            panic!("HOge!")
        }

        self.next();
    }

    pub fn expect_number(&mut self) -> i32 {
        if self.peek().kind != TokenKind::TKNUM {
            panic!("TokenIter: Expected number.")
        }

        self.next().val
    }
    
    pub fn consume(&mut self) -> bool {
        
    }

    pub fn at_eof(&mut self) -> bool {
        self.peek().kind == TokenKind::TKEOF 
    }

    // Wrapper to hide option unwrapping
    fn peek(&mut self) -> &Token {
        self.iter.peek().unwrap()
    }

    // Wrapper to hide option unwrapping
    fn next(&mut self) -> &Token {
        self.iter.next().unwrap()
    }
}
