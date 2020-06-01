use std::collections::LinkedList;

static ASSIGN_OPS: [&str; 5] = ["=", "+=", "-=", "*=", "/="];
static TYPES: [&str; 3] = ["char", "short", "int"];
static KEYWORDS: [&str; 9] = [
    "char", "short", "int", "return", "if", "else", "while", "for", "sizeof",
];

fn is_type(s: &str) -> bool {
    match s {
        _s if TYPES.contains(&_s) => true,
        _ => false,
    }
}

fn is_assign_op(s: &str) -> bool {
    match s {
        _s if ASSIGN_OPS.contains(&_s) => true,
        _ => false,
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenKind {
    TKRESERVED,
    TKIDENT,
    TKSTR,
    TKNUM,
    TKEOF,
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

pub struct TokenIter {
    tokens: LinkedList<Token>,
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

    fn string(mut self, s: &str) -> Self {
        self.string = Some(s.to_string());
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
    pub fn tokenize(mut self, in_str: String) -> TokenIter {
        use TokenKind::*;

        let len = in_str.len();
        let mut cur = 0;

        while cur != len {
            let c = in_str.chars().nth(cur).unwrap();
            match c {
                c if c.is_whitespace() => {
                    cur += 1;
                    continue;
                }

                '\n' => {
                    cur += 1;
                    continue;
                }

                '"' => {
                    let mut str_literal = String::new();
                    cur += 1;
                    while cur != len {
                        let _c = in_str.chars().nth(cur).unwrap();
                        cur += 1;
                        if _c == '"' {
                            break;
                        }
                        str_literal.push(_c);
                    }
                    self.tokens
                        .push_back(Token::new(TKSTR).string(&str_literal));
                    continue;
                }

                '<' => {
                    cur += 1;
                    // TODO: Clean up this pattern...
                    let tkstr = if cur != len && in_str.chars().nth(cur).unwrap() == '=' {
                        cur += 1;
                        c.to_string() + "="
                    } else if in_str.chars().nth(cur).unwrap() == '<' {
                        cur += 1;
                        c.to_string() + "<"
                    } else {
                        c.to_string()
                    };
                    self.tokens.push_back(Token::new(TKRESERVED).string(&tkstr));
                    continue;
                }

                '>' => {
                    cur += 1;
                    // TODO: Clean up this pattern...
                    let tkstr = if cur != len && in_str.chars().nth(cur).unwrap() == '=' {
                        cur += 1;
                        c.to_string() + "="
                    } else if in_str.chars().nth(cur).unwrap() == '>' {
                        cur += 1;
                        c.to_string() + ">"
                    } else {
                        c.to_string()
                    };
                    self.tokens.push_back(Token::new(TKRESERVED).string(&tkstr));
                    continue;
                }

                '+' => {
                    cur += 1;
                    let tkstr = if cur != len && in_str.chars().nth(cur).unwrap() == '=' {
                        cur += 1;
                        c.to_string() + "="
                    } else if in_str.chars().nth(cur).unwrap() == '+' {
                        cur += 1;
                        c.to_string() + "+"
                    } else {
                        c.to_string()
                    };
                    self.tokens.push_back(Token::new(TKRESERVED).string(&tkstr));
                    continue;
                }

                '-' => {
                    cur += 1;
                    let tkstr = if cur != len && in_str.chars().nth(cur).unwrap() == '=' {
                        cur += 1;
                        c.to_string() + "="
                    } else if in_str.chars().nth(cur).unwrap() == '-' {
                        cur += 1;
                        c.to_string() + "-"
                    } else {
                        c.to_string()
                    };
                    self.tokens.push_back(Token::new(TKRESERVED).string(&tkstr));
                    continue;
                }

                '*' | '!' | '=' => {
                    cur += 1;
                    let tkstr = if cur != len && in_str.chars().nth(cur).unwrap() == '=' {
                        cur += 1;
                        c.to_string() + "="
                    } else {
                        c.to_string()
                    };
                    self.tokens.push_back(Token::new(TKRESERVED).string(&tkstr));
                    continue;
                }

                '/' => {
                    cur += 1;
                    let tkstr = if cur != len && in_str.chars().nth(cur).unwrap() == '=' {
                        cur += 1;
                        Some("/=".to_string())
                    } else if in_str.chars().nth(cur).unwrap() == '/' {
                        cur += 1;
                        while cur != len && in_str.chars().nth(cur).unwrap() != '\n' {
                            cur += 1;
                        }
                        None
                    } else {
                        Some("/".to_string())
                    };
                    if let Some(tk) = tkstr {
                        self.tokens.push_back(Token::new(TKRESERVED).string(&tk));
                    }
                    continue;
                }

                '&' => {
                    cur += 1;
                    let tkstr = if cur != len && in_str.chars().nth(cur).unwrap() == '&' {
                        cur += 1;
                        c.to_string() + "&"
                    } else {
                        c.to_string()
                    };
                    self.tokens.push_back(Token::new(TKRESERVED).string(&tkstr));
                    continue;
                }

                '|' => {
                    cur += 1;
                    let tkstr = if cur != len && in_str.chars().nth(cur).unwrap() == '|' {
                        cur += 1;
                        c.to_string() + "|"
                    } else {
                        c.to_string()
                    };
                    self.tokens.push_back(Token::new(TKRESERVED).string(&tkstr));
                    continue;
                }

                '(' | ')' | ':' | ';' | '{' | '}' | ',' | '[' | ']' | '^' | '~' | '?' => {
                    self.tokens
                        .push_back(Token::new(TKRESERVED).string(&c.to_string()));
                    cur += 1;
                    continue;
                }

                c if c.is_ascii_alphabetic() => {
                    let mut ident_name = c.to_string();
                    cur += 1;
                    while cur != len {
                        let _c = in_str.chars().nth(cur).unwrap();
                        if !_c.is_ascii_alphanumeric() {
                            break;
                        }
                        ident_name.push(_c);
                        cur += 1;
                    }
                    // Match keywords here
                    match ident_name.as_str() {
                        ident_name if KEYWORDS.contains(&ident_name) => {
                            self.tokens
                                .push_back(Token::new(TKRESERVED).string(&ident_name));
                        }
                        _ => {
                            self.tokens
                                .push_back(Token::new(TKIDENT).string(&ident_name));
                        }
                    }
                    continue;
                }

                c if c.is_numeric() => {
                    let mut val = c.to_digit(10).unwrap() as i32;
                    cur += 1;
                    while cur != len {
                        let _c = in_str.chars().nth(cur).unwrap();
                        if !_c.is_numeric() {
                            break;
                        }
                        val = val * 10 + (_c.to_digit(10).unwrap() as i32);
                        cur += 1;
                    }
                    self.tokens.push_back(Token::new(TKNUM).val(val));
                    continue;
                }

                unmatched => panic!("Unexpected char: {}.", unmatched),
            }
        }

        // Finally add tof
        self.tokens.push_back(Token::new(TKEOF));

        TokenIter::new(self.tokens)
    }
}

impl TokenIter {
    pub fn new(tokens: LinkedList<Token>) -> Self {
        TokenIter { tokens: tokens }
    }

    pub fn expect(&mut self, s: &str) {
        let t = self.next();
        if t.kind != TokenKind::TKRESERVED {
            panic!("TokenIter: Expected reserved token {}", s);
        }
        if let Some(ref tkstr) = t.string {
            if tkstr != s {
                panic!(
                    "TokenIter: Wrong token string! Expected {} but got {}",
                    s, tkstr
                );
            }
        } else {
            panic!("TokenIter: Expected that string exists.")
        }
    }

    pub fn expect_type(&mut self) -> String {
        let t = self.next();
        if is_type(t.string.as_ref().unwrap().as_str()) {
            t.string.as_ref().unwrap().to_string()
        } else {
            panic!("TokenIter: Expected type specifier.");
        }
    }

    pub fn expect_number(&mut self) -> i32 {
        let t = self.next();
        if t.kind != TokenKind::TKNUM {
            panic!("TokenIter: Expected number.")
        }

        t.val
    }

    pub fn expect_ident(&mut self) -> String {
        let t = self.next();
        if t.kind != TokenKind::TKIDENT {
            panic!("TokenIter: Expected ident.")
        }

        t.string.clone().unwrap()
    }

    // Consumes TKRESERVED matching s
    pub fn consume(&mut self, s: &str) -> bool {
        let t = self.peek();
        let mut ret = false;
        if t.kind == TokenKind::TKRESERVED {
            if let Some(ref tkstr) = t.string {
                if tkstr == s {
                    self.next();
                    ret = true;
                }
            }
        }
        ret
    }

    pub fn consume_type(&mut self) -> Option<String> {
        let t = self.peek();
        if t.kind != TokenKind::TKRESERVED {
            return None;
        }
        if is_type(t.string.as_ref().unwrap().as_str()) {
            let n = self.next();
            Some(n.string.as_ref().unwrap().to_string())
        } else {
            None
        }
    }

    // Consumes TKIDENT
    pub fn consume_ident(&mut self) -> Option<String> {
        let t = self.peek();
        let mut ret = None;
        if t.kind == TokenKind::TKIDENT {
            ret = t.string.clone();
            self.next();
        }
        ret
    }

    pub fn consume_assign_op(&mut self) -> Option<String> {
        let t = self.peek();
        let mut ret: Option<String> = None;
        if t.kind == TokenKind::TKRESERVED {
            if let Some(ref s) = t.string {
                if is_assign_op(s) {
                    let n = self.next();
                    ret = n.string;
                }
            }
        }
        ret
    }

    pub fn is_func(&self) -> bool {
        let mut iter = self.tokens.iter();

        let t = iter.next().unwrap();
        if !is_type(t.string.as_ref().unwrap().as_str()) {
            panic!("Illegal!")
        }

        loop {
            if iter.next().unwrap().string.as_ref().unwrap().as_str() != "*" {
                break;
            }
        }

        if let Some(tk) = iter.next().unwrap().string.as_ref() {
            if tk.as_str() == "(" {
                return true;
            }
        }
        return false;
    }

    pub fn at_eof(&mut self) -> bool {
        self.peek().kind == TokenKind::TKEOF
    }

    // Wrapper to hide option unwrapping
    fn peek(&mut self) -> &Token {
        self.tokens.front().unwrap()
    }

    // Wrapper to hide option unwrapping
    fn next(&mut self) -> Token {
        self.tokens.pop_front().unwrap()
    }
}
