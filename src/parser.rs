use std::collections::LinkedList;
use crate::tokenizer::{TokenIter, TokenKind};

#[derive(Debug, PartialEq)]
pub enum NodeKind {
    NDADD,    // +
    NDSUB,    // -
    NDMUL,    // *
    NDDIV,    // /
    NDEQ,     // ==
    NDNEQ,    // !=
    NDLEQ,    // <=
    NDLT,     // <
    NDASSIGN, // =
    NDRETURN,
    NDIF, 
    NDLVAR,   // local var
    NDNUM,
}

// Node of an AST
#[derive(Debug)]
pub struct Node {
    pub kind: NodeKind,
    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,
    pub val: Option<i32>, // Used only when kind is NDNUM
    pub offset: Option<usize>, // Used only when kind is NDLVAR

    pub cond: Option<Box<Node>>, // Used for if else & while
    pub ifnode: Option<Box<Node>>, // Used when if cond is true
    pub elsenode: Option<Box<Node>>, // Used when if cond is false and else is defined
}

// Denotes the name of lvar and its stack offset
#[derive(Debug)]
pub struct LVar {
    name: String, 
    offset: usize,
}

// Parser returns this context;
// codegen should use this context to produce code
pub struct ParsedContext {
    pub nodes: LinkedList<Node>,
    pub locals: LinkedList<LVar>,
}

pub struct Parser<'a> {
    iter: TokenIter<'a>,
    locals: LinkedList<LVar>,
}

impl Node {
    fn new(kind: NodeKind, lhs: Option<Box<Node>>, rhs: Option<Box<Node>>) -> Self {
        Node {
            kind: kind, 
            lhs: lhs,
            rhs: rhs,
            val: None,
            offset: None,
            cond: None,
            ifnode: None,
            elsenode: None,
        }
    }

    fn val(mut self, value: i32) -> Self {
        self.val = Some(value);
        self
    }

    fn offset(mut self, offset: usize) -> Self {
        self.offset = Some(offset);
        self
    }

    fn cond(mut self, cond: Option<Box<Node>>) -> Self {
        self.cond = cond;
        self
    }

    fn ifnode(mut self, ifnode: Option<Box<Node>>) -> Self {
        self.ifnode = ifnode;
        self
    }

    fn elsenode(mut self, elsenode: Option<Box<Node>>) -> Self {
        self.elsenode = elsenode;
        self
    }
}

impl<'a> Parser<'a> {
    pub fn new(iter: TokenIter<'a>) -> Self {
        Parser {
            iter: iter,
            locals: LinkedList::new(),
        }
    }

    pub fn parse(mut self) -> ParsedContext {
        ParsedContext {
            nodes: self.program(),
            locals: self.locals,
        }
    }

    // program = stmt*
    fn program(&mut self) -> LinkedList<Node> {
        let mut nodes = LinkedList::new();
        while !self.iter.at_eof() {
            nodes.push_back(self.stmt());
        }
        nodes
    }

    // stmt = expr ";"
    //      | "if" "(" expr ")" stmt ("else" stmt)?
    //      | "return" expr ";"
    fn stmt(&mut self) -> Node {
        use NodeKind::*;

        let mut node;
        if self.iter.consume_kind(TokenKind::TKIF) {
            self.iter.expect("(");
            node = Node::new(NDIF, None, None)
                        .cond(Some(Box::new(self.expr())));
            self.iter.expect(")");
            node = node.ifnode(Some(Box::new(self.stmt())));
        } else if self.iter.consume_kind(TokenKind::TKRETURN) {
            node = Node::new(NDRETURN, Some(Box::new(self.expr())), None);
            self.iter.expect(";");
        } else {
            node = self.expr();
            self.iter.expect(";");
        }
        node
    }

    // expr = assign
    fn expr(&mut self) -> Node {
        self.assign() 
    }

    // assign = equality ("=" assign)?
    fn assign(&mut self) -> Node {
        use NodeKind::*;
        let mut node = self.equality();
        
        if self.iter.consume("=") {
            node = Node::new(NDASSIGN, Some(Box::new(node)), Some(Box::new(self.assign())));
        }
        node 
    }

    // equality = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> Node {
        use NodeKind::*;
        let mut node = self.relational();

        loop {
            if self.iter.consume("==") {
                node = Node::new(NDEQ, Some(Box::new(node)), Some(Box::new(self.relational())));
            } else if self.iter.consume("!=") {
                node = Node::new(NDNEQ, Some(Box::new(node)), Some(Box::new(self.relational())));
            } else{
                break;
            }
        }
        node
    }

    // relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational(&mut self) -> Node {
        use NodeKind::*;
        let mut node = self.add();

        loop {
            if self.iter.consume("<") {
                node = Node::new(NDLT, Some(Box::new(node)), Some(Box::new(self.add())));
            } else if self.iter.consume("<=") {
                node = Node::new(NDLEQ, Some(Box::new(node)), Some(Box::new(self.add())));
            } else if self.iter.consume(">") {
                // HACK: Simply flip lhs and rhs
                node = Node::new(NDLT, Some(Box::new(self.add())), Some(Box::new(node)));
            } else if self.iter.consume(">=") {
                node = Node::new(NDLEQ, Some(Box::new(self.add())), Some(Box::new(node)));
            } else {
                break;
            }
        }
        node
    }

    // add = mul ("+" mul | "-" mul)*
    fn add(&mut self) -> Node {
        use NodeKind::*;

        let mut node = self.mul(); 

        loop {
            if self.iter.consume("+") {
                node = Node::new(NDADD, Some(Box::new(node)), Some(Box::new(self.mul())));
            } else if self.iter.consume("-") {
                node = Node::new(NDSUB, Some(Box::new(node)), Some(Box::new(self.mul())));
            } else {
                break;
            }
        }

        node
    }

    // mul = unary ("*" unary | "/" unary)*
    fn mul(&mut self) -> Node {
        use NodeKind::*;

        let mut node = self.unary();

        loop {
            if self.iter.consume("*") {
                node = Node::new(NDMUL, Some(Box::new(node)), Some(Box::new(self.unary())));
            } else if self.iter.consume("/") {
                node = Node::new(NDDIV, Some(Box::new(node)), Some(Box::new(self.unary())));
            } else {
                break;
            }
        } 
        node
    }

    // unary = ("+" | "-")? primary
    fn unary(&mut self) -> Node {
        use NodeKind::*;

        let node;
        if self.iter.consume("+") {
            node = self.primary();
        } else if self.iter.consume("-") {
            node = Node::new(NDSUB, 
                             Some(Box::new(Node::new(NDNUM, None, None).val(0))),
                             Some(Box::new(self.primary())));
        } else {
            node = self.primary();
        }
        node
    }
    
    // primary = num | ident | "(" expr ")"
    fn primary(&mut self) -> Node  {
        use NodeKind::*;

        if self.iter.consume("(") {
            let node = self.expr();
            self.iter.expect(")");
            node
        } else if let Some(ident) = self.iter.consume_ident() {
            if let Some(ref lvar) = self.find_lvar(&ident) {
                // This ident already exists! Nice!
                Node::new(NDLVAR, None, None).offset(lvar.offset)
            } else {
                // Add this ident and then produce a node
                Node::new(NDLVAR, None, None).offset(self.add_lvar(ident))
            }
        } else {
            // Must be NUM at this point
            Node::new(NDNUM, None, None).val(self.iter.expect_number())
        }
    }

    // Finds if the passed identitier already exists
    fn find_lvar(&mut self, ident_name: &str) -> Option<&LVar> {
        self.locals.iter().find(|x| x.name == ident_name ) 
    }

    // Adds a new ident and returns the produced offset
    fn add_lvar(&mut self, ident_name: String) -> usize {
        let next_ofs = (self.locals.len() + 1) * 8;
        self.locals.push_back(LVar { name: ident_name, offset: next_ofs });
        next_ofs
    }
}
