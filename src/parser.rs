use std::collections::LinkedList;
use crate::tokenizer::TokenIter;

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
    NDLVAR,   // local var
    NDNUM,
}

#[derive(Debug)]
pub struct Node {
    pub kind: NodeKind,
    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,
    pub val: Option<i32>, // Used only when kind is NDNUM
    pub offset: Option<i32>, // Used only when kind is NDLVAR
}

pub struct Parser<'a> {
    iter: TokenIter<'a>,
}

impl Node {
    fn new(kind: NodeKind, lhs: Option<Box<Node>>, rhs: Option<Box<Node>>) -> Self {
        Node {
            kind: kind, 
            lhs: lhs,
            rhs: rhs,
            val: None,
            offset: None,
        }
    }

    fn val(mut self, value: i32) -> Self {
        self.val = Some(value);
        self
    }

    fn offset(mut self, offset: i32) -> Self {
        self.offset = Some(offset);
        self
    }
}

impl<'a> Parser<'a> {
    pub fn new(iter: TokenIter<'a>) -> Self {
        Parser {
            iter: iter,
        }
    }

    pub fn parse(&mut self) -> LinkedList<Node> {
        self.program()
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
    fn stmt(&mut self) -> Node {
        let node = self.expr();
        self.iter.expect(";");
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
            Node::new(NDLVAR, None, None).offset(8)
        } else {
            // Must be NUM at this point
            Node::new(NDNUM, None, None).val(self.iter.expect_number())
        }
    }
}
