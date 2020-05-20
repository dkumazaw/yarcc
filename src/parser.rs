use crate::tokenizer::TokenIter;

#[derive(Debug, PartialEq)]
pub enum NodeKind {
    NDADD,
    NDSUB,
    NDMUL,
    NDDIV,
    NDNUM,
}

#[derive(Debug)]
pub struct Node {
    pub kind: NodeKind,
    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,
    pub val: Option<i32>,
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
            val: None
        }
    }

    fn val(mut self, value: i32) -> Self {
        self.val = Some(value);
        self
    }
}

impl<'a> Parser<'a> {
    pub fn new(iter: TokenIter<'a>) -> Self {
        Parser {
            iter: iter,
        }
    }

    pub fn parse(&mut self) -> Node {
        self.expr()
    }

    // expr = mul ("+" mul | "-" mul)*
    fn expr(&mut self) -> Node {
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
    
    // primary = num | "(" expr ")"
    fn primary(&mut self) -> Node  {
        use NodeKind::*;

        if self.iter.consume("(") {
            let node = self.expr();
            self.iter.expect(")");
            node
        } else {
            Node::new(NDNUM, None, None).val(self.iter.expect_number())
        }
    }
}
