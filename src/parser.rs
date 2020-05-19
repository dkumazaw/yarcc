use crate::tokenizer::TokenIter;

#[derive(Debug, PartialEq)]
pub enum NodeKind {
    ADD,
    SUB,
    MUL,
    DIV,
    NUM,
}

#[derive(Debug)]
pub struct Node {
    kind: NodeKind,
    lhs: Option<Box<Node>>,
    rhs: Option<Box<Node>>,
    val: Option<i32>,
}

pub struct Parser<'a> {
    iter: TokenIter<'a>,
    root: Option<Node>
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
            root: None,
        }
    }

    pub fn parse(&mut self) {
        self.root = Some(self.expr());
    }

    fn expr(&mut self) -> Node {
        let mut node = self.mul(); 

        loop {
            if self.iter.consume("+") {
                node = Node::new(NodeKind::ADD, Some(Box::new(node)), Some(Box::new(self.mul())));
            } else if self.iter.consume("-") {
                node = Node::new(NodeKind::SUB, Some(Box::new(node)), Some(Box::new(self.mul())));
            } else {
                break;
            }
        }

        node
    }

    fn mul(&mut self) -> Node {
        let mut node = self.primary();

        loop {
            if self.iter.consume("*") {
                node = Node::new(NodeKind::MUL, Some(Box::new(node)), Some(Box::new(self.primary())));
            } else if self.iter.consume("/") {
                node = Node::new(NodeKind::DIV, Some(Box::new(node)), Some(Box::new(self.primary())));
            } else {
                break;
            }
        } 
        node
    }
    
    fn primary(&mut self) -> Node  {
        if self.iter.consume("(") {
            let node = self.expr();
            self.iter.expect(")");
            node
        } else {
            Node::new(NodeKind::NUM, None, None).val(self.iter.expect_number())
        }
    }
}
