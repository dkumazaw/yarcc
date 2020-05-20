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

    fn expr(&mut self) -> Node {
        let mut node = self.mul(); 

        loop {
            if self.iter.consume("+") {
                node = Node::new(NodeKind::NDADD, Some(Box::new(node)), Some(Box::new(self.mul())));
            } else if self.iter.consume("-") {
                node = Node::new(NodeKind::NDSUB, Some(Box::new(node)), Some(Box::new(self.mul())));
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
                node = Node::new(NodeKind::NDMUL, Some(Box::new(node)), Some(Box::new(self.primary())));
            } else if self.iter.consume("/") {
                node = Node::new(NodeKind::NDDIV, Some(Box::new(node)), Some(Box::new(self.primary())));
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
            Node::new(NodeKind::NDNUM, None, None).val(self.iter.expect_number())
        }
    }
}
