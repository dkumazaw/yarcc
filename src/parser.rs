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
    lhs: Box<Node>,
    rhs: Box<Node>,
    val: Option<i32>,
}

pub struct Parser<'a> {
    iter: TokenIter<'a>,
    root: Node
}

impl Node {
    fn new(kind: NodeKind, lhs: Box<Node>, rhs: Box<Node>) -> Self {
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
    }

    fn expr(&mut self) -> Node {
        let node = self.mul(); 

        loop {
            if self.iter.consume("+") {
                node = Node::new(NodeKind::ADD, Box::new(node), Box::new(self.mul()));
            } else if self.iter.consume("-") {
                node = Node::new(NodeKind::SUB, Box::new(node), Box::new(self.mul()));
            } else {
                break;
            }
        }

        node
    }

    fn primary(&mut self) -> Node {

    }

    fn mul(&mut self) -> Node {

    }
}
