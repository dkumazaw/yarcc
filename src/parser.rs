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
    NDWHILE,
    NDFOR,
    NDBLOCK,
    NDCALL,    // function call
    NDFUNCDEF, // function definition 
    NDADDR,
    NDDEREF,
    NDLVAR,    // local var
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

    pub cond: Option<Box<Node>>, // Used for if else, for, and while
    pub ifnode: Option<Box<Node>>, // Used when if cond is true
    pub elsenode: Option<Box<Node>>, // Used when if cond is false and else is defined

    pub repnode: Option<Box<Node>>, // Used for "for" & "while"

    pub initnode: Option<Box<Node>>, // Used by for
    pub stepnode: Option<Box<Node>>, // Used by for

    pub blockstmts: LinkedList<Node>, // Used by NDBLOCK & NDFUNCDEF

    pub funcname: Option<String>, // Used by NDCALL & NDFUNCDEF
    pub funcargs: LinkedList<Node>, // Used by NDCALL

    pub funcarg_offsets: LinkedList<usize>, // Offsets at which args reside, used by NDFUNCDEF

    pub num_locals: Option<usize>, // Stores the # of local vars created; used by NDFUNCDEF & NDBLOCK(TODO)
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
}

pub struct Parser<'a> {
    iter: TokenIter<'a>,
    locals: LinkedList<LinkedList<LVar>>, // Each scope should push_back a new linked list
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
            repnode: None,
            initnode: None,
            stepnode: None,
            blockstmts: LinkedList::new(),
            funcname: None,
            funcargs: LinkedList::new(),
            funcarg_offsets: LinkedList::new(),
            num_locals: None,
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

    fn repnode(mut self, repnode: Option<Box<Node>>) -> Self {
        self.repnode = repnode;
        self
    }

    fn initnode(mut self, initnode: Option<Box<Node>>) -> Self {
        self.initnode = initnode;
        self
    }

    fn stepnode(mut self, stepnode: Option<Box<Node>>) -> Self {
        self.stepnode = stepnode;
        self
    }

    fn blockstmt(mut self, node: Node) -> Self {
        self.blockstmts.push_back(node);
        self
    } 

    fn funcname(mut self, s: String) -> Self {
        self.funcname = Some(s);
        self
    }

    fn funcarg(mut self, node: Node) -> Self {
        self.funcargs.push_back(node);
        self
    }

    fn funcarg_offset(mut self, ofs: usize) -> Self {
        self.funcarg_offsets.push_back(ofs);
        self
    }

    fn num_locals(mut self, count: usize) -> Self {
        self.num_locals = Some(count);
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
        }
    }

    // program = funcdef*
    fn program(&mut self) -> LinkedList<Node> {
        let mut nodes = LinkedList::new();
        while !self.iter.at_eof() {
            nodes.push_back(self.funcdef());
        }
        nodes
    }

    // funcdef = "int" ident "(" ("int" ident ",")* ")" "{" stmt* "}"
    fn funcdef(&mut self) -> Node {
        use NodeKind::*;

        self.iter.expect_kind(TokenKind::TKINT);
        let mut node = Node::new(NDFUNCDEF, None, None)
                            .funcname(self.iter.expect_ident().unwrap());
        // Create a new scope:
        self.locals.push_back(LinkedList::new());

        // Parse arguments
        self.iter.expect("(");
        if self.iter.consume_kind(TokenKind::TKINT) {
            let arg0 = self.iter.expect_ident().unwrap(); 
            // Register the variable to locals and remember that offset
            node = node.funcarg_offset(self.add_lvar(arg0));
            while self.iter.consume(",") {
                self.iter.expect_kind(TokenKind::TKINT);
                let arg = self.iter.expect_ident().unwrap();
                node = node.funcarg_offset(self.add_lvar(arg));
            }
        }
        self.iter.expect(")");

        // Parse function body
        self.iter.expect("{");
        while !self.iter.consume("}") {
            node = node.blockstmt(self.stmt());
        }

        // Remember the # of variables created & push the scope out of the stack
        node = node.num_locals(self.locals.pop_back().unwrap().len());
        
        node
    }

    // stmt = expr ";"
    //      | "{" stmt* "}"
    //      | "if" "(" expr ")" stmt ("else" stmt)?
    //      | "while" "(" expr ")" stmt
    //      | "for" "(" expr? ";" expr? ";" expr? ")" stmt
    //      | "return" expr ";"
    fn stmt(&mut self) -> Node {
        use NodeKind::*;

        let mut node;
        if self.iter.consume("{") {
            node = Node::new(NDBLOCK, None, None);
            while !self.iter.consume("}") {
                node = node.blockstmt(self.stmt());
            }
        } else if self.iter.consume_kind(TokenKind::TKIF) {
            self.iter.expect("(");
            node = Node::new(NDIF, None, None)
                        .cond(Some(Box::new(self.expr())));
            self.iter.expect(")");
            node = node.ifnode(Some(Box::new(self.stmt())));

            // Else statement
            if self.iter.consume_kind(TokenKind::TKELSE) {
                node = node.elsenode(Some(Box::new(self.stmt())));
            }
        } else if self.iter.consume_kind(TokenKind::TKWHILE) {
            self.iter.expect("(");
            node = Node::new(NDWHILE, None, None)
                        .cond(Some(Box::new(self.expr())));
            self.iter.expect(")");
            node = node.repnode(Some(Box::new(self.stmt())));
        } else if self.iter.consume_kind(TokenKind::TKFOR) {
            self.iter.expect("(");
            node = Node::new(NDFOR, None, None);

            if !self.iter.consume(";") {
                // init expr found!
                node = node.initnode(Some(Box::new(self.expr())));
                self.iter.expect(";");
            }
            if !self.iter.consume(";") {
                // cond expr found!
                node = node.cond(Some(Box::new(self.expr())));
                self.iter.expect(";");
            }
            if !self.iter.consume(")") {
                // step expr found!
                node = node.stepnode(Some(Box::new(self.expr())));
                self.iter.expect(")");
            }
            node = node.repnode(Some(Box::new(self.stmt())));

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
    //       | "*" unary
    //       | "&" unary
    fn unary(&mut self) -> Node {
        use NodeKind::*;

        let node;
        if self.iter.consume("*") {
            node = Node::new(NDDEREF, 
                             Some(Box::new(self.unary())),
                             None);
        } else if self.iter.consume("&") {
            node = Node::new(NDADDR,
                             Some(Box::new(self.unary())),
                             None);
        } else if self.iter.consume("+") {
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
    
    // primary = num 
    //         | ident ("(" (expr, )* ")")? 
    //         | "(" expr ")"
    fn primary(&mut self) -> Node  {
        use NodeKind::*;

        if self.iter.consume("(") {
            let node = self.expr();
            self.iter.expect(")");
            node
        } else if let Some(ident) = self.iter.consume_ident() {
            if self.iter.consume("(") {
                // This is a function call
                let mut remaining = 6;
                let mut node = Node::new(NDCALL, None, None).funcname(ident);

                if self.iter.consume(")") {
                    // No argument case
                    return node;
                }
                remaining -= 1;
                // Handle the 1st arg
                node = node.funcarg(self.expr());

                while self.iter.consume(",") {
                    if remaining == 0 {
                        panic!("Parser: Func arg exceeded the max. number of args supported.");
                    }
                    remaining -= 1;
                    node = node.funcarg(self.expr()); 
                } 
                self.iter.expect(")");
                node
            } else {
                // This is a variable
                if let Some(ref lvar) = self.find_lvar(&ident) {
                    // This ident already exists! Nice!
                    Node::new(NDLVAR, None, None).offset(lvar.offset)
                } else {
                    // Add this ident and then produce a node
                    Node::new(NDLVAR, None, None).offset(self.add_lvar(ident))
                }
            }
        } else {
            // Must be NUM at this point
            Node::new(NDNUM, None, None).val(self.iter.expect_number())
        }
    }

    // Finds if the passed identitier already exists
    fn find_lvar(&mut self, ident_name: &str) -> Option<&LVar> {
        // TODO: Support hierarchical lookup
        self.locals.back().unwrap().iter().find(|x| x.name == ident_name ) 
    }

    // Adds a new ident and returns the produced offset
    fn add_lvar(&mut self, ident_name: String) -> usize {
        let next_ofs = (self.locals.back().unwrap().len() + 1) * 8;
        self.locals.back_mut().unwrap().push_back(LVar { name: ident_name, offset: next_ofs });
        next_ofs
    }
}
