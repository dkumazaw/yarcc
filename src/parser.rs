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
    NDVARDEF,  // Variable definition
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
    pub ty: Option<Type>, 

    pub cond: Option<Box<Node>>, // Used for if else, for, and while
    pub ifnode: Option<Box<Node>>, // Used when if cond is true
    pub elsenode: Option<Box<Node>>, // Used when if cond is false and else is defined

    pub repnode: Option<Box<Node>>, // Used for "for" & "while"

    pub initnode: Option<Box<Node>>, // Used by for
    pub stepnode: Option<Box<Node>>, // Used by for

    pub blockstmts: LinkedList<Node>, // Used by NDBLOCK & NDFUNCDEF

    pub funcname: Option<String>, // Used by NDCALL & NDFUNCDEF
    pub funcargs: LinkedList<Node>, // Used by NDCALL

    pub funcarg_vars: LinkedList<LVar>, // Context of args; used by NDFUNCDEF

    // Local variable context for NDFUNCDEF and NDBLOCK(TODO)
    pub lvars_offset: Option<usize>, // Stores the amount of space needed on stack for lvars.
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TypeKind {
    INT,
    LONG,
    PTR,
    ARRAY,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub ptr_to: Option<Box<Type>>,
    pub array_size: Option<usize>,
}

// Denotes the name of lvar and its stack offset
#[derive(Debug, Clone)]
pub struct LVar {
    pub name: String, 
    pub ty: Type,
    pub offset: usize,
}

// Stores the current level of local variables
#[derive(Debug)]
pub struct LVarScope {
    list: LinkedList<LVar>,
    offset: usize,
}

// Parser returns this context;
// codegen should use this context to produce code
pub struct ParsedContext {
    pub nodes: LinkedList<Node>,
}

pub struct Parser<'a> {
    iter: TokenIter<'a>,
    locals: LinkedList<LVarScope>, // Each scope should push_back a new linked list
}

impl Node {
    fn new(kind: NodeKind, lhs: Option<Box<Node>>, rhs: Option<Box<Node>>) -> Self {
        Node {
            kind: kind, 
            lhs: lhs,
            rhs: rhs,
            val: None,
            offset: None,
            ty: None,
            cond: None,
            ifnode: None,
            elsenode: None,
            repnode: None,
            initnode: None,
            stepnode: None,
            blockstmts: LinkedList::new(),
            funcname: None,
            funcargs: LinkedList::new(),
            funcarg_vars: LinkedList::new(),
            lvars_offset: None,
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

    fn ty(mut self, ty: Type) -> Self {
        self.ty = Some(ty);
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

    fn funcarg_var(mut self, lvar: LVar) -> Self {
        self.funcarg_vars.push_back(lvar);
        self
    }

    fn lvars_offset(mut self, offset: usize) -> Self {
        self.lvars_offset = Some(offset);
        self
    }

    fn populate_ty(&mut self) {
        use NodeKind::*;

        if let Some(_) = self.ty {
            return;
        }

        self.ty = match self.kind {
            NDADD | NDSUB | NDMUL | NDDIV => {
                // TODO: Update this
                Some(Type::new(TypeKind::LONG, 0))
            }
            NDADDR => {
                let lhs = self.lhs.as_mut().unwrap();
                lhs.populate_ty();
                Some(lhs.ty.as_ref().unwrap().new_ptr_to())
            }
            NDASSIGN => {
                let lhs = self.lhs.as_mut().unwrap();
                lhs.populate_ty();
                Some(lhs.ty.as_ref().unwrap().clone())
            }
            NDDEREF => {
                let lhs = self.lhs.as_mut().unwrap();
                lhs.populate_ty();
                // What lhs's type points to should be my type.
                Some(lhs.ty.as_ref().unwrap().clone_base())
            }            
            _ => {
                None
            }
        }
    }
}

impl LVarScope {
    fn new() -> Self {
        LVarScope {
            list: LinkedList::new(),
            offset: 0,
        }
    }

    fn register_lvar(&mut self, ident_name: String, ty: Type) -> LVar {
        let requested_size = ty.size();
        self.offset += requested_size;
        let my_ofs = self.offset; 

        let lvar = LVar {name: ident_name, ty: ty, offset: my_ofs};
        self.list.push_back(lvar.clone());
        lvar
    }

    fn find_lvar(& self, ident_name: &str) -> Option<&LVar> {
        self.list.iter().find(|x| x.name == ident_name)
    }
}

impl Type {
    fn new(basekind: TypeKind, ref_depth: usize) -> Self {
        if basekind == TypeKind::ARRAY {
            panic!("For array, use new_array!");
        }
        Type {
            kind: if ref_depth == 0 {
                basekind
            } else {
                TypeKind::PTR
            },
            ptr_to: if ref_depth == 0 {
                None
            } else {
                Some(Box::new(Type::new(basekind, ref_depth - 1)))
            },
            array_size: None 
        }
    }

    // Clones whatever is pointed to by ptr_to
    fn clone_base(&self) -> Self {
        if let Some(ref base) = self.ptr_to {
            *base.clone()
        } else {
            panic!("Trying to clone the base of terminal types.")
        }
    }

    // Creates a new Type that is a pointer to mas_ref()
    fn new_ptr_to(&self) -> Self {
        Type {
            kind: TypeKind::PTR,
            ptr_to: Some(Box::new(self.clone())),
            array_size: None
        }
    }

    pub fn size(&self) -> usize {
        use TypeKind::*;
        match self.kind {
            INT => 4, 
            LONG => 8,
            PTR => 8,
            ARRAY => 0,// TODO
        }
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

    // Parses local variable definition if possible
    // Returns the offset of the registered lvar or None
    fn lvar_def(&mut self) -> Option<LVar> {
        if !self.iter.consume_kind(TokenKind::TKINT) {
            // This is not a variable definition. Return.
            None 
        } else {  
            let refs = {
                // # of times * occurs will tell us the depth of references
                let mut tmp = 0;
                while self.iter.consume("*") {
                    tmp += 1;
                }
                tmp
            };
            let ident = self.iter.expect_ident().unwrap();
            if self.iter.consume("[") {
                panic!("not implemented!")
                //if refs > 0 {
                //    panic!("Parser: Array of ptr type not implemented yet.")
                //}
                //let array_size = self.iter.expect_number().val;
                //let var_type = Type::new(TypeKind::ARRAY, 0)
                //Some(self.add_lvar)
                //self.iter.expect("]");
            } else {
                let var_type = Type::new(TypeKind::INT, refs);
                Some(self.add_lvar(ident, var_type))
            }
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

    // funcdef = "int" ident "(" (lvar_def ",")* ")" "{" stmt* "}"
    fn funcdef(&mut self) -> Node {
        use NodeKind::*;

        self.iter.expect_kind(TokenKind::TKINT);
        let mut node = Node::new(NDFUNCDEF, None, None)
                            .funcname(self.iter.expect_ident().unwrap());
        // Create a new scope:
        self.locals.push_back(LVarScope::new());

        // Parse arguments
        self.iter.expect("(");
        if !self.iter.consume(")") {
            // There's at least one local variable definition.
            if let Some(lvar) = self.lvar_def() {
                node = node.funcarg_var(lvar);
            } else {
                panic!("Parser: Expected local variable definition in function def.\n");
            }
            while self.iter.consume(",") {
                if let Some(lvar) = self.lvar_def() {
                    node = node.funcarg_var(lvar);
                } else {
                    panic!("Parser: Expected local variable definition in function def.\n");
                }
            }
            self.iter.expect(")");
        }

        // Parse function body
        self.iter.expect("{");
        while !self.iter.consume("}") {
            node = node.blockstmt(self.stmt());
        }

        // Remember the # of variables created & pop the scope out of the stack
        node = node.lvars_offset(self.locals.pop_back().unwrap().offset);
        
        node
    }

    // stmt = expr ";"
    //      | lvar_def ";"
    //      | "{" stmt* "}"
    //      | "if" "(" expr ")" stmt ("else" stmt)?
    //      | "while" "(" expr ")" stmt
    //      | "for" "(" expr? ";" expr? ";" expr? ")" stmt
    //      | "return" expr ";"
    fn stmt(&mut self) -> Node {
        use NodeKind::*;

        let mut node;
        if let Some(offset) = self.lvar_def() { 
            // This was an lvar def!
            self.iter.expect(";");
            // TODO: Maybe having NDVARDEF is not a good design...
            node = Node::new(NDVARDEF, None, None);
        } else if self.iter.consume("{") {
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
            node.populate_ty();
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

    // unary = "sizeof" unary
    //       | ("+" | "-")? primary
    //       | "*" unary
    //       | "&" unary
    fn unary(&mut self) -> Node {
        use NodeKind::*;

        let mut node;
        if self.iter.consume_kind(TokenKind::TKSIZEOF) {
            let mut lhs = self.unary();
            lhs.populate_ty();
            node = Node::new(NDNUM, None, None)
                        .val(lhs.ty.unwrap().size() as i32)
                        .ty(Type::new(TypeKind::INT, 0)); 
        } else if self.iter.consume("*") {
            node = Node::new(NDDEREF, 
                             Some(Box::new(self.unary())),
                             None);
            node.populate_ty();
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
                    Node::new(NDLVAR, None, None).offset(lvar.offset).ty(lvar.ty.clone())
                } else {
                    panic!("Parser: Found an undefined variable {}\n", ident);
                }
            }
        } else {
            // Must be NUM at this point
            Node::new(NDNUM, None, None).val(self.iter.expect_number())
                                        .ty(Type::new(TypeKind::INT, 0))
        }
    }

    // Finds if the passed identitier already exists
    fn find_lvar(&mut self, ident_name: &str) -> Option<&LVar> {
        // TODO: Support hierarchical lookup
        self.locals.back().unwrap().find_lvar(ident_name)
    }

    // Adds a new ident and returns the produced offset
    fn add_lvar(&mut self, ident_name: String, ty: Type) -> LVar {
        self.locals.back_mut().unwrap().register_lvar(ident_name, ty)
    }
}
