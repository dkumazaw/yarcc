use crate::tokenizer::{TokenIter, TokenKind};
use std::collections::LinkedList;

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
    NDGVARDEF, // Global var definition
    NDVARDEF,  // Variable definition
    NDADDR,
    NDDEREF,
    NDLVAR, // local var
    NDGVAR, // global var
    NDNUM,
}

// Node of an AST
#[derive(Debug)]
pub struct Node {
    pub kind: NodeKind,
    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,
    pub val: Option<i32>,      // Used by NDNUM
    pub offset: Option<usize>, // Used by NDLVAR
    pub ty: Option<Type>,
    pub scale_lhs: Option<bool>, // Used by NDADD and NDSUB to perform ptr arithm.

    pub cond: Option<Box<Node>>,     // Used for if else, for, and while
    pub ifnode: Option<Box<Node>>,   // Used when if cond is true
    pub elsenode: Option<Box<Node>>, // Used when if cond is false and else is defined

    pub repnode: Option<Box<Node>>, // Used for "for" & "while"

    pub initnode: Option<Box<Node>>, // Used by for
    pub stepnode: Option<Box<Node>>, // Used by for

    pub blockstmts: LinkedList<Node>, // Used by NDBLOCK & NDFUNCDEF

    pub name: Option<String>,   // NDCALL, NDGVARDEF, NDFUNCDEF
    pub funcargs: LinkedList<Node>, // Used by NDCALL

    pub funcarg_vars: LinkedList<Var>, // Context of args; used by NDFUNCDEF

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

// Stores the name of var, its type, and stack offset if the variable is local.
#[derive(Debug, Clone)]
pub struct Var {
    pub name: String,
    pub ty: Type,
    pub offset: Option<usize>, // None if global
}

// Stores the current level of local variables
#[derive(Debug)]
pub struct LVarScope {
    list: LinkedList<Var>,
    offset: usize,
}

// Parser returns this context;
// codegen should use this context to produce code
pub struct ParsedContext {
    pub nodes: LinkedList<Node>,
}

pub struct Parser<'a> {
    iter: TokenIter<'a>,
    globals: LinkedList<Var>,
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
            scale_lhs: None,
            cond: None,
            ifnode: None,
            elsenode: None,
            repnode: None,
            initnode: None,
            stepnode: None,
            blockstmts: LinkedList::new(),
            name: None,
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

    fn name(mut self, s: String) -> Self {
        self.name = Some(s);
        self
    }

    fn funcarg(mut self, node: Node) -> Self {
        self.funcargs.push_back(node);
        self
    }

    fn funcarg_var(mut self, lvar: Var) -> Self {
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
            NDADD | NDSUB => {
                let lhs = self.lhs.as_mut().unwrap();
                let rhs = self.rhs.as_mut().unwrap();
                lhs.populate_ty();
                rhs.populate_ty();

                let l_ty = lhs.ty.as_ref().unwrap();
                let r_ty = rhs.ty.as_ref().unwrap();

                if l_ty.kind.is_ptr_like() {
                    if r_ty.kind.is_ptr_like() {
                        panic!("Parser: Both sides of add/sub are pointers...");
                    }
                    self.scale_lhs = Some(true);
                    Some(l_ty.clone())
                } else if r_ty.kind.is_ptr_like() {
                    // Already checked above that l_ty is not a pointer
                    self.scale_lhs = Some(false);
                    Some(r_ty.clone())
                } else {
                    // TODO: Update this
                    Some(Type::new(TypeKind::LONG, 0))
                }
            }
            NDMUL | NDDIV | NDEQ | NDNEQ | NDLEQ | NDLT => {
                // TODO: Update this
                Some(Type::new(TypeKind::INT, 0))
            }
            NDCALL => {
                // TODO: Currently the only retval is INT
                Some(Type::new(TypeKind::INT, 0))
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
            _ => None,
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

    fn register_lvar(&mut self, ident_name: String, ty: Type) -> Var {
        let requested_size = ty.total_size();
        self.offset += requested_size;
        let my_ofs = self.offset;

        let lvar = Var {
            name: ident_name,
            ty: ty,
            offset: Some(my_ofs),
        };
        self.list.push_back(lvar.clone());
        lvar
    }

    fn find_lvar(&self, ident_name: &str) -> Option<&Var> {
        self.list.iter().find(|x| x.name == ident_name)
    }
}

impl TypeKind {
    pub fn is_ptr_like(&self) -> bool {
        use TypeKind::*;
        match self {
            PTR | ARRAY => true,
            _ => false,
        }
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
            array_size: None,
        }
    }

    fn new_array(basekind: TypeKind, ref_depth: usize, array_size: usize) -> Self {
        Type {
            kind: TypeKind::ARRAY,
            ptr_to: Some(Box::new(Type::new(basekind, ref_depth))),
            array_size: Some(array_size),
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
            array_size: None,
        }
    }

    // Size of one element
    pub fn size(&self) -> usize {
        use TypeKind::*;
        match self.kind {
            INT => 4,
            LONG => 8,
            PTR => 8,
            ARRAY => self.base_size(),
        }
    }

    // Total size of this type
    pub fn total_size(&self) -> usize {
        use TypeKind::*;
        match self.kind {
            INT | LONG | PTR => self.size(),
            ARRAY => self.array_size.unwrap() * self.size(),
        }
    }

    pub fn base_size(&self) -> usize {
        if let Some(ref base) = self.ptr_to {
            base.size()
        } else {
            panic!("Requesting a base size for a terminal type.");
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(iter: TokenIter<'a>) -> Self {
        Parser {
            iter: iter,
            globals: LinkedList::new(),
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
    fn lvar_def(&mut self) -> Option<Var> {
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
            let ident = self.iter.expect_ident();
            if self.iter.consume("[") {
                // This is an array
                let array_size = self.iter.expect_number() as usize;
                let var_type = Type::new_array(TypeKind::INT, refs, array_size);
                self.iter.expect("]");
                Some(self.add_lvar(ident, var_type))
            } else {
                let var_type = Type::new(TypeKind::INT, refs);
                Some(self.add_lvar(ident, var_type))
            }
        }
    }

    // program = external_decl*
    fn program(&mut self) -> LinkedList<Node> {
        let mut nodes = LinkedList::new();
        while !self.iter.at_eof() {
            nodes.push_back(self.external_decl());
        }
        nodes
    }

    // external_decl = "int" ident ( funcdef | gvar_def )
    fn external_decl(&mut self) -> Node {
        use NodeKind::*;
        let _tk = self.iter.expect_type(); // TODO: Only int is
        let refs = {
            // # of times * occurs will tell us the depth of references
            let mut tmp = 0;
            while self.iter.consume("*") {
                tmp += 1;
            }
            tmp
        };

        let ident_name = self.iter.expect_ident();

        if let Some(node) = self.funcdef(&ident_name) {
            // This is a funcdef
            node
        } else {
            // This must be a gvar decl
            self.gvar_def(ident_name)
        }
    }

    fn gvar_def(&mut self, ident_name: String) -> Node {
        use NodeKind::*;

        let mut node = Node::new(NDGVARDEF, None, None).name(ident_name.clone());
        if self.iter.consume("[") {
            panic!("TODO");
        } else {
            let var_type = Type::new(TypeKind::INT, 0);
            node = node.ty(var_type.clone());
            self.add_gvar(ident_name, var_type);
        }
        self.iter.expect(";");
        node
    }

    // funcdef = "(" (lvar_def ",")* ")" "{" stmt* "}"
    // This performs the rest of funcdef parsing not performed by
    // external_decl
    fn funcdef(&mut self, ident_name: &str) -> Option<Node> {
        use NodeKind::*;

        if !self.iter.consume("(") {
            return None;
        }

        // Pick up from argument parsing
        let mut node = Node::new(NDFUNCDEF, None, None).name(ident_name.to_string());
        // Create a new scope:
        self.locals.push_back(LVarScope::new());

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

        Some(node)
    }

    // stmt = lvar_def ";"
    //      | "{" stmt* "}"
    //      | "if" "(" expr ")" stmt ("else" stmt)?
    //      | "while" "(" expr ")" stmt
    //      | "for" "(" expr? ";" expr? ";" expr? ")" stmt
    //      | "return" expr ";"
    //      | expr ";"
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
            node = Node::new(NDIF, None, None).cond(Some(Box::new(self.expr())));
            self.iter.expect(")");
            node = node.ifnode(Some(Box::new(self.stmt())));

            // Else statement
            if self.iter.consume_kind(TokenKind::TKELSE) {
                node = node.elsenode(Some(Box::new(self.stmt())));
            }
        } else if self.iter.consume_kind(TokenKind::TKWHILE) {
            self.iter.expect("(");
            node = Node::new(NDWHILE, None, None).cond(Some(Box::new(self.expr())));
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
            node = Node::new(
                NDASSIGN,
                Some(Box::new(node)),
                Some(Box::new(self.assign())),
            );
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
                node = Node::new(
                    NDEQ,
                    Some(Box::new(node)),
                    Some(Box::new(self.relational())),
                );
            } else if self.iter.consume("!=") {
                node = Node::new(
                    NDNEQ,
                    Some(Box::new(node)),
                    Some(Box::new(self.relational())),
                );
            } else {
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
                node.populate_ty();
            } else if self.iter.consume("-") {
                node = Node::new(NDSUB, Some(Box::new(node)), Some(Box::new(self.mul())));
                node.populate_ty();
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
    //       | postfix
    fn unary(&mut self) -> Node {
        use NodeKind::*;

        let mut node;
        if self.iter.consume_kind(TokenKind::TKSIZEOF) {
            let mut lhs = self.unary();
            lhs.populate_ty();
            node = Node::new(NDNUM, None, None)
                .val(lhs.ty.unwrap().total_size() as i32)
                .ty(Type::new(TypeKind::INT, 0));
        } else if self.iter.consume("*") {
            node = Node::new(NDDEREF, Some(Box::new(self.unary())), None);
            node.populate_ty();
        } else if self.iter.consume("&") {
            node = Node::new(NDADDR, Some(Box::new(self.unary())), None);
        } else if self.iter.consume("+") {
            node = self.primary();
        } else if self.iter.consume("-") {
            node = Node::new(
                NDSUB,
                Some(Box::new(
                    Node::new(NDNUM, None, None)
                        .val(0)
                        .ty(Type::new(TypeKind::INT, 0)),
                )),
                Some(Box::new(self.primary())),
            );
            node.populate_ty()
        } else {
            node = self.postfix();
        }
        node
    }

    // postfix = primary ('[' expr ']')?
    fn postfix(&mut self) -> Node {
        use NodeKind::*;

        let mut node = self.primary();
        if self.iter.consume("[") {
            node = Node::new(NDADD, Some(Box::new(node)), Some(Box::new(self.expr())));
            node = Node::new(NDDEREF, Some(Box::new(node)), None);
            node.populate_ty();
            self.iter.expect("]");
        }
        node
    }

    // primary = num
    //         | ident ("(" (expr, )* ")")?
    //         | "(" expr ")"
    fn primary(&mut self) -> Node {
        use NodeKind::*;

        if self.iter.consume("(") {
            let node = self.expr();
            self.iter.expect(")");
            node
        } else if let Some(ident) = self.iter.consume_ident() {
            if self.iter.consume("(") {
                // This is a function call
                let mut remaining = 6;
                let mut node = Node::new(NDCALL, None, None).name(ident);

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
                let lvar = self.find_var(&ident);
                Node::new(NDLVAR, None, None)
                     .offset(lvar.offset.unwrap())
                     .ty(lvar.ty.clone())
            }
        } else {
            // Must be NUM at this point
            Node::new(NDNUM, None, None)
                .val(self.iter.expect_number())
                .ty(Type::new(TypeKind::INT, 0))
        }
    }

    // Finds if the passed identitier already exists
    fn find_var(&mut self, ident_name: &str) -> &Var {
        // TODO: Support hierarchical lookup
        if let Some(ref v) = self.locals.back().unwrap().find_lvar(ident_name) {
            v
        } else {
            panic!("Parser: Found an undefined variable {}\n", ident_name);
        }
    }

    fn add_lvar(&mut self, ident_name: String, ty: Type) -> Var {
        self.locals
            .back_mut()
            .unwrap()
            .register_lvar(ident_name, ty)
    }

    fn add_gvar(&mut self, ident_name: String, ty: Type) {
        self.globals
            .push_back(Var { name: ident_name, ty: ty, offset: None });
    }
}
