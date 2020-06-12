use crate::tokenizer::TokenIter;
use std::collections::LinkedList;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NodeKind {
    NDADD,    // +
    NDSUB,    // -
    NDMUL,    // *
    NDDIV,    // /
    NDMOD,    // %
    NDEQ,     // ==
    NDNEQ,    // !=
    NDLEQ,    // <=
    NDLT,     // <
    NDASSIGN, // See AssignMode for more details
    NDBITAND, // &
    NDBITXOR, // ^
    NDBITOR,  // |
    NDLOGAND, // &&
    NDLOGOR,  // ||
    NDSHL,    // <<
    NDSHR,    // >>
    NDBITNOT, // ~
    NDTERNARY,
    NDRETURN,
    NDBREAK,
    NDCONTINUE,
    NDIF,
    NDSWITCH,
    NDWHILE,
    NDDOWHILE,
    NDFOR,
    NDBLOCK,
    NDCASE,
    NDDEFAULT,
    NDCALL,    // function call
    NDFUNCDEF, // function definition
    NDDECL,    // declaration
    NDINIT,    // initializer
    NDADDR,
    NDDEREF,
    NDLVAR, // local var
    NDGVAR, // global var
    NDNUM,
    NDSTR,
}

// Node of an AST
// NOTE: Maybe use enum to reduce redundancy?
// Will leave this like this until wastefulness becomes an issue
#[derive(Debug)]
pub struct Node {
    pub kind: NodeKind,
    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,
    pub val: Option<i32>,      // NDNUM, NDCASE
    pub offset: Option<usize>, // NDLVAR, NDCASE, NDSTR
    pub ty: Option<Type>,
    pub scale_lhs: Option<bool>, // Used by NDADD and NDSUB to perform ptr arithm.

    pub cond: Option<Box<Node>>,     // NDIF, NDFOR, NDWHILE, NDTERNARY
    pub ifnode: Option<Box<Node>>,   // Used when cond evaluates to true
    pub elsenode: Option<Box<Node>>, // Used when cond evaluates to false

    pub repnode: Option<Box<Node>>, // NDFOR, NDWHILE

    pub initnode: Option<Box<Node>>, // NDFOR
    pub stepnode: Option<Box<Node>>, // NDFOR

    pub blockstmts: LinkedList<Node>, //  NDBLOCK & NDFUNCDEF

    pub name: Option<String>,       // NDCALL, NDGVARDEF, NDFUNCDEF
    pub funcargs: LinkedList<Node>, // NDCALL

    pub funcarg_vars: LinkedList<Var>, // NDFUNCDEF (Context of args)

    pub lvars_offset: Option<usize>, // NDFUNCDEF (amount of space needed on stack for lvars)

    pub inits: LinkedList<Node>, // NDDECL

    pub eval_pre: Option<bool>, // NDASSIGN; Pre-evaluate the result and return that value
    pub assign_mode: Option<AssignMode>, // NDASSIGN

    pub ctrl: Option<Box<Node>>, // NDSWITCH
    pub stmt: Option<Box<Node>>, // NDSWITCH
    pub cases: LinkedList<i32>,  // NDSWITCH
    pub has_default: bool,       // NDSWITCH
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AssignMode {
    DEFAULT, // =
    ADD,     // +=, ++(pre, post)
    SUB,     // -=, --(pre, post)
    MUL,     // *=
    DIV,     // /=
    MOD,     // %=
    SHL,     // <<=
    SHR,     // >>=
    AND,     // &=
    OR,      // |=
    XOR,     // ^=
}

impl AssignMode {
    fn from_str(s: &str) -> Self {
        use AssignMode::*;
        match s {
            "=" => DEFAULT,
            "+=" => ADD,
            "-=" => SUB,
            "*=" => MUL,
            "/=" => DIV,
            "%=" => MOD,
            "<<=" => SHL,
            ">>=" => SHR,
            "&=" => AND,
            "|=" => OR,
            "^=" => XOR,
            _ => panic!("Assign op should be passed."),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TypeKind {
    CHAR,
    SHORT,
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
    pub scope: Option<usize>,  // None if global
}

#[derive(Debug)]
pub struct LVarScopes {
    list: LinkedList<Var>,
    level: usize, // Current scope's level
    offset: usize,
}

// Parser returns this context;
// codegen should use this context to produce code
pub struct Program {
    pub nodes: LinkedList<Node>,
    pub globals: LinkedList<Var>,
    pub literals: LinkedList<String>,
}

pub struct Parser {
    iter: TokenIter,
    globals: LinkedList<Var>,
    locals: LVarScopes,
    literals: LinkedList<String>,
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
            inits: LinkedList::new(),
            eval_pre: None,
            assign_mode: None,
            ctrl: None,
            stmt: None,
            cases: LinkedList::new(),
            has_default: false,
        }
    }

    fn new_assign(mode: AssignMode, lhs: Option<Box<Node>>, rhs: Option<Box<Node>>) -> Self {
        Node::new(NodeKind::NDASSIGN, lhs, rhs).assign_mode(mode)
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

    fn ifnode(mut self, ifnode: Option<Node>) -> Self {
        if let Some(node) = ifnode {
            self.ifnode = Some(Box::new(node));
        }
        self
    }

    fn elsenode(mut self, elsenode: Option<Node>) -> Self {
        if let Some(node) = elsenode {
            self.elsenode = Some(Box::new(node));
        }
        self
    }

    fn repnode(mut self, repnode: Option<Node>) -> Self {
        if let Some(node) = repnode {
            self.repnode = Some(Box::new(node));
        }
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

    fn blockstmt(mut self, node: Option<Node>) -> Self {
        if let Some(node) = node {
            self.blockstmts.push_back(node);
        }
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

    fn eval_pre(mut self, is_pre: bool) -> Self {
        self.eval_pre = Some(is_pre);
        self
    }

    fn assign_mode(mut self, mode: AssignMode) -> Self {
        self.assign_mode = Some(mode);
        self
    }

    fn ctrl(mut self, ctrl: Node) -> Self {
        self.ctrl = Some(Box::new(ctrl));
        self
    }

    fn stmt(mut self, stmt: Option<Node>) -> Self {
        if let Some(node) = stmt {
            self.stmt = Some(Box::new(node));
        }
        self
    }

    fn populate_switch(&mut self) {
        // Use offset to communicate the relative position in the
        // order of appearance
        use NodeKind::*;

        if self.kind != NDSWITCH || self.stmt.is_none() {
            return;
        }

        let stmt = self.stmt.as_mut().unwrap();

        if stmt.kind == NDCASE {
            self.cases.push_back(stmt.val.unwrap());
            stmt.offset = Some(0);
        } else if stmt.kind == NDDEFAULT {
            self.has_default = true;
        } else if stmt.kind == NDBLOCK {
            let mut iter = stmt.blockstmts.iter_mut();
            let mut counter = 0;
            while let Some(stmt) = iter.next() {
                if stmt.kind == NDCASE {
                    let val = stmt.val.unwrap();
                    // TODO: Raise error if two cases with same val
                    self.cases.push_back(val);
                    stmt.offset = Some(counter);
                    counter += 1;
                }
                if stmt.kind == NDDEFAULT {
                    self.has_default = true;
                }
            }
        }
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
                use AssignMode::*;
                let mode = self.assign_mode.unwrap();

                let lhs = self.lhs.as_mut().unwrap();
                let rhs = self.rhs.as_mut().unwrap();
                lhs.populate_ty();
                rhs.populate_ty();

                let l_ty = lhs.ty.as_ref().unwrap();
                if mode == ADD || mode == SUB {
                    if l_ty.kind.is_ptr_like() {
                        self.scale_lhs = Some(true);
                    }
                }
                Some(l_ty.clone())
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

impl LVarScopes {
    fn new() -> Self {
        LVarScopes {
            list: LinkedList::new(),
            level: 0,
            offset: 0,
        }
    }

    fn add_scope(&mut self) {
        self.level += 1;
    }

    fn remove_scope(&mut self) {
        // Pop everything in this scope
        while let Some(ref var) = self.list.front() {
            if var.scope.unwrap() != self.level {
                break;
            }
            self.list.pop_front();
        }
        self.level -= 1;
    }

    fn register_lvar(&mut self, ident_name: String, ty: Type) -> Var {
        let requested_size = ty.total_size();
        self.offset += requested_size;
        let my_ofs = self.offset;

        let lvar = Var {
            name: ident_name,
            ty: ty,
            offset: Some(my_ofs),
            scope: Some(self.level),
        };
        self.list.push_front(lvar.clone());
        lvar
    }

    fn find_lvar(&self, ident_name: &str) -> Option<&Var> {
        // Notice that this finds the ident of the closest scope
        self.list.iter().find(|x| x.name == ident_name)
    }
}

impl TypeKind {
    fn from_str(s: String) -> Self {
        match s.as_str() {
            "char" => TypeKind::CHAR,
            "int" => TypeKind::INT,
            "short" => TypeKind::SHORT,
            _ => {
                panic!("Cannot convert this token to TypeKind.");
            }
        }
    }
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
            CHAR => 1,
            SHORT => 2,
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
            CHAR | SHORT | INT | LONG | PTR => self.size(),
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

impl Parser {
    pub fn new(iter: TokenIter) -> Self {
        Parser {
            iter: iter,
            globals: LinkedList::new(),
            locals: LVarScopes::new(),
            literals: LinkedList::new(),
        }
    }

    pub fn parse(mut self) -> Program {
        let nodes = self.program();
        Program {
            nodes: nodes,
            globals: self.globals,
            literals: self.literals,
        }
    }

    // TODO: Currently only used by funcdef
    fn lvar_def(&mut self) -> Option<Var> {
        if let Some(tkkind) = self.iter.consume_type() {
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
                let var_type = Type::new_array(TypeKind::from_str(tkkind), refs, array_size);
                self.iter.expect("]");
                Some(self.add_lvar(ident, var_type))
            } else {
                let var_type = Type::new(TypeKind::from_str(tkkind), refs);
                Some(self.add_lvar(ident, var_type))
            }
        } else {
            None
        }
    }

    // program = external_decl*
    fn program(&mut self) -> LinkedList<Node> {
        let mut nodes = LinkedList::new();
        while !self.iter.at_eof() {
            if let Some(node) = self.external_decl() {
                nodes.push_back(node);
            }
        }
        nodes
    }

    // external_decl = funcdef | decl
    fn external_decl(&mut self) -> Option<Node> {
        if self.iter.is_func() {
            return self.funcdef();
        } else {
            self.decl(true);
            return None;
        }
    }

    // decl = decl_spec (init_decl ("," init_decl)*)? ";"
    fn decl(&mut self, is_global: bool) -> Option<Node> {
        use NodeKind::*;

        let kind = self.decl_spec();

        if kind.is_none() {
            if is_global {
                panic!("Expected type specifier")
            } else {
                return None
            }
        }

        let mut node = Node::new(NDDECL, None, None);

        self.init_decl(&mut node, is_global, kind.unwrap());
        while self.iter.consume(",") {
            self.init_decl(&mut node, is_global, kind.unwrap());
        }

        self.iter.expect(";");
        Some(node)
    }

    // decl_spec  
    fn decl_spec(&mut self) -> Option<TypeKind> {
        let kindstr;
        if let Some(s) = self.iter.consume_type() {
            kindstr = s;
        } else {
            return None;
        }
        
        let kind = TypeKind::from_str(kindstr);
        Some(kind)
    }

    // init_decl = "*"* ident ("[" num "]")? ("=" initializer )?
    fn init_decl(&mut self, declnode: &mut Node, is_global: bool, kind: TypeKind) {
        let refs = {
            // # of times * occurs will tell us the depth of references
            let mut tmp = 0;
            while self.iter.consume("*") {
                tmp += 1;
            }
            tmp
        };

        let ident_name = self.iter.expect_ident();
        let var_type;
        if self.iter.consume("[") {
            // This is an array
            let array_size = self.iter.expect_number() as usize;
            var_type = Type::new_array(kind, refs, array_size);
            self.iter.expect("]");
        } else {
            var_type = Type::new(kind, refs);
        }

        if is_global {
            self.add_gvar(ident_name, var_type.clone());
        } else {
            let var = self.add_lvar(ident_name, var_type.clone());
            if self.iter.consume("=") {
                self.initializer(declnode, var);
            }
        }
    }

    // declarator: "*"*
    fn declarator(&mut self) {}

    fn init_array_lhs(pos: usize, var: &Var) -> Node {
        use NodeKind::*;

        let mut lhs = Node::new(NDLVAR, None, None)
            .offset(var.offset.unwrap())
            .ty(var.ty.clone());
        let rhs = Node::new(NDNUM, None, None)
            .val(pos as i32)
            .ty(Type::new(TypeKind::INT, 0));
        lhs = Node::new(NDADD, Some(Box::new(lhs)), Some(Box::new(rhs)));
        lhs = Node::new(NDDEREF, Some(Box::new(lhs)), None);
        lhs
    }

    // initializer = assign | "{" ( assign "," )* "}"
    fn initializer(&mut self, declnode: &mut Node, var: Var) {
        use NodeKind::*;

        let is_init_list = self.iter.consume("{");
        let mut pos: usize = 0;

        let lhs = if var.ty.kind == TypeKind::ARRAY {
            Parser::init_array_lhs(pos, &var)
        } else {
            Node::new(NDLVAR, None, None)
                .offset(var.offset.unwrap())
                .ty(var.ty.clone())
        };
        let mut init = Node::new_assign(
            AssignMode::DEFAULT,
            Some(Box::new(lhs)),
            Some(Box::new(self.assign())),
        );
        init.populate_ty();
        declnode.inits.push_back(init);

        if !is_init_list {
            return;
        }

        pos += 1;
        while self.iter.consume(",") {
            if var.ty.kind != TypeKind::ARRAY || pos * var.ty.base_size() >= var.ty.total_size() {
                Parser::print_warn("Excess elements in initializer will be ignored.");
                self.assign();
                continue;
            }
            let mut init = Node::new_assign(
                AssignMode::DEFAULT,
                Some(Box::new(Parser::init_array_lhs(pos, &var))),
                Some(Box::new(self.assign())),
            );
            init.populate_ty();
            declnode.inits.push_back(init);
            pos += 1;
        }

        self.iter.expect("}");
    }

    // funcdef =  "int" * ident "(" (lvar_def ",")* ")" "{" stmt* "}"
    fn funcdef(&mut self) -> Option<Node> {
        use NodeKind::*;

        let kind = TypeKind::from_str(self.iter.expect_type());
        let refs = {
            // # of times * occurs will tell us the depth of references
            let mut tmp = 0;
            while self.iter.consume("*") {
                tmp += 1;
            }
            tmp
        };

        let ident_name = self.iter.expect_ident();

        self.iter.expect("(");

        let mut node = Node::new(NDFUNCDEF, None, None).name(ident_name.to_string());
        // Create a new scope:
        self.locals = LVarScopes::new();

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
        node = node.lvars_offset(self.locals.offset);

        Some(node)
    }

    // stmt = decl
    //      | labeled
    //      | compound
    //      | select
    //      | iter
    //      | jump
    //      | expr? ";"
    fn stmt(&mut self) -> Option<Node> {
        let node = if let Some(decl) = self.decl(false) {
            Some(decl)
        } else if let Some(labeled) = self.labeled() {
            Some(labeled)
        } else if let Some(compound) = self.compound() {
            Some(compound)
        } else if let Some(select) = self.select() {
            Some(select)
        } else if let Some(iter) = self.iter() {
            Some(iter)
        } else if let Some(jump) = self.jump() {
            Some(jump)
        } else {
            if self.iter.consume(";") {
                None
            } else {
                let node = self.expr();
                self.iter.expect(";");
                Some(node)
            }
        };
        node
    }

    // labeled = "case" num ":" stmt // TODO: constexpr
    //         | "default" ":" stmt
    fn labeled(&mut self) -> Option<Node> {
        use NodeKind::*;

        let node = if self.iter.consume("case") {
            let cond = self.iter.expect_number();
            self.iter.expect(":");
            let then = self.stmt();
            Some(Node::new(NDCASE, None, None).ifnode(then).val(cond as i32))
        } else if self.iter.consume("default") {
            self.iter.expect(":");
            let stmt = self.stmt();
            Some(Node::new(NDDEFAULT, None, None).stmt(stmt))
        } else {
            None
        };
        node
    }

    // compound = "{" stmt* "}"
    fn compound(&mut self) -> Option<Node> {
        use NodeKind::*;
        if self.iter.consume("{") {
            let mut node = Node::new(NDBLOCK, None, None);

            self.locals.add_scope();
            while !self.iter.consume("}") {
                node = node.blockstmt(self.stmt());
            }
            self.locals.remove_scope();
            Some(node)
        } else {
            None
        }
    }

    // select = "if" "(" expr ")" stmt ("else" stmt)?
    //        | "switch" "(" expr ")" stmt
    fn select(&mut self) -> Option<Node> {
        use NodeKind::*;

        let mut node;
        if self.iter.consume("if") {
            self.iter.expect("(");
            node = Node::new(NDIF, None, None).cond(Some(Box::new(self.expr())));
            self.iter.expect(")");
            node = node.ifnode(self.stmt());

            // Else statement
            if self.iter.consume("else") {
                node = node.elsenode(self.stmt());
            }
        } else if self.iter.consume("switch") {
            self.iter.expect("(");
            let ctrl = self.expr();
            self.iter.expect(")");
            let stmt = self.stmt();
            node = Node::new(NDSWITCH, None, None).ctrl(ctrl).stmt(stmt);
            node.populate_switch();
        } else {
            return None;
        }

        Some(node)
    }

    // iter = "while" "(" expr ")" stmt
    //      | "do" stmt while "(" expr ")" ";"
    //      | "for" "(" expr? ";" expr? ";" expr? ")" stmt
    fn iter(&mut self) -> Option<Node> {
        use NodeKind::*;

        let mut node;
        if self.iter.consume("while") {
            self.iter.expect("(");
            node = Node::new(NDWHILE, None, None).cond(Some(Box::new(self.expr())));
            self.iter.expect(")");
            node = node.repnode(self.stmt());
        } else if self.iter.consume("do") {
            node = Node::new(NDDOWHILE, None, None);
            let repnode = self.stmt();
            self.iter.expect("while");
            self.iter.expect("(");
            let cond = self.expr();
            self.iter.expect(")");
            self.iter.expect(";");
            node = node.repnode(repnode).cond(Some(Box::new(cond)));
        } else if self.iter.consume("for") {
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
            node = node.repnode(self.stmt());
        } else {
            return None;
        }
        Some(node)
    }

    // jump = "break" ";"
    //      | "continue" ";"
    //      | "return" expr ";"
    // TODO: No expr version
    fn jump(&mut self) -> Option<Node> {
        use NodeKind::*;
        if self.iter.consume("break") {
            self.iter.expect(";");
            Some(Node::new(NDBREAK, None, None))
        } else if self.iter.consume("continue") {
            self.iter.expect(";");
            Some(Node::new(NDCONTINUE, None, None))
        } else if self.iter.consume("return") {
            let node = Node::new(NDRETURN, Some(Box::new(self.expr())), None);
            self.iter.expect(";");
            Some(node)
        } else {
            None
        }
    }

    // expr = assign
    fn expr(&mut self) -> Node {
        self.assign()
    }

    // assign = conditional (assign_op assign)?
    fn assign(&mut self) -> Node {
        use NodeKind::*;
        let mut node = self.conditional();

        if let Some(op_str) = self.iter.consume_assign_op() {
            let mode = AssignMode::from_str(&op_str);
            node = Node::new_assign(mode, Some(Box::new(node)), Some(Box::new(self.assign())))
                .eval_pre(true);
            node.populate_ty();
        }
        node
    }

    // conditional = logical_or ("?" expr ":" conditional)?
    fn conditional(&mut self) -> Node {
        use NodeKind::*;

        let mut node = self.logical_or();

        if self.iter.consume("?") {
            let truenode = Some(self.expr());
            self.iter.expect(":");
            let falsenode = Some(self.conditional());
            node = Node::new(NDTERNARY, None, None)
                .cond(Some(Box::new(node)))
                .ifnode(truenode)
                .elsenode(falsenode);
        }
        node
    }

    // logical_or = logical_and ("||" logical_and)*
    fn logical_or(&mut self) -> Node {
        use NodeKind::*;

        let mut node = self.logical_and();

        loop {
            if self.iter.consume("||") {
                node = Node::new(
                    NDLOGOR,
                    Some(Box::new(node)),
                    Some(Box::new(self.logical_and())),
                );
            } else {
                break;
            }
        }
        node
    }

    // logical_and = bitwise_or ("&&" bitwise_or)*
    fn logical_and(&mut self) -> Node {
        use NodeKind::*;

        let mut node = self.bitwise_or();

        loop {
            if self.iter.consume("&&") {
                node = Node::new(
                    NDLOGAND,
                    Some(Box::new(node)),
                    Some(Box::new(self.bitwise_or())),
                );
            } else {
                break;
            }
        }
        node
    }

    // bitwise_or = bitwise_xor ('|' bitwise_xor)*
    fn bitwise_or(&mut self) -> Node {
        use NodeKind::*;

        let mut node = self.bitwise_xor();

        loop {
            if self.iter.consume("|") {
                node = Node::new(
                    NDBITOR,
                    Some(Box::new(node)),
                    Some(Box::new(self.bitwise_xor())),
                );
            } else {
                break;
            }
        }
        node
    }

    // bitwise_xor = bitwise_and ('^' bitwise_and)*
    fn bitwise_xor(&mut self) -> Node {
        use NodeKind::*;

        let mut node = self.bitwise_and();

        loop {
            if self.iter.consume("^") {
                node = Node::new(
                    NDBITXOR,
                    Some(Box::new(node)),
                    Some(Box::new(self.bitwise_and())),
                );
            } else {
                break;
            }
        }
        node
    }

    // bitwise_and = equality ('&' equality)*
    fn bitwise_and(&mut self) -> Node {
        use NodeKind::*;

        let mut node = self.equality();

        loop {
            if self.iter.consume("&") {
                node = Node::new(
                    NDBITAND,
                    Some(Box::new(node)),
                    Some(Box::new(self.equality())),
                );
            } else {
                break;
            }
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

    // relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
    fn relational(&mut self) -> Node {
        use NodeKind::*;
        let mut node = self.shift();

        loop {
            if self.iter.consume("<") {
                node = Node::new(NDLT, Some(Box::new(node)), Some(Box::new(self.shift())));
            } else if self.iter.consume("<=") {
                node = Node::new(NDLEQ, Some(Box::new(node)), Some(Box::new(self.shift())));
            } else if self.iter.consume(">") {
                // HACK: Simply flip lhs and rhs
                node = Node::new(NDLT, Some(Box::new(self.shift())), Some(Box::new(node)));
            } else if self.iter.consume(">=") {
                node = Node::new(NDLEQ, Some(Box::new(self.shift())), Some(Box::new(node)));
            } else {
                break;
            }
        }
        node
    }

    // shift = add ("<<" add | ">>" add)*
    fn shift(&mut self) -> Node {
        use NodeKind::*;
        let mut node = self.add();

        loop {
            if self.iter.consume("<<") {
                node = Node::new(NDSHL, Some(Box::new(node)), Some(Box::new(self.add())));
            } else if self.iter.consume(">>") {
                // As per C89 6.3.7, simply performing logical right shift
                // for both signed and unsigned should be deemed comformant
                // with the standard... (Correct me if I'm wrong!)
                node = Node::new(NDSHR, Some(Box::new(node)), Some(Box::new(self.add())));
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

    // mul = unary ("*" unary | "/" unary | "%" unary)*
    fn mul(&mut self) -> Node {
        use NodeKind::*;

        let mut node = self.unary();

        loop {
            if self.iter.consume("*") {
                node = Node::new(NDMUL, Some(Box::new(node)), Some(Box::new(self.unary())));
            } else if self.iter.consume("/") {
                node = Node::new(NDDIV, Some(Box::new(node)), Some(Box::new(self.unary())));
            } else if self.iter.consume("%") {
                node = Node::new(NDMOD, Some(Box::new(node)), Some(Box::new(self.unary())));
            } else {
                break;
            }
        }
        node
    }

    // unary = "sizeof" unary
    //       | "++" unary
    //       | "--" unary
    //       | ("+" | "-" | "*" | "&" | "~") unary
    //       | postfix
    fn unary(&mut self) -> Node {
        use NodeKind::*;

        let mut node;
        if self.iter.consume("sizeof") {
            let mut lhs = self.unary();
            lhs.populate_ty();
            node = Node::new(NDNUM, None, None)
                .val(lhs.ty.unwrap().total_size() as i32)
                .ty(Type::new(TypeKind::INT, 0));
        } else if self.iter.consume("++") {
            node = Node::new_assign(
                AssignMode::ADD,
                Some(Box::new(self.unary())),
                Some(Box::new(
                    Node::new(NDNUM, None, None)
                        .val(1)
                        .ty(Type::new(TypeKind::INT, 0)),
                )),
            )
            .eval_pre(true);
            node.populate_ty();
        } else if self.iter.consume("--") {
            node = Node::new_assign(
                AssignMode::SUB,
                Some(Box::new(self.unary())),
                Some(Box::new(
                    Node::new(NDNUM, None, None)
                        .val(1)
                        .ty(Type::new(TypeKind::INT, 0)),
                )),
            )
            .eval_pre(true);
            node.populate_ty();
        } else if self.iter.consume("~") {
            node = Node::new(NDBITNOT, Some(Box::new(self.unary())), None);
        } else if self.iter.consume("*") {
            node = Node::new(NDDEREF, Some(Box::new(self.unary())), None);
            node.populate_ty();
        } else if self.iter.consume("&") {
            node = Node::new(NDADDR, Some(Box::new(self.unary())), None);
        } else if self.iter.consume("+") {
            node = self.unary();
        } else if self.iter.consume("-") {
            node = Node::new(
                NDSUB,
                Some(Box::new(
                    Node::new(NDNUM, None, None)
                        .val(0)
                        .ty(Type::new(TypeKind::INT, 0)),
                )),
                Some(Box::new(self.unary())),
            );
            node.populate_ty()
        } else {
            node = self.postfix();
        }
        node
    }

    // postfix = primary
    //         | primary '[' expr ']'
    //         | primary "++"
    //         | primary "--"
    fn postfix(&mut self) -> Node {
        use NodeKind::*;

        let mut node = self.primary();
        if self.iter.consume("[") {
            node = Node::new(NDADD, Some(Box::new(node)), Some(Box::new(self.expr())));
            node = Node::new(NDDEREF, Some(Box::new(node)), None);
            node.populate_ty();
            self.iter.expect("]");
        } else if self.iter.consume("++") {
            node = Node::new_assign(
                AssignMode::ADD,
                Some(Box::new(node)),
                Some(Box::new(
                    Node::new(NDNUM, None, None)
                        .val(1)
                        .ty(Type::new(TypeKind::INT, 0)),
                )),
            )
            .eval_pre(false);
            node.populate_ty();
        // TODO: Check lvalue
        } else if self.iter.consume("--") {
            node = Node::new_assign(
                AssignMode::SUB,
                Some(Box::new(node)),
                Some(Box::new(
                    Node::new(NDNUM, None, None)
                        .val(1)
                        .ty(Type::new(TypeKind::INT, 0)),
                )),
            )
            .eval_pre(false);
            node.populate_ty();
        }
        node
    }

    // primary = num
    //         | str
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
                let var = self.find_var(&ident);
                if let Some(offset) = var.offset {
                    // This is local
                    Node::new(NDLVAR, None, None)
                        .offset(offset)
                        .ty(var.ty.clone())
                } else {
                    Node::new(NDGVAR, None, None).name(ident).ty(var.ty.clone())
                }
            }
        } else if let Some(literal) = self.iter.consume_str() {
            let pos = self.add_literal(literal);
            Node::new(NDSTR, None, None).offset(pos)
        } else {
            // Must be NUM at this point
            Node::new(NDNUM, None, None)
                .val(self.iter.expect_number())
                .ty(Type::new(TypeKind::INT, 0))
        }
    }

    // Finds if the passed identitier already exists
    fn find_var(&mut self, ident_name: &str) -> &Var {
        // Check locals
        if let Some(ref v) = self.locals.find_lvar(ident_name) {
            return v;
        }
        // Check globals
        if let Some(ref v) = self.globals.iter().find(|x| x.name == ident_name) {
            return v;
        }

        panic!("Parser: Found an undefined variable {}\n", ident_name);
    }

    fn add_lvar(&mut self, ident_name: String, ty: Type) -> Var {
        self.locals.register_lvar(ident_name, ty)
    }

    fn add_gvar(&mut self, ident_name: String, ty: Type) {
        self.globals.push_back(Var {
            name: ident_name,
            ty: ty,
            offset: None,
            scope: None,
        });
    }

    fn add_literal(&mut self, s: String) -> usize {
        let pos = self.literals.len();
        self.literals.push_back(s);
        pos
    }

    fn print_warn(s: &str) {
        let mut msg = "Parser: Warning: ".to_string();
        msg.push_str(s);
        println!("{}", msg);
    }
}
