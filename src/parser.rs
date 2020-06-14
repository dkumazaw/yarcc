// Recursive-descent parser
use crate::node::{AssignMode, Node};
use crate::tokenizer::TokenIter;
use crate::ctype::Type;
use std::collections::LinkedList;

// Stores the name of var, its type, and stack offset if the variable is local.
#[derive(Debug, Clone)]
pub struct Var {
    pub name: String,
    pub ty: Type,
    pub offset: Option<usize>, // None if global
    pub scope: Option<usize>,  // None if global
}

#[derive(Debug, Clone)]
pub struct Tag {
    pub name: String,
    pub ty: Option<Type>,
    pub scope: Option<usize>, // None if global
}

#[derive(Debug)]
pub struct LocalScopes {
    vars: Vec<Var>,
    tags: Vec<Tag>,
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
    gtags: Vec<Tag>,
    lscopes: LocalScopes,
    literals: LinkedList<String>,
}

impl LocalScopes {
    fn new() -> Self {
        LocalScopes {
            vars: Vec::new(),
            tags: Vec::new(),
            level: 0,
            offset: 0,
        }
    }

    fn add_scope(&mut self) {
        self.level += 1;
    }

    fn remove_scope(&mut self) {
        // Pop everything in this scope
        while let Some(ref var) = self.vars.last() {
            if var.scope.unwrap() != self.level {
                break;
            }
            self.vars.pop();
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
        self.vars.push(lvar.clone());
        lvar
    }

    fn find_lvar(&self, ident_name: &str) -> Option<&Var> {
        // Notice that this finds the ident of the closest scope
        self.vars.iter().rev().find(|x| x.name == ident_name)
    }
}

impl Parser {
    pub fn new(iter: TokenIter) -> Self {
        Parser {
            iter: iter,
            globals: LinkedList::new(),
            gtags: Vec::new(),
            lscopes: LocalScopes::new(),
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
                let var_type = Type::new_array(tkkind.as_str(), refs, array_size);
                self.iter.expect("]");
                Some(self.add_lvar(ident, var_type))
            } else {
                let var_type = Type::new(tkkind.as_str(), refs);
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
            self.declaration(true);
            return None;
        }
    }

    // decl = decl_spec (init_decl ("," init_decl)*)? ";"
    // init_decl = declarator ("=" initializer)?
    // TODO: Support global variable initializer
    fn declaration(&mut self, is_global: bool) -> Option<Node> {
        let kind;

        if let Some(k) = self.decl_spec() {
            kind = k;
        } else {
            if is_global {
                panic!("Expected type specifier")
            } else {
                return None;
            }
        }

        if self.iter.consume(";") {
            self.warn("This is a useless empty declaration.");
            return None;
        }

        println!("{:?}", kind);
        let mut inits: LinkedList<Node> = LinkedList::new();
        loop {
            let (name, ty) = self.declarator(kind.clone());
            if is_global {
                self.add_gvar(name, ty.clone());
            } else {
                let var = self.add_lvar(name, ty.clone());
                if self.iter.consume("=") {
                    inits.append(&mut self.initializer(var));
                }
            }
            if !self.iter.consume(",") {
                break;
            }
        }
        self.iter.expect(";");
        Some(Node::new_decl(inits))
    }

    // decl_spec
    fn decl_spec(&mut self) -> Option<Type> {
        let kindstr;
        if let Some(s) = self.iter.consume_type() {
            kindstr = s;
        } else {
            return None;
        }

        match kindstr.as_str() {
            "struct" => {
                let (maybe_tag, maybe_ty) = self.struct_spec();
                println!("{:?} {:?}", maybe_tag, maybe_ty);
                match (maybe_tag, maybe_ty) {
                    (Some(tag), Some(ty)) => {
                        self.add_tag(true, tag, ty.clone());
                        Some(ty)
                    }
                    (Some(tag), None) => self.find_tag(true, tag),
                    (None, Some(_)) => {
                        self.error("Not implemented");
                    }
                    (None, None) => {
                        self.error("Expected identifier or '{'");
                    }
                }
            }
            s => Some(Type::new_base(s)),
        }
    }

    // Assumes type "struct" has already been read
    // struct-or-union-specifier
    //      = struct-or-union ident? "{" (struct-decl ";")+ "}"
    //      | struct-or-union ident
    fn struct_spec(&mut self) -> (Option<String>, Option<Type>) {
        let name = self.iter.consume_ident();
        let mut ty = None;

        if self.iter.consume("{") {
            // C89 6.5.2.1 stipulates that an empty struct-decl shall
            // result in undefined behavior, so I'm just going to enforce
            // 1+ members here.
            let mut size = 0;
            let mut fields: Vec<Box<(String, Type)>> = Vec::new();
            loop {
                let (name, ty) = self.struct_declaration();
                size += ty.total_size();
                fields.push(Box::new((name, ty)));
                if self.iter.consume("}") {
                    break;
                }
            }
            ty = Some(Type::STRUCT { size, fields });
        }

        (name, ty)
    }

    // struct_declaration = decl_spec declarator ";"
    // TODO: Support bitfield etc
    // TODO: Can have comma separated declarator...
    fn struct_declaration(&mut self) -> (String, Type) {
        let base = self.decl_spec();
        let (name, ty) = self.declarator(base.unwrap());
        self.iter.expect(";");
        (name, ty)
    }

    // declarator = "*"* ident ("[" num "]")?
    fn declarator(&mut self, basety: Type) -> (String, Type) {
        let refs = {
            let mut tmp = 0;
            while self.iter.consume("*") {
                tmp += 1;
            }
            tmp
        };

        let ident_name = self.iter.expect_ident();
        let var_type = if self.iter.consume("[") {
            // This is an array
            let array_size = self.iter.expect_number() as usize;
            self.iter.expect("]");
            Type::new_array(basety.as_str(), refs, array_size)
        } else {
            Type::new_from(basety, refs)
        };

        (ident_name, var_type)
    }

    fn init_array_lhs(pos: usize, var: &Var) -> Node {
        let mut lhs = Node::new_lvar(var.offset.unwrap(), var.ty.clone());
        let rhs = Node::new_int(pos as i32);
        lhs = Node::new_binary("+", lhs, rhs);
        lhs = Node::new_unary("*", lhs);
        lhs
    }

    // initializer = assign | "{" ( assign "," )* "}"
    fn initializer(&mut self, var: Var) -> LinkedList<Node> {
        use Type::*;

        let mut inits: LinkedList<Node> = LinkedList::new();
        let is_init_list = self.iter.consume("{");
        let mut pos: usize = 0;

        let lhs = match var.ty {
            ARRAY { .. } => Parser::init_array_lhs(pos, &var),
            _ => Node::new_lvar(var.offset.unwrap(), var.ty.clone()),
        };
        let mut init = Node::new_assign(AssignMode::DEFAULT, lhs, self.assign(), false);
        init.populate_ty();
        inits.push_back(init);

        if !is_init_list {
            return inits;
        }

        let mut warned = false;
        let is_array = if let Type::ARRAY { .. } = var.ty {
            true
        } else {
            false
        };
        pos += 1;
        while self.iter.consume(",") {
            if !is_array || pos * var.ty.base_size() >= var.ty.total_size() {
                if !warned {
                    self.warn("Excess elements in initializer for an array will be ignored.");
                    warned = true;
                }
                self.assign();
                continue;
            }
            let mut init = Node::new_assign(
                AssignMode::DEFAULT,
                Parser::init_array_lhs(pos, &var),
                self.assign(),
                false,
            );
            init.populate_ty();
            inits.push_back(init);
            pos += 1;
        }

        self.iter.expect("}");
        inits
    }

    // funcdef =  "int" * ident "(" (lvar_def ",")* ")" "{" stmt* "}"
    fn funcdef(&mut self) -> Option<Node> {
        let _kind = self.iter.expect_type();
        let refs = {
            // # of times * occurs will tell us the depth of references
            let mut tmp = 0;
            while self.iter.consume("*") {
                tmp += 1;
            }
            tmp
        };

        let ident_name = self.iter.expect_ident().to_string();
        let mut argvars: LinkedList<Var> = LinkedList::new();
        let mut stmts: LinkedList<Node> = LinkedList::new();

        self.iter.expect("(");

        // Create a new scope:
        self.lscopes = LocalScopes::new();

        if !self.iter.consume(")") {
            // There's at least one local variable definition.
            if let Some(lvar) = self.lvar_def() {
                argvars.push_back(lvar);
            } else {
                panic!("Parser: Expected local variable definition in function def.\n");
            }
            while self.iter.consume(",") {
                if let Some(lvar) = self.lvar_def() {
                    argvars.push_back(lvar);
                } else {
                    panic!("Parser: Expected local variable definition in function def.\n");
                }
            }
            self.iter.expect(")");
        }

        // Parse function body
        self.iter.expect("{");
        while !self.iter.consume("}") {
            if let Some(stmt) = self.stmt() {
                stmts.push_back(stmt);
            }
        }

        Some(Node::new_funcdef(
            ident_name,
            argvars,
            stmts,
            self.lscopes.offset,
        ))
    }

    // stmt = decl
    //      | labeled
    //      | compound
    //      | select
    //      | iter
    //      | jump
    //      | expr? ";"
    fn stmt(&mut self) -> Option<Node> {
        let node = if let Some(decl) = self.declaration(false) {
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
        let node = if self.iter.consume("case") {
            let condval = self.iter.expect_number();
            self.iter.expect(":");
            let stmt = self.stmt();
            Some(Node::new_case(condval as i32, stmt))
        } else if self.iter.consume("default") {
            self.iter.expect(":");
            let stmt = self.stmt();
            Some(Node::new_default(stmt))
        } else {
            None
        };
        node
    }

    // compound = "{" stmt* "}"
    fn compound(&mut self) -> Option<Node> {
        if self.iter.consume("{") {
            let mut stmts: LinkedList<Node> = LinkedList::new();

            self.lscopes.add_scope();
            while !self.iter.consume("}") {
                if let Some(stmt) = self.stmt() {
                    stmts.push_back(stmt);
                }
            }
            self.lscopes.remove_scope();
            Some(Node::new_block(stmts))
        } else {
            None
        }
    }

    // select = "if" "(" expr ")" stmt ("else" stmt)?
    //        | "switch" "(" expr ")" stmt
    fn select(&mut self) -> Option<Node> {
        let mut node;
        if self.iter.consume("if") {
            self.iter.expect("(");
            let cond = self.expr();
            self.iter.expect(")");

            let ifnode = self.stmt();

            let elsenode = if self.iter.consume("else") {
                self.stmt()
            } else {
                None
            };

            node = Node::new_if(cond, ifnode, elsenode);
        } else if self.iter.consume("switch") {
            self.iter.expect("(");
            let ctrl = self.expr();
            self.iter.expect(")");
            let stmt = self.stmt();

            node = Node::new_switch(ctrl, stmt);
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
        let mut node;
        if self.iter.consume("while") {
            self.iter.expect("(");
            let cond = self.expr();
            self.iter.expect(")");
            let repnode = self.stmt();
            node = Node::new_while(cond, repnode);
        } else if self.iter.consume("do") {
            let repnode = self.stmt();
            self.iter.expect("while");
            self.iter.expect("(");
            let cond = self.expr();
            self.iter.expect(")");
            self.iter.expect(";");
            node = Node::new_dowhile(cond, repnode);
        } else if self.iter.consume("for") {
            self.iter.expect("(");
            let mut init = None;
            let mut cond = None;
            let mut step = None;

            if !self.iter.consume(";") {
                init = Some(self.expr());
                self.iter.expect(";");
            }
            if !self.iter.consume(";") {
                cond = Some(self.expr());
                self.iter.expect(";");
            }
            if !self.iter.consume(")") {
                step = Some(self.expr());
                self.iter.expect(")");
            }
            let repnode = self.stmt();

            node = Node::new_for(init, cond, step, repnode);
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
        if self.iter.consume("break") {
            self.iter.expect(";");
            Some(Node::new("break"))
        } else if self.iter.consume("continue") {
            self.iter.expect(";");
            Some(Node::new("continue"))
        } else if self.iter.consume("return") {
            let node = Node::new_unary("return", self.expr());
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
        let mut node = self.conditional();

        if let Some(op_str) = self.iter.consume_assign_op() {
            let mode = AssignMode::from_str(&op_str);
            node = Node::new_assign(mode, node, self.assign(), true);
            node.populate_ty();
        }
        node
    }

    // conditional = logical_or ("?" expr ":" conditional)?
    fn conditional(&mut self) -> Node {
        let mut node = self.logical_or();

        if self.iter.consume("?") {
            let truenode = Some(self.expr());
            self.iter.expect(":");
            let falsenode = Some(self.conditional());
            panic!("Not implemented!");
            //node = Node::new(NDTERNARY, None, None)
            //    .cond(Some(Box::new(node)))
            //    .ifnode(truenode)
            //    .elsenode(falsenode);
        }
        node
    }

    // logical_or = logical_and ("||" logical_and)*
    fn logical_or(&mut self) -> Node {
        let mut node = self.logical_and();

        loop {
            if self.iter.consume("||") {
                node = Node::new_binary("||", node, self.logical_and());
            } else {
                break;
            }
        }
        node
    }

    // logical_and = bitwise_or ("&&" bitwise_or)*
    fn logical_and(&mut self) -> Node {
        let mut node = self.bitwise_or();

        loop {
            if self.iter.consume("&&") {
                node = Node::new_binary("&&", node, self.bitwise_or());
            } else {
                break;
            }
        }
        node
    }

    // bitwise_or = bitwise_xor ('|' bitwise_xor)*
    fn bitwise_or(&mut self) -> Node {
        let mut node = self.bitwise_xor();

        loop {
            if self.iter.consume("|") {
                node = Node::new_binary("|", node, self.bitwise_xor());
            } else {
                break;
            }
        }
        node
    }

    // bitwise_xor = bitwise_and ('^' bitwise_and)*
    fn bitwise_xor(&mut self) -> Node {
        let mut node = self.bitwise_and();

        loop {
            if self.iter.consume("^") {
                node = Node::new_binary("^", node, self.bitwise_and());
            } else {
                break;
            }
        }
        node
    }

    // bitwise_and = equality ('&' equality)*
    fn bitwise_and(&mut self) -> Node {
        let mut node = self.equality();

        loop {
            if self.iter.consume("&") {
                node = Node::new_binary("&", node, self.equality());
            } else {
                break;
            }
        }
        node
    }

    // equality = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> Node {
        let mut node = self.relational();

        loop {
            if self.iter.consume("==") {
                node = Node::new_binary("==", node, self.relational());
            } else if self.iter.consume("!=") {
                node = Node::new_binary("!=", node, self.relational());
            } else {
                break;
            }
        }
        node
    }

    // relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
    fn relational(&mut self) -> Node {
        let mut node = self.shift();

        loop {
            if self.iter.consume("<") {
                node = Node::new_binary("<", node, self.shift());
            } else if self.iter.consume("<=") {
                node = Node::new_binary("<=", node, self.shift());
            } else if self.iter.consume(">") {
                // HACK: Simply flip lhs and rhs
                node = Node::new_binary("<", self.shift(), node);
            } else if self.iter.consume(">=") {
                node = Node::new_binary("<=", self.shift(), node);
            } else {
                break;
            }
        }
        node
    }

    // shift = add ("<<" add | ">>" add)*
    fn shift(&mut self) -> Node {
        let mut node = self.add();

        loop {
            if self.iter.consume("<<") {
                node = Node::new_binary("<<", node, self.add());
            } else if self.iter.consume(">>") {
                // As per C89 6.3.7, simply performing logical right shift
                // for both signed and unsigned should be deemed comformant
                // with the standard... (Correct me if I'm wrong!)
                node = Node::new_binary(">>", node, self.add());
            } else {
                break;
            }
        }
        node
    }

    // add = mul ("+" mul | "-" mul)*
    fn add(&mut self) -> Node {
        let mut node = self.mul();

        loop {
            if self.iter.consume("+") {
                node = Node::new_binary("+", node, self.mul());
                node.populate_ty();
            } else if self.iter.consume("-") {
                node = Node::new_binary("-", node, self.mul());
                node.populate_ty();
            } else {
                break;
            }
        }

        node
    }

    // mul = unary ("*" unary | "/" unary | "%" unary)*
    fn mul(&mut self) -> Node {
        let mut node = self.unary();

        loop {
            if self.iter.consume("*") {
                node = Node::new_binary("*", node, self.unary());
            } else if self.iter.consume("/") {
                node = Node::new_binary("/", node, self.unary());
            } else if self.iter.consume("%") {
                node = Node::new_binary("%", node, self.unary());
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
        let mut node;
        if self.iter.consume("sizeof") {
            let mut lhs = self.unary();
            lhs.populate_ty();
            node = Node::new_int(lhs.ty.unwrap().total_size() as i32);
        } else if self.iter.consume("++") {
            node = Node::new_assign(AssignMode::ADD, self.unary(), Node::new_int(1), true);
            node.populate_ty();
        } else if self.iter.consume("--") {
            node = Node::new_assign(AssignMode::SUB, self.unary(), Node::new_int(1), true);
            node.populate_ty();
        } else if self.iter.consume("~") {
            node = Node::new_unary("~", self.unary());
        } else if self.iter.consume("*") {
            node = Node::new_unary("*", self.unary());
            node.populate_ty();
        } else if self.iter.consume("&") {
            node = Node::new_unary("&", self.unary());
        } else if self.iter.consume("+") {
            node = self.unary();
        } else if self.iter.consume("-") {
            node = Node::new_binary("-", Node::new_int(0), self.unary());
            node.populate_ty()
        } else {
            node = self.postfix();
        }
        node
    }

    // postfix = primary
    //         | primary '[' expr ']'
    //         | primary "." ident
    //         | primary "++"
    //         | primary "--"
    fn postfix(&mut self) -> Node {
        let mut node = self.primary();
        if self.iter.consume("[") {
            node = Node::new_binary("+", node, self.expr());
            node = Node::new_unary("*", node);
            node.populate_ty();
            self.iter.expect("]");
        } else if self.iter.consume("++") {
            node = Node::new_assign(AssignMode::ADD, node, Node::new_int(1), false);
            node.populate_ty();
        // TODO: Check lvalue
        } else if self.iter.consume("--") {
            node = Node::new_assign(AssignMode::SUB, node, Node::new_int(1), false);
            node.populate_ty();
        }
        node
    }

    // primary = num
    //         | str
    //         | ident ("(" (expr, )* ")")?
    //         | "(" expr ")"
    fn primary(&mut self) -> Node {
        if self.iter.consume("(") {
            let node = self.expr();
            self.iter.expect(")");
            node
        } else if let Some(ident) = self.iter.consume_ident() {
            if self.iter.consume("(") {
                // This is a function call
                let mut remaining = 6;
                let mut args: LinkedList<Node> = LinkedList::new();

                if self.iter.consume(")") {
                    // No argument case
                    return Node::new_call(ident, args);
                }

                remaining -= 1;
                // Handle the 1st arg
                args.push_back(self.expr());

                while self.iter.consume(",") {
                    if remaining == 0 {
                        panic!("Parser: Func arg exceeded the max. number of args supported.");
                    }
                    remaining -= 1;
                    args.push_back(self.expr());
                }
                self.iter.expect(")");

                Node::new_call(ident, args)
            } else {
                // This is a variable
                let var = self.find_var(&ident);
                if let Some(offset) = var.offset {
                    // This is local
                    Node::new_lvar(offset, var.ty.clone())
                } else {
                    Node::new_gvar(ident, var.ty.clone())
                }
            }
        } else if let Some(literal) = self.iter.consume_str() {
            let pos = self.add_literal(literal);
            Node::new_str(pos)
        } else {
            // Must be NUM at this point
            Node::new_int(self.iter.expect_number())
        }
    }

    // Finds if the passed identitier already exists
    fn find_var(&mut self, ident_name: &str) -> &Var {
        // Check locals
        if let Some(ref v) = self.lscopes.find_lvar(ident_name) {
            return v;
        }
        // Check globals
        if let Some(ref v) = self.globals.iter().find(|x| x.name == ident_name) {
            return v;
        }

        panic!("Parser: Found an undefined variable {}\n", ident_name);
    }

    // TODO These functions shouldn't be Parser's methods...
    fn add_tag(&mut self, is_global: bool, tag: String, ty: Type) {
        if is_global {
            self.gtags.push(Tag {
                name: tag,
                ty: Some(ty),
                scope: None,
            })
        }
    }

    fn find_tag(&mut self, is_global: bool, name: String) -> Option<Type> {
        if is_global {
            if let Some(ref tag) = self.gtags.iter().rev().find(|x| x.name == name) {
                return Some(tag.ty.as_ref().unwrap().clone());
            }
        }
        None
    }

    fn add_lvar(&mut self, ident_name: String, ty: Type) -> Var {
        self.lscopes.register_lvar(ident_name, ty)
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

    fn error(&self, s: &str) -> ! {
        let mut msg = "error: ".to_string();
        msg.push_str(s);
        panic!("{}", msg);
    }

    fn warn(&self, s: &str) {
        let mut msg = "warning: ".to_string();
        msg.push_str(s);
        println!("{}", msg);
    }
}
