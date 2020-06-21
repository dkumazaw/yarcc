// Recursive-descent parser
use crate::cenv::{Env, Var};
use crate::ctype::{EnumMember, IncompleteKind, StructMember, Type, TypeConfig};
use crate::node::{AssignMode, Node};
use crate::tokenizer::{TokenIter, TokenKind};
use std::collections::{LinkedList, VecDeque};

// Parser returns this context;
// codegen should use this context to produce code
pub struct Program {
    pub nodes: LinkedList<Node>,
    pub globals: Vec<Var>,
    pub literals: VecDeque<String>,
}

pub struct Parser {
    iter: TokenIter,
    env: Env,
}

impl Parser {
    pub fn new(iter: TokenIter) -> Self {
        Parser {
            iter: iter,
            env: Env::new(),
        }
    }

    pub fn parse(mut self) -> Program {
        let nodes = self.program();
        let (g, l) = self.env.get_symbols();
        Program {
            nodes: nodes,
            globals: g,
            literals: l,
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
    // This handles the shared part of funcdef and decl
    // and delegates the rest of the work to the respective funcitons.
    fn external_decl(&mut self) -> Option<Node> {
        let basety = match self.decl_spec() {
            Some(t) => t,
            None => self.error("Expected type specifier"),
        };

        if self.iter.consume(";") {
            if !basety.is_struct() {
                self.warn("This is a useless empty declaration.");
            }
            // TODO: Clean this up
            return None;
        }

        let (name, ty) = self.declarator(basety.clone());
        if self.iter.consume("{") {
            self.funcdef(name, ty)
        } else {
            self.global_declaration(name, ty, basety);
            None
        }
    }

    // decl = decl_spec (init_decl ("," init_decl)*)? ";"
    // init_decl = declarator ("=" initializer)?
    // TODO: Support global var init
    fn global_declaration(&mut self, name: String, ty: Type, basety: Type) {
        // Pick up from the first declarator
        let mut _inits: LinkedList<Node> = LinkedList::new();

        if ty.is_function() {
            self.env.add_prototype(name, ty);
        } else {
            let var = self.env.scopes.add_var(name, ty);
            if self.iter.consume("=") {
                _inits.append(&mut self.initializer(var));
            }
        }

        while self.iter.consume(",") {
            let (name, ty) = self.declarator(basety.clone());

            if ty.is_function() {
                self.env.add_prototype(name, ty);
            } else {
                let var = self.env.scopes.add_var(name, ty);
                if self.iter.consume("=") {
                    _inits.append(&mut self.initializer(var));
                }
            }
        }
        self.iter.expect(";");
    }

    // decl = decl_spec (init_decl ("," init_decl)*)? ";"
    // init_decl = declarator ("=" initializer)?
    fn local_declaration(&mut self) -> Option<Node> {
        let basety = match self.decl_spec() {
            Some(t) => t,
            None => {
                return None;
            }
        };

        let mut inits: LinkedList<Node> = LinkedList::new();

        if self.iter.consume(";") {
            if !basety.is_struct() {
                self.warn("This is a useless empty declaration.");
            }
            return Some(Node::new_decl(inits));
        }

        loop {
            let (name, ty) = self.declarator(basety.clone());
            let var = self.env.scopes.add_var(name, ty);
            if self.iter.consume("=") {
                inits.append(&mut self.initializer(var));
            }
            if !self.iter.consume(",") {
                break;
            }
        }
        self.iter.expect(";");
        Some(Node::new_decl(inits))
    }

    // decl_spec = (storage-class-spec | type-spec | type-qual)*
    fn decl_spec(&mut self) -> Option<Type> {
        self.storage_typespec_typequal(true)
    }

    // spec_qual = (type-spec | type-qual)*
    fn spec_qual(&mut self) -> Option<Type> {
        self.storage_typespec_typequal(false)
    }

    // Reads a storage class, type specifiers, and type qualifiers
    fn storage_typespec_typequal(&mut self, allow_storage: bool) -> Option<Type> {
        let mut maybe_ty: Option<Type> = None;
        let mut ty_config = TypeConfig::new();

        let mut is_const = false;
        let mut is_volatile = false;

        let mut no_token_read = true;

        loop {
            if let Some(tystr) = self.iter.consume_type() {
                if !maybe_ty.is_none() {
                    self.error("Trying to add an additional type to enum/string.")
                }
                match tystr.as_str() {
                    "struct" => {
                        maybe_ty = Some(self.struct_spec());
                    }
                    "enum" => {
                        maybe_ty = Some(self.enum_spec());
                    }
                    others => {
                        match ty_config.add(others) {
                            Ok(_) => (),
                            Err(msg) => self.error(msg),
                        };
                    }
                }
                no_token_read = false;
                continue;
            }

            if let Some(tcstr) = self.iter.consume_type_qual() {
                match tcstr.as_str() {
                    "const" => {
                        is_const = true;
                    }
                    "volatile" => {
                        is_volatile = true;
                    }
                    _ => panic!("Invalid type qualifier found..."),
                }
                no_token_read = false;
                continue;
            }
            break;
        }

        if no_token_read {
            return None;
        }

        let mut ty = if let Some(t) = maybe_ty {
            t
        } else {
            match Type::new_from_config(ty_config) {
                Ok(t) => t,
                Err(msg) => self.error(msg),
            }
        };
        ty.set_type_qual(is_const, is_volatile);
        Some(ty)
    }

    // Assumes type "enum" has already been read
    // enum-spec = "enum" ident? "{" ident ("=" constexpr)? ("," ident ("=" constexpr)?)* "}"
    //           | "enum" ident
    fn enum_spec(&mut self) -> Type {
        let maybe_name: Option<String> = self.iter.consume_ident();
        let mut maybe_ty: Option<Type> = None;

        if self.iter.consume("{") {
            let mut val: i32 = 0;
            let mut members: Vec<EnumMember> = Vec::new();

            loop {
                let name = self.iter.expect_ident();
                if self.iter.consume("=") {
                    // TODO: Replace with constexpr
                    val = self.iter.expect_number();
                }
                let ec = EnumMember {
                    name: name,
                    val: val,
                };
                self.env.scopes.add_const(ec.clone());
                members.push(ec);
                val += 1;
                if self.iter.consume("}") {
                    break;
                }
                self.iter.expect(",")
            }

            maybe_ty = Some(Type::new_enum(members));
        }

        match (maybe_name, maybe_ty) {
            (Some(name), Some(ty)) => {
                self.env.scopes.add_tag(name.clone(), ty.clone());
                ty
            }
            (Some(name), None) => {
                if let Some(found_tag) = self.env.scopes.find_tag(name.as_str()) {
                    if !found_tag.ty.is_enum() {
                        self.error("This tag is not defined as enum.")
                    }
                    found_tag.ty.clone()
                } else {
                    // Define an incomplete enum
                    let ty = Type::new_incomplete(IncompleteKind::ENUM);
                    self.env.scopes.add_tag(name.clone(), ty.clone());
                    ty
                }
            }
            (None, Some(ty)) => ty,
            (None, None) => {
                self.error("Expected identifier or '{'");
            }
        }
    }

    // Assumes type "struct" has already been read
    // struct-or-union-specifier
    //      = struct-or-union ident? "{" (struct-decl ";")+ "}"
    //      | struct-or-union ident
    fn struct_spec(&mut self) -> Type {
        let maybe_name: Option<String> = self.iter.consume_ident();
        let mut maybe_ty: Option<Type> = None;

        if self.iter.consume("{") {
            if let Some(ref name) = maybe_name {
                // Add itself as an incomplete type
                let incomplete = Type::new_incomplete(IncompleteKind::STRUCT);
                self.env.scopes.add_tag(name.clone(), incomplete);
            }
            // C89 6.5.2.1 stipulates that an empty struct-decl shall
            // result in undefined behavior, so I'm just going to enforce
            // 1+ members here.
            let mut size = 0;
            let mut members: Vec<StructMember> = Vec::new();
            loop {
                let mut decls = self.struct_declaration();
                while let Some((name, ty)) = decls.pop_front() {
                    let mysize = ty.total_size();
                    members.push(StructMember {
                        name: name,
                        ty: ty,
                        offset: size,
                    });
                    size += mysize;
                }
                if self.iter.consume("}") {
                    break;
                }
            }
            maybe_ty = Some(Type::new_struct(size, members));
        }

        match (maybe_name, maybe_ty) {
            (Some(name), Some(ty)) => {
                self.env.scopes.update_tag(name.as_str(), ty.clone());
                ty
            }
            (Some(name), None) => {
                if let Some(found_tag) = self.env.scopes.find_tag(name.as_str()) {
                    if !found_tag.ty.is_struct() {
                        self.error("This tag is not defined as struct.")
                    }
                    found_tag.ty.clone()
                } else {
                    // Define an incomplete struct
                    let ty = Type::new_incomplete(IncompleteKind::STRUCT);
                    self.env.scopes.add_tag(name.clone(), ty.clone());
                    ty
                }
            }
            (None, Some(ty)) => ty,
            (None, None) => {
                self.error("Expected identifier or '{'");
            }
        }
    }

    // struct_declaration = spec_qual declarator ("," declarator)* ";"
    // TODO: Support bitfield etc
    fn struct_declaration(&mut self) -> VecDeque<(String, Type)> {
        let mut decls: VecDeque<(String, Type)> = VecDeque::new();
        let base = self.spec_qual().unwrap();
        loop {
            let (name, ty) = self.declarator(base.clone());
            decls.push_back((name, ty));
            if !self.iter.consume(",") {
                break;
            }
        }

        self.iter.expect(";");
        decls
    }

    // declarator =
    //      pointer (ident | "(" declarator ")") ("[" num "]" | "(" parameter-type-list? ")")?
    fn declarator(&mut self, basety: Type) -> (String, Type) {
        let basety = self.pointer(basety);
        let mut ident_name = "unseen".to_string();

        println!("I was passed {:?}", basety);
        let is_nested = if let Some(name) = self.iter.consume_ident() {
            println!("{}", name);
            ident_name = name;
            false
        } else {
            self.delay_declarator();
            true
        };

        let var_type = if self.iter.consume("[") {
            // This is an array
            let array_size = self.iter.expect_number() as usize;
            self.iter.expect("]");
            Type::new_array(basety, array_size)
        } else if self.iter.consume("(") {
            // This is a function declarator
            let args = if self.iter.consume(")") {
                Vec::new()
            } else {
                let tmp = self.parameter_type_list();
                self.iter.expect(")");
                tmp
            };
            Type::new_function(args)
        } else {
            basety
        };

        println!("Found {:?}", var_type);
        match is_nested {
            true => {
                self.iter.commit_delay();
                self.declarator(var_type)
            }
            false => (ident_name, var_type),
        }
    }

    fn delay_declarator(&mut self) {
        use TokenKind::*;

        self.iter.expect("(");
        let mut paren_cnt = 1;
        while paren_cnt > 0 {
            let tk = self.iter.peek();
            match tk.kind {
                TKRESERVED => match tk.string.as_ref().unwrap().as_str() {
                    "(" => {
                        paren_cnt += 1;
                        self.iter.delay();
                    }
                    ")" => {
                        paren_cnt -= 1;
                        if paren_cnt > 0 {
                            self.iter.delay();
                        } else {
                            self.iter.expect(")");
                        }
                    }
                    _ => {
                        self.iter.delay();
                    }
                },
                _ => {
                    self.iter.delay();
                }
            }
        }
    }

    // pointer = ("*" type-qualifier-list?)*
    fn pointer(&mut self, basety: Type) -> Type {
        let mut ty = basety;

        while self.iter.consume("*") {
            let mut is_const = false;
            let mut is_volatile = false;
            while let Some(tqstr) = self.iter.consume_type_qual() {
                match tqstr.as_str() {
                    "const" => {
                        is_const = true;
                    }
                    "volatile" => {
                        is_volatile = true;
                    }
                    _ => panic!("This should be unreacheable"),
                }
            }

            ty = Type::new_ptr(ty);
            ty.set_type_qual(is_const, is_volatile);
        }

        ty
    }

    // parameter-type-list
    //      = parameter-declaration ("," parameter-declaration)* ("," ...)?
    // TODO Support variadic fnct
    // TODO Stop if the first elem it sees is void
    fn parameter_type_list(&mut self) -> Vec<(String, Type)> {
        let mut argtypes: Vec<(String, Type)> = Vec::new();

        loop {
            let (name, ty) = self.parameter_declaration();
            argtypes.push((name, ty));

            if !self.iter.consume(",") {
                break;
            }
        }
        argtypes
    }

    // decl_spec declarator
    fn parameter_declaration(&mut self) -> (String, Type) {
        let ty = match self.decl_spec() {
            Some(t) => t,
            None => panic!("Parameter declaration expects a declaration specifier."),
        };

        self.declarator(ty)
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
        let mut inits: LinkedList<Node> = LinkedList::new();
        let is_init_list = self.iter.consume("{");
        let mut pos: usize = 0;

        let lhs = if var.ty.is_array() {
            Parser::init_array_lhs(pos, &var)
        } else {
            Node::new_lvar(var.offset.unwrap(), var.ty.clone())
        };
        let mut init = Node::new_init(AssignMode::DEFAULT, lhs, self.assign(), false);
        init.populate_ty();
        inits.push_back(init);

        if !is_init_list {
            return inits;
        }

        let mut warned = false;
        let is_array = var.ty.is_array();
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
            let mut init = Node::new_init(
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

    // funcdef = decl_spec declarator "{" stmt* "}"
    // Assumes that everything up to the first "{" has already been read
    // NOTE: K&R style definition is not supported
    fn funcdef(&mut self, ident_name: String, functy: Type) -> Option<Node> {
        let mut argvars: LinkedList<Var> = LinkedList::new();
        let mut stmts: LinkedList<Node> = LinkedList::new();

        // Create new local scopes:
        self.env.add_prototype(ident_name.clone(), functy.clone());
        let mut arg_iter = functy.iter_func_args();

        self.env.scopes.add_scope();
        while let Some((name, ty)) = arg_iter.next() {
            if ty.is_void() {
                break;
            }
            let var = self.env.scopes.add_var(name.clone(), ty.clone());
            argvars.push_back(var);
        }

        // Parse function body
        while !self.iter.consume("}") {
            if let Some(stmt) = self.stmt() {
                stmts.push_back(stmt);
            }
        }

        Some(Node::new_funcdef(
            ident_name,
            argvars,
            stmts,
            self.env.scopes.remove_scope().unwrap(),
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
        let node = if let Some(decl) = self.local_declaration() {
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

            self.env.scopes.add_scope();
            while !self.iter.consume("}") {
                if let Some(stmt) = self.stmt() {
                    stmts.push_back(stmt);
                }
            }
            self.env.scopes.remove_scope();
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
        let node;
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
        let node = self.logical_or();

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
            node.populate_ty();
        } else {
            node = self.postfix();
        }
        node
    }

    // postfix = primary ('[' expr ']' | "." ident | "->" ident | "++" | "--")*
    fn postfix(&mut self) -> Node {
        let mut node = self.primary();

        loop {
            if self.iter.consume("[") {
                node = Node::new_binary("+", node, self.expr());
                node = Node::new_unary("*", node);
                node.populate_ty();
                self.iter.expect("]");
            } else if self.iter.consume(".") {
                let ident = self.iter.expect_ident();
                node = Node::new_member(node, ident);
                node.populate_ty();
            } else if self.iter.consume("->") {
                let ident = self.iter.expect_ident();
                node = Node::new_member(Node::new_unary("*", node), ident);
                node.populate_ty();
            } else if self.iter.consume("++") {
                node = Node::new_assign(AssignMode::ADD, node, Node::new_int(1), false);
                node.populate_ty();
            // TODO: Check lvalue
            } else if self.iter.consume("--") {
                node = Node::new_assign(AssignMode::SUB, node, Node::new_int(1), false);
                node.populate_ty();
            } else {
                break;
            }
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
                if !self.env.check_prototype(&ident) {
                    self.error("This identifier has not been declared with prototype.")
                }
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
                // This is a variable or enum const
                if let Some(var) = self.env.scopes.find_var(&ident) {
                    match var.offset {
                        Some(offset) => Node::new_lvar(offset, var.ty.clone()),
                        None => Node::new_gvar(ident, var.ty.clone()),
                    }
                } else if let Some(ec) = self.env.scopes.find_const(&ident) {
                    Node::new_int(ec.member.val)
                } else {
                    println!("{}", ident);
                    self.error("Undefined identifier!");
                }
            }
        } else if let Some(literal) = self.iter.consume_str() {
            let pos = self.env.add_literal(literal);
            Node::new_str(pos)
        } else {
            // Must be NUM at this point
            Node::new_int(self.iter.expect_number())
        }
    }

    fn debug(&self, s: &str) {
        let mut msg = "debug: ".to_string();
        msg.push_str(s);
        println!("{}", msg);
    }

    fn warn(&self, s: &str) {
        let mut msg = "warning: ".to_string();
        msg.push_str(s);
        println!("{}", msg);
    }

    fn error(&self, s: &str) -> ! {
        let mut msg = "error: ".to_string();
        msg.push_str(s);
        panic!("{}", msg);
    }
}
