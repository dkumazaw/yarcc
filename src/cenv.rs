// Variables, literals, tags, scopes
use crate::ctype::Type;
use std::collections::VecDeque;

#[derive(Debug)]
pub struct Env {
    pub globals: VecDeque<Var>,
    pub literals: VecDeque<String>,
    pub tags: Vec<Tag>,
    lscopes: Option<LocalScopes>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            globals: VecDeque::new(),
            literals: VecDeque::new(),
            tags: Vec::new(),
            lscopes: None,
        }
    }

    // Scopes
    pub fn init_scopes(&mut self) {
        self.lscopes = Some(LocalScopes::new())
    }

    pub fn add_scope(&mut self) {
        self.lscopes.as_mut().unwrap().add_scope();
    }

    pub fn remove_scope(&mut self) {
        self.lscopes.as_mut().unwrap().remove_scope();
    }

    // Returns the total space needed to store local variables
    pub fn close_scopes(&mut self) -> usize {
        let off = self.lscopes.as_ref().unwrap().offset;
        self.lscopes = None;
        off
    }

    // Variables
    pub fn add_var(&mut self, is_global: bool, ident: String, ty: Type) -> Var {
        if is_global {
            let var = Var {
                name: ident,
                ty: ty,
                offset: None,
                scope: None,
            };
            self.globals.push_back(var.clone());
            var
        } else {
            self.lscopes.as_mut().unwrap().add_lvar(ident, ty)
        }
    }

    pub fn find_var(&mut self, ident: &str) -> &Var {
        // Check locals first
        if let Some(ref v) = self.lscopes.as_mut().unwrap().find_lvar(ident) {
            return v;
        }
        // Then check globals
        if let Some(ref v) = self.globals.iter().find(|x| x.name == ident) {
            return v;
        }

        panic!("Found an undeclared variable {}\n", ident);
    }

    // Tags
    pub fn add_tag(&mut self, is_global: bool, tag: String, ty: Type) {
        if is_global {
            self.tags.push(Tag {
                name: tag,
                ty: Some(ty),
                scope: None,
            })
        }
    }

    pub fn find_tag(&mut self, is_global: bool, name: String) -> Option<Type> {
        if is_global {
            if let Some(ref tag) = self.tags.iter().rev().find(|x| x.name == name) {
                return Some(tag.ty.as_ref().unwrap().clone());
            }
        }
        None
    }

    // Literals
    pub fn add_literal(&mut self, s: String) -> usize {
        let pos = self.literals.len();
        self.literals.push_back(s);
        pos
    }
}

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
struct LocalScopes {
    vars: Vec<Var>,
    tags: Vec<Tag>,
    level: usize, // Current scope's level
    offset: usize,
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

    fn add_lvar(&mut self, ident_name: String, ty: Type) -> Var {
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
