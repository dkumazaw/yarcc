// Variables, literals, tags, scopes
use crate::ctype::Type;
use std::collections::VecDeque;

#[derive(Debug)]
pub struct Env {
    pub literals: VecDeque<String>,
    pub scopes: Scopes,
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: String,
    pub ty: Type,
    pub offset: Option<usize>, // None if global
    pub scope: usize,          // 0 if global
}

#[derive(Debug, Clone)]
pub struct Tag {
    pub name: String,
    pub ty: Option<Type>, // None if incomplete
    pub scope: usize,     // 0 if global
}

impl Env {
    pub fn new() -> Self {
        Env {
            literals: VecDeque::new(),
            scopes: Scopes::new(),
        }
    }

    // Literals
    pub fn add_literal(&mut self, s: String) -> usize {
        let pos = self.literals.len();
        self.literals.push_back(s);
        pos
    }

    pub fn get_symbols(self) -> (Vec<Var>, VecDeque<String>) {
        if self.scopes.level != 0 {
            panic!("Trying to exit env from non-global level.")
        }
        (self.scopes.vars, self.literals)
    }
}

#[derive(Debug)]
pub struct Scopes {
    vars: Vec<Var>,
    tags: Vec<Tag>,
    level: usize, // Current scope's level; 0 when global
    offset: usize,
}

impl Scopes {
    pub fn new() -> Self {
        Scopes {
            vars: Vec::new(),
            tags: Vec::new(),
            level: 0,
            offset: 0,
        }
    }

    // Scopes
    /// Adds a new scope
    pub fn add_scope(&mut self) {
        self.level += 1;
    }

    /// Removes current scope.
    /// Returns offset only if exiting local context, i.e. level 1 -> 0
    pub fn remove_scope(&mut self) -> Option<usize> {
        // Pop everything in this scope
        while let Some(ref var) = self.vars.last() {
            if var.scope != self.level {
                break;
            }
            self.vars.pop();
        }
        while let Some(ref tag) = self.tags.last() {
            if tag.scope != self.level {
                break;
            }
            self.tags.pop();
        }

        self.level -= 1;
        // Check if we just moved out of local context
        let retofs = if self.level == 0 {
            let tmp = Some(self.offset);
            self.offset = 0;
            tmp
        } else {
            None
        };
        retofs
    }

    // Vars
    pub fn add_var(&mut self, ident_name: String, ty: Type) -> Var {
        let offset = if self.level > 0 {
            // This is local; perform the computation
            let requested_size = ty.total_size();
            self.offset += requested_size;
            Some(self.offset)
        } else {
            None
        };

        let var = Var {
            name: ident_name,
            ty: ty,
            offset: offset,
            scope: self.level,
        };
        self.vars.push(var.clone());
        var
    }

    pub fn find_var(&self, ident_name: &str) -> Option<&Var> {
        // Notice that this finds the ident of the closest scope
        self.vars.iter().rev().find(|x| x.name == ident_name)
    }

    // Tags
    /// Adds a tag as an incomplete type
    /// No-op if the identically named tag exists in the same scope.
    pub fn init_tag(&mut self, name: String) {
        if let Some(tag) = self.find_tag(name.as_str()) {
            if tag.scope == self.level {
                return;
            }
        }
        self.tags.push(Tag {
            name: name,
            ty: None,
            scope: self.level,
        });
    }

    pub fn update_tag(&mut self, name: &str, ty: Type) {
        // rev ensures that the right tag gets updated.
        // e.g. imagine you have "struct a" both in global and local contexts!
        if let Some(ref mut tag) = self.tags.iter_mut().rev().find(|x| x.name == name) {
            if !tag.ty.is_none() {
                panic!("This tag is already complete.")
            }
            tag.ty = Some(ty)
        } else {
            panic!("Trying to update a tag that is not present.")
        }
    }

    /// Finds the type of tag.
    pub fn find_tag(&mut self, name: &str) -> Option<&Tag> {
        self.tags.iter().rev().find(|x| x.name == name)
    }
}
