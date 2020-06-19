// AST node
use crate::cenv::Var;
use crate::ctype::Type;
use std::collections::LinkedList;

#[derive(Debug, Clone)]
pub struct Node {
    pub ty: Option<Type>,
    pub kind: NodeKind,
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    NDINT {
        val: i32,
    },
    NDSTR {
        pos: usize,
    },
    // Binary ops
    NDADD {
        lhs: Box<Node>,
        rhs: Box<Node>,
        scale_lhs: Option<bool>,
    }, // +
    NDSUB {
        lhs: Box<Node>,
        rhs: Box<Node>,
        scale_lhs: Option<bool>,
    }, // -
    NDMUL {
        lhs: Box<Node>,
        rhs: Box<Node>,
    }, // *
    NDDIV {
        lhs: Box<Node>,
        rhs: Box<Node>,
    }, // /
    NDMOD {
        lhs: Box<Node>,
        rhs: Box<Node>,
    }, // %
    NDEQ {
        lhs: Box<Node>,
        rhs: Box<Node>,
    }, // ==
    NDNEQ {
        lhs: Box<Node>,
        rhs: Box<Node>,
    }, // !=
    NDLEQ {
        lhs: Box<Node>,
        rhs: Box<Node>,
    }, // <=
    NDLT {
        lhs: Box<Node>,
        rhs: Box<Node>,
    }, // <
    NDASSIGN {
        lhs: Box<Node>,
        rhs: Box<Node>,
        scale_lhs: Option<bool>,
        eval_pre: bool,
        assign_mode: AssignMode,
        is_init: bool,
    }, // See AssignMode for more details
    NDBITAND {
        lhs: Box<Node>,
        rhs: Box<Node>,
    }, // &
    NDBITXOR {
        lhs: Box<Node>,
        rhs: Box<Node>,
    }, // ^
    NDBITOR {
        lhs: Box<Node>,
        rhs: Box<Node>,
    }, // |
    NDLOGAND {
        lhs: Box<Node>,
        rhs: Box<Node>,
    }, // &&
    NDLOGOR {
        lhs: Box<Node>,
        rhs: Box<Node>,
    }, // ||
    NDSHL {
        lhs: Box<Node>,
        rhs: Box<Node>,
    }, // <<
    NDSHR {
        lhs: Box<Node>,
        rhs: Box<Node>,
    }, // >>
    // Unary ops
    NDBITNOT {
        node: Box<Node>,
    }, // ~
    NDADDR {
        node: Box<Node>,
    }, // &
    NDDEREF {
        node: Box<Node>,
    }, // *
    NDRETURN {
        node: Box<Node>,
    },
    // struct member access
    NDMEMBER {
        node: Box<Node>,
        name: String,
        offset: Option<usize>,
    },
    // No operand
    NDBREAK,
    NDCONTINUE,
    // control statements
    NDIF {
        cond: Box<Node>,
        ifnode: Option<Box<Node>>,
        elsenode: Option<Box<Node>>,
    },
    NDSWITCH {
        ctrl: Box<Node>,
        stmt: Option<Box<Node>>,
        cases: LinkedList<i32>,
        has_default: bool,
    },
    NDWHILE {
        cond: Box<Node>,
        repnode: Option<Box<Node>>,
    },
    NDDOWHILE {
        cond: Box<Node>,
        repnode: Option<Box<Node>>,
    },
    NDFOR {
        init: Option<Box<Node>>,
        cond: Option<Box<Node>>,
        step: Option<Box<Node>>,
        repnode: Option<Box<Node>>,
    },
    NDBLOCK {
        stmts: LinkedList<Node>,
    },
    NDCASE {
        stmt: Option<Box<Node>>,
        val: i32,
        pos: Option<usize>,
    },
    NDDEFAULT {
        stmt: Option<Box<Node>>,
    },
    // funcs
    NDCALL {
        name: String,
        args: LinkedList<Node>,
    }, // function call
    NDFUNCDEF {
        name: String,
        argvars: LinkedList<Var>,
        stmts: LinkedList<Node>,
        lvars_offset: usize,
    }, // function definition
    // decl
    NDDECL {
        inits: LinkedList<Node>,
    }, // declaration
    // variables
    NDLVAR {
        offset: usize,
    }, // local var
    NDGVAR {
        name: String,
    }, // global var
}

impl Node {
    pub fn new_int(val: i32) -> Self {
        Node {
            ty: Some(Type::new_base("int")),
            kind: NodeKind::NDINT { val: val },
        }
    }

    pub fn new_str(pos: usize) -> Self {
        Node {
            ty: None,
            kind: NodeKind::NDSTR { pos: pos },
        }
    }

    pub fn new_binary(op: &str, lhs: Self, rhs: Self) -> Self {
        use NodeKind::*;

        let kind = match op {
            "+" => NDADD {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                scale_lhs: None,
            },
            "-" => NDSUB {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                scale_lhs: None,
            },
            "*" => NDMUL {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            "/" => NDDIV {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            "%" => NDMOD {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            "==" => NDEQ {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            "!=" => NDNEQ {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            "<=" => NDLEQ {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            "<" => NDLT {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            "&" => NDBITAND {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            "^" => NDBITXOR {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            "|" => NDBITOR {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            "&&" => NDLOGAND {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            "||" => NDLOGOR {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            "<<" => NDSHL {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            ">>" => NDSHR {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            _ => panic!("Invalid binary op"),
        };

        Node {
            ty: None,
            kind: kind,
        }
    }

    pub fn new_unary(op: &str, node: Self) -> Self {
        use NodeKind::*;

        let kind = match op {
            "~" => NDBITNOT {
                node: Box::new(node),
            },
            "&" => NDADDR {
                node: Box::new(node),
            },
            "*" => NDDEREF {
                node: Box::new(node),
            },
            "return" => NDRETURN {
                node: Box::new(node),
            },
            _ => panic!("Invalid unary op"),
        };

        Node {
            ty: None,
            kind: kind,
        }
    }

    pub fn new(op: &str) -> Self {
        use NodeKind::*;

        let kind = match op {
            "break" => NDBREAK,
            "continue" => NDCONTINUE,
            _ => panic!("Invalid no operand type"),
        };

        Node {
            ty: None,
            kind: kind,
        }
    }

    pub fn new_init(mode: AssignMode, lhs: Self, rhs: Self, eval_pre: bool) -> Self {
        Node {
            ty: None,
            kind: NodeKind::NDASSIGN {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                scale_lhs: None,
                eval_pre: eval_pre,
                assign_mode: mode,
                is_init: true,
            },
        }
    }

    pub fn new_assign(mode: AssignMode, lhs: Self, rhs: Self, eval_pre: bool) -> Self {
        Node {
            ty: None,
            kind: NodeKind::NDASSIGN {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                scale_lhs: None,
                eval_pre: eval_pre,
                assign_mode: mode,
                is_init: false,
            },
        }
    }

    pub fn new_if(cond: Self, ifnode: Option<Self>, elsenode: Option<Self>) -> Self {
        let if_to_use = if let Some(n) = ifnode {
            Some(Box::new(n))
        } else {
            None
        };
        let else_to_use = if let Some(n) = elsenode {
            Some(Box::new(n))
        } else {
            None
        };
        Node {
            ty: None,
            kind: NodeKind::NDIF {
                cond: Box::new(cond),
                ifnode: if_to_use,
                elsenode: else_to_use,
            },
        }
    }

    pub fn new_switch(ctrl: Self, stmt: Option<Self>) -> Self {
        let stmt_to_use = if let Some(n) = stmt {
            Some(Box::new(n))
        } else {
            None
        };

        Node {
            ty: None,
            kind: NodeKind::NDSWITCH {
                ctrl: Box::new(ctrl),
                stmt: stmt_to_use,
                cases: LinkedList::new(),
                has_default: false,
            },
        }
    }

    pub fn new_while(cond: Self, repnode: Option<Self>) -> Self {
        let repnode_to_use = if let Some(n) = repnode {
            Some(Box::new(n))
        } else {
            None
        };
        Node {
            ty: None,
            kind: NodeKind::NDWHILE {
                cond: Box::new(cond),
                repnode: repnode_to_use,
            },
        }
    }

    // TODO: COnsolidate with above
    pub fn new_dowhile(cond: Self, repnode: Option<Self>) -> Self {
        let repnode_to_use = if let Some(n) = repnode {
            Some(Box::new(n))
        } else {
            None
        };
        Node {
            ty: None,
            kind: NodeKind::NDDOWHILE {
                cond: Box::new(cond),
                repnode: repnode_to_use,
            },
        }
    }

    pub fn new_for(
        init: Option<Self>,
        cond: Option<Self>,
        step: Option<Self>,
        repnode: Option<Self>,
    ) -> Self {
        // TODO: This doesn't look cute
        let init_to_use = if let Some(n) = init {
            Some(Box::new(n))
        } else {
            None
        };
        let cond_to_use = if let Some(n) = cond {
            Some(Box::new(n))
        } else {
            None
        };
        let step_to_use = if let Some(n) = step {
            Some(Box::new(n))
        } else {
            None
        };
        let repnode_to_use = if let Some(n) = repnode {
            Some(Box::new(n))
        } else {
            None
        };

        Node {
            ty: None,
            kind: NodeKind::NDFOR {
                init: init_to_use,
                cond: cond_to_use,
                step: step_to_use,
                repnode: repnode_to_use,
            },
        }
    }

    pub fn new_block(stmts: LinkedList<Self>) -> Self {
        Node {
            ty: None,
            kind: NodeKind::NDBLOCK { stmts: stmts },
        }
    }

    pub fn new_case(val: i32, stmt: Option<Self>) -> Self {
        let stmt_to_use = if let Some(n) = stmt {
            Some(Box::new(n))
        } else {
            None
        };

        Node {
            ty: None,
            kind: NodeKind::NDCASE {
                stmt: stmt_to_use,
                val: val,
                pos: None, // Sema analyzer fills this field
            },
        }
    }

    pub fn new_default(stmt: Option<Self>) -> Self {
        let stmt_to_use = if let Some(n) = stmt {
            Some(Box::new(n))
        } else {
            None
        };

        Node {
            ty: None,
            kind: NodeKind::NDDEFAULT { stmt: stmt_to_use },
        }
    }

    pub fn new_call(name: String, args: LinkedList<Self>) -> Self {
        Node {
            ty: None,
            kind: NodeKind::NDCALL {
                name: name,
                args: args,
            },
        }
    }

    pub fn new_funcdef(
        name: String,
        argvars: LinkedList<Var>,
        stmts: LinkedList<Node>,
        lvars_offset: usize,
    ) -> Self {
        Node {
            ty: None,
            kind: NodeKind::NDFUNCDEF {
                name: name,
                argvars: argvars,
                stmts: stmts,
                lvars_offset: lvars_offset,
            },
        }
    }

    pub fn new_decl(inits: LinkedList<Self>) -> Self {
        Node {
            ty: None,
            kind: NodeKind::NDDECL { inits: inits },
        }
    }

    pub fn new_lvar(offset: usize, ty: Type) -> Self {
        Node {
            ty: Some(ty),
            kind: NodeKind::NDLVAR { offset: offset },
        }
    }

    pub fn new_gvar(name: String, ty: Type) -> Self {
        Node {
            ty: Some(ty),
            kind: NodeKind::NDGVAR { name: name },
        }
    }

    pub fn new_member(node: Self, name: String) -> Self {
        Node {
            ty: None,
            kind: NodeKind::NDMEMBER {
                node: Box::new(node),
                name: name,
                offset: None,
            },
        }
    }

    pub fn populate_switch(&mut self) {
        // Use offset to communicate the relative position in the
        // order of appearance
        use NodeKind::*;

        match self.kind {
            NDSWITCH {
                ref mut stmt,
                ref mut cases,
                ref mut has_default,
                ..
            } => {
                if stmt.is_none() {
                    return;
                }

                let switch_stmt = stmt.as_mut().unwrap();

                // TODO: CLean up this logic
                match switch_stmt.kind {
                    NDCASE {
                        ref val,
                        ref mut pos,
                        ..
                    } => {
                        cases.push_back(val.clone());
                        *pos = Some(0);
                    }
                    NDDEFAULT { .. } => {
                        *has_default = true;
                    }
                    NDBLOCK { ref mut stmts } => {
                        let mut iter = stmts.iter_mut();
                        let mut counter = 0;
                        while let Some(blockstmt) = iter.next() {
                            match blockstmt.kind {
                                NDCASE {
                                    ref val,
                                    ref mut pos,
                                    ..
                                } => {
                                    cases.push_back(val.clone());
                                    *pos = Some(counter);
                                    counter += 1;
                                }
                                NDDEFAULT { .. } => {
                                    *has_default = true;
                                }
                                _ => (),
                            }
                        }
                    }
                    _ => (),
                }
            }
            _ => (),
        }
    }

    pub fn populate_ty(&mut self) {
        use NodeKind::*;

        if !self.ty.is_none() {
            return;
        }

        self.ty = match self.kind {
            NDADD {
                ref mut lhs,
                ref mut rhs,
                ref mut scale_lhs,
            }
            | NDSUB {
                ref mut lhs,
                ref mut rhs,
                ref mut scale_lhs,
            } => {
                lhs.populate_ty();
                rhs.populate_ty();

                let l_ty = lhs.ty.as_ref().unwrap();
                let r_ty = rhs.ty.as_ref().unwrap();

                if l_ty.is_ptr_like() {
                    if r_ty.is_ptr_like() {
                        panic!("Parser: Both sides of add/sub are pointers...");
                    }
                    *scale_lhs = Some(true);
                    Some(l_ty.clone())
                } else if r_ty.is_ptr_like() {
                    // Already checked above that l_ty is not a pointer
                    *scale_lhs = Some(false);
                    Some(r_ty.clone())
                } else {
                    // TODO: Update this
                    Some(Type::new_base("long"))
                }
            }
            NDMUL { .. }
            | NDDIV { .. }
            | NDEQ { .. }
            | NDNEQ { .. }
            | NDLEQ { .. }
            | NDLT { .. } => {
                // TODO: Update this
                Some(Type::new_base("int"))
            }
            NDCALL { .. } => {
                // TODO: Currently the only retval is INT
                Some(Type::new_base("int"))
            }
            NDADDR { ref mut node } => {
                node.populate_ty();
                Some(node.ty.as_ref().unwrap().new_ptr_to())
            }
            NDASSIGN {
                ref mut lhs,
                ref mut rhs,
                ref mut scale_lhs,
                assign_mode,
                is_init,
                ..
            } => {
                use AssignMode::*;
                lhs.populate_ty();
                rhs.populate_ty();

                let l_ty = lhs.ty.as_ref().unwrap();
                if l_ty.is_const && !is_init {
                    panic!("Trying to assign to const!")
                }
                if assign_mode == ADD || assign_mode == SUB {
                    if l_ty.is_ptr_like() {
                        *scale_lhs = Some(true);
                    }
                }
                Some(l_ty.clone())
            }
            NDDEREF { ref mut node } => {
                node.populate_ty();
                // What lhs's type points to should be my type.
                Some(node.ty.as_ref().unwrap().clone_base())
            }
            NDMEMBER {
                ref mut node,
                ref name,
                ref mut offset,
            } => {
                // Should be the member's type
                node.populate_ty();
                if let Some((ofs, ty)) = node.ty.as_ref().unwrap().get_member_offset(name.as_str())
                {
                    *offset = Some(ofs);
                    Some(ty)
                } else {
                    panic!("No member with name {} found!", name);
                }
            }
            _ => None,
        }
    }
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
    pub fn from_str(s: &str) -> Self {
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
