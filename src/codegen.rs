use std::fs::File;
use crate::parser::{Node, NodeKind};
use std::io::Write;

static FUNC_REGS_4: [&str; 6] = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];
static FUNC_REGS_8: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

#[derive(Debug)]
pub struct CodeGen<'a> {
    f: &'a mut File,
    cond_label: usize, // Used to track conditional labels
}


impl<'a> CodeGen<'a> {
    pub fn new(f: &'a mut File) -> Self {
        CodeGen {
            f: f,
            cond_label: 0,
        }
    }

    // Generates preamble:
    pub fn gen_preamble(&mut self) {
        gen_line!(self.f, ".intel_syntax noprefix\n");
        gen_line!(self.f, ".global main\n\n");
    }

    fn gen_lval(&mut self, node: Node) {
        match node.kind {
            NodeKind::NDLVAR => {
                gen_line!(self.f, "  mov rax, rbp\n");
                gen_line!(self.f, "  sub rax, {}\n", node.offset.unwrap());
                gen_line!(self.f, "  push rax\n");
            }
            NodeKind::NDDEREF => {
                self.gen(*node.lhs.unwrap());
            }
            _ => {
                panic!("Expected kind NDLVAR or NDDEREF but got {:?}", node.kind);
            }
        }
    }

    fn gen_load(&mut self, size: usize) {
        gen_line!(self.f, "  pop rax\n");

        match size {
            4 => {
                gen_line!(self.f, "  movsxd rax, dword ptr [rax]\n");
            }
            8 => {
                gen_line!(self.f, "  mov rax, [rax]\n");
            }
            _ => {
                panic!("Codegen: Load requested on an invalid size.");
            }
        }
        gen_line!(self.f, "  push rax\n");
    }

    fn gen_store(&mut self, size: usize) {
        gen_line!(self.f, "  pop rdi\n");
        gen_line!(self.f, "  pop rax\n");

        match size {
            4 => {
                gen_line!(self.f, "  mov [rax], edi\n"); 
            }
            8 => {
                gen_line!(self.f, "  mov [rax], rdi\n");
            }
            _ => {
                panic!("Codegen: Store requested on an invalid size.");
            }
        }

        gen_line!(self.f, "  push rdi\n");
    }

    // Set the last result to rax, restore the rbp and return
    fn gen_return(&mut self) {
        gen_line!(self.f, "  pop rax\n");
        gen_line!(self.f, "  mov rsp, rbp\n");
        gen_line!(self.f, "  pop rbp\n");
        gen_line!(self.f, "  pop r12\n");
        gen_line!(self.f, "  ret\n");
    }

    // Entry point into codegen
    pub fn gen(&mut self, mut node: Node) {
        use crate::parser::NodeKind::*;

        match node.kind {
            NDNUM => {
                gen_line!(self.f, "  push {}\n", node.val.unwrap());
            } 
            NDLVAR => {
                let size = node.ty.as_ref().unwrap().kind.size();
                self.gen_lval(node);
                self.gen_load(size);
            } 
            NDASSIGN => {
                self.gen_lval(*node.lhs.unwrap());
                self.gen(*node.rhs.unwrap());
                
                self.gen_store(node.ty.unwrap().kind.size());
            }
            NDRETURN => {
                self.gen(*node.lhs.unwrap());
                self.gen_return();
            } 
            NDIF => {
                let my_label = self.cond_label;
                self.cond_label += 1;
                self.gen(*node.cond.unwrap());
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  cmp rax, 0\n");
                gen_line!(self.f, "  je .Lelse{}\n", my_label);

                self.gen(*node.ifnode.unwrap());
                gen_line!(self.f, "  jmp .Lend{}\n", my_label);
                gen_line!(self.f, ".Lelse{}:\n", my_label);
                // If else node exists, generate.
                if let Some(elsenode) = node.elsenode {
                    self.gen(*elsenode);
                } else {
                    // No else node provided. Push some bogus value to balance stack.
                    gen_line!(self.f, "  push 0\n"); 
                }
                gen_line!(self.f, ".Lend{}:\n", my_label);
            } 
            NDWHILE => {
                let my_label = self.cond_label;
                self.cond_label += 1;
                gen_line!(self.f, ".Lbegin{}:\n", my_label);
                self.gen(*node.cond.unwrap());
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  cmp rax, 0\n");
                gen_line!(self.f, "  je .Lend{}\n", my_label);
                self.gen(*node.repnode.unwrap());
                gen_line!(self.f, "  jmp .Lbegin{}\n", my_label);
                gen_line!(self.f, ".Lend{}:", my_label);
            } 
            NDFOR => {
                let my_label = self.cond_label;
                self.cond_label += 1;
                if let Some(initnode) = node.initnode {
                    self.gen(*initnode);
                }
                gen_line!(self.f, ".Lbegin{}:\n", my_label);
                if let Some(cond) = node.cond {
                    self.gen(*cond);
                } else {
                    // Infinite loop: push 1 to make sure the cmp always succeeds
                    gen_line!(self.f, "push 1\n");
                }
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  cmp rax, 0\n");
                gen_line!(self.f, "  je .Lend{}\n", my_label);
                self.gen(*node.repnode.unwrap());
                if let Some(stepnode) = node.stepnode {
                    self.gen(*stepnode);
                }
                gen_line!(self.f, "  jmp .Lbegin{}\n", my_label);
                gen_line!(self.f, ".Lend{}:", my_label);
            } 
            NDBLOCK => {
                while let Some(stmt) = node.blockstmts.pop_front() {
                    self.gen(stmt);
                    // pop if not the last stmt
                    if node.blockstmts.len() != 0 {
                        gen_line!(self.f, "  pop rax\n");
                    }
                }
            } 
            NDCALL => {
                self.cond_label += 2; // Consume 2
                let num_args = node.funcargs.len();
                
                // Push args to the designated registers
                for i in 0..num_args {
                    self.gen(node.funcargs.pop_front().unwrap());
                    gen_line!(self.f, "  pop {}\n", FUNC_REGS_8[i]);
                }

                // Align RSP to multiple of 16 
                gen_line!(self.f, "  mov r12, 0x10\n");
                gen_line!(self.f, "  mov r13, rsp\n");
                gen_line!(self.f, "  and r13, 0xf\n");
                gen_line!(self.f, "  sub r12, r13\n"); // Need to sub rsp this much
                // TODO: Skip alignment if its already a multiple of 16
                gen_line!(self.f, "  sub rsp, r12\n");
                gen_line!(self.f, "  call {}\n", node.funcname.unwrap());
                // Rewind the alignment 
                gen_line!(self.f, "  add rsp, r12\n");

                // Finally, store result returned from the call:
                gen_line!(self.f, "  push rax\n");
            }
            NDFUNCDEF => {
                gen_line!(self.f, "{}:\n", node.funcname.unwrap());

                // Save callee-saved regs that are used by me:
                gen_line!(self.f, "  push r12\n");
                gen_line!(self.f, "  push rbp\n");
                // Make sure to create enough space for variables
                gen_line!(self.f, "  mov rbp, rsp\n");
                gen_line!(self.f, "  sub rsp, {}\n", node.lvars_offset.unwrap());

                // Get the arguments from the correspoinding registers
                let num_args = node.funcarg_vars.len();
                for i in 0..num_args {
                    let lvar = node.funcarg_vars.pop_front().unwrap();
                    gen_line!(self.f, "  mov rax, rbp\n");
                    gen_line!(self.f, "  sub rax, {}\n", lvar.offset);
                    let regs = match lvar.ty.kind.size() {
                        4 => FUNC_REGS_4,
                        8 => FUNC_REGS_8,
                        _ => panic!("Codegen: Invalid size for lvar!")
                    };
                    gen_line!(self.f, "  mov [rax], {}\n", regs[i]);
                }

                // Go on to execute the stmts
                while let Some(stmt) = node.blockstmts.pop_front() {
                    self.gen(stmt);
                    // Pop if not the last stmt
                    if node.blockstmts.len() != 0 {
                        gen_line!(self.f, "  pop rax\n");
                    }
                }

                // Restore rbp and return
                self.gen_return();
            }
            NDVARDEF => {
                // For now, just push some bogus value.
                gen_line!(self.f, "  push 12345\n");
            }
            NDADDR => {
                self.gen_lval(*node.lhs.unwrap());
            }
            NDDEREF => {
                self.gen(*node.lhs.unwrap());
                self.gen_load(node.ty.unwrap().kind.size());
            }
            _ => {
                // Must be a primitive node
                self.gen_primitive(node);
            }
        }
    }

    // Generates code for primitive ops
    fn gen_primitive(&mut self, node: Node) {
        use crate::parser::NodeKind::*;

        if let Some(lhs) = node.lhs {
            self.gen(*lhs);
        }
        if let Some(rhs) = node.rhs {
            self.gen(*rhs);
        }
    
        gen_line!(self.f, "  pop rdi\n");
        gen_line!(self.f, "  pop rax\n");
    
        match node.kind {
            NDADD => {
                gen_line!(self.f, "  add rax, rdi\n");
            }
            NDSUB => {
                gen_line!(self.f, "  sub rax, rdi\n");
            }
            NDMUL => {
                gen_line!(self.f, "  imul rax, rdi\n");
            }
            NDDIV => {
                gen_line!(self.f, "  cqo\n");
                gen_line!(self.f, "  idiv rdi\n");
            }
            NDEQ => {
                gen_line!(self.f, "  cmp rax, rdi\n");
                gen_line!(self.f, "  sete al\n");
                gen_line!(self.f, "  movzb rax, al\n");
            }
            NDNEQ => {
                gen_line!(self.f, "  cmp rax, rdi\n");
                gen_line!(self.f, "  setne al\n");
                gen_line!(self.f, "  movzb rax, al\n");
            }
            NDLEQ => {
                gen_line!(self.f, "  cmp rax, rdi\n");
                gen_line!(self.f, "  setle al\n");
                gen_line!(self.f, "  movzb rax, al\n");
            }
            NDLT => {
                gen_line!(self.f, "  cmp rax, rdi\n");
                gen_line!(self.f, "  setl al\n");
                gen_line!(self.f, "  movzb rax, al\n");
            }
            _ => {
                panic!("Oops, found a strange node kind.")
            }
        }
    
        gen_line!(self.f, "  push rax\n");
    }
}
