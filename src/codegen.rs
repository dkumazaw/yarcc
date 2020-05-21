use std::fs::File;
use crate::parser::{Node, NodeKind};
use std::io::Write;

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

    pub fn gen_preamble(&mut self, num_vars: usize) {
        gen_line!(self.f, ".intel_syntax noprefix\n");
        gen_line!(self.f, ".global main\n\n");
        gen_line!(self.f, "main:\n");

        // Create enough space for variables
        gen_line!(self.f, "  push rbp\n");
        gen_line!(self.f, "  mov rbp, rsp\n");
        gen_line!(self.f, "  sub rsp, {}\n", num_vars * 8);
    }

    pub fn pop_rax(&mut self) {
        gen_line!(self.f, "  pop rax\n");
    }

    pub fn gen_postamble(&mut self) {
        // Restore rbp and return
        gen_line!(self.f, "  mov rsp, rbp\n");
        gen_line!(self.f, "  pop rbp\n");
        gen_line!(self.f, "  ret\n");
    }
    
    fn gen_lval(&mut self, node: Node) {
        if node.kind != NodeKind::NDLVAR {
            panic!("Expected kind NDLVAR but got {:?}", node.kind);
        }
        
        gen_line!(self.f, "  mov rax, rbp\n");
        gen_line!(self.f, "  sub rax, {}\n", node.offset.unwrap());
        gen_line!(self.f, "  push rax\n");
    }

    pub fn gen(&mut self, node: Node) {
        use crate::parser::NodeKind::*;
    
        if node.kind == NDNUM {
            gen_line!(self.f, "  push {}\n", node.val.unwrap());
            return;
        } else if node.kind == NDLVAR {
            self.gen_lval(node);
            gen_line!(self.f, "  pop rax\n");
            gen_line!(self.f, "  mov rax, [rax]\n");
            gen_line!(self.f, "  push rax\n");
            return;
        } else if node.kind == NDASSIGN {
            self.gen_lval(*node.lhs.unwrap());
            self.gen(*node.rhs.unwrap());
            gen_line!(self.f, "  pop rdi\n");
            gen_line!(self.f, "  pop rax\n");
            gen_line!(self.f, "  mov [rax], rdi\n");
            gen_line!(self.f, "  push rdi\n");
            return;
        } else if node.kind == NDRETURN {
            self.gen(*node.lhs.unwrap());
            gen_line!(self.f, "  pop rax\n");
            gen_line!(self.f, "  mov rsp, rbp\n");
            gen_line!(self.f, "  pop rbp\n");
            gen_line!(self.f, "  ret\n");
            return;
        } else if node.kind == NDIF {
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
            }
            gen_line!(self.f, ".Lend{}:\n", my_label);
            return;
        } else if node.kind == NDWHILE {
            gen_line!(self.f, ".Lbegin{}\n", self.cond_label);
        }
        
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
