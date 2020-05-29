use crate::parser::{Node, NodeKind, Program};
use std::fs::File;
use std::io::Write;

static FUNC_REGS_4: [&str; 6] = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];
static FUNC_REGS_8: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

enum StoreMode {
    MOV,
    ADD,
    SUB,
}

pub struct CodeGen<'a> {
    f: &'a mut File,
    prog: Program,
    cond_label: usize, // Used to track conditional labels
}

impl<'a> CodeGen<'a> {
    pub fn new(f: &'a mut File, prog: Program) -> Self {
        CodeGen {
            f: f,
            prog: prog,
            cond_label: 0,
        }
    }

    pub fn gen_all(&mut self) {
        self.gen_preamble();
        self.gen_data();
        self.gen_text();
    }

    fn gen_data(&mut self) {
        gen_line!(self.f, ".data\n");

        loop {
            if let Some(gvar) = self.prog.globals.pop_front() {
                gen_line!(self.f, "{}:\n", gvar.name);
                gen_line!(self.f, "  .zero {}\n", gvar.ty.total_size());
            } else {
                break;
            }
        }
    }

    fn gen_text(&mut self) {
        gen_line!(self.f, ".text\n");
        loop {
            if let Some(node) = self.prog.nodes.pop_front() {
                self.gen(node);
            } else {
                break;
            }
        }
    }

    fn gen_preamble(&mut self) {
        gen_line!(self.f, ".intel_syntax noprefix\n");
        gen_line!(self.f, ".global main\n\n");
    }

    fn gen_lval(&mut self, node: Node) {
        use NodeKind::*;

        match node.kind {
            NDLVAR => {
                gen_line!(self.f, "  mov rax, rbp\n");
                gen_line!(self.f, "  sub rax, {}\n", node.offset.unwrap());
                gen_line!(self.f, "  push rax\n");
            }
            NDGVAR => {
                gen_line!(self.f, "  push offset {}\n", node.name.unwrap());
            }
            NDDEREF => {
                self.gen(*node.lhs.unwrap());
            }
            _ => {
                panic!("Unexpected node: got {:?}", node.kind);
            }
        }
    }

    fn gen_load(&mut self, size: usize) {
        gen_line!(self.f, "  pop rax\n");

        match size {
            1 => {
                gen_line!(self.f, "  movsx rax, byte ptr [rax]\n");
            }
            2 => {
                gen_line!(self.f, "  movsx rax, word ptr [rax]\n");
            }
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

    fn gen_store(&mut self, size: usize, mode: StoreMode) {
        use StoreMode::*;

        gen_line!(self.f, "  pop rdi\n");
        gen_line!(self.f, "  pop rax\n");

        let instr = match mode {
            MOV => "mov",
            ADD => "add",
            SUB => "sub",
        };

        let src = match size {
            1 => "dil",
            2 => "di",
            4 => "edi",
            8 => "rdi",
            _ => {
                panic!("Codegen: Store requested on an invalid size.");
            }
        };

        gen_line!(self.f, "  {} [rax], {}\n", instr, src);
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
        use crate::parser::TypeKind::*;
        use StoreMode::*;

        match node.kind {
            NDNUM => {
                gen_line!(self.f, "  push {}\n", node.val.unwrap());
            }
            NDLVAR | NDGVAR => match node.ty.as_ref().unwrap().kind {
                ARRAY => {
                    self.gen_lval(node);
                }
                _ => {
                    let size = node.ty.as_ref().unwrap().size();
                    self.gen_lval(node);
                    self.gen_load(size);
                }
            },
            NDASSIGN | NDADD_ASSIGN | NDSUB_ASSIGN => {
                let mode = match node.kind {
                    NDASSIGN => MOV,
                    NDADD_ASSIGN => ADD,
                    NDSUB_ASSIGN => SUB,
                    _ => {
                        panic!("Unreacheable");
                    }
                };
                self.gen_lval(*node.lhs.unwrap());
                self.gen(*node.rhs.unwrap());

                if let Some(to_scale) = node.scale_lhs {
                    if to_scale {
                        gen_line!(self.f, "  pop rax\n");
                        gen_line!(
                            self.f,
                            "  imul rax, {}\n",
                            node.ty.as_ref().unwrap().base_size()
                        );
                        gen_line!(self.f, "  push rax\n");
                    }
                }

                self.gen_store(node.ty.unwrap().size(), mode);
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
                gen_line!(self.f, "  call {}\n", node.name.unwrap());
                // Rewind the alignment
                gen_line!(self.f, "  add rsp, r12\n");

                // Finally, store result returned from the call:
                gen_line!(self.f, "  push rax\n");
            }
            NDFUNCDEF => {
                gen_line!(self.f, "{}:\n", node.name.unwrap());

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
                    gen_line!(self.f, "  sub rax, {}\n", lvar.offset.unwrap());
                    let regs = match lvar.ty.size() {
                        4 => FUNC_REGS_4,
                        8 => FUNC_REGS_8,
                        _ => panic!("Codegen: Invalid size for lvar!"),
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
            NDDECL => {
                loop {
                    if let Some(init) = node.inits.pop_front() {
                        self.gen(init);
                        gen_line!(self.f, "  pop rax\n");
                    } else {
                        break;
                    }
                }
                // For now, just push some bogus value.
                // TODO: Fix this
                gen_line!(self.f, "  push 12345\n");
            }
            NDADDR => {
                self.gen_lval(*node.lhs.unwrap());
            }
            NDDEREF => {
                self.gen(*node.lhs.unwrap());
                self.gen_load(node.ty.unwrap().size());
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
                let ty = node.ty.unwrap();
                if ty.kind.is_ptr_like() {
                    let reg_to_scale = if node.scale_lhs.unwrap() {
                        "rdi"
                    } else {
                        "rax"
                    };
                    gen_line!(self.f, "  imul {}, {}\n", reg_to_scale, ty.base_size());
                }
                gen_line!(self.f, "  add rax, rdi\n");
            }
            NDSUB => {
                let ty = node.ty.unwrap();
                if ty.kind.is_ptr_like() {
                    gen_line!(self.f, "  imul rdi, {}\n", ty.base_size());
                }
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
            _ => panic!("Oops, found a strange node kind."),
        }

        gen_line!(self.f, "  push rax\n");
    }
}
