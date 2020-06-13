use crate::parser::{AssignMode, Node, NodeKind, Program};
use std::collections::LinkedList;
use std::fs::File;
use std::io::Write;

static FUNC_REGS_4: [&str; 6] = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];
static FUNC_REGS_8: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

static MAGIC: usize = 141421356;
static LITERAL_HEAD: &str = ".Lstr";

pub struct CodeGen<'a> {
    f: &'a mut File,
    prog: Program,
    cond_label: usize, // Next cond label to be issued
    conds: LinkedList<(usize, NodeKind)>,
}

impl<'a> CodeGen<'a> {
    pub fn new(f: &'a mut File, prog: Program) -> Self {
        CodeGen {
            f: f,
            prog: prog,
            cond_label: 0,
            conds: LinkedList::new(),
        }
    }

    pub fn gen_all(&mut self) {
        self.gen_preamble();
        self.gen_data();
        self.gen_text();
    }

    fn issue_level(&mut self) -> usize {
        let issued = self.cond_label;
        self.cond_label += 1;
        issued
    }

    fn push_level(&mut self, kind: NodeKind) -> usize {
        let current = self.issue_level();
        self.conds.push_back((current, kind));
        current
    }

    fn pop_level(&mut self) {
        self.conds.pop_back();
    }

    fn get_current_level(&self) -> (usize, NodeKind) {
        self.conds.back().unwrap().clone()
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

        let mut literal_count = 0;
        loop {
            if let Some(literal) = self.prog.literals.pop_front() {
                gen_line!(self.f, "{}{}:\n", LITERAL_HEAD, literal_count);
                gen_line!(self.f, "  .string \"{}\"\n", literal);
                literal_count += 1;
            } else {
                break;
            }
        }

        gen_line!(self.f, "\n");
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

    fn gen_push_magic(&mut self) {
        gen_line!(self.f, "  push {}\n", MAGIC);
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
            NDSTR => {
                gen_line!(
                    self.f,
                    "  push offset {}{}\n",
                    LITERAL_HEAD,
                    node.offset.unwrap()
                );
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

    fn gen_store(&mut self, size: usize) {
        gen_line!(self.f, "  pop rdi\n");
        gen_line!(self.f, "  pop rax\n");

        let src = match size {
            1 => "dil",
            2 => "di",
            4 => "edi",
            8 => "rdi",
            _ => {
                panic!("Codegen: Store requested on an invalid size.");
            }
        };

        gen_line!(self.f, "  mov [rax], {}\n", src);
        gen_line!(self.f, "  push rdi\n");
    }

    fn gen_blockstmts(&mut self, mut blockstmts: LinkedList<Node>) {
        // Let empty block evaluate to 0
        if blockstmts.len() == 0 {
            gen_line!(self.f, "  push 0\n");
            return;
        }
        while let Some(stmt) = blockstmts.pop_front() {
            self.gen(stmt);
            if blockstmts.len() != 0 {
                gen_line!(self.f, "  pop rax\n");
            }
        }
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
        use crate::parser::Type::*;

        match node.kind {
            NDNUM => {
                gen_line!(self.f, "  push {}\n", node.val.unwrap());
            }
            NDSTR => {
                self.gen_lval(node);
            }
            NDLVAR | NDGVAR => match node.ty.as_ref().unwrap() {
                ARRAY { .. } => {
                    self.gen_lval(node);
                }
                _ => {
                    let size = node.ty.as_ref().unwrap().size();
                    self.gen_lval(node);
                    self.gen_load(size);
                }
            },
            NDASSIGN => {
                use AssignMode::*;
                let mode = node.assign_mode.unwrap();
                if mode == DEFAULT {
                    self.gen_lval(*node.lhs.unwrap());
                    self.gen(*node.rhs.unwrap());
                    self.gen_store(node.ty.unwrap().size());
                    return;
                }

                // Needs to be done on a register
                let size = node.lhs.as_ref().unwrap().ty.as_ref().unwrap().size();
                self.gen_lval(*node.lhs.unwrap());
                // Duplicate the address for later store
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  push rax\n");
                gen_line!(self.f, "  push rax\n");
                self.gen_load(size);
                self.gen(*node.rhs.unwrap());

                if let Some(to_scale) = node.scale_lhs {
                    if to_scale {
                        if mode == MUL || mode == DIV {
                            panic!("Scaling should not be allowed for this node.");
                        }
                        gen_line!(self.f, "  pop rax\n");
                        gen_line!(
                            self.f,
                            "  imul rax, {}\n",
                            node.ty.as_ref().unwrap().base_size()
                        );
                        gen_line!(self.f, "  push rax\n");
                    }
                }

                gen_line!(self.f, "  pop rdi\n");
                gen_line!(self.f, "  pop rax\n");
                if !node.eval_pre.unwrap() {
                    // This is post incr/decr
                    gen_line!(self.f, "  mov r12, rax\n");
                }
                match mode {
                    ADD => {
                        gen_line!(self.f, "  add rax, rdi\n");
                    }
                    SUB => {
                        gen_line!(self.f, "  sub rax, rdi\n");
                    }
                    MUL => {
                        gen_line!(self.f, "  imul rax, rdi\n");
                    }
                    DIV => {
                        gen_line!(self.f, "  cqo\n");
                        gen_line!(self.f, "  idiv rdi\n");
                    }
                    MOD => {
                        gen_line!(self.f, "  cqo\n");
                        gen_line!(self.f, "  idiv rdi\n");
                        gen_line!(self.f, "  mov rax, rdx\n");
                    }
                    AND => {
                        gen_line!(self.f, "  and rax, rdi\n");
                    }
                    OR => {
                        gen_line!(self.f, "  or rax, rdi\n");
                    }
                    XOR => {
                        gen_line!(self.f, "  xor rax, rdi\n");
                    }
                    SHL => {
                        // TODO: Not pretty moving around vals on registers:(
                        gen_line!(self.f, "  mov rcx, rdi\n");
                        gen_line!(self.f, "  shl rax, cl\n");
                    }
                    SHR => {
                        gen_line!(self.f, "  mov rcx, rdi\n");
                        gen_line!(self.f, "  shr rax, cl\n");
                    }
                    DEFAULT => panic!("Default assignment shouldn't reach here."),
                }
                gen_line!(self.f, "  push rax\n");

                self.gen_store(node.ty.unwrap().size());
                if !node.eval_pre.unwrap() {
                    gen_line!(self.f, "  pop rax\n");
                    gen_line!(self.f, "  push r12\n");
                }
            }
            NDRETURN => {
                self.gen(*node.lhs.unwrap());
                self.gen_return();
            }
            NDIF => {
                let my_label = self.issue_level();
                self.gen(*node.cond.unwrap());
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  cmp rax, 0\n");
                gen_line!(self.f, "  je .Lelse{}\n", my_label);
                if let Some(ifnode) = node.ifnode {
                    self.gen(*ifnode);
                    gen_line!(self.f, "  pop r15\n");
                }
                gen_line!(self.f, "  jmp .Lend{}\n", my_label);
                gen_line!(self.f, ".Lelse{}:\n", my_label);
                if let Some(elsenode) = node.elsenode {
                    self.gen(*elsenode);
                    gen_line!(self.f, "  pop r15\n");
                }
                gen_line!(self.f, ".Lend{}:\n", my_label);
                self.gen_push_magic();
            }
            NDCASE => {
                let (label, kind) = self.get_current_level();
                if kind != NDSWITCH {
                    panic!("Kind can't be anything other than switch");
                }
                gen_line!(self.f, ".Lcase{}of{}:\n", node.offset.unwrap(), label);
                self.gen(*node.ifnode.unwrap());
            }
            NDDEFAULT => {
                let (label, kind) = self.get_current_level();
                if kind != NDSWITCH {
                    panic!("Kind can't be anything other than switch");
                }
                gen_line!(self.f, ".Ldefault{}:\n", label);
                if let Some(stmt) = node.stmt {
                    self.gen(*stmt);
                } else {
                    self.gen_push_magic();
                }
            }
            NDSWITCH => {
                let my_label = self.push_level(NDSWITCH);
                self.gen(*node.ctrl.unwrap());
                gen_line!(self.f, "  pop rax\n");

                let mut counter = 0;
                while let Some(val) = node.cases.pop_front() {
                    gen_line!(self.f, "  cmp rax, {}\n", val);
                    gen_line!(self.f, "  je .Lcase{}of{}\n", counter, my_label);
                    counter += 1;
                }
                let loc = if node.has_default {
                    ".Ldefault"
                } else {
                    ".Lend"
                };
                gen_line!(self.f, "  jmp {}{}\n", loc, my_label);

                if let Some(stmt) = node.stmt {
                    self.gen(*stmt);
                    gen_line!(self.f, "  pop r15\n");
                }
                gen_line!(self.f, ".Lend{}:\n", my_label);
                self.gen_push_magic();
            }
            NDBREAK => {
                let (label, _) = self.get_current_level();
                gen_line!(self.f, "  jmp .Lend{}\n", label);
            }
            NDCONTINUE => {
                let (label, kind) = self.get_current_level();
                let loc = match kind {
                    NDFOR => ".Lstep",
                    NDDOWHILE => ".Lcond",
                    NDWHILE => ".Lbegin",
                    _ => panic!("Shouldn't reach here."),
                };
                gen_line!(self.f, "  jmp {}{}\n", loc, label);
            }
            NDWHILE => {
                let my_label = self.push_level(NDWHILE);
                gen_line!(self.f, ".Lbegin{}:\n", my_label);
                self.gen(*node.cond.unwrap());
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  cmp rax, 0\n");
                gen_line!(self.f, "  je .Lend{}\n", my_label);
                self.gen(*node.repnode.unwrap());
                gen_line!(self.f, "  pop r15\n"); // Pop unneeded stuff
                gen_line!(self.f, "  jmp .Lbegin{}\n", my_label);
                gen_line!(self.f, ".Lend{}:\n", my_label);
                self.gen_push_magic();
                self.pop_level();
            }
            NDDOWHILE => {
                let my_label = self.push_level(NDDOWHILE);
                gen_line!(self.f, ".Lbegin{}:\n", my_label);
                self.gen(*node.repnode.unwrap());
                gen_line!(self.f, "  pop r15\n");
                gen_line!(self.f, ".Lcond{}:\n", my_label);
                self.gen(*node.cond.unwrap());
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  cmp rax, 0\n");
                gen_line!(self.f, "  jne .Lbegin{}\n", my_label);
                gen_line!(self.f, ".Lend{}:\n", my_label);
                self.gen_push_magic();
                self.pop_level();
            }
            NDFOR => {
                let my_label = self.push_level(NDFOR);
                if let Some(initnode) = node.initnode {
                    self.gen(*initnode);
                    gen_line!(self.f, "  pop r15\n");
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
                if let Some(repnode) = node.repnode {
                    self.gen(*repnode);
                    gen_line!(self.f, "  pop r15\n"); // Throw away garbage
                }
                gen_line!(self.f, ".Lstep{}:\n", my_label);
                if let Some(stepnode) = node.stepnode {
                    self.gen(*stepnode);
                    gen_line!(self.f, "  pop r15\n"); // Throw away garbage
                }
                gen_line!(self.f, "  jmp .Lbegin{}\n", my_label);
                gen_line!(self.f, ".Lend{}:\n", my_label);
                self.gen_push_magic();
                self.pop_level();
            }
            NDBLOCK => {
                self.gen_blockstmts(node.blockstmts);
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

                self.gen_blockstmts(node.blockstmts);
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
                self.gen_push_magic();
            }
            NDADDR => {
                self.gen_lval(*node.lhs.unwrap());
            }
            NDDEREF => {
                self.gen(*node.lhs.unwrap());
                self.gen_load(node.ty.unwrap().size());
            }
            NDBITNOT => {
                self.gen(*node.lhs.unwrap());
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  not rax\n");
                gen_line!(self.f, "  push rax\n");
            }
            NDLOGAND | NDLOGOR => {
                // Only evaluate the rhs if the lhs evaluates to 1
                // as per C89 6.3.13 and 6.3.14
                let my_label = self.issue_level();
                self.gen(*node.lhs.unwrap());
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  cmp rax, 0\n");
                gen_line!(self.f, "  setne al\n");
                gen_line!(self.f, "  movzb rax, al\n");
                if node.kind == NDLOGAND {
                    gen_line!(self.f, "  je .Lend{}\n", my_label);
                } else {
                    gen_line!(self.f, "  jne .Lend{}\n", my_label);
                }
                self.gen(*node.rhs.unwrap());
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  cmp rax, 0\n");
                gen_line!(self.f, "  setne al\n");
                gen_line!(self.f, "  movzb rax, al\n");

                gen_line!(self.f, ".Lend{}:\n", my_label);
                gen_line!(self.f, "  push rax\n");
            }
            NDSHL | NDSHR => {
                self.gen(*node.lhs.unwrap());
                self.gen(*node.rhs.unwrap());
                gen_line!(self.f, "  pop rcx\n");
                gen_line!(self.f, "  pop rax\n");

                // Also read my comments in parser
                let instr = if node.kind == NDSHL { "shl" } else { "shr" };
                gen_line!(self.f, "  {} rax, cl\n", instr);
                gen_line!(self.f, "  push rax\n");
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
                if ty.is_ptr_like() {
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
                if ty.is_ptr_like() {
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
            NDMOD => {
                gen_line!(self.f, "  cqo\n");
                gen_line!(self.f, "  idiv rdi\n");
                gen_line!(self.f, "  mov rax, rdx\n");
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
            NDBITAND => {
                gen_line!(self.f, "  and rax, rdi\n");
            }
            NDBITXOR => {
                gen_line!(self.f, "  xor rax, rdi\n");
            }
            NDBITOR => {
                gen_line!(self.f, "  or rax, rdi\n");
            }
            _ => panic!("Oops, found a strange node kind."),
        }

        gen_line!(self.f, "  push rax\n");
    }
}
