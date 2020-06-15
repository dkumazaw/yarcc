use crate::ctype::Type;
use crate::node::{AssignMode, Node, NodeKind};
use crate::parser::Program;
use std::collections::LinkedList;
use std::fs::File;
use std::io::Write;

static FUNC_REGS_4: [&str; 6] = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];
static FUNC_REGS_8: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

static MAGIC: usize = 141421356;
static LITERAL_HEAD: &str = ".Lstr";

#[derive(Debug, Copy, Clone, PartialEq)]
enum LevelKind {
    SWITCH,
    WHILE,
    DOWHILE,
    FOR,
}

pub struct CodeGen<'a> {
    f: &'a mut File,
    prog: Program,
    cond_label: usize, // Next cond label to be issued
    conds: LinkedList<(usize, LevelKind)>,
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

    fn push_level(&mut self, kind: LevelKind) -> usize {
        let current = self.issue_level();
        self.conds.push_back((current, kind));
        current
    }

    fn pop_level(&mut self) {
        self.conds.pop_back();
    }

    fn get_current_level(&self) -> (usize, LevelKind) {
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
            NDLVAR { offset } => {
                gen_line!(self.f, "  mov rax, rbp\n");
                gen_line!(self.f, "  sub rax, {}\n", offset);
                gen_line!(self.f, "  push rax\n");
            }
            NDGVAR { name } => {
                gen_line!(self.f, "  push offset {}\n", name);
            }
            NDSTR { pos } => {
                gen_line!(self.f, "  push offset {}{}\n", LITERAL_HEAD, pos);
            }
            NDDEREF { node: operand } => {
                self.gen(*operand);
            }
            NDMEMBER {
                node: varnode,
                offset: relative_offset,
                ..
            } => {
                self.gen_lval(*varnode);
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  add rax, {}\n", relative_offset.unwrap());
                gen_line!(self.f, "  push rax\n");
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

    // For NDLOGAND and NDLOGOR
    fn gen_logical(&mut self, lhs: Node, rhs: Node, is_and: bool) {
        // Only evaluate the rhs if the lhs evaluates to 1
        // as per C89 6.3.13 and 6.3.14
        let my_label = self.issue_level();
        let instr = if is_and { "je" } else { "jne" };

        self.gen(lhs);
        gen_line!(self.f, "  pop rax\n");
        gen_line!(self.f, "  cmp rax, 0\n");
        gen_line!(self.f, "  setne al\n");
        gen_line!(self.f, "  movzb rax, al\n");
        gen_line!(self.f, "  {} .Lend{}\n", instr, my_label);

        self.gen(rhs);
        gen_line!(self.f, "  pop rax\n");
        gen_line!(self.f, "  cmp rax, 0\n");
        gen_line!(self.f, "  setne al\n");
        gen_line!(self.f, "  movzb rax, al\n");

        gen_line!(self.f, ".Lend{}:\n", my_label);
        gen_line!(self.f, "  push rax\n");
    }

    // Entry point into codegen
    pub fn gen(&mut self, mut node: Node) {
        use NodeKind::*;
        use Type::*;

        match node.kind {
            NDINT { val } => {
                gen_line!(self.f, "  push {}\n", val);
            }
            NDSTR { .. } => {
                self.gen_lval(node);
            }
            NDLVAR { .. } | NDGVAR { .. } => match node.ty.as_ref().unwrap() {
                ARRAY { .. } => {
                    self.gen_lval(node);
                }
                _ => {
                    let size = node.ty.as_ref().unwrap().size();
                    self.gen_lval(node);
                    self.gen_load(size);
                }
            },
            NDMEMBER { .. } => {
                let size = node.ty.as_ref().unwrap().size();
                self.gen_lval(node);
                self.gen_load(size);
            }
            NDASSIGN {
                lhs,
                rhs,
                scale_lhs,
                eval_pre,
                assign_mode,
            } => {
                use AssignMode::*;
                if assign_mode == DEFAULT {
                    self.gen_lval(*lhs);
                    self.gen(*rhs);
                    self.gen_store(node.ty.unwrap().size());
                    return;
                }

                // Needs to be done on a register
                let size = lhs.as_ref().ty.as_ref().unwrap().size();
                self.gen_lval(*lhs);
                // Duplicate the address for later store
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  push rax\n");
                gen_line!(self.f, "  push rax\n");
                self.gen_load(size);
                self.gen(*rhs);

                if let Some(to_scale) = scale_lhs {
                    if to_scale {
                        if assign_mode == MUL || assign_mode == DIV {
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
                if !eval_pre {
                    // This is post incr/decr
                    gen_line!(self.f, "  mov r12, rax\n");
                }
                match assign_mode {
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
                if !eval_pre {
                    gen_line!(self.f, "  pop rax\n");
                    gen_line!(self.f, "  push r12\n");
                }
            }
            NDRETURN { node: operand } => {
                self.gen(*operand);
                self.gen_return();
            }
            NDIF {
                cond,
                ifnode,
                elsenode,
            } => {
                let my_label = self.issue_level();
                self.gen(*cond);
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  cmp rax, 0\n");
                gen_line!(self.f, "  je .Lelse{}\n", my_label);
                if let Some(ifnode) = ifnode {
                    self.gen(*ifnode);
                    gen_line!(self.f, "  pop r15\n");
                }
                gen_line!(self.f, "  jmp .Lend{}\n", my_label);
                gen_line!(self.f, ".Lelse{}:\n", my_label);
                if let Some(elsenode) = elsenode {
                    self.gen(*elsenode);
                    gen_line!(self.f, "  pop r15\n");
                }
                gen_line!(self.f, ".Lend{}:\n", my_label);
                self.gen_push_magic();
            }
            NDCASE { stmt, pos, .. } => {
                let (label, kind) = self.get_current_level();
                if kind != LevelKind::SWITCH {
                    panic!("Kind can't be anything other than switch");
                }
                gen_line!(self.f, ".Lcase{}of{}:\n", pos.unwrap(), label);
                self.gen(*stmt.unwrap());
            }
            NDDEFAULT { stmt } => {
                let (label, kind) = self.get_current_level();
                if kind != LevelKind::SWITCH {
                    panic!("Kind can't be anything other than switch");
                }
                gen_line!(self.f, ".Ldefault{}:\n", label);
                if let Some(stmt) = stmt {
                    self.gen(*stmt);
                } else {
                    self.gen_push_magic();
                }
            }
            NDSWITCH {
                ctrl,
                stmt,
                mut cases,
                has_default,
            } => {
                let my_label = self.push_level(LevelKind::SWITCH);
                self.gen(*ctrl);
                gen_line!(self.f, "  pop rax\n");

                let mut counter = 0;
                while let Some(val) = cases.pop_front() {
                    gen_line!(self.f, "  cmp rax, {}\n", val);
                    gen_line!(self.f, "  je .Lcase{}of{}\n", counter, my_label);
                    counter += 1;
                }
                let loc = if has_default { ".Ldefault" } else { ".Lend" };
                gen_line!(self.f, "  jmp {}{}\n", loc, my_label);

                if let Some(stmt) = stmt {
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
                    LevelKind::FOR => ".Lstep",
                    LevelKind::DOWHILE => ".Lcond",
                    LevelKind::WHILE => ".Lbegin",
                    _ => panic!("Shouldn't reach here."),
                };
                gen_line!(self.f, "  jmp {}{}\n", loc, label);
            }
            NDWHILE { cond, repnode } => {
                let my_label = self.push_level(LevelKind::WHILE);
                gen_line!(self.f, ".Lbegin{}:\n", my_label);
                self.gen(*cond);
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  cmp rax, 0\n");
                gen_line!(self.f, "  je .Lend{}\n", my_label);
                self.gen(*repnode.unwrap());
                gen_line!(self.f, "  pop r15\n"); // Pop unneeded stuff
                gen_line!(self.f, "  jmp .Lbegin{}\n", my_label);
                gen_line!(self.f, ".Lend{}:\n", my_label);
                self.gen_push_magic();
                self.pop_level();
            }
            NDDOWHILE { cond, repnode } => {
                let my_label = self.push_level(LevelKind::DOWHILE);
                gen_line!(self.f, ".Lbegin{}:\n", my_label);
                self.gen(*repnode.unwrap());
                gen_line!(self.f, "  pop r15\n");
                gen_line!(self.f, ".Lcond{}:\n", my_label);
                self.gen(*cond);
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  cmp rax, 0\n");
                gen_line!(self.f, "  jne .Lbegin{}\n", my_label);
                gen_line!(self.f, ".Lend{}:\n", my_label);
                self.gen_push_magic();
                self.pop_level();
            }
            NDFOR {
                init,
                cond,
                step,
                repnode,
            } => {
                let my_label = self.push_level(LevelKind::FOR);
                if let Some(init) = init {
                    self.gen(*init);
                    gen_line!(self.f, "  pop r15\n");
                }
                gen_line!(self.f, ".Lbegin{}:\n", my_label);
                if let Some(cond) = cond {
                    self.gen(*cond);
                } else {
                    // Infinite loop: push 1 to make sure the cmp always succeeds
                    gen_line!(self.f, "push 1\n");
                }
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  cmp rax, 0\n");
                gen_line!(self.f, "  je .Lend{}\n", my_label);
                if let Some(repnode) = repnode {
                    self.gen(*repnode);
                    gen_line!(self.f, "  pop r15\n"); // Throw away garbage
                }
                gen_line!(self.f, ".Lstep{}:\n", my_label);
                if let Some(step) = step {
                    self.gen(*step);
                    gen_line!(self.f, "  pop r15\n"); // Throw away garbage
                }
                gen_line!(self.f, "  jmp .Lbegin{}\n", my_label);
                gen_line!(self.f, ".Lend{}:\n", my_label);
                self.gen_push_magic();
                self.pop_level();
            }
            NDBLOCK { stmts } => {
                self.gen_blockstmts(stmts);
            }
            NDCALL { name, mut args } => {
                self.cond_label += 2; // Consume 2
                let num_args = args.len();

                // Push args to the designated registers
                for i in 0..num_args {
                    self.gen(args.pop_front().unwrap());
                    gen_line!(self.f, "  pop {}\n", FUNC_REGS_8[i]);
                }

                // Align RSP to multiple of 16
                gen_line!(self.f, "  mov r12, 0x10\n");
                gen_line!(self.f, "  mov r13, rsp\n");
                gen_line!(self.f, "  and r13, 0xf\n");
                gen_line!(self.f, "  sub r12, r13\n"); // Need to sub rsp this much
                                                       // TODO: Skip alignment if its already a multiple of 16
                gen_line!(self.f, "  sub rsp, r12\n");
                gen_line!(self.f, "  call {}\n", name);
                // Rewind the alignment
                gen_line!(self.f, "  add rsp, r12\n");

                // Finally, store result returned from the call:
                gen_line!(self.f, "  push rax\n");
            }
            NDFUNCDEF {
                name,
                mut argvars,
                stmts,
                lvars_offset,
            } => {
                gen_line!(self.f, "{}:\n", name);

                // Save callee-saved regs that are used by me:
                gen_line!(self.f, "  push r12\n");
                gen_line!(self.f, "  push rbp\n");
                // Make sure to create enough space for variables
                gen_line!(self.f, "  mov rbp, rsp\n");
                gen_line!(self.f, "  sub rsp, {}\n", lvars_offset);

                // Get the arguments from the correspoinding registers
                let num_args = argvars.len();
                for i in 0..num_args {
                    let lvar = argvars.pop_front().unwrap();
                    gen_line!(self.f, "  mov rax, rbp\n");
                    gen_line!(self.f, "  sub rax, {}\n", lvar.offset.unwrap());
                    let regs = match lvar.ty.size() {
                        4 => FUNC_REGS_4,
                        8 => FUNC_REGS_8,
                        _ => panic!("Codegen: Invalid size for lvar!"),
                    };
                    gen_line!(self.f, "  mov [rax], {}\n", regs[i]);
                }

                self.gen_blockstmts(stmts);
                self.gen_return();
            }
            NDDECL { mut inits } => {
                loop {
                    if let Some(init) = inits.pop_front() {
                        self.gen(init);
                        gen_line!(self.f, "  pop rax\n");
                    } else {
                        break;
                    }
                }
                self.gen_push_magic();
            }
            NDADDR { node: operand } => {
                self.gen_lval(*operand);
            }
            NDDEREF { node: operand } => {
                self.gen(*operand);
                self.gen_load(node.ty.unwrap().size());
            }
            NDBITNOT { node: operand } => {
                self.gen(*operand);
                gen_line!(self.f, "  pop rax\n");
                gen_line!(self.f, "  not rax\n");
                gen_line!(self.f, "  push rax\n");
            }
            NDLOGAND { lhs, rhs } => {
                self.gen_logical(*lhs, *rhs, true);
            }
            NDLOGOR { lhs, rhs } => {
                self.gen_logical(*lhs, *rhs, false);
            }
            NDSHL { lhs, rhs } => {
                self.gen(*lhs);
                self.gen(*rhs);
                gen_line!(self.f, "  pop rcx\n");
                gen_line!(self.f, "  pop rax\n");

                gen_line!(self.f, "  shl rax, cl\n");
                gen_line!(self.f, "  push rax\n");
            }
            NDSHR { lhs, rhs } => {
                self.gen(*lhs);
                self.gen(*rhs);
                gen_line!(self.f, "  pop rcx\n");
                gen_line!(self.f, "  pop rax\n");

                gen_line!(self.f, "  shr rax, cl\n");
                gen_line!(self.f, "  push rax\n");
            }
            _ => {
                // Must be a primitive node
                self.gen_primitive(node);
            }
        }
    }

    // Generates code for primitive ops
    // TODO CLean up repetition
    fn gen_primitive(&mut self, node: Node) {
        use NodeKind::*;

        match node.kind {
            NDADD {
                lhs,
                rhs,
                scale_lhs,
            } => {
                self.gen(*lhs);
                self.gen(*rhs);
                gen_line!(self.f, "  pop rdi\n");
                gen_line!(self.f, "  pop rax\n");

                let ty = node.ty.unwrap();
                if ty.is_ptr_like() {
                    let reg_to_scale = if scale_lhs.unwrap() { "rdi" } else { "rax" };
                    gen_line!(self.f, "  imul {}, {}\n", reg_to_scale, ty.base_size());
                }
                gen_line!(self.f, "  add rax, rdi\n");
            }
            NDSUB { lhs, rhs, .. } => {
                self.gen(*lhs);
                self.gen(*rhs);
                gen_line!(self.f, "  pop rdi\n");
                gen_line!(self.f, "  pop rax\n");

                let ty = node.ty.unwrap();
                if ty.is_ptr_like() {
                    gen_line!(self.f, "  imul rdi, {}\n", ty.base_size());
                }
                gen_line!(self.f, "  sub rax, rdi\n");
            }
            NDMUL { lhs, rhs } => {
                self.gen(*lhs);
                self.gen(*rhs);
                gen_line!(self.f, "  pop rdi\n");
                gen_line!(self.f, "  pop rax\n");

                gen_line!(self.f, "  imul rax, rdi\n");
            }
            NDDIV { lhs, rhs } => {
                self.gen(*lhs);
                self.gen(*rhs);
                gen_line!(self.f, "  pop rdi\n");
                gen_line!(self.f, "  pop rax\n");

                gen_line!(self.f, "  cqo\n");
                gen_line!(self.f, "  idiv rdi\n");
            }
            NDMOD { lhs, rhs } => {
                self.gen(*lhs);
                self.gen(*rhs);
                gen_line!(self.f, "  pop rdi\n");
                gen_line!(self.f, "  pop rax\n");

                gen_line!(self.f, "  cqo\n");
                gen_line!(self.f, "  idiv rdi\n");
                gen_line!(self.f, "  mov rax, rdx\n");
            }
            NDEQ { lhs, rhs } => {
                self.gen(*lhs);
                self.gen(*rhs);
                gen_line!(self.f, "  pop rdi\n");
                gen_line!(self.f, "  pop rax\n");

                gen_line!(self.f, "  cmp rax, rdi\n");
                gen_line!(self.f, "  sete al\n");
                gen_line!(self.f, "  movzb rax, al\n");
            }
            NDNEQ { lhs, rhs } => {
                self.gen(*lhs);
                self.gen(*rhs);
                gen_line!(self.f, "  pop rdi\n");
                gen_line!(self.f, "  pop rax\n");

                gen_line!(self.f, "  cmp rax, rdi\n");
                gen_line!(self.f, "  setne al\n");
                gen_line!(self.f, "  movzb rax, al\n");
            }
            NDLEQ { lhs, rhs } => {
                self.gen(*lhs);
                self.gen(*rhs);
                gen_line!(self.f, "  pop rdi\n");
                gen_line!(self.f, "  pop rax\n");

                gen_line!(self.f, "  cmp rax, rdi\n");
                gen_line!(self.f, "  setle al\n");
                gen_line!(self.f, "  movzb rax, al\n");
            }
            NDLT { lhs, rhs } => {
                self.gen(*lhs);
                self.gen(*rhs);
                gen_line!(self.f, "  pop rdi\n");
                gen_line!(self.f, "  pop rax\n");

                gen_line!(self.f, "  cmp rax, rdi\n");
                gen_line!(self.f, "  setl al\n");
                gen_line!(self.f, "  movzb rax, al\n");
            }
            NDBITAND { lhs, rhs } => {
                self.gen(*lhs);
                self.gen(*rhs);
                gen_line!(self.f, "  pop rdi\n");
                gen_line!(self.f, "  pop rax\n");

                gen_line!(self.f, "  and rax, rdi\n");
            }
            NDBITXOR { lhs, rhs } => {
                self.gen(*lhs);
                self.gen(*rhs);
                gen_line!(self.f, "  pop rdi\n");
                gen_line!(self.f, "  pop rax\n");

                gen_line!(self.f, "  xor rax, rdi\n");
            }
            NDBITOR { lhs, rhs } => {
                self.gen(*lhs);
                self.gen(*rhs);
                gen_line!(self.f, "  pop rdi\n");
                gen_line!(self.f, "  pop rax\n");

                gen_line!(self.f, "  or rax, rdi\n");
            }
            _ => panic!("Oops, found a strange node kind."),
        }

        gen_line!(self.f, "  push rax\n");
    }
}
