use std::fs::File;
use crate::parser::{Node, NodeKind};
use std::io::Write;

fn gen_lval(f: &mut File, node: Node) {
    if node.kind != NodeKind::NDLVAR {
        panic!("Expected kind NDLVAR but got {:?}", node.kind);
    }
    
    gen_line!(f, "  mov rax, rbp\n");
    gen_line!(f, "  sub rax, {}\n", node.offset.unwrap());
    gen_line!(f, "  push rax\n");
}

pub fn gen(f: &mut File, node: Node) {
    use crate::parser::NodeKind::*;

    // Logic for terminal nodes
    if node.kind == NDNUM {
        gen_line!(f, "  push {}\n", node.val.unwrap());
        return;
    } else if node.kind == NDLVAR {
        gen_lval(f, node);
        gen_line!(f, "  pop rax\n");
        gen_line!(f, "  mov rax, [rax]\n");
        gen_line!(f, "  push rax\n");
        return;
    } else if node.kind == NDASSIGN {
        gen_lval(f, *node.lhs.unwrap());
        gen(f, *node.rhs.unwrap());
        gen_line!(f, "  pop rdi\n");
        gen_line!(f, "  pop rax\n");
        gen_line!(f, "  mov [rax], rdi\n");
        gen_line!(f, "  push rdi\n");
        return;
    }
    
    if let Some(lhs) = node.lhs {
        gen(f, *lhs);
    }
    if let Some(rhs) = node.rhs {
        gen(f, *rhs);
    }

    gen_line!(f, "  pop rdi\n");
    gen_line!(f, "  pop rax\n");

    match node.kind {
        NDADD => {
            gen_line!(f, "  add rax, rdi\n");
        }
        NDSUB => {
            gen_line!(f, "  sub rax, rdi\n");
        }
        NDMUL => {
            gen_line!(f, "  imul rax, rdi\n");
        }
        NDDIV => {
            gen_line!(f, "  cqo\n");
            gen_line!(f, "  idiv rdi\n");
        }
        NDEQ => {
            gen_line!(f, "  cmp rax, rdi\n");
            gen_line!(f, "  sete al\n");
            gen_line!(f, "  movzb rax, al\n");
        }
        NDNEQ => {
            gen_line!(f, "  cmp rax, rdi\n");
            gen_line!(f, "  setne al\n");
            gen_line!(f, "  movzb rax, al\n");
        }
        NDLEQ => {
            gen_line!(f, "  cmp rax, rdi\n");
            gen_line!(f, "  setle al\n");
            gen_line!(f, "  movzb rax, al\n");
        }
        NDLT => {
            gen_line!(f, "  cmp rax, rdi\n");
            gen_line!(f, "  setl al\n");
            gen_line!(f, "  movzb rax, al\n");
        }
        _ => {
            panic!("Oops, found a strange node kind.")
        }
    }

    gen_line!(f, "  push rax\n");
}
