use std::fs::File;
use crate::parser::Node;
use crate::parser::NodeKind::*;
use std::io::Write;

macro_rules! gen_line {
    ($dst:expr, $($arg: tt)*) => {
        write!($dst, $($arg)*).unwrap()
    }
}

pub fn gen(f: &mut File, node: Node) {
    if node.kind == NDNUM {
        gen_line!(f, "  push {}\n", node.val.unwrap());
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
        _ => {
            panic!("Oops, found a strange node kind.")
        }
    }

    gen_line!(f, "  push rax\n");
}
