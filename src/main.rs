use std::env;
use std::fs::File;
use std::io::Write;

#[macro_use]
macro_rules! gen_line {
    ($dst:expr, $($arg: tt)*) => {
        write!($dst, $($arg)*).unwrap()
    }
}

mod tokenizer;
mod parser;
mod codegen;

use tokenizer::{TokenIter, Tokenizer};
use parser::Parser;
use codegen::gen;

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        2 => {
            let mut f = match File::create("tmp.s") {
                Err(why) => panic!("Couldn't create tmp.s because {}", why.to_string()),
                Ok(f) => f,
            };

            let mut tk = Tokenizer::new();
            let tkiter = TokenIter::new(tk.tokenize(&args[1]));
            let mut parser = Parser::new(tkiter);
            let root = parser.parse();

            // Preamble:
            gen_line!(&mut f, ".intel_syntax noprefix\n");
            gen_line!(&mut f, ".global main\n\n");
            gen_line!(&mut f, "main:\n");

            gen(&mut f, root);

            gen_line!(&mut f, "  pop rax\n");
            gen_line!(&mut f, "  ret\n");
        }
        _ => {
            eprintln!("Wrong number of arguments!");
        }
    }
}
