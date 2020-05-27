use std::env;
use std::fs::File;

#[macro_use]
macro_rules! gen_line {
    ($dst:expr, $($arg: tt)*) => {
        write!($dst, $($arg)*).unwrap()
    }
}

mod codegen;
mod parser;
mod tokenizer;

use codegen::CodeGen;
use parser::Parser;
use tokenizer::{TokenIter, Tokenizer};

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
            let parser = Parser::new(tkiter);
            let mut parsed = parser.parse();
            let mut codegen = CodeGen::new(&mut f);

            // Preamble:
            codegen.gen_preamble();

            loop {
                if let Some(node) = parsed.nodes.pop_front() {
                    codegen.gen(node);
                } else {
                    break;
                }
            }
        }
        _ => {
            eprintln!("Wrong number of arguments!");
        }
    }
}
