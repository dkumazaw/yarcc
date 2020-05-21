use std::env;
use std::fs::File;

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
use codegen::CodeGen;

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
            codegen.gen_preamble(parsed.locals.len());

            // Create enough space for variables
            loop {
                if let Some(node) = parsed.nodes.pop_front() {
                    codegen.gen(node);
                    codegen.pop_rax();
                } else {
                    break;
                }
            }

            // Postamble:
            codegen.gen_postamble();
        }
        _ => {
            eprintln!("Wrong number of arguments!");
        }
    }
}
