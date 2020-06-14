use std::env;
use std::fs::{self, File};
use std::path::Path;

#[macro_use]
macro_rules! gen_line {
    ($dst:expr, $($arg: tt)*) => {
        write!($dst, $($arg)*).unwrap()
    }
}

mod codegen;
mod node;
mod parser;
mod tokenizer;
mod ty;

use codegen::CodeGen;
use parser::Parser;
use tokenizer::Tokenizer;

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        2 => {
            let mut f = match File::create("tmp.s") {
                Err(why) => panic!("yarcc: Couldn't create tmp.s because {}", why.to_string()),
                Ok(f) => f,
            };

            let in_str = if Path::new(&args[1]).exists() {
                if let Ok(text) = fs::read_to_string(&args[1]) {
                    text
                } else {
                    panic!("yarcc: Cannot read the provided file.");
                }
            } else {
                args[1].to_string()
            };

            let tokens = Tokenizer::new().tokenize(in_str);
            let parser = Parser::new(tokens);
            let parsed_program = parser.parse();
            let mut codegen = CodeGen::new(&mut f, parsed_program);

            codegen.gen_all();
        }
        _ => {
            eprintln!("yarcc: Wrong number of arguments!");
        }
    }
}
