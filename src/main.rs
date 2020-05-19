use std::env;
use std::fs::File;
use std::io::Write;

mod tokenizer;
mod parser;

use tokenizer::{TokenIter, Tokenizer};

macro_rules! gen_line {
    ($dst:expr, $($arg: tt)*) => {
        write!($dst, $($arg)*).unwrap()
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        2 => {
            let mut f = match File::create("tmp.s") {
                Err(why) => panic!("Couldn't create tmp.s because {}", why.to_string()),
                Ok(f) => f,
            };

            let mut tk = Tokenizer::new();
            let mut tkiter = TokenIter::new(tk.tokenize(&args[1]));

            // Preamble:
            gen_line!(&mut f, ".intel_syntax noprefix\n");
            gen_line!(&mut f, ".global main\n\n");
            gen_line!(&mut f, "main:\n");

            gen_line!(&mut f, "  mov rax, {}\n", tkiter.expect_number());

            while !tkiter.at_eof() {
                if tkiter.consume("+") {
                    gen_line!(&mut f, "  add rax, {}\n", tkiter.expect_number());
                    continue;
                }

                tkiter.expect("-");
                gen_line!(&mut f, "  sub rax, {}\n", tkiter.expect_number());
            }
            gen_line!(&mut f, "  ret\n");
        }
        _ => {
            eprintln!("Wrong number of arguments!");
        }
    }
}
