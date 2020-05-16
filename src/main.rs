use std::env;
use std::error::Error;
use std::fs::File;
use std::io::Write;

mod tokenizer;

use tokenizer::Tokenizer;

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

            let tk = Tokenizer::new()
                               .tokenize(&args[1]);

            // Preamble: 
            gen_line!(&mut f, ".intel_syntax noprefix\n");
            gen_line!(&mut f, ".global main\n\n");
            gen_line!(&mut f, "main:\n");


            gen_line!(&mut f, "  mov rax, {}\n", args[1]);

            //while !tk.at_eof() {
                
            //}

            gen_line!(&mut f, "  ret\n");
        }
        _ => {
            eprintln!("Wrong number of arguments!");
        }
    }
}
