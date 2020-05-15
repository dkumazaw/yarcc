use std::env;
use std::error::Error;
use std::fs::File;
use std::io::Write;

mod tokenizer;

use tokenizer::Tokenizer;

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

            f.write_all(b".intel_syntax noprefix\n").unwrap();
            f.write_all(b".global main\n\n").unwrap();
            f.write_all(b"main:\n").unwrap();
            write!(&mut f, "  mov rax, {}\n", args[1]).unwrap();
            f.write_all(b"  ret\n").unwrap();
        }
        _ => {
            eprintln!("Wrong number of arguments!");
        }
    }
}
