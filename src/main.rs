use std::error::Error;
use std::env;
use std::fs::File;
use std::io::Write;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    match args.len() {
        2 => {
            let mut f = match File::create("tmp.s") {
                Err(why) => panic!("Couldn't create tmp.s because {}", why.to_string()),
                Ok(f) => f,
            };

            f.write_all(b"hoge").unwrap();
            f.write_all(b".intel_syntax noprefix").unwrap();
            f.write_all(b".global main\n").unwrap();
            f.write_all(b"main:\n").unwrap();
            write!(& mut f, "  mov rax, {}\n", args[1]).unwrap();
            f.write_all(b"  ret\n").unwrap();
        },
        _ => {
            eprintln!("Wrong number of arguments!");
        }
    }
}
