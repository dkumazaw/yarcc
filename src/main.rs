use std::error::Error;
use std::env;
use std::fs::File;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    match args.len() {
        2 => {
            let mut f = match File::create("tmp.s") {
                Err(why) => panic!("Couldn't create tmp.s because {}", why.to_string()),
                Ok(f) => f,
            };

            println!(".intel_syntax noprefix");
            println!(".global main\n");
            println!("main:\n");
            println!("  mov rax, {}\n", args[1]);
            println!("  ret\n");
        },
        _ => {
            eprintln!("Wrong number of arguments!");
        }
    }
}
