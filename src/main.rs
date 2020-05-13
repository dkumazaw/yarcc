use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    match args.len() {
        2 => {
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
