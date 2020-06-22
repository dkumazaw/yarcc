# yarcc: A hobby C compiler written in Rust [WIP]
This is a hobby C compiler written from scratch in Rust. This compiler emits x86_64 assembly compliant with System V ABI. This ongoing project aims to support the majority of C89/90 features. 

## Usage:
Currently, you can either provide the raw C code as the first argument or pass the path to the file you want to compile:

```cargo run examples/singlenum.c```

All of the development and testing have taken place on an Ubuntu 18.04 VM. 


## Major TODOs:
- Preprocessor
- Initializer for multi-level arrays
- Floating point arithmetics
- Type casting
- Replace the lexer with a DFA-based implementation

## Caveats:
The nature of this project being educational, the codebase is quite lax on error checking. 

On another note, I decided not to support obsolete features such as the following: 
- Trigraphs
- K&R style declarations and definitions
- Function declarations without a prototype

## References:
In the earliest stage of development, I have predominantly referred to [this book](https://www.sigbus.info/compilerbook). In addition to [the normative reference](https://www.pdf-archive.com/2014/10/02/ansi-iso-9899-1990-1/ansi-iso-9899-1990-1.pdf) which is the standard itself, I have also referred to [the Dragon Book](https://en.wikipedia.org/wiki/Compilers:_Principles,_Techniques,_and_Tools) for gaining design insights.
