mod ast;
mod cli;
mod error;
mod eval;
mod lexer;
mod matrix;
mod parser;
mod symbol;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => cli::repl(std::io::stdin()),
        2 => match std::fs::File::open(&args[1]) {
            Ok(fp) => cli::repl(fp),
            Err(msg) => {
                println!("ERROR: {msg}");
                println!("Switch to stdin instead");
                cli::repl(std::io::stdin());
            }
        },
        _ => {
            println!("usage: ./{} path/to/input/file", args[0]);
            println!("or don't pass any argument")
        }
    };
}
