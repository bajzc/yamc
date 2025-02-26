use crate::lexer::{lexer, Token};
use crate::parser::parse;
use std::io::{BufRead, Read};
pub fn repl<R: Read>(reader: R) {
    let buffer = std::io::BufReader::new(reader);
    for line in buffer.lines() {
        let tokens = lexer(line.unwrap()).unwrap_or_else(|e| {
            println!("{e}");
            Vec::<Token>::new()
        });
        dbg!(&tokens);
        let b_result = parse(&tokens);
        match b_result {
            Ok(b) => println!("{:?}", b),
            Err(msg) => println!("{msg}"),
        }
    }
}
