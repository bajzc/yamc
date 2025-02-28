use crate::ast::Block;
use crate::lexer::{lexer, Token};
use crate::parser::parse;
use crate::symbol::SymbolTable;
use std::io::{BufRead, Read};
pub fn repl<R: Read>(reader: R) {
    let buffer = std::io::BufReader::new(reader);
    let mut t = SymbolTable::new();
    for line in buffer.lines() {
        let tokens = lexer(line.unwrap()).unwrap_or_else(|e| {
            println!("{:?}", e);
            Vec::<Token>::new()
        });
        let ast = parse(&tokens).unwrap_or(Block::Stmts(Vec::new()));
        let val = ast.eval(&mut t);
        println!("ans = {:?}", val)
    }
}
