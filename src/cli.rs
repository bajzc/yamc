use crate::lexer::{lexer, Token};
use crate::parser::parse;
use crate::symbol::SymbolTable;
use std::io::{BufRead, Read};
pub fn repl<R: Read>(reader: R) {
    let buffer = std::io::BufReader::new(reader);
    let mut t = SymbolTable::new();
    let mut line_number = 1;
    for line in buffer.lines() {
        let tokens = lexer(line.unwrap()).unwrap_or_else(|e| {
            println!("{:?}", e);
            Vec::<Token>::new()
        });
        let ast_result = parse(&tokens);
        match ast_result {
            Ok(ast) => {
                if let Ok(val) = ast.eval(&mut t).map_err(|e| println!("{:?}", e)) {
                    println!("${line_number} ans = {:?}", val);
                    t.insert("ans".to_string(), val.clone());
                }
            }
            Err(e) => println!("${line_number} {:?}", e),
        }
        line_number += 1;
    }
}
