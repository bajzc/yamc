use std::iter::{self, from_fn};
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    VarName(String),
    ConstValue(f64),
    Plus,
    Minus,
    Times,
    Divide,
    LParen, // '('
    RParen, // ')'
    LBrack, // '['
    RBrack, // ']'
    LBrace, // '{'
    RBrace, // '}'
    Dot,    // '.'
    Assign, // '='
    Eq,     // '=='
    Neq,    // '!='
    SQuote, // '\''
    Comma,  // ','
}

// check the first token and return the rest tokens if matched
pub fn check_tok(t: Token, ts: &[Token]) -> Result<&[Token], String> {
    match ts {
        [s, tail @ ..] => {
            if t == *s {
                Ok(tail)
            } else {
                Err(format!("ERROR: {:?} expected, but found {:?}", t, s))
            }
        }
        [] => Err(format!("ERROR: {:?} expected, but found nothing", t)),
    }
}

pub fn lexer(input: String) -> Result<Vec<Token>, String> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut iter = input.chars().peekable();
    let mut line_count = 1;

    while let Some(ch) = iter.next() {
        match ch {
            '\n' => {
                line_count += 1;
                continue;
            }
            ch if ch.is_whitespace() => continue,
            '+' => tokens.push(Token::Plus),
            '-' => tokens.push(Token::Minus),
            '*' => tokens.push(Token::Times),
            '/' => tokens.push(Token::Divide),
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            '[' => tokens.push(Token::LBrack),
            ']' => tokens.push(Token::RBrack),
            '{' => tokens.push(Token::LBrace),
            '}' => tokens.push(Token::RBrace),
            '.' => tokens.push(Token::Dot),
            ',' => tokens.push(Token::Comma),
            '=' => {
                if *iter.peek().unwrap_or(&'\0') == '=' {
                    iter.next();
                    tokens.push(Token::Eq);
                } else {
                    tokens.push(Token::Assign);
                }
            }
            '!' => {
                if *iter.peek().unwrap_or(&'\0') == '=' {
                    iter.next();
                    tokens.push(Token::Neq);
                }
            }
            '\'' => tokens.push(Token::SQuote),
            '1'..='9' => {
                // the integer part
                let i: i64 = iter::once(ch)
                    .chain(from_fn(|| iter.by_ref().next_if(|s| s.is_ascii_digit())))
                    .collect::<String>()
                    .parse()
                    .unwrap();

                if *iter.peek().unwrap_or(&'\0') == '.' {
                    // has decimal part
                    let mut d: f64 = f64::from(i as i32);
                    d += iter::once(iter.next().unwrap())
                        .chain(from_fn(|| iter.by_ref().next_if(|s| s.is_ascii_digit())))
                        .collect::<String>()
                        .parse::<f64>()
                        .unwrap();
                    tokens.push(Token::ConstValue(d));
                } else {
                    tokens.push(Token::ConstValue(i as f64));
                }
            }
            ch if ch.is_ascii_alphabetic() => {
                // variable name
                let name = iter::once(ch)
                    .chain(from_fn(|| {
                        iter.by_ref()
                            .next_if(|s| s.is_ascii_alphanumeric() || *s == '_')
                    }))
                    .collect::<String>();
                tokens.push(Token::VarName(name));
            }
            _ => {
                return Err(format!(
                    "ERROR: unexpected character '{ch}' at line {line_count}"
                ));
            }
        }
    }
    Ok(tokens)
}
