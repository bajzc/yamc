use crate::lexer::{check_tok, Token};

#[derive(Debug)]
pub enum Block {
    Stmts(Vec<Stmt>),
}
#[derive(Debug)]
pub enum Stmt {
    Assign(Expr, Expr),
    Exp(Box<Expr>),
}

#[derive(Debug)]
pub enum Expr {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>), // actually not used
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Const(f64),
    Var(String),
}

pub fn parse(tokens: &[Token]) -> Result<Block, String> {
    let (ts, b) = parse_block(tokens)?;
    if ts.is_empty() {
        Ok(b)
    } else {
        Err(format!("ERROR: Unparsed input: {:?}", ts))
    }
}

fn parse_atom(tokens: &[Token]) -> Result<(&[Token], Expr), String> {
    let t = tokens
        .first()
        .ok_or("ERROR: Unexpected end of input".to_string())?;
    match t {
        Token::VarName(id) => Ok((&tokens[1..], Expr::Var(id.to_string()))),
        Token::ConstValue(n) => Ok((&tokens[1..], Expr::Const(*n))),
        Token::Minus => tokens
            .get(1)
            .ok_or_else(|| "ERROR: numeric literal expected, but found nothing".to_string())
            .and_then(|t| match t {
                Token::ConstValue(n) => Ok((&tokens[2..], Expr::Const((*n) * -1.0))),
                _ => Err(format!(
                    "ERROR: numeric literal expected, but found {:?}",
                    t
                )),
            }),
        Token::LParen => {
            let (ts1, e) = parse_expr(&tokens[1..])?;
            let ts2 = check_tok(Token::RParen, ts1)?;
            Ok((ts2, e))
        }
        _ => Err(format!("ERROR: No expresion found {:?}", tokens)),
    }
}

fn parse_term(tokens: &[Token]) -> Result<(&[Token], Expr), String> {
    let (tokens_rest, e1) = parse_atom(tokens)?;
    fn go(e: Expr, ts: &[Token]) -> Result<(&[Token], Expr), String> {
        match ts {
            [Token::Times, ts @ ..] => {
                let (ts1, x) = parse_atom(ts)?;
                go(Expr::Mul(Box::new(e), Box::new(x)), ts1)
            }
            [Token::Divide, ts @ ..] => {
                let (ts1, x) = parse_atom(ts)?;
                go(Expr::Div(Box::new(e), Box::new(x)), ts1)
            }
            _ => Ok((ts, e)),
        }
    }
    go(e1, tokens_rest)
}

fn parse_expr(tokens: &[Token]) -> Result<(&[Token], Expr), String> {
    let (tokens_rest, e1) = parse_term(tokens)?;
    fn go(e: Expr, ts: &[Token]) -> Result<(&[Token], Expr), String> {
        match ts {
            [Token::Plus, ts @ ..] => {
                let (ts1, x) = parse_term(ts)?;
                go(Expr::Add(Box::new(e), Box::new(x)), ts1)
            }
            [Token::Minus, ts @ ..] => {
                let (ts1, x) = parse_term(ts)?;
                go(Expr::Sub(Box::new(e), Box::new(x)), ts1)
            }
            _ => Ok((ts, e)),
        }
    }
    go(e1, tokens_rest)
}

fn parse_stmt(tokens: &[Token]) -> Result<(&[Token], Stmt), String> {
    match tokens {
        [Token::VarName(id), Token::Assign, ts @ ..] => {
            let (ts1, e2) = parse_expr(ts)?;
            Ok((ts1, Stmt::Assign(Expr::Var(id.to_string()), e2)))
        }
        _ => {
            let (ts1, e) = parse_expr(tokens)?;
            Ok((ts1, Stmt::Exp(Box::new(e))))
        }
    }
}

fn parse_block(tokens: &[Token]) -> Result<(&[Token], Block), String> {
    let mut stmts: Vec<Stmt> = Vec::new();
    let (mut ts, mut e) = parse_stmt(tokens)?;
    stmts.push(e);
    while !ts.is_empty() {
        (ts, e) = parse_stmt(ts)?;
        stmts.push(e);
    }
    Ok((ts, Block::Stmts(stmts)))
}
