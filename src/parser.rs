use crate::lexer::{check_tok, Token};
use crate::matrix::Matrix;

#[derive(Debug)]
pub enum Block {
    Stmts(Vec<Stmt>),
}
#[derive(Debug)]
pub enum Stmt {
    Assign(Expr, Expr),
    Exp(Box<Expr>),
}

#[derive(Debug, Clone, Default)]
pub enum Expr {
    #[default]
    Nothing,

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>), // actually not used
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Const(f64),
    Var(String),
    Matrix(Matrix<Expr>),
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

// exp | array ' ' exp | array ',' exp
fn parse_array(tokens: &[Token]) -> Result<(&[Token], Vec<Expr>), String> {
    let mut nums: Vec<Expr> = Vec::new();
    let (tokens_rest, e1) = parse_expr(tokens)?;
    nums.push(e1);
    fn go<'a>(ts: &'a [Token], nums: &mut Vec<Expr>) -> Result<&'a [Token], String> {
        match ts {
            [Token::Comma, rest @ ..] => {
                let (rest_after_expr, expr) = parse_expr(rest)?;
                nums.push(expr);
                go(rest_after_expr, nums)
            }
            [Token::SColon, ..] => Ok(ts),
            [Token::RBrack, ..] => Ok(ts),
            _ => {
                let (ts1, x) = parse_expr(ts)?;
                nums.push(x);
                go(ts1, nums)
            }
        }
    }
    let ts = go(tokens_rest, &mut nums)?;
    Ok((ts, nums))
}

// array | matrix ; array | epsilon
fn parse_matrix(tokens: &[Token]) -> Result<(&[Token], Expr), String> {
    let mut rows: Vec<Vec<Expr>> = Vec::new();
    let (tokens_rest, row) = parse_array(tokens)?;
    rows.push(row);
    fn go<'a>(ts: &'a [Token], rows: &mut Vec<Vec<Expr>>) -> Result<&'a [Token], String> {
        match ts {
            [Token::RBrack, ..] => Ok(ts),
            [Token::SColon, rest @ ..] => {
                let (ts, row) = parse_array(rest)?;
                rows.push(row);
                go(ts, rows)
            }
            [] => Ok(ts),
            _ => Err(format!(
                "ERROR: array seperator ';' expected, but found {:?}",
                ts
            )),
        }
    }
    let ts = go(tokens_rest, &mut rows)?;
    let m = Matrix::from_vec(rows)?;
    Ok((ts, Expr::Matrix(m)))
}

fn parse_expr(tokens: &[Token]) -> Result<(&[Token], Expr), String> {
    match tokens {
        [Token::LBrack, rest @ ..] => {
            let (ts1, x) = parse_matrix(rest)?;
            let ts2 = check_tok(Token::RBrack, ts1)?;
            Ok((ts2, x))
        }
        _ => {
            let (tokens_rest, e1) = parse_term(tokens)?;
            fn go(e: Expr, ts: &[Token]) -> Result<(&[Token], Expr), String> {
                match ts {
                    [Token::Plus, rest @ ..] => {
                        let (ts1, x) = parse_term(rest)?;
                        go(Expr::Add(Box::new(e), Box::new(x)), ts1)
                    }
                    [Token::Minus, rest @ ..] => {
                        let (ts1, x) = parse_term(rest)?;
                        go(Expr::Sub(Box::new(e), Box::new(x)), ts1)
                    }
                    [Token::LBrack, rest @ ..] => {
                        dbg!("called");
                        let (ts1, x) = parse_matrix(rest)?;
                        let ts2 = check_tok(Token::RBrack, ts1)?;
                        Ok((ts2, x))
                    }
                    _ => Ok((ts, e)),
                }
            }
            go(e1, tokens_rest)
        }
    }
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
