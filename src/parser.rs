use crate::ast::*;
use crate::error::ScanError;
use crate::lexer::{check_tok, Token};
use crate::matrix::Matrix;

type R<T> = Result<T, ScanError>;

pub fn parse(tokens: &[Token]) -> R<Block> {
    let (ts, b) = parse_block(tokens)?;
    if ts.is_empty() {
        Ok(b)
    } else {
        Err(ScanError::UnparsedInput(ts.to_vec().into_boxed_slice()))
    }
}

fn parse_suffix(tokens: &[Token], expr: Expr) -> R<(&[Token], Expr)> {
    match tokens {
        [Token::SQuote, rest @ ..] => parse_suffix(rest, Expr::Transpose(Box::new(expr))),
        _ => Ok((tokens, expr)),
    }
}

fn parse_atom(tokens: &[Token]) -> R<(&[Token], Expr)> {
    let (ts, e) = match tokens {
        [Token::VarName(fun_id), Token::LParen, rest @ ..] => {
            let (ts, e) = parse_expr(rest)?;
            let ts1 = check_tok(Token::RParen, ts)?;
            match fun_id.as_str() {
                "rref" => Ok((ts1, Expr::Rref(Box::new(e)))),
                _ => Err(ScanError::FunctionNotDefined(fun_id.clone())),
            }
        }
        [Token::VarName(id), rest @ ..] => Ok((rest, Expr::Var(id.clone()))),
        [Token::ConstValue(n), rest @ ..] => Ok((rest, Expr::Const(*n))),
        [Token::Minus, Token::ConstValue(n), rest @ ..] => Ok((rest, Expr::Const(*n * -1.0))),
        [Token::Minus, _] => Err(ScanError::UnknownException(
            tokens.to_vec().into_boxed_slice(),
        )),
        [Token::LParen, rest @ ..] => {
            let (ts1, e) = parse_expr(rest)?;
            let ts2 = check_tok(Token::RParen, ts1)?;
            Ok((ts2, e))
        }
        [Token::LBrack, rest @ ..] => {
            let (ts1, x) = parse_matrix(rest)?;
            let ts2 = check_tok(Token::RBrack, ts1)?;
            Ok((ts2, x))
        }
        [] => Err(ScanError::EndBeforeExpected),
        _ => Err(ScanError::CannotParseOn(tokens.to_vec().into_boxed_slice())),
    }?;
    parse_suffix(ts, e)
}

fn parse_term(tokens: &[Token]) -> R<(&[Token], Expr)> {
    let (tokens_rest, e1) = parse_atom(tokens)?;
    fn go(e: Expr, ts: &[Token]) -> R<(&[Token], Expr)> {
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
fn parse_array(tokens: &[Token]) -> R<(&[Token], Vec<Expr>)> {
    let mut nums: Vec<Expr> = Vec::new();
    let (tokens_rest, e1) = parse_expr(tokens)?;
    nums.push(e1);
    fn go<'a>(ts: &'a [Token], nums: &mut Vec<Expr>) -> R<&'a [Token]> {
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
fn parse_matrix(tokens: &[Token]) -> R<(&[Token], Expr)> {
    let mut rows: Vec<Vec<Expr>> = Vec::new();
    if let Some(Token::RBrack) = tokens.first() {
        // empty matrix
        let m = Matrix::new(0, 0, Expr::Const(0.0));
        return Ok((tokens, Expr::Matrix(m)));
    }
    let (tokens_rest, row) = parse_array(tokens)?;
    rows.push(row);
    fn go<'a>(ts: &'a [Token], rows: &mut Vec<Vec<Expr>>) -> R<&'a [Token]> {
        match ts {
            [Token::RBrack, ..] => Ok(ts),
            [Token::SColon, rest @ ..] => {
                let (ts, row) = parse_array(rest)?;
                rows.push(row);
                go(ts, rows)
            }
            [] => Ok(ts),
            _ => Err(ScanError::ArraySeperatorNotFound),
        }
    }
    let ts = go(tokens_rest, &mut rows)?;
    let m = Matrix::from_vec(rows).map_err(ScanError::Matrix)?;
    Ok((ts, Expr::Matrix(m)))
}

fn parse_expr(tokens: &[Token]) -> R<(&[Token], Expr)> {
    fn go(e: Expr, ts: &[Token]) -> R<(&[Token], Expr)> {
        match ts {
            [Token::Plus, rest @ ..] => {
                let (ts1, x) = parse_term(rest)?;
                go(Expr::Add(Box::new(e), Box::new(x)), ts1)
            }
            [Token::Minus, rest @ ..] => {
                let (ts1, x) = parse_term(rest)?;
                go(Expr::Sub(Box::new(e), Box::new(x)), ts1)
            }
            _ => Ok((ts, e)),
        }
    }
    let (tokens_rest, e1) = parse_term(tokens)?;
    go(e1, tokens_rest)
}

fn parse_stmt(tokens: &[Token]) -> R<(&[Token], Stmt)> {
    match tokens {
        [Token::VarName(id), Token::Assign, ts @ ..] => {
            let (ts1, e2) = parse_expr(ts)?;
            Ok((ts1, Stmt::Assign(Expr::Var(id.clone()), e2)))
        }
        _ => {
            let (ts1, e) = parse_expr(tokens)?;
            Ok((ts1, Stmt::Exp(Box::new(e))))
        }
    }
}

fn parse_block(tokens: &[Token]) -> R<(&[Token], Block)> {
    let mut stmts: Vec<Stmt> = Vec::new();
    let (mut ts, mut e) = parse_stmt(tokens)?;
    stmts.push(e);
    while !ts.is_empty() {
        (ts, e) = parse_stmt(ts)?;
        stmts.push(e);
    }
    Ok((ts, Block::Stmts(stmts)))
}
