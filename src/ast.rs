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

#[derive(Debug, Clone)]
pub enum Expr {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Const(f64),
    Var(String),
    Matrix(Matrix<Expr>),
    Transpose(Box<Expr>),
    Rref(Box<Expr>),
}
