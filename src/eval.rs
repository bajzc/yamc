use crate::ast::*;
use crate::error::EvalError;
use crate::matrix::Matrix;
use crate::symbol::SymbolTable;
use std::ops::{Add, Div, Mul, Sub};

#[derive(Debug, Clone)]
pub enum Val {
    Matrix(Matrix<Val>),
    Const(f64),
}

type R<T> = Result<T, EvalError>;

impl From<Val> for Matrix<Val> {
    fn from(value: Val) -> Matrix<Val> {
        match value {
            Val::Matrix(matrix) => matrix,
            Val::Const(_) => Matrix::new(1, 1, value),
        }
    }
}

impl Add<Val> for Val {
    type Output = R<Val>;

    fn add(self, rhs: Val) -> Self::Output {
        match (&self, &rhs) {
            (Val::Const(a), Val::Const(b)) => Ok(Val::Const(a + b)),
            _ => Matrix::from(self).add(&Matrix::from(rhs)).map(Val::Matrix),
        }
    }
}

impl Sub<Val> for Val {
    type Output = R<Val>;

    fn sub(self, rhs: Val) -> Self::Output {
        match (&self, &rhs) {
            (Val::Const(a), Val::Const(b)) => Ok(Val::Const(a - b)),
            _ => Matrix::from(self).sub(&Matrix::from(rhs)).map(Val::Matrix),
        }
    }
}

impl Mul<Val> for Val {
    type Output = R<Val>;

    fn mul(self, rhs: Val) -> Self::Output {
        match (&self, &rhs) {
            (Val::Const(a), Val::Const(b)) => Ok(Val::Const(a * b)),
            _ => Matrix::from(self).mul(&Matrix::from(rhs)).map(Val::Matrix),
        }
    }
}

impl Div<Val> for Val {
    type Output = R<Val>;

    fn div(self, rhs: Val) -> Self::Output {
        match (&self, &rhs) {
            (Val::Const(a), Val::Const(b)) => Ok(Val::Const(a / b)),
            _ => Matrix::from(self).div(&Matrix::from(rhs)).map(Val::Matrix),
        }
    }
}

impl Val {
    pub fn abs(&self) -> R<f64> {
        match &self {
            Val::Const(n) => Ok(n.abs()),
            _ => Err(EvalError::TypeMisMatch),
        }
    }
}

impl Expr {
    fn eval(&self, t: &SymbolTable) -> R<Val> {
        match self {
            Expr::Add(e1, e2) => {
                let a1 = e1.eval(t)?;
                let a2 = e2.eval(t)?;
                a1.add(a2)
            }
            Expr::Const(n) => Ok(Val::Const(*n)),
            Expr::Sub(e1, e2) => {
                let s1 = e1.eval(t)?;
                let s2 = e2.eval(t)?;
                s1.sub(s2)
            }
            Expr::Mul(e1, e2) => {
                let m1 = e1.eval(t)?;
                let m2 = e2.eval(t)?;
                m1.mul(m2)
            }
            Expr::Div(e1, e2) => {
                let d1 = e1.eval(t)?;
                let d2 = e2.eval(t)?;
                d1.div(d2)
            }
            Expr::Var(name) => t
                .get(name)
                .cloned()
                .ok_or(EvalError::VariableNotFound(name.clone())),
            Expr::Matrix(m) => {
                let e = m.try_map(|e| e.eval(t))?;
                Ok(Val::Matrix(e))
            }
            Expr::Transpose(e) => match **e {
                Expr::Matrix(ref m) => m
                    .try_map(|e| e.eval(t))?
                    .transpose()
                    .map(Val::Matrix)
                    .map_err(EvalError::Matrix),
                ref m => {
                    if let Val::Matrix(m) = m.eval(t)? {
                        m.transpose().map(Val::Matrix).map_err(EvalError::Matrix)
                    } else {
                        Err(EvalError::TypeMisMatch)
                    }
                }
            },
            Expr::Rref(expr) => {
                let e = expr.eval(t)?;
                match e {
                    Val::Matrix(matrix) => Ok(matrix.rref().map(Val::Matrix)?),
                    Val::Const(_) => Err(EvalError::TypeMisMatch),
                }
            }
        }
    }
}

impl Block {
    pub fn eval(&self, t: &mut SymbolTable) -> R<Val> {
        match &self {
            // TODO
            Block::Stmts(stmts) => stmts[0].eval(t),
        }
    }
}

impl Stmt {
    pub fn eval(&self, t: &mut SymbolTable) -> R<Val> {
        match self {
            // TODO
            Stmt::Exp(e) => e.eval(t),
            Stmt::Assign(Expr::Var(id), r) => {
                let e = r.eval(t)?;
                t.insert(id.clone(), e.clone());
                Ok(e)
            }
            Stmt::Assign(_, _) => Err(EvalError::AssignToRValue),
        }
    }
}
