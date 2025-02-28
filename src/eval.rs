use crate::ast::*;
use crate::error::EvalError;
use crate::matrix::Matrix;
use crate::symbol::SymbolTable;
use std::ops::Add;

#[derive(Debug, Clone)]
pub enum Val {
    Unit,
    Matrix(Matrix<Val>),
    Const(f64),
}

impl std::ops::Add<Val> for Val {
    type Output = R<Val>;

    fn add(self, rhs: Val) -> Self::Output {
        match (self, rhs) {
            (Val::Const(a), Val::Const(b)) => Ok(Val::Const(a + b)),
            (Val::Matrix(a), Val::Matrix(b)) => {
                let sum = a.add(b)?;
                Ok(Val::Matrix(sum))
            }
            _ => Err(EvalError::TypeMisMatch),
        }
    }
}

type R<T> = Result<T, EvalError>;

impl Val {
    pub fn add(&self, e: Val) -> R<Val> {
        match (self, e) {
            (Val::Matrix(m1), Val::Matrix(m2)) => m1.clone().add(m2).map(Val::Matrix),
            (Val::Const(n1), Val::Const(n2)) => Ok(Val::Const(n1 + n2)),
            _ => todo!(),
        }
    }

    pub fn sub(&self, e: Val) -> R<Val> {
        match (self, e) {
            (Val::Matrix(m1), Val::Matrix(m2)) => todo!(),
            (Val::Const(n1), Val::Const(n2)) => Ok(Val::Const(n1 - n2)),
            _ => todo!(),
        }
    }
    pub fn mul(&self, e: Val) -> R<Val> {
        match (self, e) {
            (Val::Matrix(m1), Val::Matrix(m2)) => todo!(),
            (Val::Const(n1), Val::Const(n2)) => Ok(Val::Const(n1 * n2)),
            _ => todo!(),
        }
    }
    pub fn div(&self, e: Val) -> R<Val> {
        match (self, e) {
            (Val::Matrix(m1), Val::Matrix(m2)) => todo!(),
            (Val::Const(n1), Val::Const(n2)) => Ok(Val::Const(n1 / n2)),
            _ => todo!(),
        }
    }
}

impl Expr {
    fn eval(&self, t: &mut SymbolTable) -> R<Val> {
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
                _ => todo!(),
            },
        }
    }
}

impl Block {
    pub fn eval(&self, t: &mut SymbolTable) -> R<Val> {
        match self {
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
            Stmt::Assign(_, _) => Err(EvalError::AssignToNoneLValue),
        }
    }
}
