use crate::error::EvalError;
use crate::error::MatrixError;
use std::ops::{Add, Index};

type R<T> = Result<T, MatrixError>;

#[derive(Debug, Clone, PartialEq)]
pub struct Matrix<T> {
    rows: usize,
    cols: usize,
    data: Vec<T>,
}

impl<T: Clone> Matrix<T> {
    pub fn new(rows: usize, cols: usize, init_value: T) -> Self {
        Self {
            rows,
            cols,
            data: vec![init_value; rows * cols],
        }
    }

    pub fn from_vec(vec: Vec<Vec<T>>) -> R<Self> {
        let rows = vec.len();
        let cols = if rows > 0 {
            vec.first()
                .ok_or(MatrixError::ConstructionFromEmptyVector)?
                .len()
        } else {
            0
        };
        for r in vec[1..].iter() {
            if r.len() != cols {
                return Err(MatrixError::DimensionNotConsistent);
            }
        }
        let data = vec.into_iter().flatten().collect();
        Ok(Self { rows, cols, data })
    }

    pub fn rows(&self) -> usize {
        self.rows
    }

    pub fn cols(&self) -> usize {
        self.cols
    }

    pub fn try_map<F, B, E>(&self, mut f: F) -> Result<Matrix<B>, E>
    where
        F: FnMut(&T) -> Result<B, E>,
    {
        let new_data = self
            .data
            .iter()
            .map(&mut f)
            .collect::<Result<Vec<B>, E>>()?;
        Ok(Matrix {
            rows: self.rows,
            cols: self.cols,
            data: new_data,
        })
    }

    pub fn map<F, B>(&self, f: F) -> Matrix<B>
    where
        F: FnMut(&T) -> B,
    {
        let new_data = self.data.iter().map(f).collect();
        Matrix {
            rows: self.rows,
            cols: self.cols,
            data: new_data,
        }
    }

    pub fn transpose(&self) -> R<Self> {
        let mut data = Vec::with_capacity(self.data.len());
        for c in 0..self.cols {
            for r in 0..self.rows {
                data.push(self[(r, c)].clone());
            }
        }
        Ok(Self {
            rows: self.cols,
            cols: self.rows,
            data,
        })
    }
}
impl<T: Clone + Add<Output = Result<T, EvalError>>> std::ops::Add<Matrix<T>> for Matrix<T> {
    type Output = Result<Self, EvalError>;
    fn add(self, rhs: Matrix<T>) -> Self::Output {
        if self.rows != rhs.rows || self.cols != rhs.cols {
            Err(EvalError::Matrix(MatrixError::DiemesionIncorrect(
                (self.rows, self.cols),
                (rhs.rows, rhs.cols),
            )))
        } else {
            let res = self
                .data
                .iter()
                .zip(rhs.data.iter())
                .map(|(x, y)| x.clone() + y.clone())
                .collect::<Result<_, _>>()?;
            Ok(Matrix {
                rows: self.cols,
                cols: self.cols,
                data: res,
            })
        }
    }
}

impl<T> Index<(usize, usize)> for Matrix<T> {
    type Output = T;
    fn index(&self, (row, col): (usize, usize)) -> &Self::Output {
        assert!(row < self.rows && col < self.cols);
        &self.data[row * self.cols + col]
    }
}
