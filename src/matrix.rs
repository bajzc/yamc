use crate::error::EvalError;
use crate::error::MatrixError;
use crate::eval::Val;
use std::ops::{Add, Div, Index, Mul, Sub};

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

    // https://mathworks.com/help/matlab/matlab_prog/compatible-array-sizes-for-basic-operations.html
    pub fn expand(&self, b: &Matrix<T>) -> R<Matrix<T>> {
        if self.cols == b.cols && self.rows == b.rows {
            return Ok(self.clone());
        }
        match ((self.rows, self.cols), (b.rows, b.cols)) {
            ((_, _), (1, 1)) => Ok(self.clone()),
            ((i, j1), (1, j2)) => {
                if j1 == j2 {
                    Ok(self.clone())
                } else {
                    Err(MatrixError::DimensionIncorrect((i, j1), (1, j2)))
                }
            }
            ((i1, j), (i2, 1)) => {
                if i1 == i2 {
                    Ok(self.clone())
                } else {
                    Err(MatrixError::DimensionIncorrect((i1, j), (i2, 1)))
                }
            }
            ((1, 1), (i, j)) => Ok(Self {
                rows: i,
                cols: j,
                data: vec![self.data[0].clone(); i * j],
            }),
            ((1, j1), (i, j2)) => {
                if j1 != j2 {
                    Err(MatrixError::DimensionIncorrect((1, j1), (i, j2)))
                } else {
                    Ok(Self {
                        rows: i,
                        cols: j1,
                        data: vec![self.data.clone(); i].concat(),
                    })
                }
            }
            ((i1, 1), (i2, j)) => {
                if i1 != i2 {
                    Err(MatrixError::DimensionIncorrect((i1, 1), (i2, j)))
                } else {
                    let mut new_data = Vec::with_capacity(i1 * j);
                    for row in &self.data {
                        new_data.extend(std::iter::repeat(row.clone()).take(j));
                    }
                    Ok(Self {
                        rows: i1,
                        cols: j,
                        data: new_data,
                    })
                }
            }
            _ => Err(MatrixError::DimensionIncorrect(
                (self.rows, self.cols),
                (b.rows, b.cols),
            )),
        }
    }
}
impl Matrix<Val> {
    fn horzcat(column_elements: &[Val]) -> R<Matrix<Val>> {
        assert!(!column_elements.is_empty());
        let flatten = column_elements
            .iter()
            .map(|e| match e {
                Val::Matrix(matrix) => Self::flat(matrix).map(Val::Matrix),
                Val::Const(_) => Ok(e.clone()),
            })
            .collect::<Result<Vec<_>, _>>()?;

        let rows = match &flatten[0] {
            Val::Matrix(matrix) => matrix.rows,
            Val::Const(_) => 1,
        };
        let mut data: Vec<Vec<Val>> = vec![vec![]; rows];
        flatten.into_iter().try_for_each(|e| match e {
            Val::Matrix(matrix) => {
                if matrix.rows != rows {
                    Err(MatrixError::DimensionNotConsistent)
                } else {
                    for i in 0..rows {
                        for j in 0..matrix.cols {
                            data[i].push(matrix[(i, j)].clone());
                        }
                    }
                    Ok(())
                }
            }
            Val::Const(_) => {
                if rows != 1 {
                    Err(MatrixError::DimensionNotConsistent)
                } else {
                    data[0].push(e);
                    Ok(())
                }
            }
        })?;
        Ok(Self {
            rows,
            cols: data[0].len(),
            data: data.concat(),
        })
    }

    // row_elements need to be flat
    fn vertcat(row_elements: &[Val]) -> R<Matrix<Val>> {
        assert!(!row_elements.is_empty());
        let cols = match &row_elements[0] {
            Val::Matrix(matrix) => matrix.cols,
            Val::Const(_) => 1,
        };

        let mut data: Vec<Val> = Vec::new();
        let mut rows = 0;

        row_elements.iter().try_for_each(|e| match e {
            Val::Matrix(matrix) => {
                if matrix.cols != cols {
                    Err(MatrixError::DimensionNotConsistent)
                } else {
                    data.append(&mut matrix.data.clone());
                    rows += matrix.rows;
                    Ok(())
                }
            }
            Val::Const(_) => {
                if cols != 1 {
                    Err(MatrixError::DimensionNotConsistent)
                } else {
                    data.push(e.clone());
                    rows += 1;
                    Ok(())
                }
            }
        })?;

        Ok(Self { rows, cols, data })
    }

    /*
    >> A=[1 2 3; 4 5 6]
    >> B = [1;2]
    >> C = [A B]
        */
    pub fn flat(&self) -> R<Matrix<Val>> {
        Self::vertcat(
            &(0..self.rows)
                .map(|i| {
                    Self::horzcat(&self.data[(i * self.cols)..((i + 1) * self.cols)])
                        .map(Val::Matrix)
                })
                .collect::<Result<Vec<_>, _>>()?,
        )
    }

    // flat the matrix before calling this function
    pub fn rref(self) -> Result<Matrix<Val>, EvalError> {
        // row-echelon form
        // https://en.wikipedia.org/wiki/Gaussian_elimination#Pseudocode
        let matrix = self.flat()?;
        let mut data = matrix.data;
        let mut h = 0; // pivot row
        let mut k = 0; // pivot column
        while h < matrix.rows && k < matrix.cols {
            // find the k-th pivot
            let mut i_max = h;
            for i in h..matrix.rows {
                if data[i * matrix.cols + k].abs()? > data[i_max * matrix.cols + k].abs()? {
                    i_max = i;
                }
            }
            if let Val::Const(0.0) = data[i_max * matrix.cols + k] {
                k += 1;
            } else {
                // swap rows(h, i_max)
                for i in 0..matrix.cols {
                    let tmp = data[matrix.cols * h + i].clone();
                    data[matrix.cols * h + i] = data[matrix.cols * i_max + i].clone();
                    data[matrix.cols * i_max + i] = tmp;
                }

                for i in h + 1..matrix.rows {
                    let f = data[i * matrix.cols + k]
                        .clone()
                        .div(data[h * matrix.cols + k].clone())?;
                    data[i * matrix.cols + k] = Val::Const(0.0);
                    for j in k + 1..matrix.cols {
                        data[i * matrix.cols + j] = data[i * matrix.cols + j]
                            .clone()
                            .sub(data[h * matrix.cols + j].clone().mul(f.clone())?)?;
                    }
                }
                h += 1;
                k += 1;
            }
        }
        // reduced ref
        // TODO

        Ok(Matrix {
            rows: self.rows,
            cols: self.cols,
            data,
        })
    }
}
impl<T: Clone + Add<Output = Result<T, EvalError>>> std::ops::Add<&Matrix<T>> for &Matrix<T> {
    type Output = Result<Matrix<T>, EvalError>;
    fn add(self, rhs: &Matrix<T>) -> Self::Output {
        let a = self.expand(rhs)?;
        let b = rhs.expand(&a)?;
        let data = a
            .data
            .iter()
            .zip(b.data.iter())
            .map(|(x, y)| x.clone() + y.clone())
            .collect::<Result<_, _>>()?;
        Ok(Matrix {
            rows: self.rows,
            cols: self.cols,
            data,
        })
    }
}

impl<T: Clone + Sub<Output = Result<T, EvalError>>> std::ops::Sub<&Matrix<T>> for &Matrix<T> {
    type Output = Result<Matrix<T>, EvalError>;

    fn sub(self, rhs: &Matrix<T>) -> Self::Output {
        let a = self.expand(rhs)?;
        let b = rhs.expand(&a)?;
        let data = a
            .data
            .iter()
            .zip(b.data.iter())
            .map(|(x, y)| x.clone() - y.clone())
            .collect::<Result<_, _>>()?;
        Ok(Matrix {
            rows: self.cols,
            cols: self.cols,
            data,
        })
    }
}

impl<T: Clone + Add<Output = Result<T, EvalError>> + Mul<Output = Result<T, EvalError>>>
    Mul<&Matrix<T>> for &Matrix<T>
{
    type Output = Result<Matrix<T>, EvalError>;

    fn mul(self, rhs: &Matrix<T>) -> Self::Output {
        if self.cols != rhs.rows {
            return Err(EvalError::Matrix(MatrixError::DimensionIncorrect(
                (self.rows, self.cols),
                (rhs.rows, rhs.cols),
            )));
        }
        assert!(self.rows * rhs.cols > 0);
        let mut data = Vec::with_capacity(self.rows * rhs.cols);
        for i in 0..self.rows {
            for j in 0..rhs.cols {
                let mut sum: T = self[(i, 0)].clone().mul(rhs[(0, j)].clone())?;
                for k in 1..self.cols {
                    let p = self[(i, k)].clone().mul(rhs[(k, j)].clone())?;
                    sum = sum.add(p)?;
                }
                data.push(sum);
            }
        }

        Ok(Matrix {
            rows: self.rows,
            cols: self.cols,
            data,
        })
    }
}

impl<T: Clone + Div<Output = Result<T, EvalError>>> Div<&Matrix<T>> for &Matrix<T> {
    type Output = Result<Matrix<T>, EvalError>;

    fn div(self, rhs: &Matrix<T>) -> Self::Output {
        todo!()
    }
}

impl<T> Index<(usize, usize)> for Matrix<T> {
    type Output = T;
    fn index(&self, (row, col): (usize, usize)) -> &Self::Output {
        assert!(row < self.rows && col < self.cols);
        &self.data[row * self.cols + col]
    }
}
