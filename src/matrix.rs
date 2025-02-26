#[derive(Debug, Clone, PartialEq)]
pub struct Matrix<T> {
    rows: usize,
    cols: usize,
    data: Vec<T>,
}

impl<T: Default + Clone> Matrix<T> {
    pub fn new(rows: usize, cols: usize) -> Self {
        Self {
            rows,
            cols,
            data: vec![T::default(); rows * cols],
        }
    }

    pub fn from_vec(vec: Vec<Vec<T>>) -> Result<Self, String> {
        let rows = vec.len();
        let cols = if rows > 0 { vec[0].len() } else { 0 };
        for r in vec[1..].iter() {
            if (r.len() != cols) {
                return Err(
                    "ERROR: Dimensions of arrays being concatenated are not consistent".to_string(),
                );
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
}
