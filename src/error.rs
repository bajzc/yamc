use crate::lexer::Token;
#[derive(Debug)]
pub enum MatrixError {
    ConstructionFromEmptyVector,
    DimensionNotConsistent,
    DimensionIncorrect((usize, usize), (usize, usize)),
}

#[derive(Debug)]
pub enum ScanError {
    Matrix(MatrixError),
    UnparsedInput(Box<[Token]>),
    EndBeforeExpected,
    CannotParseOn(Box<[Token]>),
    UnknownException(Box<[Token]>),
    Exception(Token, Box<[Token]>),
    UnknownCharacter(char),
    ArraySeperatorNotFound,
    FunctionNotDefined(String),
}

#[derive(Debug)]
pub enum EvalError {
    Matrix(MatrixError),
    TypeMisMatch,
    VariableNotFound(String),
    AssignToRValue,
    EmptyBlock,
}

impl From<MatrixError> for EvalError {
    fn from(value: MatrixError) -> Self {
        EvalError::Matrix(value)
    }
}
