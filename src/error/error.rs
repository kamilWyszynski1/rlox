use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct RLoxError {
    pub error: ErrorType,
    pub line: usize,
    pub column: usize,
    pub start: usize,
    pub end: usize,
}

impl Display for RLoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.error {
            ErrorType::Resolve(msg) => {
                write!(
                    f,
                    "resolve error: [line {}, column {}], {}",
                    self.line, self.column, msg,
                )
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ErrorType {
    Resolve(String),
}

impl Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorType::Resolve(msg) => {
                write!(f, "{msg}")
            }
        }
    }
}
