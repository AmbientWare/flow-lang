//! Lexer for Flow language

use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("Unexpected character '{0}' at line {1}, column {2}")]
    UnexpectedChar(char, usize, usize),
    
    #[error("Unterminated string at line {0}")]
    UnterminatedString(usize),
}

pub struct Lexer;

impl Lexer {
    pub fn new() -> Self {
        Lexer
    }
}