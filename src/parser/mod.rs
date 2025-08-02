//! Parser for Flow language

use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Syntax error at line {0}: {1}")]
    SyntaxError(usize, String),
}

pub struct Parser;

impl Parser {
    pub fn new() -> Self {
        Parser
    }
}