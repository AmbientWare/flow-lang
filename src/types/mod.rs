//! Type system for Flow language

use thiserror::Error;

#[derive(Debug, Error)]
pub enum TypeError {
    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },
}