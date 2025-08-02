//! Runtime for Flow language

use thiserror::Error;

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("Runtime error: {0}")]
    General(String),
}