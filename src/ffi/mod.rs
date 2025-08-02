//! Foreign Function Interface for Flow language

use thiserror::Error;

#[derive(Debug, Error)]
pub enum FfiError {
    #[error("FFI error: {0}")]
    General(String),
}