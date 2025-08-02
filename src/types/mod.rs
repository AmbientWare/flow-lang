//! Type system for Flow language

mod type_system;

pub use type_system::{Type, TypeChecker};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum TypeError {
    #[error("Type mismatch: expected {expected}, got {found}")]
    TypeMismatch { expected: Type, found: Type },
    
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),
    
    #[error("Undefined function: {0}")]
    UndefinedFunction(String),
    
    #[error("Undefined type: {0}")]
    UndefinedType(String),
    
    #[error("Arity mismatch: expected {expected} arguments, found {found}")]
    ArityMismatch { expected: usize, found: usize },
    
    #[error("Type {0} is not callable")]
    NotCallable(Type),
    
    #[error("Type {0} is not a record")]
    NotARecord(Type),
    
    #[error("Record type {record_type} has no field '{field}'")]
    NoSuchField { record_type: Type, field: String },
    
    #[error("Type {0} is not indexable")]
    NotIndexable(Type),
    
    #[error("Type {0} is not iterable")]
    NotIterable(Type),
    
    #[error("Invalid operand type {ty} for operator {op}")]
    InvalidOperandType { op: String, ty: Type },
    
    #[error("Invalid transform")]
    InvalidTransform,
}