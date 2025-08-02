//! Type system implementation for Flow language

use crate::parser::{Expr, Literal, TypeExpr, Function, Statement, Pattern};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Integer,
    Decimal,
    String,
    Boolean,
    List(Box<Type>),
    Map { key: Box<Type>, value: Box<Type> },
    Function { params: Vec<Type>, returns: Box<Type> },
    Record { fields: HashMap<String, Type> },
    Named(String),
    Any,
    Never,
    Optional(Box<Type>),
    Union(Vec<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Integer => write!(f, "Integer"),
            Type::Decimal => write!(f, "Decimal"),
            Type::String => write!(f, "String"),
            Type::Boolean => write!(f, "Boolean"),
            Type::List(inner) => write!(f, "List<{}>", inner),
            Type::Map { key, value } => write!(f, "Map<{}, {}>", key, value),
            Type::Function { params, returns } => {
                write!(f, "(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", returns)
            }
            Type::Record { fields } => {
                write!(f, "{{")?;
                let mut first = true;
                for (name, ty) in fields {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, ty)?;
                    first = false;
                }
                write!(f, "}}")
            }
            Type::Named(name) => write!(f, "{}", name),
            Type::Any => write!(f, "Any"),
            Type::Never => write!(f, "Never"),
            Type::Optional(inner) => write!(f, "{}?", inner),
            Type::Union(types) => {
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                Ok(())
            }
        }
    }
}

pub struct TypeChecker {
    globals: HashMap<String, Type>,
    locals: Vec<HashMap<String, Type>>,
    functions: HashMap<String, Type>,
    type_aliases: HashMap<String, Type>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut checker = TypeChecker {
            globals: HashMap::new(),
            locals: Vec::new(),
            functions: HashMap::new(),
            type_aliases: HashMap::new(),
        };
        
        // Add built-in functions
        checker.add_builtin_functions();
        
        checker
    }
    
    fn add_builtin_functions(&mut self) {
        // Add some basic built-in functions
        self.functions.insert("print".to_string(), Type::Function {
            params: vec![Type::Any],
            returns: Box::new(Type::Never),
        });
    }
    
    pub fn check_function(&mut self, func: &Function) -> Result<Type, TypeError> {
        // Enter new scope
        self.push_scope();
        
        // Add parameters to scope
        for param in &func.params {
            let param_type = if let Some(type_expr) = &param.type_expr {
                self.resolve_type_expr(type_expr)?
            } else {
                Type::Any // Infer later
            };
            self.add_local(&param.name, param_type);
        }
        
        // Check function body
        let body_type = match &func.body {
            crate::parser::FunctionBody::Expression(expr) => self.infer_expr(expr)?,
            crate::parser::FunctionBody::Block(statements) => {
                let mut last_type = Type::Never;
                for stmt in statements {
                    last_type = self.check_statement(stmt)?;
                }
                last_type
            }
            crate::parser::FunctionBody::PatternMatch(clauses, otherwise) => {
                let mut return_types = Vec::new();
                
                for clause in clauses {
                    let clause_type = self.check_statement(&clause.body)?;
                    return_types.push(clause_type);
                }
                
                if let Some(otherwise_stmt) = otherwise {
                    let otherwise_type = self.check_statement(otherwise_stmt)?;
                    return_types.push(otherwise_type);
                }
                
                // All branches should return the same type
                self.unify_types(&return_types)?
            }
            crate::parser::FunctionBody::InferFromExamples => {
                // TODO: Implement example-based inference
                Type::Any
            }
        };
        
        // Check return type matches declaration
        if let Some(declared_return) = &func.returns {
            let expected = self.resolve_type_expr(declared_return)?;
            self.unify(&body_type, &expected)?;
        }
        
        // Exit scope
        self.pop_scope();
        
        // Create function type
        let param_types: Vec<Type> = func.params.iter()
            .map(|p| {
                if let Some(type_expr) = &p.type_expr {
                    self.resolve_type_expr(type_expr).unwrap_or(Type::Any)
                } else {
                    Type::Any
                }
            })
            .collect();
        
        let return_type = if let Some(ret) = &func.returns {
            self.resolve_type_expr(ret)?
        } else {
            body_type
        };
        
        let func_type = Type::Function {
            params: param_types,
            returns: Box::new(return_type),
        };
        
        // Register function in global scope
        self.functions.insert(func.name.clone(), func_type.clone());
        
        Ok(func_type)
    }
    
    pub fn check_statement(&mut self, stmt: &Statement) -> Result<Type, TypeError> {
        match stmt {
            Statement::Let { name, value } => {
                let value_type = self.infer_expr(value)?;
                self.add_local(name, value_type.clone());
                Ok(Type::Never)
            }
            Statement::Return(expr) => {
                self.infer_expr(expr)
            }
            Statement::Expression(expr) => {
                self.infer_expr(expr)?;
                Ok(Type::Never)
            }
            Statement::Checkpoint { .. } => {
                // Checkpoints don't affect types
                Ok(Type::Never)
            }
            Statement::Attempt { body, recover, finally_fail } => {
                let body_type = self.check_statement(body)?;
                
                for recover_clause in recover {
                    let recover_type = self.check_statement(&recover_clause.body)?;
                    self.unify(&body_type, &recover_type)?;
                }
                
                if let Some(fail_stmt) = finally_fail {
                    self.check_statement(fail_stmt)?;
                }
                
                Ok(body_type)
            }
            Statement::Ensuring { body, cleanup } => {
                let body_type = self.check_statement(body)?;
                self.check_statement(cleanup)?;
                Ok(body_type)
            }
        }
    }
    
    pub fn infer_expr(&mut self, expr: &Expr) -> Result<Type, TypeError> {
        match expr {
            Expr::Literal(lit) => Ok(self.infer_literal(lit)),
            
            Expr::Identifier(name) => {
                self.lookup_variable(name)
                    .or_else(|| self.functions.get(name).cloned())
                    .ok_or_else(|| TypeError::UndefinedVariable(name.clone()))
            }
            
            Expr::Binary { left, op, right } => {
                let left_type = self.infer_expr(left)?;
                let right_type = self.infer_expr(right)?;
                self.infer_binary_op(&left_type, op, &right_type)
            }
            
            Expr::Unary { op, expr } => {
                let expr_type = self.infer_expr(expr)?;
                self.infer_unary_op(op, &expr_type)
            }
            
            Expr::Call { func, args } => {
                let func_type = self.lookup_variable(func)
                    .or_else(|| self.functions.get(func).cloned())
                    .ok_or_else(|| TypeError::UndefinedFunction(func.clone()))?;
                
                match func_type {
                    Type::Function { params, returns } => {
                        if args.len() != params.len() {
                            return Err(TypeError::ArityMismatch {
                                expected: params.len(),
                                found: args.len(),
                            });
                        }
                        
                        for (arg, param_type) in args.iter().zip(params.iter()) {
                            let arg_type = self.infer_expr(arg)?;
                            self.unify(&arg_type, param_type)?;
                        }
                        
                        Ok(*returns)
                    }
                    _ => Err(TypeError::NotCallable(func_type)),
                }
            }
            
            Expr::MemberAccess { object, member } => {
                let object_type = self.infer_expr(object)?;
                
                match object_type {
                    Type::Record { fields } => {
                        fields.get(member)
                            .cloned()
                            .ok_or_else(|| TypeError::NoSuchField {
                                record_type: Type::Record { fields },
                                field: member.clone(),
                            })
                    }
                    _ => Err(TypeError::NotARecord(object_type)),
                }
            }
            
            Expr::Index { object, index } => {
                let object_type = self.infer_expr(object)?;
                let index_type = self.infer_expr(index)?;
                
                match object_type {
                    Type::List(elem_type) => {
                        self.unify(&index_type, &Type::Integer)?;
                        Ok(*elem_type)
                    }
                    Type::Map { key, value } => {
                        self.unify(&index_type, &key)?;
                        Ok(*value)
                    }
                    _ => Err(TypeError::NotIndexable(object_type)),
                }
            }
            
            Expr::List { elements } => {
                if elements.is_empty() {
                    Ok(Type::List(Box::new(Type::Any)))
                } else {
                    let mut elem_types = Vec::new();
                    for elem in elements {
                        elem_types.push(self.infer_expr(elem)?);
                    }
                    let unified = self.unify_types(&elem_types)?;
                    Ok(Type::List(Box::new(unified)))
                }
            }
            
            Expr::Record { fields } => {
                let mut field_types = HashMap::new();
                for (name, expr) in fields {
                    field_types.insert(name.clone(), self.infer_expr(expr)?);
                }
                Ok(Type::Record { fields: field_types })
            }
            
            Expr::Lambda { params, body } => {
                self.push_scope();
                
                // Add lambda parameters as Any type for now
                let param_types: Vec<Type> = params.iter()
                    .map(|p| {
                        self.add_local(p, Type::Any);
                        Type::Any
                    })
                    .collect();
                
                let body_type = self.infer_expr(body)?;
                
                self.pop_scope();
                
                Ok(Type::Function {
                    params: param_types,
                    returns: Box::new(body_type),
                })
            }
            
            Expr::Ternary { condition, then_expr, else_expr } => {
                let cond_type = self.infer_expr(condition)?;
                self.unify(&cond_type, &Type::Boolean)?;
                
                let then_type = self.infer_expr(then_expr)?;
                let else_type = self.infer_expr(else_expr)?;
                
                self.unify(&then_type, &else_type)
            }
            
            Expr::Pipe { expr, transform } => {
                let expr_type = self.infer_expr(expr)?;
                self.infer_transform(&expr_type, transform)
            }
            
            Expr::Cast { expr, target_type } => {
                let _expr_type = self.infer_expr(expr)?;
                self.resolve_type_expr(target_type)
            }
        }
    }
    
    fn infer_literal(&self, lit: &Literal) -> Type {
        match lit {
            Literal::Integer(_) => Type::Integer,
            Literal::Decimal(_) => Type::Decimal,
            Literal::String(_) => Type::String,
            Literal::Boolean(_) => Type::Boolean,
        }
    }
    
    fn infer_binary_op(&self, left: &Type, op: &crate::parser::BinaryOp, right: &Type) -> Result<Type, TypeError> {
        use crate::parser::BinaryOp;
        
        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                self.unify(left, right)?;
                match left {
                    Type::Integer => Ok(Type::Integer),
                    Type::Decimal => Ok(Type::Decimal),
                    Type::String if matches!(op, BinaryOp::Add) => Ok(Type::String),
                    _ => Err(TypeError::InvalidOperandType {
                        op: format!("{:?}", op),
                        ty: left.clone(),
                    }),
                }
            }
            
            BinaryOp::Equal | BinaryOp::NotEqual => {
                self.unify(left, right)?;
                Ok(Type::Boolean)
            }
            
            BinaryOp::Less | BinaryOp::Greater | BinaryOp::LessEqual | BinaryOp::GreaterEqual => {
                self.unify(left, right)?;
                match left {
                    Type::Integer | Type::Decimal => Ok(Type::Boolean),
                    _ => Err(TypeError::InvalidOperandType {
                        op: format!("{:?}", op),
                        ty: left.clone(),
                    }),
                }
            }
            
            BinaryOp::And | BinaryOp::Or => {
                self.unify(left, &Type::Boolean)?;
                self.unify(right, &Type::Boolean)?;
                Ok(Type::Boolean)
            }
        }
    }
    
    fn infer_unary_op(&self, op: &crate::parser::UnaryOp, ty: &Type) -> Result<Type, TypeError> {
        use crate::parser::UnaryOp;
        
        match op {
            UnaryOp::Not => {
                self.unify(ty, &Type::Boolean)?;
                Ok(Type::Boolean)
            }
            UnaryOp::Minus => {
                match ty {
                    Type::Integer => Ok(Type::Integer),
                    Type::Decimal => Ok(Type::Decimal),
                    _ => Err(TypeError::InvalidOperandType {
                        op: "unary minus".to_string(),
                        ty: ty.clone(),
                    }),
                }
            }
        }
    }
    
    fn infer_transform(&mut self, input_type: &Type, transform: &crate::parser::Transform) -> Result<Type, TypeError> {
        use crate::parser::Transform;
        
        match transform {
            Transform::Function(name) => {
                // Look up transform function
                let func_type = self.functions.get(name)
                    .ok_or_else(|| TypeError::UndefinedFunction(name.clone()))?;
                
                match func_type {
                    Type::Function { params, returns } if params.len() == 1 => {
                        self.unify(input_type, &params[0])?;
                        Ok((**returns).clone())
                    }
                    _ => Err(TypeError::InvalidTransform),
                }
            }
            
            Transform::Map { func } => {
                match input_type {
                    Type::List(elem_type) => {
                        // func should be a function from elem_type to some type
                        let func_type = self.infer_expr(func)?;
                        match func_type {
                            Type::Function { params, returns } if params.len() == 1 => {
                                self.unify(&params[0], elem_type)?;
                                Ok(Type::List(returns))
                            }
                            _ => Err(TypeError::InvalidTransform),
                        }
                    }
                    _ => Err(TypeError::NotIterable(input_type.clone())),
                }
            }
            
            Transform::Filter { predicate } => {
                match input_type {
                    Type::List(elem_type) => {
                        // predicate should be a function from elem_type to Boolean
                        let pred_type = self.infer_expr(predicate)?;
                        match pred_type {
                            Type::Function { params, returns } if params.len() == 1 => {
                                self.unify(&params[0], elem_type)?;
                                self.unify(&returns, &Box::new(Type::Boolean))?;
                                Ok(input_type.clone())
                            }
                            _ => Err(TypeError::InvalidTransform),
                        }
                    }
                    _ => Err(TypeError::NotIterable(input_type.clone())),
                }
            }
            
            Transform::Transform { language: _, code: _ } => {
                // External transforms return Any for now
                Ok(Type::Any)
            }
            
            Transform::Extract { pattern: _ } => {
                // Pattern extraction returns a new record type
                // TODO: Implement pattern-based type extraction
                Ok(Type::Any)
            }
        }
    }
    
    fn resolve_type_expr(&self, type_expr: &TypeExpr) -> Result<Type, TypeError> {
        match type_expr {
            TypeExpr::Integer => Ok(Type::Integer),
            TypeExpr::Decimal => Ok(Type::Decimal),
            TypeExpr::String => Ok(Type::String),
            TypeExpr::Boolean => Ok(Type::Boolean),
            TypeExpr::Named(name) => {
                self.type_aliases.get(name)
                    .cloned()
                    .ok_or_else(|| TypeError::UndefinedType(name.clone()))
            }
            TypeExpr::List(inner) => {
                Ok(Type::List(Box::new(self.resolve_type_expr(inner)?)))
            }
            TypeExpr::Map { key, value } => {
                Ok(Type::Map {
                    key: Box::new(self.resolve_type_expr(key)?),
                    value: Box::new(self.resolve_type_expr(value)?),
                })
            }
            TypeExpr::Function { params, returns } => {
                let param_types = params.iter()
                    .map(|p| self.resolve_type_expr(p))
                    .collect::<Result<Vec<_>, _>>()?;
                let return_type = self.resolve_type_expr(returns)?;
                Ok(Type::Function {
                    params: param_types,
                    returns: Box::new(return_type),
                })
            }
            TypeExpr::Record { fields } => {
                let mut field_types = HashMap::new();
                for (name, ty) in fields {
                    field_types.insert(name.clone(), self.resolve_type_expr(ty)?);
                }
                Ok(Type::Record { fields: field_types })
            }
            TypeExpr::Optional(inner) => {
                Ok(Type::Optional(Box::new(self.resolve_type_expr(inner)?)))
            }
            TypeExpr::Where { base, constraint: _ } => {
                // For now, ignore constraints
                self.resolve_type_expr(base)
            }
        }
    }
    
    fn unify(&self, a: &Type, b: &Type) -> Result<Type, TypeError> {
        match (a, b) {
            (Type::Any, other) | (other, Type::Any) => Ok(other.clone()),
            (Type::Integer, Type::Integer) => Ok(Type::Integer),
            (Type::Decimal, Type::Decimal) => Ok(Type::Decimal),
            (Type::String, Type::String) => Ok(Type::String),
            (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
            (Type::Never, other) | (other, Type::Never) => Ok(other.clone()),
            
            (Type::List(a), Type::List(b)) => {
                Ok(Type::List(Box::new(self.unify(a, b)?)))
            }
            
            (Type::Optional(a), Type::Optional(b)) => {
                Ok(Type::Optional(Box::new(self.unify(a, b)?)))
            }
            
            (Type::Optional(inner), other) | (other, Type::Optional(inner)) => {
                Ok(Type::Optional(Box::new(self.unify(inner, other)?)))
            }
            
            _ => {
                if a == b {
                    Ok(a.clone())
                } else {
                    Err(TypeError::TypeMismatch {
                        expected: a.clone(),
                        found: b.clone(),
                    })
                }
            }
        }
    }
    
    fn unify_types(&self, types: &[Type]) -> Result<Type, TypeError> {
        if types.is_empty() {
            return Ok(Type::Never);
        }
        
        let mut result = types[0].clone();
        for ty in types.iter().skip(1) {
            result = self.unify(&result, ty)?;
        }
        
        Ok(result)
    }
    
    fn push_scope(&mut self) {
        self.locals.push(HashMap::new());
    }
    
    fn pop_scope(&mut self) {
        self.locals.pop();
    }
    
    fn add_local(&mut self, name: &str, ty: Type) {
        if let Some(scope) = self.locals.last_mut() {
            scope.insert(name.to_string(), ty);
        }
    }
    
    fn lookup_variable(&self, name: &str) -> Option<Type> {
        // Search locals from innermost to outermost
        for scope in self.locals.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        
        // Check globals
        self.globals.get(name).cloned()
    }
}