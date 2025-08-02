//! Runtime values for Flow language

use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Decimal(f64),
    String(String),
    Boolean(bool),
    List(Vec<Value>),
    Map(HashMap<String, Value>),
    Record(HashMap<String, Value>),
    Function {
        params: Vec<String>,
        body: Rc<crate::parser::Expr>,
        env: Environment,
    },
    BuiltinFunction {
        name: String,
        arity: usize,
        func: fn(&[Value]) -> Result<Value, String>,
    },
    Null,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Decimal(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::List(values) => {
                write!(f, "[")?;
                for (i, v) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Map(map) => {
                write!(f, "{{")?;
                let mut first = true;
                for (k, v) in map {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", k, v)?;
                    first = false;
                }
                write!(f, "}}")
            }
            Value::Record(fields) => {
                write!(f, "{{")?;
                let mut first = true;
                for (name, value) in fields {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, value)?;
                    first = false;
                }
                write!(f, "}}")
            }
            Value::Function { params, .. } => {
                write!(f, "<function({})>", params.join(", "))
            }
            Value::BuiltinFunction { name, arity, .. } => {
                write!(f, "<builtin {}/{}>", name, arity)
            }
            Value::Null => write!(f, "null"),
        }
    }
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "Integer",
            Value::Decimal(_) => "Decimal",
            Value::String(_) => "String",
            Value::Boolean(_) => "Boolean",
            Value::List(_) => "List",
            Value::Map(_) => "Map",
            Value::Record(_) => "Record",
            Value::Function { .. } => "Function",
            Value::BuiltinFunction { .. } => "BuiltinFunction",
            Value::Null => "Null",
        }
    }
    
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Null => false,
            Value::Integer(n) => *n != 0,
            Value::Decimal(n) => *n != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::List(l) => !l.is_empty(),
            _ => true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    scopes: Vec<HashMap<String, Value>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            scopes: vec![HashMap::new()],
        }
    }
    
    pub fn with_builtins() -> Self {
        let mut env = Environment::new();
        env.add_builtins();
        env
    }
    
    fn add_builtins(&mut self) {
        // Print function
        self.define("print", Value::BuiltinFunction {
            name: "print".to_string(),
            arity: 1,
            func: |args| {
                if args.len() != 1 {
                    return Err("print expects 1 argument".to_string());
                }
                println!("{}", args[0]);
                Ok(Value::Null)
            },
        });
        
        // Length function
        self.define("len", Value::BuiltinFunction {
            name: "len".to_string(),
            arity: 1,
            func: |args| {
                if args.len() != 1 {
                    return Err("len expects 1 argument".to_string());
                }
                match &args[0] {
                    Value::String(s) => Ok(Value::Integer(s.len() as i64)),
                    Value::List(l) => Ok(Value::Integer(l.len() as i64)),
                    Value::Map(m) => Ok(Value::Integer(m.len() as i64)),
                    _ => Err(format!("len not supported for {}", args[0].type_name())),
                }
            },
        });
        
        // Type function
        self.define("type", Value::BuiltinFunction {
            name: "type".to_string(),
            arity: 1,
            func: |args| {
                if args.len() != 1 {
                    return Err("type expects 1 argument".to_string());
                }
                Ok(Value::String(args[0].type_name().to_string()))
            },
        });
    }
    
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }
    
    pub fn define(&mut self, name: &str, value: Value) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), value);
        }
    }
    
    pub fn get(&self, name: &str) -> Option<&Value> {
        // Search from innermost to outermost scope
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }
    
    pub fn set(&mut self, name: &str, value: Value) -> Result<(), String> {
        // Find the scope where the variable is defined
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value);
                return Ok(());
            }
        }
        Err(format!("Undefined variable: {}", name))
    }
}

impl PartialEq for Environment {
    fn eq(&self, _other: &Self) -> bool {
        // Environments are not compared for equality
        false
    }
}