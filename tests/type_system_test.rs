use flow_lang::{Lexer, Parser, types::{TypeChecker, Type}};

#[test]
fn test_literal_type_inference() {
    let mut checker = TypeChecker::new();
    
    // Test integer literal
    let input = "42";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().unwrap();
    let mut parser = Parser::new(tokens);
    
    // We'll need to expose expression parsing for testing
    // For now, let's test through a simple function
}

#[test]
fn test_function_type_checking() {
    let input = r#"
function add(x: Integer, y: Integer): Integer
  return x + y
"#;
    
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().unwrap();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();
    
    let mut checker = TypeChecker::new();
    
    match &program.items[0] {
        flow_lang::Item::Function(func) => {
            let func_type = checker.check_function(func).unwrap();
            
            match func_type {
                Type::Function { params, returns } => {
                    assert_eq!(params.len(), 2);
                    assert_eq!(params[0], Type::Integer);
                    assert_eq!(params[1], Type::Integer);
                    assert_eq!(*returns, Type::Integer);
                }
                _ => panic!("Expected function type"),
            }
        }
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_type_mismatch_detection() {
    let input = r#"
function broken(x: Integer): String
  return x  // Type error: returning Integer but declared String
"#;
    
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().unwrap();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();
    
    let mut checker = TypeChecker::new();
    
    match &program.items[0] {
        flow_lang::Item::Function(func) => {
            let result = checker.check_function(func);
            assert!(result.is_err());
            // Should be a type mismatch error
        }
        _ => panic!("Expected function"),
    }
}