use flow_lang::{Lexer, Parser};

#[test]
fn test_parse_simple_function() {
    let input = r#"
function add(x, y):
  return x + y
"#;
    
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().unwrap();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();
    
    assert_eq!(program.items.len(), 1);
    
    match &program.items[0] {
        flow_lang::Item::Function(func) => {
            assert_eq!(func.name, "add");
            assert_eq!(func.params.len(), 2);
            assert_eq!(func.params[0].name, "x");
            assert_eq!(func.params[1].name, "y");
        }
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_parse_function_with_metadata() {
    let input = r#"
intent: Add two numbers together
accepts: Integer x, Integer y
returns: Integer
tests:
  add(2, 3) yields 5
  add(-1, 1) yields 0
  
function add(x, y):
  return x + y
"#;
    
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().unwrap();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();
    
    assert_eq!(program.items.len(), 1);
    
    match &program.items[0] {
        flow_lang::Item::Function(func) => {
            assert!(func.intent.is_some());
            assert!(func.accepts.is_some());
            assert!(func.returns.is_some());
            assert_eq!(func.tests.len(), 2);
        }
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_parse_pattern_matching() {
    let input = r#"
function factorial(n):
  when n is 0: return 1
  when n is 1: return 1
  otherwise: return n * factorial(n - 1)
"#;
    
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().unwrap();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();
    
    assert_eq!(program.items.len(), 1);
    
    match &program.items[0] {
        flow_lang::Item::Function(func) => {
            match &func.body {
                flow_lang::FunctionBody::PatternMatch(clauses, otherwise) => {
                    assert_eq!(clauses.len(), 2);
                    assert!(otherwise.is_some());
                }
                _ => panic!("Expected pattern match body"),
            }
        }
        _ => panic!("Expected function"),
    }
}