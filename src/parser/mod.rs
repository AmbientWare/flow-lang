//! Parser for Flow language

mod ast;

pub use ast::*;
use crate::lexer::{Token, TokenKind, Lexer};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Syntax error at line {0}: {1}")]
    SyntaxError(usize, String),
    
    #[error("Unexpected token: expected {expected}, found {found} at line {line}")]
    UnexpectedToken { expected: String, found: String, line: usize },
    
    #[error("Unexpected end of file")]
    UnexpectedEof,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }
    
    pub fn parse(&mut self) -> Result<Program, ParserError> {
        let mut items = Vec::new();
        
        while !self.is_at_end() {
            // Skip newlines at top level
            if self.match_token(&TokenKind::Newline) {
                continue;
            }
            
            items.push(self.parse_item()?);
        }
        
        Ok(Program { items })
    }
    
    fn parse_item(&mut self) -> Result<Item, ParserError> {
        if self.match_token(&TokenKind::External) {
            Ok(Item::ExternalTool(self.parse_external_tool()?))
        } else if self.match_token(&TokenKind::Type) {
            Ok(Item::Type(self.parse_type_def()?))
        } else if self.match_token(&TokenKind::Context) {
            Ok(Item::Context(self.parse_context()?))
        } else if self.check(&TokenKind::Intent) || self.check(&TokenKind::Function) {
            Ok(Item::Function(self.parse_function()?))
        } else {
            Err(self.error("Expected 'external', 'type', 'context', 'intent', or 'function'"))
        }
    }
    
    fn parse_function(&mut self) -> Result<Function, ParserError> {
        let mut intent = None;
        let mut accepts = None;
        let mut returns = None;
        let mut tests = Vec::new();
        let mut properties = Vec::new();
        let mut invariants = Vec::new();
        let mut examples = Vec::new();
        
        // Parse metadata
        if self.match_token(&TokenKind::Intent) {
            self.consume(&TokenKind::Colon, "Expected ':' after 'intent'")?;
            intent = Some(self.parse_string_content()?);
            self.skip_newlines();
        }
        
        if self.match_token(&TokenKind::Accepts) {
            self.consume(&TokenKind::Colon, "Expected ':' after 'accepts'")?;
            accepts = Some(self.parse_type_expr()?);
            self.skip_newlines();
        }
        
        if self.match_token(&TokenKind::Returns) {
            self.consume(&TokenKind::Colon, "Expected ':' after 'returns'")?;
            returns = Some(self.parse_type_expr()?);
            self.skip_newlines();
        }
        
        if self.match_token(&TokenKind::Tests) {
            self.consume(&TokenKind::Colon, "Expected ':' after 'tests'")?;
            self.skip_newlines();
            tests = self.parse_test_cases()?;
        }
        
        if self.match_token(&TokenKind::Properties) {
            self.consume(&TokenKind::Colon, "Expected ':' after 'properties'")?;
            self.skip_newlines();
            properties = self.parse_properties()?;
        }
        
        if self.match_token(&TokenKind::Invariants) {
            self.consume(&TokenKind::Colon, "Expected ':' after 'invariants'")?;
            self.skip_newlines();
            invariants = self.parse_invariants()?;
        }
        
        if self.match_token(&TokenKind::Examples) {
            self.consume(&TokenKind::Colon, "Expected ':' after 'examples'")?;
            self.skip_newlines();
            examples = self.parse_examples()?;
        }
        
        // Parse function declaration
        self.consume(&TokenKind::Function, "Expected 'function'")?;
        let name = self.parse_identifier()?;
        
        self.consume(&TokenKind::LeftParen, "Expected '(' after function name")?;
        let params = self.parse_parameters()?;
        self.consume(&TokenKind::RightParen, "Expected ')' after parameters")?;
        self.consume(&TokenKind::Colon, "Expected ':' after function signature")?;
        self.skip_newlines();
        
        // Parse body
        let body = if self.match_token(&TokenKind::Infer) {
            self.consume(&TokenKind::Implementation, "Expected 'implementation' after 'infer'")?;
            self.consume(&TokenKind::From, "Expected 'from' after 'implementation'")?;
            self.consume(&TokenKind::Examples, "Expected 'examples' after 'from'")?;
            FunctionBody::InferFromExamples
        } else if self.check(&TokenKind::When) {
            FunctionBody::PatternMatch(self.parse_pattern_match()?)
        } else {
            FunctionBody::Block(self.parse_block()?)
        };
        
        Ok(Function {
            intent,
            name,
            params,
            accepts,
            returns,
            tests,
            properties,
            invariants,
            examples,
            body,
        })
    }
    
    fn parse_test_cases(&mut self) -> Result<Vec<TestCase>, ParserError> {
        let mut tests = Vec::new();
        
        while self.is_indented() && !self.is_at_end() {
            let call = self.parse_until_yields()?;
            
            let expected = if self.match_token(&TokenKind::Yields) {
                TestExpectation::Yields(self.parse_expression()?)
            } else if self.match_token(&TokenKind::Fail) {
                self.consume(&TokenKind::With, "Expected 'with' after 'fails'")?;
                let message = if self.check_string() {
                    Some(self.parse_string()?)
                } else {
                    None
                };
                TestExpectation::Fails { message }
            } else {
                return Err(self.error("Expected 'yields' or 'fails' in test case"));
            };
            
            tests.push(TestCase { call, expected });
            self.skip_newlines();
        }
        
        Ok(tests)
    }
    
    fn parse_pattern_match(&mut self) -> Result<(Vec<WhenClause>, Option<Box<Statement>>), ParserError> {
        let mut clauses = Vec::new();
        let mut otherwise = None;
        
        while self.match_token(&TokenKind::When) {
            let pattern = self.parse_pattern()?;
            self.consume(&TokenKind::Colon, "Expected ':' after pattern")?;
            let body = self.parse_statement()?;
            clauses.push(WhenClause { pattern, body });
            self.skip_newlines();
        }
        
        if self.match_token(&TokenKind::Otherwise) {
            self.consume(&TokenKind::Colon, "Expected ':' after 'otherwise'")?;
            otherwise = Some(Box::new(self.parse_statement()?));
        }
        
        Ok((clauses, otherwise))
    }
    
    fn parse_pattern(&mut self) -> Result<Pattern, ParserError> {
        if let Some(ident) = self.match_identifier() {
            if self.match_token(&TokenKind::Is) {
                // Pattern: n is 0
                let value = self.parse_expression()?;
                Ok(Pattern::Comparison {
                    var: ident,
                    op: CompareOp::Equal,
                    value,
                })
            } else if self.check_compare_op() {
                // Pattern: n > 0
                let op = self.parse_compare_op()?;
                let value = self.parse_expression()?;
                Ok(Pattern::Comparison { var: ident, op, value })
            } else {
                // Just a variable pattern
                Ok(Pattern::Variable(ident))
            }
        } else if self.check_literal() {
            Ok(Pattern::Literal(self.parse_literal()?))
        } else if self.match_identifier_str("_") {
            Ok(Pattern::Wildcard)
        } else {
            Err(self.error("Expected pattern"))
        }
    }
    
    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        if self.match_token(&TokenKind::Let) {
            let name = self.parse_identifier()?;
            self.consume(&TokenKind::Equal, "Expected '=' after variable name")?;
            let value = self.parse_expression()?;
            Ok(Statement::Let { name, value })
        } else if self.match_token(&TokenKind::Return) {
            Ok(Statement::Return(self.parse_expression()?))
        } else if self.match_token(&TokenKind::Checkpoint) {
            self.consume(&TokenKind::After, "Expected 'after' after 'checkpoint'")?;
            self.consume(&TokenKind::Colon, "Expected ':' after 'after'")?;
            let name = self.parse_string()?;
            let yields = if self.match_token(&TokenKind::Yields) {
                Some(self.parse_identifier()?)
            } else {
                None
            };
            Ok(Statement::Checkpoint { name, yields })
        } else if self.match_token(&TokenKind::Attempt) {
            self.consume(&TokenKind::Colon, "Expected ':' after 'attempt'")?;
            let body = Box::new(self.parse_statement()?);
            let mut recover = Vec::new();
            
            while self.match_token(&TokenKind::Recover) {
                self.consume(&TokenKind::With, "Expected 'with' after 'recover'")?;
                let strategy = self.parse_recover_strategy()?;
                self.consume(&TokenKind::Colon, "Expected ':' after recover strategy")?;
                let recover_body = Box::new(self.parse_statement()?);
                recover.push(RecoverClause { strategy, body: recover_body });
            }
            
            let finally_fail = if self.match_token(&TokenKind::Finally) {
                self.consume(&TokenKind::Fail, "Expected 'fail' after 'finally'")?;
                self.consume(&TokenKind::Colon, "Expected ':' after 'finally fail'")?;
                Some(Box::new(self.parse_statement()?))
            } else {
                None
            };
            
            Ok(Statement::Attempt { body, recover, finally_fail })
        } else {
            Ok(Statement::Expression(self.parse_expression()?))
        }
    }
    
    fn parse_expression(&mut self) -> Result<Expr, ParserError> {
        self.parse_or()
    }
    
    fn parse_or(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_and()?;
        
        while self.match_token(&TokenKind::OrOp) {
            let right = self.parse_and()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::Or,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_and(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_equality()?;
        
        while self.match_token(&TokenKind::And) {
            let right = self.parse_equality()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::And,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_comparison()?;
        
        while let Some(op) = self.match_tokens(&[TokenKind::Equal, TokenKind::NotEqual]) {
            let op = match op {
                TokenKind::Equal => BinaryOp::Equal,
                TokenKind::NotEqual => BinaryOp::NotEqual,
                _ => unreachable!(),
            };
            let right = self.parse_comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_term()?;
        
        while let Some(op) = self.match_tokens(&[
            TokenKind::Less,
            TokenKind::Greater,
            TokenKind::LessEqual,
            TokenKind::GreaterEqual,
        ]) {
            let op = match op {
                TokenKind::Less => BinaryOp::Less,
                TokenKind::Greater => BinaryOp::Greater,
                TokenKind::LessEqual => BinaryOp::LessEqual,
                TokenKind::GreaterEqual => BinaryOp::GreaterEqual,
                _ => unreachable!(),
            };
            let right = self.parse_term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_factor()?;
        
        while let Some(op) = self.match_tokens(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = match op {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Sub,
                _ => unreachable!(),
            };
            let right = self.parse_factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_unary()?;
        
        while let Some(op) = self.match_tokens(&[TokenKind::Star, TokenKind::Slash, TokenKind::Percent]) {
            let op = match op {
                TokenKind::Star => BinaryOp::Mul,
                TokenKind::Slash => BinaryOp::Div,
                TokenKind::Percent => BinaryOp::Mod,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_unary(&mut self) -> Result<Expr, ParserError> {
        if let Some(op) = self.match_tokens(&[TokenKind::Not, TokenKind::Minus]) {
            let op = match op {
                TokenKind::Not => UnaryOp::Not,
                TokenKind::Minus => UnaryOp::Minus,
                _ => unreachable!(),
            };
            let expr = self.parse_unary()?;
            Ok(Expr::Unary {
                op,
                expr: Box::new(expr),
            })
        } else {
            self.parse_pipe()
        }
    }
    
    fn parse_pipe(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_postfix()?;
        
        while self.match_token(&TokenKind::Pipe) {
            let transform = self.parse_transform()?;
            expr = Expr::Pipe {
                expr: Box::new(expr),
                transform: Box::new(transform),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_postfix(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_primary()?;
        
        loop {
            if self.match_token(&TokenKind::LeftParen) {
                // Function call
                let args = self.parse_arguments()?;
                self.consume(&TokenKind::RightParen, "Expected ')' after arguments")?;
                
                if let Expr::Identifier(func) = expr {
                    expr = Expr::Call { func, args };
                } else {
                    return Err(self.error("Cannot call non-identifier"));
                }
            } else if self.match_token(&TokenKind::Dot) {
                // Member access
                let member = self.parse_identifier()?;
                expr = Expr::MemberAccess {
                    object: Box::new(expr),
                    member,
                };
            } else if self.match_token(&TokenKind::LeftBracket) {
                // Index access
                let index = self.parse_expression()?;
                self.consume(&TokenKind::RightBracket, "Expected ']' after index")?;
                expr = Expr::Index {
                    object: Box::new(expr),
                    index: Box::new(index),
                };
            } else {
                break;
            }
        }
        
        Ok(expr)
    }
    
    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        if self.check_literal() {
            Ok(Expr::Literal(self.parse_literal()?))
        } else if let Some(ident) = self.match_identifier() {
            Ok(Expr::Identifier(ident))
        } else if self.match_token(&TokenKind::LeftParen) {
            let expr = self.parse_expression()?;
            self.consume(&TokenKind::RightParen, "Expected ')' after expression")?;
            Ok(expr)
        } else if self.match_token(&TokenKind::LeftBracket) {
            let elements = self.parse_comma_separated(Self::parse_expression)?;
            self.consume(&TokenKind::RightBracket, "Expected ']' after list elements")?;
            Ok(Expr::List { elements })
        } else if self.match_token(&TokenKind::LeftBrace) {
            let fields = self.parse_record_fields()?;
            self.consume(&TokenKind::RightBrace, "Expected '}' after record fields")?;
            Ok(Expr::Record { fields })
        } else {
            Err(self.error("Expected expression"))
        }
    }
    
    // Helper methods
    
    fn parse_type_expr(&mut self) -> Result<TypeExpr, ParserError> {
        // Simple type parsing for now
        if let Some(ident) = self.match_identifier() {
            match ident.as_str() {
                "Integer" => Ok(TypeExpr::Integer),
                "Decimal" => Ok(TypeExpr::Decimal),
                "String" => Ok(TypeExpr::String),
                "Boolean" => Ok(TypeExpr::Boolean),
                _ => Ok(TypeExpr::Named(ident)),
            }
        } else {
            Err(self.error("Expected type expression"))
        }
    }
    
    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, ParserError> {
        let mut params = Vec::new();
        
        if !self.check(&TokenKind::RightParen) {
            loop {
                let name = self.parse_identifier()?;
                let type_expr = if self.match_token(&TokenKind::Colon) {
                    Some(self.parse_type_expr()?)
                } else {
                    None
                };
                params.push(Parameter { name, type_expr });
                
                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }
        
        Ok(params)
    }
    
    fn parse_block(&mut self) -> Result<Vec<Statement>, ParserError> {
        let mut statements = Vec::new();
        
        while !self.is_at_end() && self.is_indented() {
            statements.push(self.parse_statement()?);
            self.skip_newlines();
        }
        
        Ok(statements)
    }
    
    fn parse_literal(&mut self) -> Result<Literal, ParserError> {
        let token = self.advance();
        match &token.kind {
            TokenKind::Integer(n) => Ok(Literal::Integer(*n)),
            TokenKind::Decimal(n) => Ok(Literal::Decimal(*n)),
            TokenKind::String(s) => Ok(Literal::String(s.clone())),
            TokenKind::Boolean(b) => Ok(Literal::Boolean(*b)),
            _ => Err(self.error("Expected literal")),
        }
    }
    
    fn parse_identifier(&mut self) -> Result<String, ParserError> {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(name) => {
                    self.advance();
                    Ok(name.clone())
                }
                _ => Err(self.error("Expected identifier")),
            },
            None => Err(ParserError::UnexpectedEof),
        }
    }
    
    fn parse_string(&mut self) -> Result<String, ParserError> {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::String(s) => {
                    self.advance();
                    Ok(s.clone())
                }
                _ => Err(self.error("Expected string")),
            },
            None => Err(ParserError::UnexpectedEof),
        }
    }
    
    fn parse_string_content(&mut self) -> Result<String, ParserError> {
        // For now, we'll read until newline or EOF
        let mut content = String::new();
        while !self.check(&TokenKind::Newline) && !self.is_at_end() {
            if let Some(token) = self.peek() {
                content.push_str(&token.lexeme);
                content.push(' ');
                self.advance();
            }
        }
        Ok(content.trim().to_string())
    }
    
    fn parse_until_yields(&mut self) -> Result<String, ParserError> {
        let mut content = String::new();
        while !self.check(&TokenKind::Yields) && !self.check(&TokenKind::Fail) && !self.is_at_end() {
            if let Some(token) = self.peek() {
                content.push_str(&token.lexeme);
                content.push(' ');
                self.advance();
            }
        }
        Ok(content.trim().to_string())
    }
    
    fn parse_transform(&mut self) -> Result<Transform, ParserError> {
        if let Some(ident) = self.match_identifier() {
            if ident == "transform" {
                self.consume(&TokenKind::With, "Expected 'with' after 'transform'")?;
                let language = self.parse_identifier()?;
                self.consume(&TokenKind::Colon, "Expected ':' after language")?;
                let code = self.parse_string()?;
                Ok(Transform::Transform { language, code })
            } else {
                Ok(Transform::Function(ident))
            }
        } else {
            Err(self.error("Expected transform"))
        }
    }
    
    fn parse_comma_separated<T, F>(&mut self, mut parse_fn: F) -> Result<Vec<T>, ParserError>
    where
        F: FnMut(&mut Self) -> Result<T, ParserError>,
    {
        let mut items = Vec::new();
        
        if !self.check(&TokenKind::RightBracket) && !self.check(&TokenKind::RightParen) {
            loop {
                items.push(parse_fn(self)?);
                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }
        
        Ok(items)
    }
    
    fn parse_record_fields(&mut self) -> Result<Vec<(String, Expr)>, ParserError> {
        let mut fields = Vec::new();
        
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            let name = self.parse_identifier()?;
            self.consume(&TokenKind::Colon, "Expected ':' after field name")?;
            let value = self.parse_expression()?;
            fields.push((name, value));
            
            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }
        
        Ok(fields)
    }
    
    // Placeholder implementations for complex parsing
    
    fn parse_properties(&mut self) -> Result<Vec<Property>, ParserError> {
        // TODO: Implement property parsing
        Ok(Vec::new())
    }
    
    fn parse_invariants(&mut self) -> Result<Vec<Invariant>, ParserError> {
        // TODO: Implement invariant parsing
        Ok(Vec::new())
    }
    
    fn parse_examples(&mut self) -> Result<Vec<Example>, ParserError> {
        // TODO: Implement example parsing
        Ok(Vec::new())
    }
    
    fn parse_external_tool(&mut self) -> Result<ExternalTool, ParserError> {
        self.consume(&TokenKind::Tool, "Expected 'tool' after 'external'")?;
        self.consume(&TokenKind::From, "Expected 'from' after 'tool'")?;
        let language = self.parse_identifier()?;
        self.consume(&TokenKind::Colon, "Expected ':' after language")?;
        let import_path = self.parse_string_content()?;
        Ok(ExternalTool {
            language,
            import_path,
            alias: None,
        })
    }
    
    fn parse_type_def(&mut self) -> Result<TypeDef, ParserError> {
        let name = self.parse_identifier()?;
        self.consume(&TokenKind::Equal, "Expected '=' after type name")?;
        // TODO: Implement full type definition parsing
        let definition = TypeDefinition::Alias(TypeExpr::String);
        Ok(TypeDef { name, definition })
    }
    
    fn parse_context(&mut self) -> Result<Context, ParserError> {
        let name = self.parse_identifier()?;
        self.consume(&TokenKind::Colon, "Expected ':' after context name")?;
        // TODO: Implement context settings parsing
        Ok(Context {
            name,
            settings: Vec::new(),
        })
    }
    
    fn parse_compare_op(&mut self) -> Result<CompareOp, ParserError> {
        let token = self.advance();
        match token.kind {
            TokenKind::Equal => Ok(CompareOp::Equal),
            TokenKind::NotEqual => Ok(CompareOp::NotEqual),
            TokenKind::Less => Ok(CompareOp::Less),
            TokenKind::Greater => Ok(CompareOp::Greater),
            TokenKind::LessEqual => Ok(CompareOp::LessEqual),
            TokenKind::GreaterEqual => Ok(CompareOp::GreaterEqual),
            _ => Err(self.error("Expected comparison operator")),
        }
    }
    
    fn parse_recover_strategy(&mut self) -> Result<RecoverStrategy, ParserError> {
        if self.match_identifier_str("default") {
            Ok(RecoverStrategy::Default)
        } else if self.match_identifier_str("retry") {
            self.consume(&TokenKind::LeftParen, "Expected '(' after 'retry'")?;
            let count = self.parse_integer()?;
            self.consume(&TokenKind::RightParen, "Expected ')' after retry count")?;
            Ok(RecoverStrategy::Retry(count as u32))
        } else {
            let handler = self.parse_identifier()?;
            Ok(RecoverStrategy::With(handler))
        }
    }
    
    fn parse_integer(&mut self) -> Result<i64, ParserError> {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::Integer(n) => {
                    self.advance();
                    Ok(*n)
                }
                _ => Err(self.error("Expected integer")),
            },
            None => Err(ParserError::UnexpectedEof),
        }
    }
    
    fn parse_arguments(&mut self) -> Result<Vec<Expr>, ParserError> {
        self.parse_comma_separated(Self::parse_expression)
    }
    
    // Token matching helpers
    
    fn match_token(&mut self, kind: &TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }
    
    fn match_tokens(&mut self, kinds: &[TokenKind]) -> Option<TokenKind> {
        for kind in kinds {
            if self.check(kind) {
                let token = self.advance();
                return Some(token.kind);
            }
        }
        None
    }
    
    fn match_identifier(&mut self) -> Option<String> {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(name) => {
                    let name = name.clone();
                    self.advance();
                    Some(name)
                }
                _ => None,
            },
            None => None,
        }
    }
    
    fn match_identifier_str(&mut self, expected: &str) -> bool {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(name) if name == expected => {
                    self.advance();
                    true
                }
                _ => false,
            },
            None => false,
        }
    }
    
    fn check(&self, kind: &TokenKind) -> bool {
        match self.peek() {
            Some(token) => std::mem::discriminant(&token.kind) == std::mem::discriminant(kind),
            None => false,
        }
    }
    
    fn check_string(&self) -> bool {
        matches!(self.peek().map(|t| &t.kind), Some(TokenKind::String(_)))
    }
    
    fn check_literal(&self) -> bool {
        match self.peek().map(|t| &t.kind) {
            Some(TokenKind::Integer(_)) | Some(TokenKind::Decimal(_)) | 
            Some(TokenKind::String(_)) | Some(TokenKind::Boolean(_)) => true,
            _ => false,
        }
    }
    
    fn check_compare_op(&self) -> bool {
        matches!(
            self.peek().map(|t| &t.kind),
            Some(TokenKind::Equal) | Some(TokenKind::NotEqual) |
            Some(TokenKind::Less) | Some(TokenKind::Greater) |
            Some(TokenKind::LessEqual) | Some(TokenKind::GreaterEqual)
        )
    }
    
    fn consume(&mut self, kind: &TokenKind, message: &str) -> Result<Token, ParserError> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            Err(self.error(message))
        }
    }
    
    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous().clone()
    }
    
    fn is_at_end(&self) -> bool {
        matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Eof))
    }
    
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }
    
    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
    
    fn skip_newlines(&mut self) {
        while self.match_token(&TokenKind::Newline) {
            // Skip
        }
    }
    
    fn is_indented(&self) -> bool {
        // Simple indentation check - in a real implementation we'd track indentation levels
        true
    }
    
    fn error(&self, message: &str) -> ParserError {
        let line = self.peek().map(|t| t.line).unwrap_or(0);
        ParserError::SyntaxError(line, message.to_string())
    }
}