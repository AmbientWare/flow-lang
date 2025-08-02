//! Lexer for Flow language

mod token;

pub use token::{Token, TokenKind};
use thiserror::Error;
use std::collections::HashMap;

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("Unexpected character '{0}' at line {1}, column {2}")]
    UnexpectedChar(char, usize, usize),
    
    #[error("Unterminated string at line {0}")]
    UnterminatedString(usize),
    
    #[error("Invalid number format at line {0}, column {1}")]
    InvalidNumber(usize, usize),
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    line: usize,
    column: usize,
    keywords: HashMap<String, TokenKind>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut keywords = HashMap::new();
        
        // Initialize keywords
        keywords.insert("intent".to_string(), TokenKind::Intent);
        keywords.insert("accepts".to_string(), TokenKind::Accepts);
        keywords.insert("returns".to_string(), TokenKind::Returns);
        keywords.insert("tests".to_string(), TokenKind::Tests);
        keywords.insert("function".to_string(), TokenKind::Function);
        keywords.insert("let".to_string(), TokenKind::Let);
        keywords.insert("when".to_string(), TokenKind::When);
        keywords.insert("is".to_string(), TokenKind::Is);
        keywords.insert("otherwise".to_string(), TokenKind::Otherwise);
        keywords.insert("return".to_string(), TokenKind::Return);
        keywords.insert("with".to_string(), TokenKind::With);
        keywords.insert("type".to_string(), TokenKind::Type);
        keywords.insert("where".to_string(), TokenKind::Where);
        keywords.insert("external".to_string(), TokenKind::External);
        keywords.insert("tool".to_string(), TokenKind::Tool);
        keywords.insert("from".to_string(), TokenKind::From);
        keywords.insert("context".to_string(), TokenKind::Context);
        keywords.insert("within".to_string(), TokenKind::Within);
        keywords.insert("checkpoint".to_string(), TokenKind::Checkpoint);
        keywords.insert("after".to_string(), TokenKind::After);
        keywords.insert("yields".to_string(), TokenKind::Yields);
        keywords.insert("attempt".to_string(), TokenKind::Attempt);
        keywords.insert("recover".to_string(), TokenKind::Recover);
        keywords.insert("finally".to_string(), TokenKind::Finally);
        keywords.insert("fail".to_string(), TokenKind::Fail);
        keywords.insert("properties".to_string(), TokenKind::Properties);
        keywords.insert("for".to_string(), TokenKind::For);
        keywords.insert("all".to_string(), TokenKind::All);
        keywords.insert("invariants".to_string(), TokenKind::Invariants);
        keywords.insert("examples".to_string(), TokenKind::Examples);
        keywords.insert("infer".to_string(), TokenKind::Infer);
        keywords.insert("implementation".to_string(), TokenKind::Implementation);
        keywords.insert("expecting".to_string(), TokenKind::Expecting);
        keywords.insert("on".to_string(), TokenKind::On);
        keywords.insert("error".to_string(), TokenKind::Error);
        keywords.insert("as".to_string(), TokenKind::As);
        keywords.insert("or".to_string(), TokenKind::Or);
        keywords.insert("skip".to_string(), TokenKind::Skip);
        keywords.insert("exists".to_string(), TokenKind::Exists);
        keywords.insert("regardless".to_string(), TokenKind::Regardless);
        keywords.insert("of".to_string(), TokenKind::Of);
        keywords.insert("outcome".to_string(), TokenKind::Outcome);
        keywords.insert("cache".to_string(), TokenKind::Cache);
        keywords.insert("retry".to_string(), TokenKind::Retry);
        keywords.insert("wait".to_string(), TokenKind::Wait);
        keywords.insert("seconds".to_string(), TokenKind::Seconds);
        keywords.insert("true".to_string(), TokenKind::Boolean(true));
        keywords.insert("false".to_string(), TokenKind::Boolean(false));
        
        Lexer {
            input: input.chars().collect(),
            position: 0,
            line: 1,
            column: 1,
            keywords,
        }
    }
    
    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        
        while !self.is_at_end() {
            self.skip_whitespace();
            
            if self.is_at_end() {
                break;
            }
            
            let token = self.scan_token()?;
            if let TokenKind::Comment(_) = token.kind {
                // Skip comments for now (could be preserved for documentation)
                continue;
            }
            tokens.push(token);
        }
        
        tokens.push(Token::new(TokenKind::Eof, String::new(), self.line, self.column));
        Ok(tokens)
    }
    
    fn scan_token(&mut self) -> Result<Token, LexerError> {
        let start_column = self.column;
        let ch = self.advance();
        
        match ch {
            // Single character tokens
            '+' => Ok(self.make_token(TokenKind::Plus, start_column)),
            '-' => {
                if self.peek() == Some('>') {
                    self.advance();
                    Ok(self.make_token(TokenKind::Arrow, start_column))
                } else {
                    Ok(self.make_token(TokenKind::Minus, start_column))
                }
            },
            '*' => Ok(self.make_token(TokenKind::Star, start_column)),
            '/' => {
                if self.peek() == Some('/') {
                    self.advance();
                    let comment = self.scan_comment();
                    Ok(Token::new(TokenKind::Comment(comment), format!("//{}", comment), self.line, start_column))
                } else {
                    Ok(self.make_token(TokenKind::Slash, start_column))
                }
            },
            '%' => Ok(self.make_token(TokenKind::Percent, start_column)),
            '(' => Ok(self.make_token(TokenKind::LeftParen, start_column)),
            ')' => Ok(self.make_token(TokenKind::RightParen, start_column)),
            '{' => Ok(self.make_token(TokenKind::LeftBrace, start_column)),
            '}' => Ok(self.make_token(TokenKind::RightBrace, start_column)),
            '[' => Ok(self.make_token(TokenKind::LeftBracket, start_column)),
            ']' => Ok(self.make_token(TokenKind::RightBracket, start_column)),
            ';' => Ok(self.make_token(TokenKind::Semicolon, start_column)),
            ',' => Ok(self.make_token(TokenKind::Comma, start_column)),
            '.' => Ok(self.make_token(TokenKind::Dot, start_column)),
            '?' => Ok(self.make_token(TokenKind::Question, start_column)),
            ':' => {
                if self.peek() == Some(':') {
                    self.advance();
                    Ok(self.make_token(TokenKind::DoubleColon, start_column))
                } else {
                    Ok(self.make_token(TokenKind::Colon, start_column))
                }
            },
            '=' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Ok(self.make_token(TokenKind::Equal, start_column))
                } else if self.peek() == Some('>') {
                    self.advance();
                    Ok(self.make_token(TokenKind::FatArrow, start_column))
                } else {
                    Err(LexerError::UnexpectedChar('=', self.line, start_column))
                }
            },
            '!' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Ok(self.make_token(TokenKind::NotEqual, start_column))
                } else {
                    Ok(self.make_token(TokenKind::Not, start_column))
                }
            },
            '<' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Ok(self.make_token(TokenKind::LessEqual, start_column))
                } else {
                    Ok(self.make_token(TokenKind::Less, start_column))
                }
            },
            '>' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Ok(self.make_token(TokenKind::GreaterEqual, start_column))
                } else {
                    Ok(self.make_token(TokenKind::Greater, start_column))
                }
            },
            '&' => {
                if self.peek() == Some('&') {
                    self.advance();
                    Ok(self.make_token(TokenKind::And, start_column))
                } else {
                    Err(LexerError::UnexpectedChar('&', self.line, start_column))
                }
            },
            '|' => {
                if self.peek() == Some('|') {
                    self.advance();
                    Ok(self.make_token(TokenKind::OrOp, start_column))
                } else if self.peek() == Some('>') {
                    self.advance();
                    Ok(self.make_token(TokenKind::Pipe, start_column))
                } else {
                    Err(LexerError::UnexpectedChar('|', self.line, start_column))
                }
            },
            '"' => {
                // Check for triple quotes
                if self.peek() == Some('"') && self.peek_next() == Some('"') {
                    self.advance(); // second quote
                    self.advance(); // third quote
                    let content = self.scan_triple_quote_string()?;
                    Ok(Token::new(TokenKind::String(content.clone()), format!("\"\"\"{}\"\"\"", content), self.line, start_column))
                } else {
                    let content = self.scan_string()?;
                    Ok(Token::new(TokenKind::String(content.clone()), format!("\"{}\"", content), self.line, start_column))
                }
            },
            '\n' => {
                self.line += 1;
                self.column = 1;
                Ok(self.make_token(TokenKind::Newline, start_column))
            },
            _ => {
                if ch.is_alphabetic() || ch == '_' {
                    self.position -= 1;
                    self.column -= 1;
                    self.scan_identifier_or_keyword(start_column)
                } else if ch.is_numeric() {
                    self.position -= 1;
                    self.column -= 1;
                    self.scan_number(start_column)
                } else {
                    Err(LexerError::UnexpectedChar(ch, self.line, start_column))
                }
            }
        }
    }
    
    fn scan_comment(&mut self) -> String {
        let mut comment = String::new();
        while self.peek() != Some('\n') && !self.is_at_end() {
            comment.push(self.advance());
        }
        comment.trim().to_string()
    }
    
    fn scan_string(&mut self) -> Result<String, LexerError> {
        let mut value = String::new();
        let start_line = self.line;
        
        while self.peek() != Some('"') && !self.is_at_end() {
            if self.peek() == Some('\n') {
                self.line += 1;
                self.column = 0;
            }
            if self.peek() == Some('\\') {
                self.advance();
                if let Some(escaped) = self.peek() {
                    self.advance();
                    match escaped {
                        'n' => value.push('\n'),
                        't' => value.push('\t'),
                        'r' => value.push('\r'),
                        '\\' => value.push('\\'),
                        '"' => value.push('"'),
                        _ => {
                            value.push('\\');
                            value.push(escaped);
                        }
                    }
                }
            } else {
                value.push(self.advance());
            }
        }
        
        if self.is_at_end() {
            return Err(LexerError::UnterminatedString(start_line));
        }
        
        self.advance(); // closing quote
        Ok(value)
    }
    
    fn scan_triple_quote_string(&mut self) -> Result<String, LexerError> {
        let mut value = String::new();
        let start_line = self.line;
        
        while !(self.peek() == Some('"') && self.peek_next() == Some('"') && self.peek_next_next() == Some('"')) && !self.is_at_end() {
            if self.peek() == Some('\n') {
                self.line += 1;
                self.column = 0;
            }
            value.push(self.advance());
        }
        
        if self.is_at_end() {
            return Err(LexerError::UnterminatedString(start_line));
        }
        
        self.advance(); // first closing quote
        self.advance(); // second closing quote
        self.advance(); // third closing quote
        
        Ok(value)
    }
    
    fn scan_identifier_or_keyword(&mut self, start_column: usize) -> Result<Token, LexerError> {
        let mut value = String::new();
        
        while let Some(ch) = self.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                value.push(self.advance());
            } else {
                break;
            }
        }
        
        let token_kind = self.keywords.get(&value)
            .cloned()
            .unwrap_or_else(|| TokenKind::Identifier(value.clone()));
        
        Ok(Token::new(token_kind, value, self.line, start_column))
    }
    
    fn scan_number(&mut self, start_column: usize) -> Result<Token, LexerError> {
        let mut value = String::new();
        let mut is_decimal = false;
        
        while let Some(ch) = self.peek() {
            if ch.is_numeric() {
                value.push(self.advance());
            } else if ch == '.' && !is_decimal && self.peek_next().map_or(false, |c| c.is_numeric()) {
                is_decimal = true;
                value.push(self.advance());
            } else {
                break;
            }
        }
        
        if is_decimal {
            match value.parse::<f64>() {
                Ok(n) => Ok(Token::new(TokenKind::Decimal(n), value, self.line, start_column)),
                Err(_) => Err(LexerError::InvalidNumber(self.line, start_column)),
            }
        } else {
            match value.parse::<i64>() {
                Ok(n) => Ok(Token::new(TokenKind::Integer(n), value, self.line, start_column)),
                Err(_) => Err(LexerError::InvalidNumber(self.line, start_column)),
            }
        }
    }
    
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() && ch != '\n' {
                self.advance();
            } else {
                break;
            }
        }
    }
    
    fn advance(&mut self) -> char {
        let ch = self.input[self.position];
        self.position += 1;
        self.column += 1;
        ch
    }
    
    fn peek(&self) -> Option<char> {
        if self.is_at_end() {
            None
        } else {
            Some(self.input[self.position])
        }
    }
    
    fn peek_next(&self) -> Option<char> {
        if self.position + 1 >= self.input.len() {
            None
        } else {
            Some(self.input[self.position + 1])
        }
    }
    
    fn peek_next_next(&self) -> Option<char> {
        if self.position + 2 >= self.input.len() {
            None
        } else {
            Some(self.input[self.position + 2])
        }
    }
    
    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }
    
    fn make_token(&self, kind: TokenKind, start_column: usize) -> Token {
        let lexeme = match &kind {
            TokenKind::Plus => "+".to_string(),
            TokenKind::Minus => "-".to_string(),
            TokenKind::Star => "*".to_string(),
            TokenKind::Slash => "/".to_string(),
            TokenKind::Percent => "%".to_string(),
            TokenKind::Equal => "==".to_string(),
            TokenKind::NotEqual => "!=".to_string(),
            TokenKind::Less => "<".to_string(),
            TokenKind::Greater => ">".to_string(),
            TokenKind::LessEqual => "<=".to_string(),
            TokenKind::GreaterEqual => ">=".to_string(),
            TokenKind::And => "&&".to_string(),
            TokenKind::OrOp => "||".to_string(),
            TokenKind::Not => "!".to_string(),
            TokenKind::Pipe => "|>".to_string(),
            TokenKind::Arrow => "->".to_string(),
            TokenKind::FatArrow => "=>".to_string(),
            TokenKind::Colon => ":".to_string(),
            TokenKind::DoubleColon => "::".to_string(),
            TokenKind::Semicolon => ";".to_string(),
            TokenKind::Comma => ",".to_string(),
            TokenKind::Dot => ".".to_string(),
            TokenKind::Question => "?".to_string(),
            TokenKind::LeftParen => "(".to_string(),
            TokenKind::RightParen => ")".to_string(),
            TokenKind::LeftBrace => "{".to_string(),
            TokenKind::RightBrace => "}".to_string(),
            TokenKind::LeftBracket => "[".to_string(),
            TokenKind::RightBracket => "]".to_string(),
            TokenKind::Newline => "\n".to_string(),
            _ => kind.to_string(),
        };
        
        Token::new(kind, lexeme, self.line, start_column)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_basic_tokens() {
        let input = "function add(x, y): return x + y";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        
        assert_eq!(tokens[0].kind, TokenKind::Function);
        assert_eq!(tokens[1].kind, TokenKind::Identifier("add".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::LeftParen);
        assert_eq!(tokens[3].kind, TokenKind::Identifier("x".to_string()));
        assert_eq!(tokens[4].kind, TokenKind::Comma);
        assert_eq!(tokens[5].kind, TokenKind::Identifier("y".to_string()));
        assert_eq!(tokens[6].kind, TokenKind::RightParen);
        assert_eq!(tokens[7].kind, TokenKind::Colon);
        assert_eq!(tokens[8].kind, TokenKind::Return);
        assert_eq!(tokens[9].kind, TokenKind::Identifier("x".to_string()));
        assert_eq!(tokens[10].kind, TokenKind::Plus);
        assert_eq!(tokens[11].kind, TokenKind::Identifier("y".to_string()));
        assert_eq!(tokens[12].kind, TokenKind::Eof);
    }
    
    #[test]
    fn test_string_literals() {
        let input = r#""hello world" """multi
line
string""""#;
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        
        assert_eq!(tokens[0].kind, TokenKind::String("hello world".to_string()));
        assert_eq!(tokens[1].kind, TokenKind::String("multi\nline\nstring".to_string()));
    }
    
    #[test]
    fn test_numbers() {
        let input = "42 3.14 0 999";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        
        assert_eq!(tokens[0].kind, TokenKind::Integer(42));
        assert_eq!(tokens[1].kind, TokenKind::Decimal(3.14));
        assert_eq!(tokens[2].kind, TokenKind::Integer(0));
        assert_eq!(tokens[3].kind, TokenKind::Integer(999));
    }
    
    #[test]
    fn test_comments() {
        let input = "let x = 5 // this is a comment\nlet y = 10";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        
        // Comments are skipped
        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier("x".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::Equal);
        assert_eq!(tokens[3].kind, TokenKind::Integer(5));
        assert_eq!(tokens[4].kind, TokenKind::Newline);
        assert_eq!(tokens[5].kind, TokenKind::Let);
    }
}