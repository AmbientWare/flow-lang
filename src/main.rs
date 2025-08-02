//! Flow Language CLI

use flow_lang::{Lexer, Parser, Interpreter, FlowError};
use colored::Colorize;
use rustyline::DefaultEditor;
use std::fs;
use std::path::PathBuf;
use clap::{Parser as ClapParser, Subcommand};

#[derive(ClapParser)]
#[command(name = "flow")]
#[command(about = "Flow Language - An LLM-optimized programming language", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Run a Flow program
    Run {
        /// The Flow source file to run
        file: PathBuf,
    },
    /// Start an interactive REPL
    Repl,
    /// Check a Flow program without running it
    Check {
        /// The Flow source file to check
        file: PathBuf,
    },
    /// Run tests in a Flow program
    Test {
        /// The Flow source file to test
        file: PathBuf,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Run { file }) => run_file(file),
        Some(Commands::Repl) => run_repl(),
        Some(Commands::Check { file }) => check_file(file),
        Some(Commands::Test { file }) => test_file(file),
        None => run_repl(),
    }
}

fn run_file(path: PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    let source = fs::read_to_string(&path)?;
    
    println!("{}", "Running Flow program...".green());
    
    // Lex
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize()?;
    
    // Parse
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    
    // Interpret
    let mut interpreter = Interpreter::new();
    match interpreter.interpret(&program) {
        Ok(_) => {
            println!("{}", "Program completed successfully".green());
        }
        Err(e) => {
            eprintln!("{}", format!("Runtime error: {}", e).red());
            return Err(Box::new(e));
        }
    }
    
    Ok(())
}

fn run_repl() -> Result<(), Box<dyn std::error::Error>> {
    println!("{}", "Flow Language REPL v0.1.0".green().bold());
    println!("Type 'exit' to quit\n");
    
    let mut rl = DefaultEditor::new()?;
    let prompt = "flow> ".cyan().to_string();
    let mut interpreter = Interpreter::new();
    
    loop {
        match rl.readline(&prompt) {
            Ok(line) => {
                if line.trim() == "exit" {
                    break;
                }
                
                rl.add_history_entry(&line)?;
                
                // Try to parse and execute the input
                let mut lexer = Lexer::new(&line);
                match lexer.tokenize() {
                    Ok(tokens) => {
                        let mut parser = Parser::new(tokens);
                        // Try parsing as expression first
                        match parser.parse() {
                            Ok(program) => {
                                match interpreter.interpret(&program) {
                                    Ok(value) => {
                                        if !matches!(value, flow_lang::interpreter::Value::Null) {
                                            println!("{}", value);
                                        }
                                    }
                                    Err(e) => {
                                        eprintln!("{}", format!("Runtime error: {}", e).red());
                                    }
                                }
                            }
                            Err(e) => {
                                eprintln!("{}", format!("Parse error: {}", e).red());
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!("{}", format!("Lexer error: {}", e).red());
                    }
                }
            }
            Err(rustyline::error::ReadlineError::Interrupted) => {
                println!("\nUse 'exit' to quit");
            }
            Err(rustyline::error::ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }
    
    println!("Goodbye!");
    Ok(())
}

fn check_file(path: PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    let source = fs::read_to_string(&path)?;
    
    println!("{}", "Checking Flow program...".green());
    
    // TODO: Implement syntax and type checking
    println!("{}", "Type checker not yet implemented".yellow());
    
    Ok(())
}

fn test_file(path: PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    let source = fs::read_to_string(&path)?;
    
    println!("{}", "Running Flow tests...".green());
    
    // TODO: Implement test runner
    println!("{}", "Test runner not yet implemented".yellow());
    
    Ok(())
}