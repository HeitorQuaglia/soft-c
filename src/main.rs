mod tokenizer;
mod ast;
mod parser;
mod interpreter;

use std::env;
use std::fs;
use std::process;

use tokenizer::Tokenizer;
use parser::Parser;
use interpreter::Interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <source_file.softc>", args[0]);
        process::exit(1);
    }

    let file_path = &args[1];

    let source = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", file_path, err);
            process::exit(1);
        }
    };

    let mut tokenizer = Tokenizer::new(&source);
    let tokens = match tokenizer.tokenize() {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("Tokenization error: {}", err);
            process::exit(1);
        }
    };
    

    let mut parser = Parser::new(tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("Parse error: {}", err);
            process::exit(1);
        }
    };

    let mut interpreter = Interpreter::new();
    match interpreter.execute(&ast) {
        Ok(_) => {},
        Err(err) => {
            eprintln!("Runtime error: {}", err);
            process::exit(1);
        }
    }
}
