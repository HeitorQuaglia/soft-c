mod tokenizer;
mod ast;
mod parser;
mod interpreter;
mod bytecode;
mod compiler;
mod vm;
mod native_interface;

use std::env;
use std::fs;
use std::process;

use tokenizer::Tokenizer;
use parser::Parser;
use interpreter::Interpreter;
use compiler::Compiler;
use vm::VirtualMachine;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} [--bytecode|--ast] <source_file.softc>", args[0]);
        process::exit(1);
    }

    let mut use_bytecode = true;
    let mut file_path = "";

    // Parse argumentos
    if args.len() == 2 {
        file_path = &args[1];
    } else if args.len() == 3 {
        match args[1].as_str() {
            "--bytecode" => {
                use_bytecode = true;
                file_path = &args[2];
            },
            "--ast" => {
                use_bytecode = false;
                file_path = &args[2];
            },
            _ => {
                eprintln!("Invalid option: {}. Use --bytecode or --ast", args[1]);
                process::exit(1);
            }
        }
    } else {
        eprintln!("Usage: {} [--bytecode|--ast] <source_file.softc>", args[0]);
        process::exit(1);
    }

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

    if use_bytecode {
        // Modo bytecode VM
        let mut compiler = Compiler::new();
        let program = match compiler.compile(&ast) {
            Ok(program) => program,
            Err(err) => {
                eprintln!("Compilation error: {}", err);
                process::exit(1);
            }
        };

        // Debug: mostrar bytecode se solicitado
        if std::env::var("SOFTC_DEBUG").is_ok() {
            println!("{}", program.disassemble());
        }

        let mut vm = VirtualMachine::new(program);
        match vm.execute() {
            Ok(_) => {},
            Err(err) => {
                eprintln!("VM Runtime error: {}", err);
                process::exit(1);
            }
        }
    } else {
        // Modo AST interpreter (legacy)
        let mut interpreter = Interpreter::new();
        match interpreter.execute(&ast) {
            Ok(_) => {},
            Err(err) => {
                eprintln!("AST Runtime error: {}", err);
                process::exit(1);
            }
        }
    }
}
