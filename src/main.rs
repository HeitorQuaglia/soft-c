mod tokenizer;
mod ast;
mod parser;
mod interpreter;
mod bytecode;
mod compiler;
mod vm;
mod native_interface;
mod module_system;

use clap::{Parser, Subcommand};
use std::fs;
use std::path::Path;
use std::process;

use tokenizer::Tokenizer;
use parser::Parser as SoftParser;
use interpreter::Interpreter;
use compiler::Compiler;
use vm::VirtualMachine;
use bytecode::BytecodeProgram;

#[derive(Parser)]
#[command(name = "soft")]
#[command(about = "The Soft programming language")]
#[command(version = "0.1.0")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Run {
        file: String,
        #[arg(long)]
        debug: bool,
        #[arg(long)]
        ast: bool,
    },
    Build {
        file: String,
        #[arg(short, long)]
        output: Option<String>,
        #[arg(long)]
        debug: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Run { file, debug, ast } => {
            run_file(file, *debug, *ast);
        }
        Commands::Build { file, output, debug } => {
            build_file(file, output.as_deref(), *debug);
        }
    }
}

fn run_file(file_path: &str, debug: bool, use_ast: bool) {
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

    let mut parser = SoftParser::new(tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("Parse error: {}", err);
            process::exit(1);
        }
    };

    if use_ast {
        let mut interpreter = Interpreter::new();
        match interpreter.execute(&ast) {
            Ok(_) => {},
            Err(err) => {
                eprintln!("Runtime error: {}", err);
                process::exit(1);
            }
        }
    } else {
        let mut compiler = Compiler::new();
        let program = match compiler.compile(&ast) {
            Ok(program) => program,
            Err(err) => {
                eprintln!("Compilation error: {}", err);
                process::exit(1);
            }
        };

        if debug {
            println!("{}", program.disassemble());
        }

        let mut vm = VirtualMachine::new(program);
        match vm.execute() {
            Ok(_) => {},
            Err(err) => {
                eprintln!("Runtime error: {}", err);
                process::exit(1);
            }
        }
    }
}

fn build_file(file_path: &str, output_path: Option<&str>, debug: bool) {
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

    let mut parser = SoftParser::new(tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("Parse error: {}", err);
            process::exit(1);
        }
    };

    let mut compiler = Compiler::new();
    let program = match compiler.compile(&ast) {
        Ok(program) => program,
        Err(err) => {
            eprintln!("Compilation error: {}", err);
            process::exit(1);
        }
    };

    if debug {
        println!("{}", program.disassemble());
    }

    let output_file = match output_path {
        Some(path) => path.to_string(),
        None => {
            let path = Path::new(file_path);
            path.file_stem()
                .unwrap_or_else(|| {
                    eprintln!("Cannot determine output filename from '{}'", file_path);
                    process::exit(1);
                })
                .to_string_lossy()
                .to_string()
        }
    };

    match save_bytecode_to_file(&program, &output_file) {
        Ok(_) => {
            println!("Built '{}' -> '{}'", file_path, output_file);
        }
        Err(err) => {
            eprintln!("Error saving bytecode to '{}': {}", output_file, err);
            process::exit(1);
        }
    }
}

fn save_bytecode_to_file(program: &BytecodeProgram, output_path: &str) -> Result<(), std::io::Error> {
    // TODO: Implementar formato binário serializado
    let disassembly = program.disassemble();
    fs::write(output_path, disassembly)?;
    Ok(())
}
