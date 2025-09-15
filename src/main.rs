mod tokenizer;
mod ast;
mod parser;
mod bytecode;
pub mod vm;
mod native_interface;
mod module_system;
mod compiler;
mod stdlib;

use crate::bytecode::BytecodeProgram;
use crate::vm::VirtualMachine;

use clap::{Parser, Subcommand};
use std::fs;
use std::path::Path;
use std::process;

use compiler::MultiPassCompiler;
use parser::Parser as SoftParser;
use tokenizer::Tokenizer;

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
        file: Option<String>,
        #[arg(long)]
        debug: bool,
        #[arg(long)]
        ast: bool,
    },
    Build {
        file: Option<String>,
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
            let file_path = get_source_file_path(file.as_deref());
            run_file(&file_path, *debug, *ast);
        }
        Commands::Build { file, output, debug } => {
            let file_path = get_source_file_path(file.as_deref());
            build_file(&file_path, output.as_deref(), *debug);
        }
    }
}

fn get_source_file_path(cli_file: Option<&str>) -> String {
    if let Ok(env_file) = std::env::var("SOFT_SOURCE") {
        println!("Using SOFT_SOURCE: {}", env_file);
        return env_file;
    }

    if let Some(file) = cli_file {
        return file.to_string();
    }

    eprintln!("Error: No source file specified. Use SOFT_SOURCE environment variable or provide file argument.");
    process::exit(1);
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
    let ast = match parser.parse_with_error_recovery() {
        Ok(ast) => ast,
        Err(errors) => {
            eprintln!("Parsing failed with errors:");
            eprintln!("{}", errors);
            process::exit(1);
        }
    };

    if use_ast {
        eprintln!("AST interpreter mode is no longer supported. Use bytecode VM instead.");
        process::exit(1);
    } else {
        let mut compiler = MultiPassCompiler::new();
        let (program, function_addresses) = match compiler.compile(&ast) {
            Ok(result) => result,
            Err(err) => {
                eprintln!("Compilation error: {}", err);
                process::exit(1);
            }
        };

        if debug {
            println!("{}", program.disassemble());
        }

        let mut vm = VirtualMachine::new(program);
        vm.set_function_addresses(function_addresses);
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
    let ast = match parser.parse_with_error_recovery() {
        Ok(ast) => ast,
        Err(errors) => {
            eprintln!("Parsing failed with errors:");
            eprintln!("{}", errors);
            process::exit(1);
        }
    };

    let mut compiler = MultiPassCompiler::new();
    let (program, _function_addresses) = match compiler.compile(&ast) {
        Ok(result) => result,
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
