use crate::native_interface::NativeFunction;
use crate::vm::value::Value;
use std::io::{self, Write};

#[derive(Debug)]
pub struct PrintFunction;

impl NativeFunction for PrintFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.is_empty() {
            println!();
            return Ok(Value::Int(0));
        }
        
        let output = args.iter()
            .map(|v| format!("{}", v))
            .collect::<Vec<_>>()
            .join(" ");
        
        println!("{}", output);
        io::stdout().flush().unwrap_or(());
        
        Ok(Value::Int(0))
    }
    
    fn arity(&self) -> u8 { 255 }
    fn name(&self) -> &str { "print" }
}

#[derive(Debug)]
pub struct PrintlnFunction;

impl NativeFunction for PrintlnFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.is_empty() {
            println!();
            return Ok(Value::Int(0));
        }
        
        let output = args.iter()
            .map(|v| format!("{}", v))
            .collect::<Vec<_>>()
            .join(" ");
        
        println!("{}", output);
        
        Ok(Value::Int(0))
    }
    
    fn arity(&self) -> u8 { 255 }
    fn name(&self) -> &str { "println" }
}

#[derive(Debug)]
pub struct EprintFunction;

impl NativeFunction for EprintFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.is_empty() {
            eprintln!();
            return Ok(Value::Int(0));
        }
        
        let output = args.iter()
            .map(|v| format!("{}", v))
            .collect::<Vec<_>>()
            .join(" ");
        
        eprint!("{}", output);
        io::stderr().flush().unwrap_or(());
        
        Ok(Value::Int(0))
    }
    
    fn arity(&self) -> u8 { 255 }
    fn name(&self) -> &str { "eprint" }
}

#[derive(Debug)]
pub struct EprintlnFunction;

impl NativeFunction for EprintlnFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.is_empty() {
            eprintln!();
            return Ok(Value::Int(0));
        }
        
        let output = args.iter()
            .map(|v| format!("{}", v))
            .collect::<Vec<_>>()
            .join(" ");
        
        eprintln!("{}", output);
        
        Ok(Value::Int(0))
    }
    
    fn arity(&self) -> u8 { 255 }
    fn name(&self) -> &str { "eprintln" }
}

#[derive(Debug)]
pub struct ReadLineFunction;

impl NativeFunction for ReadLineFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if !args.is_empty() {
            return Err(format!("read_line() takes no arguments ({} given)", args.len()));
        }
        
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                if input.ends_with('\n') {
                    input.pop();
                    if input.ends_with('\r') {
                        input.pop();
                    }
                }
                Ok(Value::String(input))
            }
            Err(err) => Err(format!("Failed to read line: {}", err)),
        }
    }
    
    fn arity(&self) -> u8 { 0 }
    fn name(&self) -> &str { "read_line" }
}