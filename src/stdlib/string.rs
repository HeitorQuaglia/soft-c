use crate::native_interface::NativeFunction;
use crate::vm::value::Value;

#[derive(Debug)]
pub struct LenFunction;

impl NativeFunction for LenFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err(format!("len() takes exactly 1 argument ({} given)", args.len()));
        }
        
        match &args[0] {
            Value::String(s) => Ok(Value::Int(s.len() as i64)),
            _ => Err("len() argument must be a string".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 1 }
    fn name(&self) -> &str { "len" }
}

#[derive(Debug)]
pub struct ToUpperFunction;

impl NativeFunction for ToUpperFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err(format!("to_upper() takes exactly 1 argument ({} given)", args.len()));
        }
        
        match &args[0] {
            Value::String(s) => Ok(Value::String(s.to_uppercase())),
            _ => Err("to_upper() argument must be a string".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 1 }
    fn name(&self) -> &str { "to_upper" }
}

#[derive(Debug)]
pub struct ToLowerFunction;

impl NativeFunction for ToLowerFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err(format!("to_lower() takes exactly 1 argument ({} given)", args.len()));
        }
        
        match &args[0] {
            Value::String(s) => Ok(Value::String(s.to_lowercase())),
            _ => Err("to_lower() argument must be a string".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 1 }
    fn name(&self) -> &str { "to_lower" }
}

#[derive(Debug)]
pub struct TrimFunction;

impl NativeFunction for TrimFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err(format!("trim() takes exactly 1 argument ({} given)", args.len()));
        }
        
        match &args[0] {
            Value::String(s) => Ok(Value::String(s.trim().to_string())),
            _ => Err("trim() argument must be a string".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 1 }
    fn name(&self) -> &str { "trim" }
}

#[derive(Debug)]
pub struct SplitFunction;

impl NativeFunction for SplitFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err(format!("split() takes exactly 2 arguments ({} given)", args.len()));
        }
        
        match (&args[0], &args[1]) {
            (Value::String(s), Value::String(delimiter)) => {
                let parts: Vec<Value> = s.split(delimiter)
                    .map(|part| Value::String(part.to_string()))
                    .collect();
                Ok(Value::Array(parts))
            }
            _ => Err("split() arguments must be strings".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 2 }
    fn name(&self) -> &str { "split" }
}

#[derive(Debug)]
pub struct JoinFunction;

impl NativeFunction for JoinFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err(format!("join() takes exactly 2 arguments ({} given)", args.len()));
        }
        
        match (&args[0], &args[1]) {
            (Value::Array(arr), Value::String(delimiter)) => {
                let strings: Result<Vec<String>, String> = arr.iter()
                    .map(|v| match v {
                        Value::String(s) => Ok(s.clone()),
                        _ => Err("join() array must contain only strings".to_string()),
                    })
                    .collect();
                
                match strings {
                    Ok(strs) => Ok(Value::String(strs.join(delimiter))),
                    Err(e) => Err(e),
                }
            }
            _ => Err("join() first argument must be an array, second must be a string".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 2 }
    fn name(&self) -> &str { "join" }
}

#[derive(Debug)]
pub struct ContainsFunction;

impl NativeFunction for ContainsFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err(format!("contains() takes exactly 2 arguments ({} given)", args.len()));
        }
        
        match (&args[0], &args[1]) {
            (Value::String(haystack), Value::String(needle)) => {
                Ok(Value::Bool(haystack.contains(needle)))
            }
            _ => Err("contains() arguments must be strings".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 2 }
    fn name(&self) -> &str { "contains" }
}

#[derive(Debug)]
pub struct StartsWithFunction;

impl NativeFunction for StartsWithFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err(format!("starts_with() takes exactly 2 arguments ({} given)", args.len()));
        }
        
        match (&args[0], &args[1]) {
            (Value::String(s), Value::String(prefix)) => {
                Ok(Value::Bool(s.starts_with(prefix)))
            }
            _ => Err("starts_with() arguments must be strings".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 2 }
    fn name(&self) -> &str { "starts_with" }
}

#[derive(Debug)]
pub struct EndsWithFunction;

impl NativeFunction for EndsWithFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err(format!("ends_with() takes exactly 2 arguments ({} given)", args.len()));
        }
        
        match (&args[0], &args[1]) {
            (Value::String(s), Value::String(suffix)) => {
                Ok(Value::Bool(s.ends_with(suffix)))
            }
            _ => Err("ends_with() arguments must be strings".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 2 }
    fn name(&self) -> &str { "ends_with" }
}