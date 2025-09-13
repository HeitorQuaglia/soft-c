use crate::vm::{Value, VirtualMachine};
use std::collections::HashMap;

use std::fmt;

pub trait NativeFunction: fmt::Debug {
    fn call(&self, args: Vec<Value>) -> Result<Value, String>;
    fn arity(&self) -> u8;
    fn name(&self) -> &str;
}

pub struct SimpleNativeFunction {
    name: String,
    arity: u8,
    function: Box<dyn Fn(Vec<Value>) -> Result<Value, String>>,
}

impl fmt::Debug for SimpleNativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SimpleNativeFunction")
            .field("name", &self.name)
            .field("arity", &self.arity)
            .finish()
    }
}

impl SimpleNativeFunction {
    pub fn new<F>(name: String, arity: u8, function: F) -> Self 
    where 
        F: Fn(Vec<Value>) -> Result<Value, String> + 'static 
    {
        SimpleNativeFunction {
            name,
            arity,
            function: Box::new(function),
        }
    }
}

impl NativeFunction for SimpleNativeFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != self.arity as usize {
            return Err(format!("Function {} expects {} arguments, got {}", 
                self.name, self.arity, args.len()));
        }
        (self.function)(args)
    }
    
    fn arity(&self) -> u8 {
        self.arity
    }
    
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug)]
pub struct NativeRegistry {
    functions: HashMap<String, Box<dyn NativeFunction>>,
}

impl NativeRegistry {
    pub fn new() -> Self {
        let mut registry = NativeRegistry {
            functions: HashMap::new(),
        };
        
        registry.register_stdlib_functions();
        registry
    }
    
    pub fn register(&mut self, function: Box<dyn NativeFunction>) {
        self.functions.insert(function.name().to_string(), function);
    }
    
    pub fn get(&self, name: &str) -> Option<&dyn NativeFunction> {
        self.functions.get(name).map(|f| f.as_ref())
    }
    
    pub fn call(&self, name: &str, args: Vec<Value>) -> Result<Value, String> {
        match self.get(name) {
            Some(func) => func.call(args),
            None => Err(format!("Unknown native function: {}", name)),
        }
    }
    
    fn register_stdlib_functions(&mut self) {
        let print_func = SimpleNativeFunction::new(
            "print".to_string(),
            1,
            |args| {
                if let Some(arg) = args.get(0) {
                    println!("{}", arg);
                    Ok(Value::Null)
                } else {
                    Err("print() requires 1 argument".to_string())
                }
            }
        );
        self.register(Box::new(print_func));
        
        let println_func = SimpleNativeFunction::new(
            "println".to_string(),
            1,
            |args| {
                if let Some(arg) = args.get(0) {
                    println!("{}", arg);
                    Ok(Value::Null)
                } else {
                    Err("println() requires 1 argument".to_string())
                }
            }
        );
        self.register(Box::new(println_func));
        
        let len_func = SimpleNativeFunction::new(
            "len".to_string(),
            1,
            |args| {
                if let Some(arg) = args.get(0) {
                    match arg {
                        Value::String(s) => Ok(Value::Int(s.len() as i64)),
                        Value::Array(arr) => Ok(Value::Int(arr.len() as i64)),
                        _ => Err("len() can only be applied to strings or arrays".to_string()),
                    }
                } else {
                    Err("len() requires 1 argument".to_string())
                }
            }
        );
        self.register(Box::new(len_func));
        
        let abs_func = SimpleNativeFunction::new(
            "abs".to_string(),
            1,
            |args| {
                if let Some(arg) = args.get(0) {
                    match arg {
                        Value::Int(i) => Ok(Value::Int(i.abs())),
                        Value::Float(f) => Ok(Value::Float(f.abs())),
                        _ => Err("abs() can only be applied to numbers".to_string()),
                    }
                } else {
                    Err("abs() requires 1 argument".to_string())
                }
            }
        );
        self.register(Box::new(abs_func));
        
        let max_func = SimpleNativeFunction::new(
            "max".to_string(),
            2,
            |args| {
                if args.len() != 2 {
                    return Err("max() requires 2 arguments".to_string());
                }
                
                match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.max(b))),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.max(*b))),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64).max(*b))),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a.max(*b as f64))),
                    _ => Err("max() can only be applied to numbers".to_string()),
                }
            }
        );
        self.register(Box::new(max_func));
        
        let min_func = SimpleNativeFunction::new(
            "min".to_string(),
            2,
            |args| {
                if args.len() != 2 {
                    return Err("min() requires 2 arguments".to_string());
                }
                
                match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.min(b))),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.min(*b))),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64).min(*b))),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a.min(*b as f64))),
                    _ => Err("min() can only be applied to numbers".to_string()),
                }
            }
        );
        self.register(Box::new(min_func));
        
        let sqrt_func = SimpleNativeFunction::new(
            "sqrt".to_string(),
            1,
            |args| {
                if let Some(arg) = args.get(0) {
                    match arg {
                        Value::Int(i) => {
                            if *i < 0 {
                                Err("sqrt() of negative number".to_string())
                            } else {
                                Ok(Value::Float((*i as f64).sqrt()))
                            }
                        },
                        Value::Float(f) => {
                            if *f < 0.0 {
                                Err("sqrt() of negative number".to_string())
                            } else {
                                Ok(Value::Float(f.sqrt()))
                            }
                        },
                        _ => Err("sqrt() can only be applied to numbers".to_string()),
                    }
                } else {
                    Err("sqrt() requires 1 argument".to_string())
                }
            }
        );
        self.register(Box::new(sqrt_func));
        
        let pow_func = SimpleNativeFunction::new(
            "pow".to_string(),
            2,
            |args| {
                if args.len() != 2 {
                    return Err("pow() requires 2 arguments".to_string());
                }
                
                match (&args[0], &args[1]) {
                    (Value::Int(base), Value::Int(exp)) => {
                        if *exp < 0 {
                            Ok(Value::Float((*base as f64).powf(*exp as f64)))
                        } else {
                            Ok(Value::Int(base.pow(*exp as u32)))
                        }
                    },
                    (Value::Float(base), Value::Float(exp)) => Ok(Value::Float(base.powf(*exp))),
                    (Value::Int(base), Value::Float(exp)) => Ok(Value::Float((*base as f64).powf(*exp))),
                    (Value::Float(base), Value::Int(exp)) => Ok(Value::Float(base.powf(*exp as f64))),
                    _ => Err("pow() can only be applied to numbers".to_string()),
                }
            }
        );
        self.register(Box::new(pow_func));
    }
    
    pub fn list_functions(&self) -> Vec<String> {
        self.functions.keys().cloned().collect()
    }
}

#[macro_export]
macro_rules! native_function {
    ($name:expr, $arity:expr, |$args:ident| $body:expr) => {
        SimpleNativeFunction::new($name.to_string(), $arity, |$args| $body)
    };
}
