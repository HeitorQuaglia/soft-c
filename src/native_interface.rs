use crate::vm::Value;
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
        
        crate::stdlib::register_stdlib(&mut registry);
        registry
    }
    
    pub fn register(&mut self, name: &str, function: Box<dyn NativeFunction>) {
        self.functions.insert(name.to_string(), function);
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
