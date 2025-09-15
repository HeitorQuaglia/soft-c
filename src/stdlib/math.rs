use crate::native_interface::NativeFunction;
use crate::vm::value::Value;

#[derive(Debug)]
pub struct AbsFunction;

impl NativeFunction for AbsFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err(format!("abs() takes exactly 1 argument ({} given)", args.len()));
        }
        
        match &args[0] {
            Value::Int(n) => Ok(Value::Int(n.abs())),
            Value::Float(n) => Ok(Value::Float(n.abs())),
            _ => Err("abs() argument must be a number".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 1 }
    fn name(&self) -> &str { "abs" }
}

#[derive(Debug)]
pub struct MaxFunction;

impl NativeFunction for MaxFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err(format!("max() takes exactly 2 arguments ({} given)", args.len()));
        }
        
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.max(b))),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.max(*b))),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64).max(*b))),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a.max(*b as f64))),
            _ => Err("max() arguments must be numbers".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 2 }
    fn name(&self) -> &str { "max" }
}

#[derive(Debug)]
pub struct MinFunction;

impl NativeFunction for MinFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err(format!("min() takes exactly 2 arguments ({} given)", args.len()));
        }
        
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.min(b))),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.min(*b))),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64).min(*b))),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a.min(*b as f64))),
            _ => Err("min() arguments must be numbers".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 2 }
    fn name(&self) -> &str { "min" }
}

#[derive(Debug)]
pub struct SqrtFunction;

impl NativeFunction for SqrtFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err(format!("sqrt() takes exactly 1 argument ({} given)", args.len()));
        }
        
        match &args[0] {
            Value::Int(n) => {
                if *n < 0 {
                    return Err("sqrt() of negative number".to_string());
                }
                Ok(Value::Float((*n as f64).sqrt()))
            },
            Value::Float(n) => {
                if *n < 0.0 {
                    return Err("sqrt() of negative number".to_string());
                }
                Ok(Value::Float(n.sqrt()))
            },
            _ => Err("sqrt() argument must be a number".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 1 }
    fn name(&self) -> &str { "sqrt" }
}

#[derive(Debug)]
pub struct PowFunction;

impl NativeFunction for PowFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err(format!("pow() takes exactly 2 arguments ({} given)", args.len()));
        }
        
        match (&args[0], &args[1]) {
            (Value::Int(base), Value::Int(exp)) => {
                Ok(Value::Float((*base as f64).powf(*exp as f64)))
            },
            (Value::Float(base), Value::Float(exp)) => {
                Ok(Value::Float(base.powf(*exp)))
            },
            (Value::Int(base), Value::Float(exp)) => {
                Ok(Value::Float((*base as f64).powf(*exp)))
            },
            (Value::Float(base), Value::Int(exp)) => {
                Ok(Value::Float(base.powf(*exp as f64)))
            },
            _ => Err("pow() arguments must be numbers".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 2 }
    fn name(&self) -> &str { "pow" }
}

#[derive(Debug)]
pub struct FloorFunction;

impl NativeFunction for FloorFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err(format!("floor() takes exactly 1 argument ({} given)", args.len()));
        }
        
        match &args[0] {
            Value::Int(n) => Ok(Value::Int(*n)),
            Value::Float(n) => Ok(Value::Int(n.floor() as i64)),
            _ => Err("floor() argument must be a number".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 1 }
    fn name(&self) -> &str { "floor" }
}

#[derive(Debug)]
pub struct CeilFunction;

impl NativeFunction for CeilFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err(format!("ceil() takes exactly 1 argument ({} given)", args.len()));
        }
        
        match &args[0] {
            Value::Int(n) => Ok(Value::Int(*n)),
            Value::Float(n) => Ok(Value::Int(n.ceil() as i64)),
            _ => Err("ceil() argument must be a number".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 1 }
    fn name(&self) -> &str { "ceil" }
}

#[derive(Debug)]
pub struct RoundFunction;

impl NativeFunction for RoundFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err(format!("round() takes exactly 1 argument ({} given)", args.len()));
        }
        
        match &args[0] {
            Value::Int(n) => Ok(Value::Int(*n)),
            Value::Float(n) => Ok(Value::Int(n.round() as i64)),
            _ => Err("round() argument must be a number".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 1 }
    fn name(&self) -> &str { "round" }
}