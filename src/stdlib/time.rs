use crate::native_interface::NativeFunction;
use crate::vm::value::Value;
use std::thread;
use std::time::{Instant, SystemTime, UNIX_EPOCH};

#[derive(Debug)]
pub struct NowFunction;

impl NativeFunction for NowFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if !args.is_empty() {
            return Err(format!("now() takes no arguments ({} given)", args.len()));
        }
        
        match SystemTime::now().duration_since(UNIX_EPOCH) {
            Ok(duration) => Ok(Value::Int(duration.as_secs() as i64)),
            Err(_) => Err("Failed to get current time".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 0 }
    fn name(&self) -> &str { "now" }
}

#[derive(Debug)]
pub struct NowMillisFunction;

impl NativeFunction for NowMillisFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if !args.is_empty() {
            return Err(format!("now_millis() takes no arguments ({} given)", args.len()));
        }
        
        match SystemTime::now().duration_since(UNIX_EPOCH) {
            Ok(duration) => Ok(Value::Int(duration.as_millis() as i64)),
            Err(_) => Err("Failed to get current time".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 0 }
    fn name(&self) -> &str { "now_millis" }
}

#[derive(Debug)]
pub struct NowMicrosFunction;

impl NativeFunction for NowMicrosFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if !args.is_empty() {
            return Err(format!("now_micros() takes no arguments ({} given)", args.len()));
        }
        
        match SystemTime::now().duration_since(UNIX_EPOCH) {
            Ok(duration) => Ok(Value::Int(duration.as_micros() as i64)),
            Err(_) => Err("Failed to get current time".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 0 }
    fn name(&self) -> &str { "now_micros" }
}

#[derive(Debug)]
pub struct SleepFunction;

impl NativeFunction for SleepFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err(format!("sleep() takes exactly 1 argument ({} given)", args.len()));
        }
        
        match &args[0] {
            Value::Int(seconds) => {
                if *seconds < 0 {
                    return Err("sleep() argument must be non-negative".to_string());
                }
                thread::sleep(std::time::Duration::from_secs(*seconds as u64));
                Ok(Value::Int(0))
            }
            Value::Float(seconds) => {
                if *seconds < 0.0 {
                    return Err("sleep() argument must be non-negative".to_string());
                }
                thread::sleep(std::time::Duration::from_secs_f64(*seconds));
                Ok(Value::Int(0))
            }
            _ => Err("sleep() argument must be a number".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 1 }
    fn name(&self) -> &str { "sleep" }
}

#[derive(Debug)]
pub struct SleepMillisFunction;

impl NativeFunction for SleepMillisFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err(format!("sleep_millis() takes exactly 1 argument ({} given)", args.len()));
        }
        
        match &args[0] {
            Value::Int(millis) => {
                if *millis < 0 {
                    return Err("sleep_millis() argument must be non-negative".to_string());
                }
                thread::sleep(std::time::Duration::from_millis(*millis as u64));
                Ok(Value::Int(0))
            }
            _ => Err("sleep_millis() argument must be an integer".to_string()),
        }
    }
    
    fn arity(&self) -> u8 { 1 }
    fn name(&self) -> &str { "sleep_millis" }
}

static mut TIMER_START: Option<Instant> = None;

#[derive(Debug)]
pub struct TimerStartFunction;

impl NativeFunction for TimerStartFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if !args.is_empty() {
            return Err(format!("timer_start() takes no arguments ({} given)", args.len()));
        }
        
        unsafe {
            TIMER_START = Some(Instant::now());
        }
        
        Ok(Value::Int(0))
    }
    
    fn arity(&self) -> u8 { 0 }
    fn name(&self) -> &str { "timer_start" }
}

#[derive(Debug)]
pub struct TimerElapsedFunction;

impl NativeFunction for TimerElapsedFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        if !args.is_empty() {
            return Err(format!("timer_elapsed() takes no arguments ({} given)", args.len()));
        }
        
        unsafe {
            match TIMER_START {
                Some(start) => {
                    let elapsed = start.elapsed();
                    Ok(Value::Float(elapsed.as_secs_f64()))
                }
                None => Err("timer_start() must be called before timer_elapsed()".to_string()),
            }
        }
    }
    
    fn arity(&self) -> u8 { 0 }
    fn name(&self) -> &str { "timer_elapsed" }
}