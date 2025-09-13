use crate::bytecode::{BytecodeProgram, Instruction, Constant, Function, InstructionPointer};
use crate::ast::DataType;
use crate::native_interface::NativeRegistry;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Array(Vec<Value>),
    Null,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::String(s) => write!(f, "{}", s),
            Value::Char(c) => write!(f, "{}", c),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Array(arr) => {
                write!(f, "[")?;
                for (i, val) in arr.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", val)?;
                }
                write!(f, "]")
            },
            Value::Null => write!(f, "null"),
        }
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Array(arr) => !arr.is_empty(),
            Value::Null => false,
            Value::Char(c) => *c != '\0',
        }
    }
    
    pub fn to_int(&self) -> Result<i64, String> {
        match self {
            Value::Int(i) => Ok(*i),
            Value::Float(f) => Ok(*f as i64),
            Value::Bool(b) => Ok(if *b { 1 } else { 0 }),
            Value::Char(c) => Ok(*c as i64),
            _ => Err(format!("Cannot convert {:?} to int", self)),
        }
    }
    
    pub fn to_float(&self) -> Result<f64, String> {
        match self {
            Value::Int(i) => Ok(*i as f64),
            Value::Float(f) => Ok(*f),
            Value::Bool(b) => Ok(if *b { 1.0 } else { 0.0 }),
            _ => Err(format!("Cannot convert {:?} to float", self)),
        }
    }
    
    pub fn to_string(&self) -> String {
        format!("{}", self)
    }
    
    pub fn to_bool(&self) -> bool {
        self.is_truthy()
    }
}

#[derive(Debug)]
pub struct CallFrame {
    pub function_name: String,
    pub locals: Vec<Value>,
    pub return_address: InstructionPointer,
}

#[derive(Debug)]
pub struct VirtualMachine {
    pub stack: Vec<Value>,
    pub call_stack: Vec<CallFrame>,
    pub globals: HashMap<String, Value>,
    pub ip: InstructionPointer,
    pub program: BytecodeProgram,
    pub halted: bool,
    pub native_registry: NativeRegistry,
}

impl VirtualMachine {
    pub fn new(program: BytecodeProgram) -> Self {
        VirtualMachine {
            stack: Vec::new(),
            call_stack: Vec::new(),
            globals: HashMap::new(),
            ip: 0,
            program,
            halted: false,
            native_registry: NativeRegistry::new(),
        }
    }
    
    pub fn with_native_registry(mut self, registry: NativeRegistry) -> Self {
        self.native_registry = registry;
        self
    }
    
    pub fn execute(&mut self) -> Result<(), String> {
        // Inicializar frame principal (simulando função main)
        let main_frame = CallFrame {
            function_name: "main".to_string(),
            locals: Vec::new(),
            return_address: 0,
        };
        self.call_stack.push(main_frame);
        
        while !self.halted && (self.ip as usize) < self.program.instructions.len() {
            self.execute_instruction()?;
        }
        Ok(())
    }
    
    fn execute_instruction(&mut self) -> Result<(), String> {
        let instruction = self.program.instructions[self.ip as usize].clone();
        
        match instruction {
            Instruction::LoadInt(val) => {
                self.stack.push(Value::Int(val));
            },
            
            Instruction::LoadFloat(val) => {
                self.stack.push(Value::Float(val));
            },
            
            Instruction::LoadString(val) => {
                self.stack.push(Value::String(val));
            },
            
            Instruction::LoadChar(val) => {
                self.stack.push(Value::Char(val));
            },
            
            Instruction::LoadBool(val) => {
                self.stack.push(Value::Bool(val));
            },
            
            Instruction::LoadNull => {
                self.stack.push(Value::Null);
            },
            
            Instruction::LoadLocal(slot) => {
                if let Some(frame) = self.call_stack.last() {
                    if (slot as usize) < frame.locals.len() {
                        self.stack.push(frame.locals[slot as usize].clone());
                    } else {
                        return Err(format!("Local variable slot {} out of bounds", slot));
                    }
                } else {
                    return Err("No call frame for local variable access".to_string());
                }
            },
            
            Instruction::StoreLocal(slot) => {
                let value = self.pop_stack()?;
                if let Some(frame) = self.call_stack.last_mut() {
                    if (slot as usize) < frame.locals.len() {
                        frame.locals[slot as usize] = value;
                    } else {
                        // Expandir locals se necessário
                        frame.locals.resize((slot as usize) + 1, Value::Null);
                        frame.locals[slot as usize] = value;
                    }
                } else {
                    return Err("No call frame for local variable storage".to_string());
                }
            },
            
            Instruction::LoadGlobal(name) => {
                if let Some(value) = self.globals.get(&name) {
                    self.stack.push(value.clone());
                } else {
                    return Err(format!("Undefined global variable: {}", name));
                }
            },
            
            Instruction::StoreGlobal(name) => {
                let value = self.pop_stack()?;
                self.globals.insert(name, value);
            },
            
            Instruction::Add => {
                let b = self.pop_stack()?;
                let a = self.pop_stack()?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a + b)),
                    (Value::Float(a), Value::Float(b)) => self.stack.push(Value::Float(a + b)),
                    (Value::Int(a), Value::Float(b)) => self.stack.push(Value::Float(a as f64 + b)),
                    (Value::Float(a), Value::Int(b)) => self.stack.push(Value::Float(a + b as f64)),
                    (Value::String(mut a), Value::String(b)) => {
                        a.push_str(&b);
                        self.stack.push(Value::String(a));
                    },
                    _ => return Err("Invalid types for addition".to_string()),
                }
            },
            
            Instruction::Sub => {
                let b = self.pop_stack()?;
                let a = self.pop_stack()?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a - b)),
                    (Value::Float(a), Value::Float(b)) => self.stack.push(Value::Float(a - b)),
                    (Value::Int(a), Value::Float(b)) => self.stack.push(Value::Float(a as f64 - b)),
                    (Value::Float(a), Value::Int(b)) => self.stack.push(Value::Float(a - b as f64)),
                    _ => return Err("Invalid types for subtraction".to_string()),
                }
            },
            
            Instruction::Mul => {
                let b = self.pop_stack()?;
                let a = self.pop_stack()?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a * b)),
                    (Value::Float(a), Value::Float(b)) => self.stack.push(Value::Float(a * b)),
                    (Value::Int(a), Value::Float(b)) => self.stack.push(Value::Float(a as f64 * b)),
                    (Value::Float(a), Value::Int(b)) => self.stack.push(Value::Float(a * b as f64)),
                    _ => return Err("Invalid types for multiplication".to_string()),
                }
            },
            
            Instruction::Div => {
                let b = self.pop_stack()?;
                let a = self.pop_stack()?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        if b == 0 {
                            return Err("Division by zero".to_string());
                        }
                        self.stack.push(Value::Int(a / b));
                    },
                    (Value::Float(a), Value::Float(b)) => {
                        if b == 0.0 {
                            return Err("Division by zero".to_string());
                        }
                        self.stack.push(Value::Float(a / b));
                    },
                    (Value::Int(a), Value::Float(b)) => {
                        if b == 0.0 {
                            return Err("Division by zero".to_string());
                        }
                        self.stack.push(Value::Float(a as f64 / b));
                    },
                    (Value::Float(a), Value::Int(b)) => {
                        if b == 0 {
                            return Err("Division by zero".to_string());
                        }
                        self.stack.push(Value::Float(a / b as f64));
                    },
                    _ => return Err("Invalid types for division".to_string()),
                }
            },
            
            Instruction::Equal => {
                let b = self.pop_stack()?;
                let a = self.pop_stack()?;
                self.stack.push(Value::Bool(a == b));
            },
            
            Instruction::NotEqual => {
                let b = self.pop_stack()?;
                let a = self.pop_stack()?;
                self.stack.push(Value::Bool(a != b));
            },
            
            Instruction::Less => {
                let b = self.pop_stack()?;
                let a = self.pop_stack()?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Bool(a < b)),
                    (Value::Float(a), Value::Float(b)) => self.stack.push(Value::Bool(a < b)),
                    (Value::Int(a), Value::Float(b)) => self.stack.push(Value::Bool((a as f64) < b)),
                    (Value::Float(a), Value::Int(b)) => self.stack.push(Value::Bool(a < (b as f64))),
                    _ => return Err("Invalid types for comparison".to_string()),
                }
            },
            
            Instruction::Greater => {
                let b = self.pop_stack()?;
                let a = self.pop_stack()?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Bool(a > b)),
                    (Value::Float(a), Value::Float(b)) => self.stack.push(Value::Bool(a > b)),
                    (Value::Int(a), Value::Float(b)) => self.stack.push(Value::Bool((a as f64) > b)),
                    (Value::Float(a), Value::Int(b)) => self.stack.push(Value::Bool(a > (b as f64))),
                    _ => return Err("Invalid types for comparison".to_string()),
                }
            },
            
            Instruction::LessEqual => {
                let b = self.pop_stack()?;
                let a = self.pop_stack()?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Bool(a <= b)),
                    (Value::Float(a), Value::Float(b)) => self.stack.push(Value::Bool(a <= b)),
                    (Value::Int(a), Value::Float(b)) => self.stack.push(Value::Bool((a as f64) <= b)),
                    (Value::Float(a), Value::Int(b)) => self.stack.push(Value::Bool(a <= (b as f64))),
                    _ => return Err("Invalid types for comparison".to_string()),
                }
            },
            
            Instruction::GreaterEqual => {
                let b = self.pop_stack()?;
                let a = self.pop_stack()?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Bool(a >= b)),
                    (Value::Float(a), Value::Float(b)) => self.stack.push(Value::Bool(a >= b)),
                    (Value::Int(a), Value::Float(b)) => self.stack.push(Value::Bool((a as f64) >= b)),
                    (Value::Float(a), Value::Int(b)) => self.stack.push(Value::Bool(a >= (b as f64))),
                    _ => return Err("Invalid types for comparison".to_string()),
                }
            },
            
            Instruction::Mod => {
                let b = self.pop_stack()?;
                let a = self.pop_stack()?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        if b == 0 {
                            return Err("Modulo by zero".to_string());
                        }
                        self.stack.push(Value::Int(a % b));
                    },
                    _ => return Err("Invalid types for modulo".to_string()),
                }
            },
            
            Instruction::Neg => {
                let a = self.pop_stack()?;
                match a {
                    Value::Int(val) => self.stack.push(Value::Int(-val)),
                    Value::Float(val) => self.stack.push(Value::Float(-val)),
                    _ => return Err("Invalid type for negation".to_string()),
                }
            },
            
            Instruction::LogicalAnd => {
                let b = self.pop_stack()?;
                let a = self.pop_stack()?;
                let result = a.is_truthy() && b.is_truthy();
                self.stack.push(Value::Bool(result));
            },
            
            Instruction::LogicalOr => {
                let b = self.pop_stack()?;
                let a = self.pop_stack()?;
                let result = a.is_truthy() || b.is_truthy();
                self.stack.push(Value::Bool(result));
            },
            
            Instruction::LogicalNot => {
                let a = self.pop_stack()?;
                self.stack.push(Value::Bool(!a.is_truthy()));
            },
            
            Instruction::Jump(addr) => {
                self.ip = addr;
                return Ok(());
            },
            
            Instruction::JumpIfFalse(addr) => {
                let condition = self.pop_stack()?;
                if !condition.is_truthy() {
                    self.ip = addr;
                    return Ok(());
                }
            },
            
            Instruction::JumpIfTrue(addr) => {
                let condition = self.pop_stack()?;
                if condition.is_truthy() {
                    self.ip = addr;
                    return Ok(());
                }
            },
            
            Instruction::Print => {
                let value = self.pop_stack()?;
                // Usar função nativa print
                match self.native_registry.call("print", vec![value]) {
                    Ok(_) => {},
                    Err(err) => return Err(format!("Native function error: {}", err)),
                }
            },
            
            Instruction::Call(name, arg_count) => {
                // Coletar argumentos da pilha
                let mut args = Vec::new();
                for _ in 0..arg_count {
                    args.push(self.pop_stack()?);
                }
                // Argumentos estão em ordem reversa, vamos corrigir
                args.reverse();
                
                // Tentar chamar função nativa primeiro
                match self.native_registry.call(&name, args) {
                    Ok(result) => {
                        // Empurrar resultado na pilha (se não for null)
                        if result != Value::Null {
                            self.stack.push(result);
                        }
                    },
                    Err(_) => {
                        // Se não é função nativa, tentar função SoftC
                        return Err(format!("Function not found: {}", name));
                    }
                }
            },
            
            Instruction::Pop => {
                self.pop_stack()?;
            },
            
            Instruction::Dup => {
                if let Some(top) = self.stack.last() {
                    self.stack.push(top.clone());
                } else {
                    return Err("Stack underflow for dup".to_string());
                }
            },
            
            Instruction::Halt => {
                self.halted = true;
                return Ok(());
            },
            
            _ => {
                return Err(format!("Unimplemented instruction: {:?}", instruction));
            },
        }
        
        self.ip += 1;
        Ok(())
    }
    
    fn pop_stack(&mut self) -> Result<Value, String> {
        self.stack.pop().ok_or_else(|| "Stack underflow".to_string())
    }
}