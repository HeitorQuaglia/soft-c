use crate::bytecode::{BytecodeProgram, Instruction, InstructionPointer};
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
    Struct(HashMap<String, Value>),
    ObjectRef(usize),
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
            Value::Struct(fields) => {
                write!(f, "{{")?;
                let mut first = true;
                for (key, value) in fields {
                    if !first { write!(f, ", ")?; }
                    write!(f, "{}: {}", key, value)?;
                    first = false;
                }
                write!(f, "}}")
            },
            Value::ObjectRef(id) => write!(f, "ObjectRef({})", id),
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
            Value::Struct(_) => true,
            Value::ObjectRef(_) => true,
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
    pub object_heap: Vec<HashMap<String, Value>>,
    pub this_stack: Vec<Option<usize>>,
    pub function_addresses: HashMap<String, u32>,
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
            object_heap: Vec::new(),
            this_stack: vec![None],
            function_addresses: HashMap::new(),
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
                match self.native_registry.call("print", vec![value]) {
                    Ok(_) => {},
                    Err(err) => return Err(format!("Native function error: {}", err)),
                }
            },
            
            Instruction::Call(name, arg_count) => {
                let mut args = Vec::new();
                for _ in 0..arg_count {
                    args.push(self.pop_stack()?);
                }
                args.reverse();
                
                match self.native_registry.call(&name, args) {
                    Ok(result) => {
                        if result != Value::Null {
                            self.stack.push(result);
                        }
                    },
                    Err(_) => {
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
            
            Instruction::ImportModule(module_name) => {
                // TODO: Implementar import de módulo quando module_registry estiver integrado
                println!("DEBUG: Importing module: {}", module_name);
            },
            
            Instruction::ImportSymbol(module_name, symbol_name) => {
                // TODO: Implementar import de símbolo
                println!("DEBUG: Importing symbol {} from {}", symbol_name, module_name);
            },
            
            Instruction::ImportWildcard(module_name) => {
                // TODO: Implementar wildcard import
                println!("DEBUG: Wildcard import from {}", module_name);
            },
            
            Instruction::ExportSymbol(symbol_name) => {
                // TODO: Implementar export de símbolo
                println!("DEBUG: Exporting symbol: {}", symbol_name);
            },
            
            Instruction::LoadModuleSymbol(module_name, symbol_name) => {
                // TODO: Implementar carregamento de símbolo de módulo
                println!("DEBUG: Loading symbol {} from module {}", symbol_name, module_name);
                self.stack.push(Value::Null);
            },
            
            Instruction::NewStruct(struct_name) => {
                let struct_instance = HashMap::new();
                self.stack.push(Value::Struct(struct_instance));
            },
            
            Instruction::SetField(field_name) => {
                let value = self.pop_stack()?;
                let struct_val = self.pop_stack()?;
                
                match struct_val {
                    Value::Struct(mut fields) => {
                        fields.insert(field_name.clone(), value);
                        self.stack.push(Value::Struct(fields));
                    },
                    _ => return Err(format!("Cannot set field '{}' on non-struct value", field_name)),
                }
            },
            
            Instruction::GetField(field_name) => {
                let struct_val = self.pop_stack()?;
                
                match struct_val {
                    Value::Struct(fields) => {
                        match fields.get(&field_name) {
                            Some(value) => self.stack.push(value.clone()),
                            None => return Err(format!("Field '{}' not found in struct", field_name)),
                        }
                    },
                    _ => return Err(format!("Cannot get field '{}' from non-struct value", field_name)),
                }
            },

            Instruction::NewObject(class_name) => {
                let object_fields = HashMap::new();
                let object_ref = self.allocate_object(object_fields);
                self.stack.push(object_ref);
            },

            Instruction::GetObjectField(field_name) => {
                let object_ref = self.pop_stack()?;

                match object_ref {
                    Value::ObjectRef(object_id) => {
                        let value = self.get_object_field(object_id, &field_name)?;
                        self.stack.push(value);
                    },
                    Value::Struct(ref fields) => {
                        match fields.get(&field_name) {
                            Some(value) => self.stack.push(value.clone()),
                            None => return Err(format!("Field '{}' not found in object", field_name)),
                        }
                    },
                    _ => return Err(format!("Cannot get field '{}' from non-object value", field_name)),
                }
            },

            Instruction::SetObjectField(field_name) => {
                let value = self.pop_stack()?;
                let object_ref = self.pop_stack()?;

                match object_ref {
                    Value::ObjectRef(object_id) => {
                        self.set_object_field(object_id, field_name.clone(), value)?;
                        self.stack.push(Value::ObjectRef(object_id));
                    },
                    _ => return Err(format!("Cannot set field '{}' on non-object value", field_name)),
                }
            },

            Instruction::CallConstructor(class_name, arity) => {
                let mut args = Vec::new();
                for _ in 0..arity {
                    args.insert(0, self.pop_stack()?);
                }

                let object_ref = self.pop_stack()?;

                if let Value::ObjectRef(object_id) = object_ref {
                    let constructor_name = format!("{}::constructor", class_name);

                    if let Some(&constructor_address) = self.function_addresses.get(&constructor_name) {
                        let mut call_frame = CallFrame {
                            function_name: constructor_name.clone(),
                            locals: vec![Value::Null; 256],
                            return_address: self.ip + 1,
                        };

                        call_frame.locals[0] = Value::ObjectRef(object_id);

                        for (i, arg) in args.into_iter().enumerate() {
                            let slot = i + 1;
                            if slot < call_frame.locals.len() {
                                call_frame.locals[slot] = arg;
                            }
                        }

                        self.this_stack.push(Some(object_id));
                        self.call_stack.push(call_frame);

                        self.ip = constructor_address;
                        return Ok(());

                    } else {
                        self.stack.push(Value::ObjectRef(object_id));
                    }
                } else {
                    return Err("CallConstructor called on non-object".to_string());
                }
            },

            Instruction::LoadThis => {
                if let Some(Some(this_id)) = self.this_stack.last() {
                    self.stack.push(Value::ObjectRef(*this_id));
                } else {
                    return Err("Cannot use 'this' outside of object context".to_string());
                }
            },

            Instruction::PushThisContext => {
                let object_ref = self.pop_stack()?;
                match object_ref {
                    Value::ObjectRef(this_id) => {
                        self.this_stack.push(Some(this_id));
                        self.stack.push(Value::ObjectRef(this_id));
                    },
                    _ => return Err("PushThisContext called with non-object".to_string()),
                }
            },

            Instruction::PopThisContext => {
                if self.this_stack.len() > 1 {
                    self.this_stack.pop();
                } else {
                    return Err("Cannot pop root this context".to_string());
                }
            },

            Instruction::CallFunction(function_name, arity) => {
                if let Some(&function_address) = self.function_addresses.get(&function_name) {
                    let mut args = Vec::new();
                    let arity_value = arity;
                    for _ in 0..arity_value {
                        args.insert(0, self.pop_stack()?);
                    }

                    let mut call_frame = CallFrame {
                        function_name: function_name.clone(),
                        locals: vec![Value::Null; 256],
                        return_address: self.ip + 1,
                    };

                    for (i, arg) in args.into_iter().enumerate() {
                        if i < call_frame.locals.len() {
                            call_frame.locals[i] = arg;
                        }
                    }

                    self.call_stack.push(call_frame);

                    self.ip = function_address;
                    return Ok(());
                } else {
                    return Err(format!("Function '{}' not found", function_name));
                }
            },

            Instruction::ReturnValue => {
                if let Some(call_frame) = self.call_stack.pop() {
                    if call_frame.function_name.ends_with("::constructor") {
                        if let Some(this_id) = self.this_stack.pop().flatten() {
                        } else {
                            return Err("Constructor called without proper 'this' context".to_string());
                        }
                    } else if call_frame.function_name == "main" {
                        self.pop_stack().ok();
                        self.halted = true;
                        return Ok(());
                    }
                    self.ip = call_frame.return_address;
                    return Ok(());
                } else {
                    self.halted = true;
                }
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

    pub fn allocate_object(&mut self, fields: HashMap<String, Value>) -> Value {
        let object_id = self.object_heap.len();
        self.object_heap.push(fields);
        Value::ObjectRef(object_id)
    }

    pub fn get_object(&self, object_id: usize) -> Result<&HashMap<String, Value>, String> {
        self.object_heap.get(object_id)
            .ok_or_else(|| format!("Object with ID {} not found", object_id))
    }

    pub fn get_object_mut(&mut self, object_id: usize) -> Result<&mut HashMap<String, Value>, String> {
        self.object_heap.get_mut(object_id)
            .ok_or_else(|| format!("Object with ID {} not found", object_id))
    }

    pub fn get_object_field(&self, object_id: usize, field_name: &str) -> Result<Value, String> {
        let object = self.get_object(object_id)?;
        object.get(field_name)
            .cloned()
            .ok_or_else(|| format!("Field '{}' not found in object", field_name))
    }

    pub fn set_object_field(&mut self, object_id: usize, field_name: String, value: Value) -> Result<(), String> {
        let object = self.get_object_mut(object_id)?;
        object.insert(field_name, value);
        Ok(())
    }

    pub fn set_function_addresses(&mut self, addresses: HashMap<String, u32>) {
        self.function_addresses = addresses;
    }
}