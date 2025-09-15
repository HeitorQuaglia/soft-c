use crate::bytecode::InstructionPointer;
use super::value::Value;

#[derive(Debug)]
pub struct CallFrame {
    pub function_name: String,
    pub locals: Vec<Value>,
    pub return_address: InstructionPointer,
}

impl CallFrame {
    pub fn new(return_address: InstructionPointer, locals_count: usize) -> Self {
        Self {
            function_name: String::new(),
            locals: vec![Value::Null; locals_count],
            return_address,
        }
    }

    pub fn new_with_name(function_name: String, return_address: InstructionPointer, locals_count: usize) -> Self {
        Self {
            function_name,
            locals: vec![Value::Null; locals_count],
            return_address,
        }
    }
}
