use crate::bytecode::{BytecodeProgram, InstructionPointer};
use crate::native_interface::NativeRegistry;
use std::collections::HashMap;
use super::value::Value;
use super::call_frame::CallFrame;

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

    pub(super) fn pop_stack(&mut self) -> Result<Value, String> {
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
