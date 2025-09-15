use crate::bytecode::Instruction;
use super::machine::VirtualMachine;
use super::value::Value;

impl VirtualMachine {
    pub(super) fn execute_instruction(&mut self) -> Result<(), String> {
        let instruction = self.program.instructions[self.ip as usize].clone();

        match instruction {
            Instruction::LoadInt(val) => self.stack.push(Value::Int(val)),
            Instruction::LoadFloat(val) => self.stack.push(Value::Float(val)),
            Instruction::LoadString(val) => self.stack.push(Value::String(val)),
            Instruction::LoadChar(val) => self.stack.push(Value::Char(val)),
            Instruction::LoadBool(val) => self.stack.push(Value::Bool(val)),
            Instruction::LoadNull => self.stack.push(Value::Null),
            Instruction::LoadLocal(slot) => self.load_local(slot)?,
            Instruction::StoreLocal(slot) => self.store_local(slot)?,
            Instruction::LoadGlobal(name) => self.load_global(name)?,
            Instruction::StoreGlobal(name) => self.store_global(name)?,
            Instruction::Add => self.binary_op_add()?,
            Instruction::Sub => self.binary_op_sub()?,
            Instruction::Mul => self.binary_op_mul()?,
            Instruction::Div => self.binary_op_div()?,
            Instruction::Equal => self.binary_op_equal()?,
            Instruction::NotEqual => self.binary_op_not_equal()?,
            Instruction::Less => self.binary_op_less()?,
            Instruction::Greater => self.binary_op_greater()?,
            Instruction::LessEqual => self.binary_op_less_equal()?,
            Instruction::GreaterEqual => self.binary_op_greater_equal()?,
            Instruction::Mod => self.binary_op_mod()?,
            Instruction::Neg => self.unary_op_neg()?,
            Instruction::LogicalAnd => self.logical_and()?,
            Instruction::LogicalOr => self.logical_or()?,
            Instruction::LogicalNot => self.logical_not()?,
            Instruction::Jump(addr) => {
                self.ip = addr;
                return Ok(());
            }
            Instruction::JumpIfFalse(addr) => {
                let condition = self.pop_stack()?;
                if !condition.is_truthy() {
                    self.ip = addr;
                    return Ok(());
                }
            }
            Instruction::JumpIfTrue(addr) => {
                let condition = self.pop_stack()?;
                if condition.is_truthy() {
                    self.ip = addr;
                    return Ok(());
                }
            }
            Instruction::Print => self.call_print()?,
            Instruction::Call(name, arg_count) => self.call_native(name, arg_count)?,
            Instruction::CallMain => self.call_main()?,
            Instruction::ReturnMain => {
                // Main function is returning - program ends
                self.halted = true;
                return Ok(());
            }
            Instruction::Pop => {
                self.pop_stack()?;
            }
            Instruction::Dup => self.dup_stack()?,
            Instruction::Halt => {
                self.halted = true;
                return Ok(());
            }
            _ => return Err(format!("Unimplemented instruction: {:?}", instruction)),
        }

        self.ip += 1;
        Ok(())
    }

    fn load_local(&mut self, slot: u16) -> Result<(), String> {
        if let Some(frame) = self.call_stack.last() {
            if (slot as usize) < frame.locals.len() {
                self.stack.push(frame.locals[slot as usize].clone());
                Ok(())
            } else {
                Err(format!("Local variable slot {} out of bounds", slot))
            }
        } else {
            Err("No call frame for local variable access".to_string())
        }
    }

    fn store_local(&mut self, slot: u16) -> Result<(), String> {
        eprintln!("DEBUG: StoreLocal({}) - call_stack size: {}, stack size: {}", slot, self.call_stack.len(), self.stack.len());
        let value = self.pop_stack()?;
        if let Some(frame) = self.call_stack.last_mut() {
            eprintln!("DEBUG: StoreLocal({}) - frame locals size: {}", slot, frame.locals.len());
            if (slot as usize) < frame.locals.len() {
                frame.locals[slot as usize] = value;
            } else {
                frame.locals.resize((slot as usize) + 1, Value::Null);
                frame.locals[slot as usize] = value;
            }
            Ok(())
        } else {
            Err("No call frame for local variable storage".to_string())
        }
    }

    fn load_global(&mut self, name: String) -> Result<(), String> {
        if let Some(value) = self.globals.get(&name) {
            self.stack.push(value.clone());
            Ok(())
        } else {
            Err(format!("Undefined global variable: {}", name))
        }
    }

    fn store_global(&mut self, name: String) -> Result<(), String> {
        let value = self.pop_stack()?;
        self.globals.insert(name, value);
        Ok(())
    }

    fn binary_op_add(&mut self) -> Result<(), String> {
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
            }
            _ => return Err("Invalid types for addition".to_string()),
        }
        Ok(())
    }

    fn binary_op_sub(&mut self) -> Result<(), String> {
        let b = self.pop_stack()?;
        let a = self.pop_stack()?;
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a - b)),
            (Value::Float(a), Value::Float(b)) => self.stack.push(Value::Float(a - b)),
            (Value::Int(a), Value::Float(b)) => self.stack.push(Value::Float(a as f64 - b)),
            (Value::Float(a), Value::Int(b)) => self.stack.push(Value::Float(a - b as f64)),
            _ => return Err("Invalid types for subtraction".to_string()),
        }
        Ok(())
    }

    fn binary_op_mul(&mut self) -> Result<(), String> {
        let b = self.pop_stack()?;
        let a = self.pop_stack()?;
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a * b)),
            (Value::Float(a), Value::Float(b)) => self.stack.push(Value::Float(a * b)),
            (Value::Int(a), Value::Float(b)) => self.stack.push(Value::Float(a as f64 * b)),
            (Value::Float(a), Value::Int(b)) => self.stack.push(Value::Float(a * b as f64)),
            _ => return Err("Invalid types for multiplication".to_string()),
        }
        Ok(())
    }

    fn binary_op_div(&mut self) -> Result<(), String> {
        let b = self.pop_stack()?;
        let a = self.pop_stack()?;
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => {
                if b == 0 { return Err("Division by zero".to_string()); }
                self.stack.push(Value::Int(a / b));
            }
            (Value::Float(a), Value::Float(b)) => {
                if b == 0.0 { return Err("Division by zero".to_string()); }
                self.stack.push(Value::Float(a / b));
            }
            (Value::Int(a), Value::Float(b)) => {
                if b == 0.0 { return Err("Division by zero".to_string()); }
                self.stack.push(Value::Float(a as f64 / b));
            }
            (Value::Float(a), Value::Int(b)) => {
                if b == 0 { return Err("Division by zero".to_string()); }
                self.stack.push(Value::Float(a / b as f64));
            }
            _ => return Err("Invalid types for division".to_string()),
        }
        Ok(())
    }

    fn binary_op_equal(&mut self) -> Result<(), String> {
        let b = self.pop_stack()?;
        let a = self.pop_stack()?;
        self.stack.push(Value::Bool(a == b));
        Ok(())
    }

    fn binary_op_not_equal(&mut self) -> Result<(), String> {
        let b = self.pop_stack()?;
        let a = self.pop_stack()?;
        self.stack.push(Value::Bool(a != b));
        Ok(())
    }

    fn binary_op_less(&mut self) -> Result<(), String> {
        let b = self.pop_stack()?;
        let a = self.pop_stack()?;
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Bool(a < b)),
            (Value::Float(a), Value::Float(b)) => self.stack.push(Value::Bool(a < b)),
            (Value::Int(a), Value::Float(b)) => self.stack.push(Value::Bool((a as f64) < b)),
            (Value::Float(a), Value::Int(b)) => self.stack.push(Value::Bool(a < (b as f64))),
            _ => return Err("Invalid types for comparison".to_string()),
        }
        Ok(())
    }

    fn binary_op_greater(&mut self) -> Result<(), String> {
        let b = self.pop_stack()?;
        let a = self.pop_stack()?;
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Bool(a > b)),
            (Value::Float(a), Value::Float(b)) => self.stack.push(Value::Bool(a > b)),
            (Value::Int(a), Value::Float(b)) => self.stack.push(Value::Bool((a as f64) > b)),
            (Value::Float(a), Value::Int(b)) => self.stack.push(Value::Bool(a > (b as f64))),
            _ => return Err("Invalid types for comparison".to_string()),
        }
        Ok(())
    }

    fn binary_op_less_equal(&mut self) -> Result<(), String> {
        let b = self.pop_stack()?;
        let a = self.pop_stack()?;
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Bool(a <= b)),
            (Value::Float(a), Value::Float(b)) => self.stack.push(Value::Bool(a <= b)),
            (Value::Int(a), Value::Float(b)) => self.stack.push(Value::Bool((a as f64) <= b)),
            (Value::Float(a), Value::Int(b)) => self.stack.push(Value::Bool(a <= (b as f64))),
            _ => return Err("Invalid types for comparison".to_string()),
        }
        Ok(())
    }

    fn binary_op_greater_equal(&mut self) -> Result<(), String> {
        let b = self.pop_stack()?;
        let a = self.pop_stack()?;
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Bool(a >= b)),
            (Value::Float(a), Value::Float(b)) => self.stack.push(Value::Bool(a >= b)),
            (Value::Int(a), Value::Float(b)) => self.stack.push(Value::Bool((a as f64) >= b)),
            (Value::Float(a), Value::Int(b)) => self.stack.push(Value::Bool(a >= (b as f64))),
            _ => return Err("Invalid types for comparison".to_string()),
        }
        Ok(())
    }

    fn binary_op_mod(&mut self) -> Result<(), String> {
        let b = self.pop_stack()?;
        let a = self.pop_stack()?;
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => {
                if b == 0 { return Err("Modulo by zero".to_string()); }
                self.stack.push(Value::Int(a % b));
            }
            _ => return Err("Invalid types for modulo".to_string()),
        }
        Ok(())
    }

    fn unary_op_neg(&mut self) -> Result<(), String> {
        let a = self.pop_stack()?;
        match a {
            Value::Int(val) => self.stack.push(Value::Int(-val)),
            Value::Float(val) => self.stack.push(Value::Float(-val)),
            _ => return Err("Invalid type for negation".to_string()),
        }
        Ok(())
    }

    fn logical_and(&mut self) -> Result<(), String> {
        let b = self.pop_stack()?;
        let a = self.pop_stack()?;
        let result = a.is_truthy() && b.is_truthy();
        self.stack.push(Value::Bool(result));
        Ok(())
    }

    fn logical_or(&mut self) -> Result<(), String> {
        let b = self.pop_stack()?;
        let a = self.pop_stack()?;
        let result = a.is_truthy() || b.is_truthy();
        self.stack.push(Value::Bool(result));
        Ok(())
    }

    fn logical_not(&mut self) -> Result<(), String> {
        let a = self.pop_stack()?;
        self.stack.push(Value::Bool(!a.is_truthy()));
        Ok(())
    }

    fn call_print(&mut self) -> Result<(), String> {
        let value = self.pop_stack()?;
        match self.native_registry.call("print", vec![value]) {
            Ok(_) => Ok(()),
            Err(err) => Err(format!("Native function error: {}", err)),
        }
    }

    fn call_native(&mut self, name: String, arg_count: u8) -> Result<(), String> {
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
                Ok(())
            }
            Err(_) => Err(format!("Function not found: {}", name)),
        }
    }

    fn dup_stack(&mut self) -> Result<(), String> {
        if let Some(top) = self.stack.last() {
            self.stack.push(top.clone());
            Ok(())
        } else {
            Err("Stack underflow for dup".to_string())
        }
    }

    fn call_main(&mut self) -> Result<(), String> {
        // Lookup main function address
        if let Some(&main_address) = self.function_addresses.get("main") {
            // Create new call frame for main function
            // TODO: Get actual local count from function metadata
            let frame = super::call_frame::CallFrame::new_with_name(
                "main".to_string(),
                self.ip + 1,  // return address is next instruction
                10  // Allocate 10 locals for now (should be from function metadata)
            );
            self.call_stack.push(frame);

            // Debug: Check call stack
            eprintln!("DEBUG: CallMain - Created frame for main, call_stack size: {}", self.call_stack.len());
            eprintln!("DEBUG: CallMain - Frame locals size: {}", self.call_stack.last().unwrap().locals.len());

            // Jump to main function (subtract 1 because execute loop will increment IP)
            self.ip = main_address - 1;
            Ok(())
        } else {
            Err("Main function not found".to_string())
        }
    }
}
