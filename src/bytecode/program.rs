use super::{Instruction, Constant, Function};

#[derive(Debug, Clone)]
pub struct BytecodeProgram {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
    pub globals: Vec<String>,
}

impl BytecodeProgram {
    pub fn new() -> Self {
        BytecodeProgram {
            instructions: Vec::new(),
            constants: Vec::new(),
            functions: Vec::new(),
            globals: Vec::new(),
        }
    }

    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn add_constant(&mut self, constant: Constant) -> u16 {
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }

    pub fn add_function(&mut self, function: Function) {
        self.functions.push(function);
    }

    pub fn add_global(&mut self, name: String) {
        self.globals.push(name);
    }

    pub fn current_address(&self) -> u32 {
        self.instructions.len() as u32
    }

    pub fn instruction_count(&self) -> usize {
        self.instructions.len()
    }

    pub fn constant_count(&self) -> usize {
        self.constants.len()
    }

    pub fn function_count(&self) -> usize {
        self.functions.len()
    }

    pub fn global_count(&self) -> usize {
        self.globals.len()
    }

    pub fn get_instruction(&self, address: u32) -> Option<&Instruction> {
        self.instructions.get(address as usize)
    }

    pub fn get_constant(&self, index: u16) -> Option<&Constant> {
        self.constants.get(index as usize)
    }

    pub fn get_function(&self, name: &str) -> Option<&Function> {
        self.functions.iter().find(|f| f.name == name)
    }

    pub fn get_global(&self, index: usize) -> Option<&String> {
        self.globals.get(index)
    }

    pub fn stats(&self) -> ProgramStats {
        let mut instruction_counts = std::collections::HashMap::new();

        for instruction in &self.instructions {
            let category = self.categorize_instruction(instruction);
            *instruction_counts.entry(category).or_insert(0) += 1;
        }

        ProgramStats {
            total_instructions: self.instruction_count(),
            total_constants: self.constant_count(),
            total_functions: self.function_count(),
            total_globals: self.global_count(),
            instruction_counts,
        }
    }

    fn categorize_instruction(&self, instruction: &Instruction) -> InstructionCategory {
        match instruction {
            _ if instruction.is_load() => InstructionCategory::Load,
            _ if instruction.is_store() => InstructionCategory::Store,
            _ if instruction.is_arithmetic() => InstructionCategory::Arithmetic,
            _ if instruction.is_comparison() => InstructionCategory::Comparison,
            _ if instruction.is_logical() => InstructionCategory::Logical,
            _ if instruction.is_jump() => InstructionCategory::ControlFlow,
            _ if instruction.is_call() => InstructionCategory::FunctionCall,
            _ if instruction.is_return() => InstructionCategory::FunctionReturn,
            _ if instruction.is_stack_op() => InstructionCategory::StackOp,
            _ if instruction.is_module_op() => InstructionCategory::Module,
            _ if instruction.is_object_op() => InstructionCategory::Object,
            _ => InstructionCategory::Other,
        }
    }

    pub fn disassemble(&self) -> String {
        let mut output = String::new();
        output.push_str("=== Bytecode Disassembly ===\n");

        for (i, instruction) in self.instructions.iter().enumerate() {
            output.push_str(&format!("{:04} | {:?}\n", i, instruction));
        }

        if !self.constants.is_empty() {
            output.push_str("\n=== Constants ===\n");
            for (i, constant) in self.constants.iter().enumerate() {
                output.push_str(&format!("{:04} | {:?}\n", i, constant));
            }
        }

        if !self.functions.is_empty() {
            output.push_str("\n=== Functions ===\n");
            for function in &self.functions {
                output.push_str(&format!(
                    "{} ({} params, {} locals) [{}-{}]\n",
                    function.name,
                    function.arity,
                    function.locals_count,
                    function.start_address,
                    function.end_address
                ));
            }
        }

        if !self.globals.is_empty() {
            output.push_str("\n=== Globals ===\n");
            for (i, global) in self.globals.iter().enumerate() {
                output.push_str(&format!("{:04} | {}\n", i, global));
            }
        }

        output
    }

    pub fn analyze(&self) -> String {
        let stats = self.stats();
        let mut output = String::new();

        output.push_str("=== Bytecode Analysis ===\n");
        output.push_str(&format!("Total Instructions: {}\n", stats.total_instructions));
        output.push_str(&format!("Total Constants: {}\n", stats.total_constants));
        output.push_str(&format!("Total Functions: {}\n", stats.total_functions));
        output.push_str(&format!("Total Globals: {}\n", stats.total_globals));

        output.push_str("\n=== Instruction Distribution ===\n");
        for (category, count) in &stats.instruction_counts {
            let percentage = (*count as f64 / stats.total_instructions as f64) * 100.0;
            output.push_str(&format!("{:?}: {} ({:.1}%)\n", category, count, percentage));
        }

        output
    }
}

impl Default for BytecodeProgram {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct ProgramStats {
    pub total_instructions: usize,
    pub total_constants: usize,
    pub total_functions: usize,
    pub total_globals: usize,
    pub instruction_counts: std::collections::HashMap<InstructionCategory, usize>,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum InstructionCategory {
    Load,
    Store,
    Arithmetic,
    Comparison,
    Logical,
    ControlFlow,
    FunctionCall,
    FunctionReturn,
    StackOp,
    Module,
    Object,
    Other,
}