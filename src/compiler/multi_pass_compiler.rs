use crate::ast::{Node, Parameter};
use crate::bytecode::{BytecodeProgram, Instruction};
use super::symbol_collector::SymbolCollector;
use super::symbol_table::{SymbolTable, SymbolType};
use super::scope_manager::ScopeManager;
use super::code_generator::CodeGenerator;
use super::debug_printer::DebugPrinter;
use std::collections::HashMap;

pub struct MultiPassCompiler {
    symbol_table: SymbolTable,
    scope_manager: ScopeManager,
}

impl MultiPassCompiler {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            scope_manager: ScopeManager::new(),
        }
    }

    pub fn compile(&mut self, ast: &Node) -> Result<(BytecodeProgram, HashMap<String, u32>), String> {
        self.collect_symbols(ast)?;

        let (program, function_addresses) = self.generate_code()?;

        Ok((program, function_addresses))
    }

    fn collect_symbols(&mut self, ast: &Node) -> Result<(), String> {
        DebugPrinter::print_compilation_progress(1, "Collecting symbols");

        let mut symbol_collector = SymbolCollector::new();
        self.symbol_table = symbol_collector.collect_symbols(ast)?;

        DebugPrinter::print_symbols(&self.symbol_table);
        Ok(())
    }

    fn generate_code(&mut self) -> Result<(BytecodeProgram, HashMap<String, u32>), String> {
        DebugPrinter::print_compilation_progress(2, "Generating code");

        let mut program = BytecodeProgram::new();

        program.add_instruction(Instruction::CallMain);
        program.add_instruction(Instruction::Halt);

        self.compile_function_definitions(&mut program)?;

        let function_addresses = self.extract_function_addresses();

        Ok((program, function_addresses))
    }

    fn compile_function_definitions(&mut self, program: &mut BytecodeProgram) -> Result<(), String> {
        let callable_info = self.collect_callable_info();

        for (name, parameters, body, is_constructor) in callable_info {
            self.compile_callable(&name, &parameters, &body, is_constructor, program)?;
        }

        Ok(())
    }

    fn collect_callable_info(&self) -> Vec<(String, Vec<Parameter>, Node, bool)> {
        let mut callable_info = Vec::new();

        for symbol in self.symbol_table.list_functions() {
            match &symbol.symbol_type {
                SymbolType::Function { parameters, body, .. } => {
                    callable_info.push((
                        symbol.name.clone(),
                        parameters.clone(),
                        *body.clone(),
                        false,
                    ));
                },
                SymbolType::Constructor { parameters, body, .. } => {
                    callable_info.push((
                        symbol.name.clone(),
                        parameters.clone(),
                        *body.clone(),
                        true,
                    ));
                },
                _ => {}
            }
        }

        callable_info
    }

    fn compile_callable(
        &mut self,
        name: &str,
        parameters: &[Parameter],
        body: &Node,
        is_constructor: bool,
        program: &mut BytecodeProgram,
    ) -> Result<(), String> {
        let start_address = program.instructions.len() as u32;
        self.symbol_table.set_function_address(name, start_address)?;

        DebugPrinter::print_function_compilation(name, is_constructor, start_address);

        self.setup_callable_scope(parameters, is_constructor);

        {
            let mut code_generator = CodeGenerator::new(&mut self.scope_manager, &self.symbol_table);
            code_generator.set_current_function(Some(name.to_string()));
            code_generator.compile_node(body, program)?;
        }

        self.add_default_return(program, is_constructor, name);

        Ok(())
    }

    fn setup_callable_scope(&mut self, parameters: &[Parameter], is_constructor: bool) {
        self.scope_manager.clear();

        if is_constructor {
            self.scope_manager.setup_constructor(parameters);
        } else {
            self.scope_manager.setup_parameters(parameters);
        }
    }

    fn add_default_return(&mut self, program: &mut BytecodeProgram, is_constructor: bool, name: &str) {
        let needs_return = program.instructions.last()
            .map(|inst| !matches!(inst, Instruction::ReturnValue | Instruction::ReturnMain))
            .unwrap_or(true);

        if needs_return {
            if is_constructor {
                program.add_instruction(Instruction::LoadLocal(0));
                program.add_instruction(Instruction::ReturnValue);
            } else {
                // Default return value for main function when no explicit return
                program.add_instruction(Instruction::LoadInt(42));
                if name == "main" {
                    program.add_instruction(Instruction::ReturnMain);
                } else {
                    program.add_instruction(Instruction::ReturnValue);
                }
            }
        }
    }

    fn extract_function_addresses(&self) -> HashMap<String, u32> {
        let mut function_addresses = HashMap::new();

        for (name, symbol) in &self.symbol_table.symbols {
            match &symbol.symbol_type {
                SymbolType::Function { start_address: Some(addr), .. } => {
                    let function_name = self.extract_function_name(name);
                    function_addresses.insert(function_name, *addr);
                },
                SymbolType::Constructor { start_address: Some(addr), .. } => {
                    function_addresses.insert(name.clone(), *addr);
                },
                _ => {}
            }
        }

        function_addresses
    }

    fn extract_function_name(&self, qualified_name: &str) -> String {
        if qualified_name.contains("::") {
            qualified_name.split("::").last().unwrap_or(qualified_name).to_string()
        } else {
            qualified_name.to_string()
        }
    }

    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }

    pub fn scope_manager(&self) -> &ScopeManager {
        &self.scope_manager
    }
}

impl Default for MultiPassCompiler {
    fn default() -> Self {
        Self::new()
    }
}