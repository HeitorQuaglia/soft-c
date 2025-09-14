use crate::ast::{Node, NodeData};
use crate::bytecode::{BytecodeProgram, Instruction};
use crate::symbol_collector::SymbolCollector;
use crate::symbol_table::{SymbolTable, SymbolType};
use std::collections::HashMap;

pub struct MultiPassCompiler {
    symbol_table: SymbolTable,
    local_variables: HashMap<String, u16>,
    local_count: u16,
    break_stack: Vec<u32>,
    continue_stack: Vec<u32>,
}

impl MultiPassCompiler {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            local_variables: HashMap::new(),
            local_count: 0,
            break_stack: Vec::new(),
            continue_stack: Vec::new(),
        }
    }

    pub fn compile(&mut self, ast: &Node) -> Result<(BytecodeProgram, HashMap<String, u32>), String> {
        println!("Pass 1: Collecting symbols...");
        let mut symbol_collector = SymbolCollector::new();
        self.symbol_table = symbol_collector.collect_symbols(ast)?;
        self.debug_print_symbols();

        println!("Pass 2: Generating code...");
        let mut program = BytecodeProgram::new();

        program.add_instruction(Instruction::CallFunction("main".to_string(), 0));
        program.add_instruction(Instruction::Halt);

        self.compile_function_definitions(&mut program)?;

        let mut function_addresses = HashMap::new();
        for (name, symbol) in &self.symbol_table.symbols {
            match &symbol.symbol_type {
                SymbolType::Function { start_address: Some(addr), .. } => {
                    let function_name = if name.contains("::") {
                        name.split("::").last().unwrap_or(name).to_string()
                    } else {
                        name.clone()
                    };
                    function_addresses.insert(function_name, *addr);
                },
                SymbolType::Constructor { start_address: Some(addr), .. } => {
                    function_addresses.insert(name.clone(), *addr);
                },
                _ => {}
            }
        }

        Ok((program, function_addresses))
    }

    fn debug_print_symbols(&self) {
        println!("=== Symbol Table ===");
        for (name, symbol) in &self.symbol_table.symbols {
            match &symbol.symbol_type {
                SymbolType::Function { return_type, parameters, .. } => {
                    print!("FUNCTION {}: {} -> {:?}", name, parameters.len(), return_type);
                    if let Some(addr) = self.symbol_table.get_function_address(name) {
                        print!(" @ 0x{:04X}", addr);
                    }
                    println!();
                },
                SymbolType::Constructor { class_name, parameters, .. } => {
                    print!("CONSTRUCTOR {} for class {}: {} params", name, class_name, parameters.len());
                    if let Some(addr) = self.symbol_table.get_function_address(name) {
                        print!(" @ 0x{:04X}", addr);
                    }
                    println!();
                },
                SymbolType::Class { fields, .. } => {
                    println!("CLASS {}: {} fields", name, fields.len());
                },
                SymbolType::Variable { data_type, is_global, .. } => {
                    println!("VARIABLE {}: {:?} (global: {})", name, data_type, is_global);
                },
                SymbolType::Module { .. } => {
                    println!("MODULE {}", name);
                },
            }
        }
        println!("==================");

        println!("=== Function Addresses ===");
        for func in self.symbol_table.list_functions() {
            match &func.symbol_type {
                SymbolType::Function { .. } => println!("Function: {}", func.name),
                SymbolType::Constructor { class_name, .. } => println!("Constructor: {} (for class {})", func.name, class_name),
                _ => {}
            }
        }
        println!("========================");
    }

    fn compile_function_definitions(&mut self, program: &mut BytecodeProgram) -> Result<(), String> {
        let mut callable_info = Vec::new();
        for symbol in self.symbol_table.list_functions() {
            match &symbol.symbol_type {
                SymbolType::Function { body, parameters, .. } => {
                    callable_info.push((symbol.name.clone(), body.clone(), parameters.clone(), false));
                },
                SymbolType::Constructor { body, parameters, .. } => {
                    callable_info.push((symbol.name.clone(), body.clone(), parameters.clone(), true));
                },
                _ => {}
            }
        }

        for (name, body, parameters, is_constructor) in callable_info {
            let start_address = program.instructions.len() as u32;
            self.symbol_table.set_function_address(&name, start_address)?;

            let callable_type = if is_constructor { "constructor" } else { "function" };
            println!("Compiling {} '{}' at address 0x{:04X}", callable_type, name, start_address);

            self.local_variables.clear();
            self.local_count = 0;

            if is_constructor {
                self.local_variables.insert("this".to_string(), 0);
                self.local_count = 1;
            }

            for param in &parameters {
                let slot = self.local_count;
                self.local_variables.insert(param.name.clone(), slot);
                self.local_count += 1;
            }

            self.compile_node(&body, program)?;

            if let Some(last_instruction) = program.instructions.last() {
                if !matches!(last_instruction, Instruction::ReturnValue) {
                    if is_constructor {
                        program.add_instruction(Instruction::LoadLocal(0));
                        program.add_instruction(Instruction::ReturnValue);
                    } else {
                        program.add_instruction(Instruction::LoadInt(0));
                        program.add_instruction(Instruction::ReturnValue);
                    }
                }
            } else {
                if is_constructor {
                    program.add_instruction(Instruction::LoadLocal(0));
                    program.add_instruction(Instruction::ReturnValue);
                } else {
                    program.add_instruction(Instruction::LoadInt(0));
                    program.add_instruction(Instruction::ReturnValue);
                }
            }
        }

        Ok(())
    }


    fn compile_node(&mut self, node: &Node, program: &mut BytecodeProgram) -> Result<(), String> {
        match &node.data {
            NodeData::Program { statements } => {
                for stmt in statements {
                    self.compile_node(stmt, program)?;
                }
            },

            NodeData::VarDecl { name, init_expr, .. } => {
                if let Some(expr) = init_expr {
                    self.compile_node(expr, program)?;
                } else {
                    program.add_instruction(Instruction::LoadInt(0));
                }

                let slot = self.local_count;
                self.local_variables.insert(name.clone(), slot);
                self.local_count += 1;
                program.add_instruction(Instruction::StoreLocal(slot));
            },

            NodeData::Assignment { target, value, .. } => {
                match &target.data {
                    NodeData::Identifier { name } => {
                        self.compile_node(value, program)?;
                        if let Some(&slot) = self.local_variables.get(name) {
                            program.add_instruction(Instruction::StoreLocal(slot));
                        } else {
                            program.add_instruction(Instruction::StoreGlobal(name.clone()));
                        }
                    },
                    NodeData::MemberAccess { object, member_name } => {
                        self.compile_node(object, program)?;
                        self.compile_node(value, program)?;
                        program.add_instruction(Instruction::SetObjectField(member_name.clone()));
                        program.add_instruction(Instruction::Pop);
                    },
                    _ => {
                        return Err("Complex assignment targets not yet implemented".to_string());
                    }
                }
            },

            NodeData::Identifier { name } => {
                if name == "this" {
                    program.add_instruction(Instruction::LoadThis);
                } else if let Some(&slot) = self.local_variables.get(name) {
                    program.add_instruction(Instruction::LoadLocal(slot));
                } else {
                    program.add_instruction(Instruction::LoadGlobal(name.clone()));
                }
            },

            NodeData::FunctionCall { name, args } => {
                if let Some(symbol) = self.symbol_table.lookup(name) {
                    if let SymbolType::Function { .. } = symbol.symbol_type {
                        for arg in args {
                            self.compile_node(arg, program)?;
                        }

                        program.add_instruction(Instruction::CallFunction(name.clone(), args.len() as u8));
                        return Ok(());
                    }
                }

                for arg in args {
                    self.compile_node(arg, program)?;
                }
                program.add_instruction(Instruction::Call(name.clone(), args.len() as u8));
            },

            NodeData::ReturnStmt { value } => {
                if let Some(return_value) = value {
                    self.compile_node(return_value, program)?;
                    program.add_instruction(Instruction::ReturnValue);
                } else {
                    program.add_instruction(Instruction::Return);
                }
            },

            NodeData::Block { statements } => {
                for stmt in statements {
                    self.compile_node(stmt, program)?;
                }
            },

            NodeData::BinaryOp { op, left, right } => {
                self.compile_node(left, program)?;
                self.compile_node(right, program)?;
                match op {
                    crate::ast::BinaryOpType::Add => program.add_instruction(Instruction::Add),
                    crate::ast::BinaryOpType::Sub => program.add_instruction(Instruction::Sub),
                    crate::ast::BinaryOpType::Mul => program.add_instruction(Instruction::Mul),
                    crate::ast::BinaryOpType::Div => program.add_instruction(Instruction::Div),
                    crate::ast::BinaryOpType::Equal => program.add_instruction(Instruction::Equal),
                    crate::ast::BinaryOpType::NotEqual => program.add_instruction(Instruction::NotEqual),
                    crate::ast::BinaryOpType::Less => program.add_instruction(Instruction::Less),
                    crate::ast::BinaryOpType::Greater => program.add_instruction(Instruction::Greater),
                    crate::ast::BinaryOpType::LessEqual => program.add_instruction(Instruction::LessEqual),
                    crate::ast::BinaryOpType::GreaterEqual => program.add_instruction(Instruction::GreaterEqual),
                    crate::ast::BinaryOpType::LogicalAnd => program.add_instruction(Instruction::LogicalAnd),
                    crate::ast::BinaryOpType::LogicalOr => program.add_instruction(Instruction::LogicalOr),
                    _ => return Err(format!("Binary operator {:?} not implemented", op)),
                }
            },

            NodeData::Literal { value, .. } => {
                match value {
                    crate::ast::LiteralValue::Int(i) => program.add_instruction(Instruction::LoadInt(*i)),
                    crate::ast::LiteralValue::Float(f) => program.add_instruction(Instruction::LoadFloat(*f)),
                    crate::ast::LiteralValue::String(s) => program.add_instruction(Instruction::LoadString(s.clone())),
                    crate::ast::LiteralValue::Char(c) => program.add_instruction(Instruction::LoadChar(*c)),
                    crate::ast::LiteralValue::Bool(b) => program.add_instruction(Instruction::LoadBool(*b)),
                }
            },

            NodeData::ExprStmt { expr } => {
                self.compile_node(expr, program)?;
                program.add_instruction(Instruction::Pop);
            },

            NodeData::StructLiteral { struct_name, field_values } => {
                program.add_instruction(Instruction::NewObject(struct_name.clone()));

                if !field_values.is_empty() {
                    program.add_instruction(Instruction::Dup);
                    for field_value in field_values {
                        self.compile_node(&field_value.value, program)?;
                    }

                    program.add_instruction(Instruction::CallConstructor(
                        struct_name.clone(),
                        field_values.len() as u8
                    ));

                    program.add_instruction(Instruction::Pop);
                }
            },

            NodeData::MemberAccess { object, member_name } => {
                self.compile_node(object, program)?;
                program.add_instruction(Instruction::GetObjectField(member_name.clone()));
            },

            NodeData::IfStmt { condition, then_stmt, else_stmt } => {
                self.compile_node(condition, program)?;

                let else_jump = program.instructions.len();
                program.add_instruction(Instruction::JumpIfFalse(0));

                self.compile_node(then_stmt, program)?;

                let end_jump = program.instructions.len();
                program.add_instruction(Instruction::Jump(0));

                let else_start = program.instructions.len() as u32;
                if let Some(Instruction::JumpIfFalse(addr)) = program.instructions.get_mut(else_jump) {
                    *addr = else_start;
                }

                if let Some(else_node) = else_stmt {
                    self.compile_node(else_node, program)?;
                }

                let end_addr = program.instructions.len() as u32;
                if let Some(Instruction::Jump(addr)) = program.instructions.get_mut(end_jump) {
                    *addr = end_addr;
                }
            },

            NodeData::PrintStmt { arguments } => {
                for arg in arguments {
                    self.compile_node(arg, program)?;
                    program.add_instruction(Instruction::Call("print".to_string(), 1));
                    program.add_instruction(Instruction::Pop);
                }
            },

            NodeData::ImportStmt { .. } => {
            },

            // TODO: Implement other node types as needed
            _ => {
                return Err(format!("Node type {:?} not yet implemented in multi-pass compiler", node.node_type));
            }
        }

        Ok(())
    }
}