use crate::ast::{Node, NodeType, NodeData, BinaryOpType, UnaryOpType, LiteralValue};
use crate::bytecode::{BytecodeProgram, Instruction, Constant, Compilable};
use std::collections::HashMap;
pub struct Compiler {
    locals: HashMap<String, u16>,
    local_count: u16,
    break_stack: Vec<u32>,
    continue_stack: Vec<u32>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            locals: HashMap::new(),
            local_count: 0,
            break_stack: Vec::new(),
            continue_stack: Vec::new(),
        }
    }
    
    pub fn compile(&mut self, ast: &Node) -> Result<BytecodeProgram, String> {
        let mut program = BytecodeProgram::new();
        self.compile_node(ast, &mut program)?;
        program.add_instruction(Instruction::Halt);
        Ok(program)
    }
    
    fn compile_node(&mut self, node: &Node, program: &mut BytecodeProgram) -> Result<(), String> {
        match &node.data {
            NodeData::Program { statements } => {
                for stmt in statements {
                    self.compile_node(stmt, program)?;
                }
            },
            
            NodeData::Literal { value, .. } => {
                match value {
                    LiteralValue::Int(i) => program.add_instruction(Instruction::LoadInt(*i)),
                    LiteralValue::Float(f) => program.add_instruction(Instruction::LoadFloat(*f)),
                    LiteralValue::String(s) => program.add_instruction(Instruction::LoadString(s.clone())),
                    LiteralValue::Char(c) => program.add_instruction(Instruction::LoadChar(*c)),
                    LiteralValue::Bool(b) => program.add_instruction(Instruction::LoadBool(*b)),
                }
            },
            
            NodeData::Identifier { name } => {
                if let Some(&slot) = self.locals.get(name) {
                    program.add_instruction(Instruction::LoadLocal(slot));
                } else {
                    program.add_instruction(Instruction::LoadGlobal(name.clone()));
                }
            },
            
            NodeData::VarDecl { name, init_expr, .. } => {
                if let Some(expr) = init_expr {
                    self.compile_node(expr, program)?;
                } else {
                    program.add_instruction(Instruction::LoadNull);
                }
                
                let slot = self.local_count;
                self.locals.insert(name.clone(), slot);
                self.local_count += 1;
                
                program.add_instruction(Instruction::StoreLocal(slot));
            },
            
            NodeData::Assignment { target, value, .. } => {
                self.compile_node(value, program)?;
                
                if let NodeData::Identifier { name } = &target.data {
                    if let Some(&slot) = self.locals.get(name) {
                        program.add_instruction(Instruction::StoreLocal(slot));
                    } else {
                        program.add_instruction(Instruction::StoreGlobal(name.clone()));
                    }
                } else {
                    return Err("Complex assignment targets not yet implemented".to_string());
                }
            },
            
            NodeData::BinaryOp { op, left, right } => {
                self.compile_node(left, program)?;
                self.compile_node(right, program)?;
                
                let instruction = match op {
                    BinaryOpType::Add => Instruction::Add,
                    BinaryOpType::Sub => Instruction::Sub,
                    BinaryOpType::Mul => Instruction::Mul,
                    BinaryOpType::Div => Instruction::Div,
                    BinaryOpType::Mod => Instruction::Mod,
                    BinaryOpType::Equal => Instruction::Equal,
                    BinaryOpType::NotEqual => Instruction::NotEqual,
                    BinaryOpType::Less => Instruction::Less,
                    BinaryOpType::Greater => Instruction::Greater,
                    BinaryOpType::LessEqual => Instruction::LessEqual,
                    BinaryOpType::GreaterEqual => Instruction::GreaterEqual,
                    BinaryOpType::LogicalAnd => Instruction::LogicalAnd,
                    BinaryOpType::LogicalOr => Instruction::LogicalOr,
                };
                program.add_instruction(instruction);
            },
            
            NodeData::UnaryOp { op, operand } => {
                match op {
                    UnaryOpType::Minus => {
                        self.compile_node(operand, program)?;
                        program.add_instruction(Instruction::Neg);
                    },
                    UnaryOpType::LogicalNot => {
                        self.compile_node(operand, program)?;
                        program.add_instruction(Instruction::LogicalNot);
                    },
                    UnaryOpType::PreIncrement => {
                        self.compile_node(operand, program)?;
                        program.add_instruction(Instruction::PreIncrement);
                    },
                    UnaryOpType::PostIncrement => {
                        self.compile_node(operand, program)?;
                        program.add_instruction(Instruction::PostIncrement);
                    },
                    UnaryOpType::PreDecrement => {
                        self.compile_node(operand, program)?;
                        program.add_instruction(Instruction::PreDecrement);
                    },
                    UnaryOpType::PostDecrement => {
                        self.compile_node(operand, program)?;
                        program.add_instruction(Instruction::PostDecrement);
                    },
                    _ => return Err(format!("Unary operator {:?} not yet implemented", op)),
                }
            },
            
            NodeData::IfStmt { condition, then_stmt, else_stmt } => {
                self.compile_node(condition, program)?;
                
                let else_jump = program.current_address();
                program.add_instruction(Instruction::JumpIfFalse(0)); // placeholder
                
                self.compile_node(then_stmt, program)?;
                
                if else_stmt.is_some() {
                    let end_jump = program.current_address();
                    program.add_instruction(Instruction::Jump(0)); // placeholder
                    
                    let else_addr = program.current_address();
                    program.instructions[else_jump as usize] = Instruction::JumpIfFalse(else_addr);
                    
                    self.compile_node(else_stmt.as_ref().unwrap(), program)?;
                    
                    let end_addr = program.current_address();
                    program.instructions[end_jump as usize] = Instruction::Jump(end_addr);
                } else {
                    let end_addr = program.current_address();
                    program.instructions[else_jump as usize] = Instruction::JumpIfFalse(end_addr);
                }
            },
            
            NodeData::WhileStmt { condition, body } => {
                let loop_start = program.current_address();
                self.continue_stack.push(loop_start);
                
                self.compile_node(condition, program)?;
                
                let exit_jump = program.current_address();
                program.add_instruction(Instruction::JumpIfFalse(0)); // placeholder
                
                self.compile_node(body, program)?;
                
                program.add_instruction(Instruction::Jump(loop_start));
                
                let exit_addr = program.current_address();
                program.instructions[exit_jump as usize] = Instruction::JumpIfFalse(exit_addr);
                
                if let Some(break_addr) = self.break_stack.pop() {
                }
                self.continue_stack.pop();
            },
            
            NodeData::PrintStmt { arguments } => {
                for arg in arguments {
                    self.compile_node(arg, program)?;
                    program.add_instruction(Instruction::Call("print".to_string(), 1));
                }
            },
            
            NodeData::FunctionCall { name, args } => {
                for arg in args {
                    self.compile_node(arg, program)?;
                }
                
                program.add_instruction(Instruction::Call(name.clone(), args.len() as u8));
            },
            
            NodeData::Block { statements } => {
                let saved_local_count = self.local_count;
                let saved_locals = self.locals.clone();
                
                for stmt in statements {
                    self.compile_node(stmt, program)?;
                }
                
                self.local_count = saved_local_count;
                self.locals = saved_locals;
            },
            
            NodeData::ExprStmt { expr } => {
                self.compile_node(expr, program)?;
                program.add_instruction(Instruction::Pop); // Remove resultado da expressão
            },
            
            NodeData::ReturnStmt { value } => {
                if let Some(val) = value {
                    self.compile_node(val, program)?;
                    program.add_instruction(Instruction::ReturnValue);
                } else {
                    program.add_instruction(Instruction::Return);
                }
            },
            
            NodeData::ImportStmt { import_type } => {
                match import_type {
                    crate::ast::ImportType::Module(module_name) => {
                        program.add_instruction(Instruction::ImportModule(module_name.clone()));
                    },
                    crate::ast::ImportType::Wildcard(module_name) => {
                        program.add_instruction(Instruction::ImportWildcard(module_name.clone()));
                    },
                    crate::ast::ImportType::Selective(module_name, symbols) => {
                        for symbol in symbols {
                            program.add_instruction(Instruction::ImportSymbol(module_name.clone(), symbol.clone()));
                        }
                    },
                    crate::ast::ImportType::Aliased(module_name, _alias) => {
                        program.add_instruction(Instruction::ImportModule(module_name.clone()));
                    },
                }
            },
            
            NodeData::ExportStmt { export_type } => {
                match export_type {
                    crate::ast::ExportType::Variable(name, data_type, init_expr) => {
                        if let Some(expr) = init_expr {
                            self.compile_node(expr, program)?;
                        } else {
                            program.add_instruction(Instruction::LoadNull);
                        }
                        
                        program.add_instruction(Instruction::StoreGlobal(name.clone()));
                        program.add_instruction(Instruction::ExportSymbol(name.clone()));
                    },
                    crate::ast::ExportType::List(symbols) => {
                        for symbol in symbols {
                            program.add_instruction(Instruction::ExportSymbol(symbol.clone()));
                        }
                    },
                    crate::ast::ExportType::Reexport(module_name) => {
                        program.add_instruction(Instruction::ImportWildcard(module_name.clone()));
                    },
                    crate::ast::ExportType::Function(_name, _func_node) => {
                        // TODO: Implementar export de função
                        return Err("Export de função não implementado ainda".to_string());
                    },
                }
            },
            
            NodeData::ModuleDecl { name, body } => {
                // TODO: Implementar declaração de módulo
                self.compile_node(body, program)?;
            },
            
            _ => {
                return Err(format!("Node type {:?} not yet implemented in compiler", node.node_type));
            },
        }
        
        Ok(())
    }
}

impl BytecodeProgram {
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
        
        output
    }
}