use crate::ast::{Node, NodeData, BinaryOpType, LiteralValue};
use crate::bytecode::{BytecodeProgram, Instruction};
use super::scope_manager::ScopeManager;
use super::symbol_table::SymbolTable;

pub struct CodeGenerator<'a> {
    scope_manager: &'a mut ScopeManager,
    symbol_table: &'a SymbolTable,
    current_function: Option<String>,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(scope_manager: &'a mut ScopeManager, symbol_table: &'a SymbolTable) -> Self {
        Self {
            scope_manager,
            symbol_table,
            current_function: None,
        }
    }

    pub fn set_current_function(&mut self, function_name: Option<String>) {
        self.current_function = function_name;
    }

    pub fn compile_node(&mut self, node: &Node, program: &mut BytecodeProgram) -> Result<(), String> {
        match &node.data {
            NodeData::Program { statements } => {
                self.compile_statements(statements, program)
            },

            NodeData::VarDecl { name, init_expr, .. } => {
                self.compile_variable_declaration(name, init_expr, program)
            },

            NodeData::Assignment { target, value, .. } => {
                self.compile_assignment(target, value, program)
            },

            NodeData::Identifier { name } => {
                self.compile_identifier(name, program)
            },

            NodeData::FunctionCall { name, args } => {
                self.compile_function_call(name, args, program)
            },

            NodeData::ReturnStmt { value } => {
                self.compile_return_statement(value, program)
            },

            NodeData::Block { statements } => {
                self.compile_block(statements, program)
            },

            NodeData::BinaryOp { op, left, right } => {
                self.compile_binary_operation(op, left, right, program)
            },

            NodeData::Literal { value, .. } => {
                self.compile_literal(value, program)
            },

            NodeData::ExprStmt { expr } => {
                self.compile_expression_statement(expr, program)
            },

            NodeData::StructLiteral { struct_name, field_values } => {
                self.compile_struct_literal(struct_name, field_values, program)
            },

            NodeData::MemberAccess { object, member_name } => {
                self.compile_member_access(object, member_name, program)
            },

            NodeData::IfStmt { condition, then_stmt, else_stmt } => {
                self.compile_if_statement(condition, then_stmt, else_stmt, program)
            },

            NodeData::PrintStmt { arguments } => {
                self.compile_print_statement(arguments, program)
            },

            NodeData::ImportStmt { .. } => {
                Ok(())
            },

            _ => {
                Err(format!("Node type {:?} not yet implemented in code generator", node.node_type))
            }
        }
    }

    fn compile_statements(&mut self, statements: &[Node], program: &mut BytecodeProgram) -> Result<(), String> {
        for stmt in statements {
            self.compile_node(stmt, program)?;
        }
        Ok(())
    }

    fn compile_variable_declaration(
        &mut self,
        name: &str,
        init_expr: &Option<Box<Node>>,
        program: &mut BytecodeProgram,
    ) -> Result<(), String> {
        if let Some(expr) = init_expr {
            self.compile_node(expr, program)?;
        } else {
            program.add_instruction(Instruction::LoadInt(0));
        }

        let slot = self.scope_manager.add_local(name.to_string());
        program.add_instruction(Instruction::StoreLocal(slot));

        Ok(())
    }

    fn compile_assignment(&mut self, target: &Node, value: &Node, program: &mut BytecodeProgram) -> Result<(), String> {
        match &target.data {
            NodeData::Identifier { name } => {
                self.compile_node(value, program)?;

                if let Some(slot) = self.scope_manager.get_local(name) {
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

        Ok(())
    }

    fn compile_identifier(&mut self, name: &str, program: &mut BytecodeProgram) -> Result<(), String> {
        if name == "this" {
            program.add_instruction(Instruction::LoadThis);
        } else if let Some(slot) = self.scope_manager.get_local(name) {
            program.add_instruction(Instruction::LoadLocal(slot));
        } else {
            program.add_instruction(Instruction::LoadGlobal(name.to_string()));
        }

        Ok(())
    }

    fn compile_function_call(&mut self, name: &str, args: &[Box<Node>], program: &mut BytecodeProgram) -> Result<(), String> {
        for arg in args {
            self.compile_node(arg, program)?;
        }

        if let Some(symbol) = self.symbol_table.lookup(name) {
            if symbol.is_function() {
                program.add_instruction(Instruction::CallFunction(name.to_string(), args.len() as u8));
                return Ok(());
            }
        }

        program.add_instruction(Instruction::Call(name.to_string(), args.len() as u8));
        Ok(())
    }

    fn compile_return_statement(&mut self, value: &Option<Box<Node>>, program: &mut BytecodeProgram) -> Result<(), String> {
        if let Some(return_value) = value {
            self.compile_node(return_value, program)?;
            if self.current_function.as_ref() == Some(&"main".to_string()) {
                program.add_instruction(Instruction::ReturnMain);
            } else {
                program.add_instruction(Instruction::ReturnValue);
            }
        } else {
            program.add_instruction(Instruction::Return);
        }

        Ok(())
    }

    fn compile_block(&mut self, statements: &[Node], program: &mut BytecodeProgram) -> Result<(), String> {
        self.scope_manager.enter_scope();
        let result = self.compile_statements(statements, program);
        self.scope_manager.exit_scope();
        result
    }

    fn compile_binary_operation(
        &mut self,
        op: &BinaryOpType,
        left: &Node,
        right: &Node,
        program: &mut BytecodeProgram,
    ) -> Result<(), String> {
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
        Ok(())
    }

    fn compile_literal(&mut self, value: &LiteralValue, program: &mut BytecodeProgram) -> Result<(), String> {
        let instruction = match value {
            LiteralValue::Int(i) => Instruction::LoadInt(*i),
            LiteralValue::Float(f) => Instruction::LoadFloat(*f),
            LiteralValue::String(s) => Instruction::LoadString(s.clone()),
            LiteralValue::Char(c) => Instruction::LoadChar(*c),
            LiteralValue::Bool(b) => Instruction::LoadBool(*b),
        };

        program.add_instruction(instruction);
        Ok(())
    }

    fn compile_expression_statement(&mut self, expr: &Node, program: &mut BytecodeProgram) -> Result<(), String> {
        self.compile_node(expr, program)?;
        program.add_instruction(Instruction::Pop);
        Ok(())
    }

    fn compile_struct_literal(
        &mut self,
        struct_name: &str,
        field_values: &[crate::ast::FieldValue],
        program: &mut BytecodeProgram,
    ) -> Result<(), String> {
        program.add_instruction(Instruction::NewObject(struct_name.to_string()));

        if !field_values.is_empty() {
            program.add_instruction(Instruction::Dup);

            for field_value in field_values {
                self.compile_node(&field_value.value, program)?;
            }

            program.add_instruction(Instruction::CallConstructor(
                struct_name.to_string(),
                field_values.len() as u8,
            ));

            program.add_instruction(Instruction::Pop);
        }

        Ok(())
    }

    fn compile_member_access(&mut self, object: &Node, member_name: &str, program: &mut BytecodeProgram) -> Result<(), String> {
        self.compile_node(object, program)?;
        program.add_instruction(Instruction::GetObjectField(member_name.to_string()));
        Ok(())
    }

    fn compile_if_statement(
        &mut self,
        condition: &Node,
        then_stmt: &Node,
        else_stmt: &Option<Box<Node>>,
        program: &mut BytecodeProgram,
    ) -> Result<(), String> {
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

        Ok(())
    }

    fn compile_print_statement(&mut self, arguments: &[Box<Node>], program: &mut BytecodeProgram) -> Result<(), String> {
        for arg in arguments {
            self.compile_node(arg, program)?;
            program.add_instruction(Instruction::Call("print".to_string(), 1));
            program.add_instruction(Instruction::Pop);
        }

        Ok(())
    }
}