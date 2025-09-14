use crate::ast::{Node, NodeData};
use crate::symbol_table::{Symbol, SymbolTable, SymbolType};

pub struct SymbolCollector {
    symbol_table: SymbolTable,
}

impl SymbolCollector {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn collect_symbols(&mut self, ast: &Node) -> Result<SymbolTable, String> {
        self.visit_node(ast)?;
        Ok(std::mem::replace(&mut self.symbol_table, SymbolTable::new()))
    }

    fn visit_node(&mut self, node: &Node) -> Result<(), String> {
        match &node.data {
            NodeData::Program { statements } => {
                for stmt in statements {
                    self.visit_node(stmt)?;
                }
            },

            NodeData::ModuleDecl { name, body } => {
                self.symbol_table.enter_module(name.clone());
                self.visit_node(body)?;
                self.symbol_table.exit_module();
            },

            NodeData::FunctionDef { name, return_type, parameters, body } => {
                let symbol = Symbol {
                    name: name.clone(),
                    symbol_type: SymbolType::Function {
                        return_type: return_type.clone(),
                        parameters: parameters.clone(),
                        body: body.clone(),
                        start_address: None,
                    },
                    module_name: self.symbol_table.current_module.clone(),
                    is_public: true, // TODO: Implement visibility modifiers
                };
                self.symbol_table.add_symbol(symbol)?;
            },

            NodeData::ClassDef { name, fields, constructor } => {
                let symbol = Symbol {
                    name: name.clone(),
                    symbol_type: SymbolType::Class {
                        fields: fields.clone(),
                        constructor: constructor.clone(),
                        methods: std::collections::HashMap::new(), // TODO: Add methods
                    },
                    module_name: self.symbol_table.current_module.clone(),
                    is_public: true,
                };
                self.symbol_table.add_symbol(symbol)?;

                if let Some(constructor_node) = constructor {
                    if let NodeData::Constructor { parameters, body } = &constructor_node.data {
                        let constructor_name = format!("{}::constructor", name);
                        let constructor_symbol = Symbol {
                            name: constructor_name,
                            symbol_type: SymbolType::Constructor {
                                class_name: name.clone(),
                                parameters: parameters.clone(),
                                body: body.clone(),
                                start_address: None,
                            },
                            module_name: self.symbol_table.current_module.clone(),
                            is_public: true,
                        };
                        self.symbol_table.add_symbol(constructor_symbol)?;
                    }
                }
            },

            NodeData::VarDecl { name, data_type, init_expr, .. } => {
                if self.is_global_scope() {
                    let symbol = Symbol {
                        name: name.clone(),
                        symbol_type: SymbolType::Variable {
                            data_type: data_type.clone(),
                            is_global: true,
                            address: None,
                        },
                        module_name: self.symbol_table.current_module.clone(),
                        is_public: true,
                    };
                    self.symbol_table.add_symbol(symbol)?;
                }

                if let Some(expr) = init_expr {
                    self.visit_node(expr)?;
                }
            },

            NodeData::Block { statements } => {
                for stmt in statements {
                    self.visit_node(stmt)?;
                }
            },

            NodeData::IfStmt { condition, then_stmt, else_stmt } => {
                self.visit_node(condition)?;
                self.visit_node(then_stmt)?;
                if let Some(else_node) = else_stmt {
                    self.visit_node(else_node)?;
                }
            },

            NodeData::WhileStmt { condition, body } => {
                self.visit_node(condition)?;
                self.visit_node(body)?;
            },

            NodeData::ForStmt { init, condition, update, body } => {
                if let Some(init_node) = init {
                    self.visit_node(init_node)?;
                }
                if let Some(cond_node) = condition {
                    self.visit_node(cond_node)?;
                }
                if let Some(update_node) = update {
                    self.visit_node(update_node)?;
                }
                self.visit_node(body)?;
            },

            NodeData::Assignment { target, value, .. } => {
                self.visit_node(target)?;
                self.visit_node(value)?;
            },

            NodeData::BinaryOp { left, right, .. } => {
                self.visit_node(left)?;
                self.visit_node(right)?;
            },

            NodeData::UnaryOp { operand, .. } => {
                self.visit_node(operand)?;
            },

            NodeData::FunctionCall { args, .. } => {
                for arg in args {
                    self.visit_node(arg)?;
                }
            },

            NodeData::StructLiteral { field_values, .. } => {
                for field_value in field_values {
                    self.visit_node(&field_value.value)?;
                }
            },

            NodeData::MemberAccess { object, .. } => {
                self.visit_node(object)?;
            },

            NodeData::ExprStmt { expr } => {
                self.visit_node(expr)?;
            },

            NodeData::PrintStmt { arguments } => {
                for arg in arguments {
                    self.visit_node(arg)?;
                }
            },

            NodeData::ReturnStmt { value } => {
                if let Some(return_value) = value {
                    self.visit_node(return_value)?;
                }
            },

            NodeData::Literal { .. } |
            NodeData::Identifier { .. } |
            NodeData::BreakStmt |
            NodeData::ContinueStmt => {
            },

            NodeData::ImportStmt { .. } |
            NodeData::ExportStmt { .. } => {
                // TODO: Handle imports/exports for symbol resolution
            },

            // TODO: Handle other node types as needed
            _ => {
            },
        }

        Ok(())
    }

    fn is_global_scope(&self) -> bool {
        // TODO: Implement proper scope tracking
        true
    }
}