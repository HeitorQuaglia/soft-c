use crate::ast::{Node, NodeData};
use super::symbol_table::{Symbol, SymbolTable};

pub struct SymbolCollector {
    symbol_table: SymbolTable,
    scope_depth: usize,
}

impl SymbolCollector {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            scope_depth: 0,
        }
    }

    pub fn collect_symbols(&mut self, ast: &Node) -> Result<SymbolTable, String> {
        self.visit_node(ast)?;
        Ok(std::mem::replace(&mut self.symbol_table, SymbolTable::new()))
    }

    fn visit_node(&mut self, node: &Node) -> Result<(), String> {
        match &node.data {
            NodeData::Program { statements } => {
                self.visit_statements(statements)
            },

            NodeData::ModuleDecl { name, body } => {
                self.visit_module_declaration(name, body)
            },

            NodeData::FunctionDef { name, return_type, parameters, body } => {
                self.visit_function_definition(name, return_type, parameters, body)
            },

            NodeData::ClassDef { name, fields, constructor } => {
                self.visit_class_definition(name, fields, constructor)
            },

            NodeData::VarDecl { name, data_type, init_expr, .. } => {
                self.visit_variable_declaration(name, data_type, init_expr)
            },

            NodeData::Block { statements } => {
                self.visit_block(statements)
            },

            NodeData::IfStmt { condition, then_stmt, else_stmt } => {
                self.visit_if_statement(condition, then_stmt, else_stmt)
            },

            NodeData::WhileStmt { condition, body } => {
                self.visit_while_statement(condition, body)
            },

            NodeData::ForStmt { init, condition, update, body } => {
                self.visit_for_statement(init, condition, update, body)
            },

            NodeData::Assignment { target, value, .. } => {
                self.visit_assignment(target, value)
            },

            NodeData::BinaryOp { left, right, .. } => {
                self.visit_binary_operation(left, right)
            },

            NodeData::UnaryOp { operand, .. } => {
                self.visit_unary_operation(operand)
            },

            NodeData::FunctionCall { args, .. } => {
                self.visit_function_call(args)
            },

            NodeData::StructLiteral { field_values, .. } => {
                self.visit_struct_literal(field_values)
            },

            NodeData::MemberAccess { object, .. } => {
                self.visit_member_access(object)
            },

            NodeData::ExprStmt { expr } => {
                self.visit_expression_statement(expr)
            },

            NodeData::PrintStmt { arguments } => {
                self.visit_print_statement(arguments)
            },

            NodeData::ReturnStmt { value } => {
                self.visit_return_statement(value)
            },

            NodeData::Literal { .. } |
            NodeData::Identifier { .. } |
            NodeData::BreakStmt |
            NodeData::ContinueStmt => {
                Ok(())
            },

            // Import/Export - TODO: Implement proper module resolution
            NodeData::ImportStmt { .. } |
            NodeData::ExportStmt { .. } => {
                Ok(())
            },

            _ => {
                self.visit_default_node(node)
            },
        }
    }

    fn visit_statements(&mut self, statements: &[Node]) -> Result<(), String> {
        for stmt in statements {
            self.visit_node(stmt)?;
        }
        Ok(())
    }

    fn visit_module_declaration(&mut self, name: &str, body: &Node) -> Result<(), String> {
        self.symbol_table.enter_module(name.to_string());
        self.visit_node(body)?;
        self.symbol_table.exit_module();
        Ok(())
    }

    fn visit_function_definition(
        &mut self,
        name: &str,
        return_type: &crate::ast::DataType,
        parameters: &[crate::ast::Parameter],
        body: &Node,
    ) -> Result<(), String> {
        let symbol = Symbol::function(
            name.to_string(),
            return_type.clone(),
            parameters.to_vec(),
            body.clone().into(),
            self.symbol_table.current_module.clone(),
        );
        self.symbol_table.add_symbol(symbol)?;
        Ok(())
    }

    fn visit_class_definition(
        &mut self,
        name: &str,
        fields: &[crate::ast::Parameter],
        constructor: &Option<Box<Node>>,
    ) -> Result<(), String> {
        let symbol = Symbol::class(
            name.to_string(),
            fields.to_vec(),
            constructor.clone(),
            self.symbol_table.current_module.clone(),
        );
        self.symbol_table.add_symbol(symbol)?;

        if let Some(constructor_node) = constructor {
            if let NodeData::Constructor { parameters, body } = &constructor_node.data {
                let constructor_name = format!("{}::constructor", name);
                let constructor_symbol = Symbol::constructor(
                    constructor_name,
                    name.to_string(),
                    parameters.clone(),
                    body.clone(),
                    self.symbol_table.current_module.clone(),
                );
                self.symbol_table.add_symbol(constructor_symbol)?;
            }
        }

        Ok(())
    }

    fn visit_variable_declaration(
        &mut self,
        name: &str,
        data_type: &crate::ast::DataType,
        init_expr: &Option<Box<Node>>,
    ) -> Result<(), String> {
        if self.is_global_scope() {
            let symbol = Symbol::variable(
                name.to_string(),
                data_type.clone(),
                true,
                self.symbol_table.current_module.clone(),
            );
            self.symbol_table.add_symbol(symbol)?;
        }

        if let Some(expr) = init_expr {
            self.visit_node(expr)?;
        }

        Ok(())
    }

    fn visit_block(&mut self, statements: &[Node]) -> Result<(), String> {
        self.enter_scope();
        let result = self.visit_statements(statements);
        self.exit_scope();
        result
    }

    fn visit_if_statement(
        &mut self,
        condition: &Node,
        then_stmt: &Node,
        else_stmt: &Option<Box<Node>>,
    ) -> Result<(), String> {
        self.visit_node(condition)?;
        self.visit_node(then_stmt)?;
        if let Some(else_node) = else_stmt {
            self.visit_node(else_node)?;
        }
        Ok(())
    }

    fn visit_while_statement(&mut self, condition: &Node, body: &Node) -> Result<(), String> {
        self.visit_node(condition)?;
        self.visit_node(body)?;
        Ok(())
    }

    fn visit_for_statement(
        &mut self,
        init: &Option<Box<Node>>,
        condition: &Option<Box<Node>>,
        update: &Option<Box<Node>>,
        body: &Node,
    ) -> Result<(), String> {
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
        Ok(())
    }

    fn visit_assignment(&mut self, target: &Node, value: &Node) -> Result<(), String> {
        self.visit_node(target)?;
        self.visit_node(value)?;
        Ok(())
    }

    fn visit_binary_operation(&mut self, left: &Node, right: &Node) -> Result<(), String> {
        self.visit_node(left)?;
        self.visit_node(right)?;
        Ok(())
    }

    fn visit_unary_operation(&mut self, operand: &Node) -> Result<(), String> {
        self.visit_node(operand)
    }

    fn visit_function_call(&mut self, args: &[Box<Node>]) -> Result<(), String> {
        for arg in args {
            self.visit_node(arg)?;
        }
        Ok(())
    }

    fn visit_struct_literal(&mut self, field_values: &[crate::ast::FieldValue]) -> Result<(), String> {
        for field_value in field_values {
            self.visit_node(&field_value.value)?;
        }
        Ok(())
    }

    fn visit_member_access(&mut self, object: &Node) -> Result<(), String> {
        self.visit_node(object)
    }

    fn visit_expression_statement(&mut self, expr: &Node) -> Result<(), String> {
        self.visit_node(expr)
    }

    fn visit_print_statement(&mut self, arguments: &[Box<Node>]) -> Result<(), String> {
        for arg in arguments {
            self.visit_node(arg)?;
        }
        Ok(())
    }

    fn visit_return_statement(&mut self, value: &Option<Box<Node>>) -> Result<(), String> {
        if let Some(return_value) = value {
            self.visit_node(return_value)?;
        }
        Ok(())
    }

    fn visit_default_node(&mut self, _node: &Node) -> Result<(), String> {
        Ok(())
    }

    fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn exit_scope(&mut self) {
        if self.scope_depth > 0 {
            self.scope_depth -= 1;
        }
    }

    fn is_global_scope(&self) -> bool {
        self.scope_depth == 0
    }

    pub fn scope_depth(&self) -> usize {
        self.scope_depth
    }
}

impl Default for SymbolCollector {
    fn default() -> Self {
        Self::new()
    }
}