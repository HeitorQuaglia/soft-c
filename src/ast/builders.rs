use super::{Node, NodeType, NodeData, DataType, LiteralValue, BinaryOpType, UnaryOpType};

impl Node {
    pub fn create_literal(data_type: DataType, value: LiteralValue, line: u32, column: u32) -> Self {
        Node::new(
            NodeType::Literal,
            NodeData::Literal { data_type, value },
            line,
            column,
        )
    }

    pub fn create_identifier(name: String, line: u32, column: u32) -> Self {
        Node::new(
            NodeType::Identifier,
            NodeData::Identifier { name },
            line,
            column,
        )
    }

    pub fn create_binary_op(
        op: BinaryOpType,
        left: Box<Node>,
        right: Box<Node>,
        line: u32,
        column: u32,
    ) -> Self {
        Node::new(
            NodeType::BinaryOp,
            NodeData::BinaryOp { op, left, right },
            line,
            column,
        )
    }

    pub fn create_unary_op(op: UnaryOpType, operand: Box<Node>, line: u32, column: u32) -> Self {
        Node::new(
            NodeType::UnaryOp,
            NodeData::UnaryOp { op, operand },
            line,
            column,
        )
    }

    pub fn create_program(statements: Vec<Node>) -> Self {
        Node::new(
            NodeType::Program,
            NodeData::Program { statements },
            1,
            1,
        )
    }

    pub fn create_block(statements: Vec<Node>, line: u32, column: u32) -> Self {
        Node::new(
            NodeType::Block,
            NodeData::Block { statements },
            line,
            column,
        )
    }

    pub fn create_function_call(name: String, args: Vec<Box<Node>>, line: u32, column: u32) -> Self {
        Node::new(
            NodeType::FunctionCall,
            NodeData::FunctionCall { name, args },
            line,
            column,
        )
    }

    pub fn create_int_literal(value: i64, line: u32, column: u32) -> Self {
        Self::create_literal(
            DataType::Int,
            LiteralValue::Int(value),
            line,
            column,
        )
    }

    pub fn create_float_literal(value: f64, line: u32, column: u32) -> Self {
        Self::create_literal(
            DataType::Float,
            LiteralValue::Float(value),
            line,
            column,
        )
    }

    pub fn create_string_literal(value: String, line: u32, column: u32) -> Self {
        Self::create_literal(
            DataType::String,
            LiteralValue::String(value),
            line,
            column,
        )
    }

    pub fn create_char_literal(value: char, line: u32, column: u32) -> Self {
        Self::create_literal(
            DataType::Char,
            LiteralValue::Char(value),
            line,
            column,
        )
    }

    pub fn create_bool_literal(value: bool, line: u32, column: u32) -> Self {
        Self::create_literal(
            DataType::Bool,
            LiteralValue::Bool(value),
            line,
            column,
        )
    }
}