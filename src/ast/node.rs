use super::{NodeType, NodeData};

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub node_type: NodeType,
    pub data: NodeData,
    pub line: u32,
    pub column: u32,
}

impl Node {
    pub fn new(node_type: NodeType, data: NodeData, line: u32, column: u32) -> Self {
        Node {
            node_type,
            data,
            line,
            column,
        }
    }
    pub fn position(&self) -> (u32, u32) {
        (self.line, self.column)
    }
    pub fn is_statement(&self) -> bool {
        matches!(
            self.node_type,
            NodeType::VarDecl
            | NodeType::Assignment
            | NodeType::IfStmt
            | NodeType::WhileStmt
            | NodeType::ForStmt
            | NodeType::DoWhileStmt
            | NodeType::SwitchStmt
            | NodeType::BreakStmt
            | NodeType::ContinueStmt
            | NodeType::ReturnStmt
            | NodeType::PrintStmt
            | NodeType::ExprStmt
            | NodeType::Block
        )
    }
    pub fn is_expression(&self) -> bool {
        matches!(
            self.node_type,
            NodeType::BinaryOp
            | NodeType::UnaryOp
            | NodeType::TernaryOp
            | NodeType::FunctionCall
            | NodeType::Cast
            | NodeType::Literal
            | NodeType::Identifier
            | NodeType::ArrayLiteral
            | NodeType::ArrayAccess
            | NodeType::MemberAccess
            | NodeType::StructLiteral
        )
    }
    pub fn is_declaration(&self) -> bool {
        matches!(
            self.node_type,
            NodeType::FunctionDef
            | NodeType::VarDecl
            | NodeType::StructDef
            | NodeType::ClassDef
            | NodeType::Constructor
        )
    }
}