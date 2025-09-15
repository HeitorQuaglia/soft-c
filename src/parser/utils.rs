use crate::ast::{AssignOpType, BinaryOpType, UnaryOpType, Node, NodeType};
use crate::tokenizer::TokenType;

pub struct ParserUtils;

impl ParserUtils {
    pub fn map_assign_op(token_type: TokenType) -> AssignOpType {
        match token_type {
            TokenType::Assign => AssignOpType::Assign,
            TokenType::PlusAssign => AssignOpType::AddAssign,
            TokenType::MinusAssign => AssignOpType::SubAssign,
            TokenType::MultiplyAssign => AssignOpType::MulAssign,
            TokenType::DivideAssign => AssignOpType::DivAssign,
            _ => unreachable!("Invalid assignment operator token"),
        }
    }

    pub fn map_binary_op(token_type: TokenType) -> BinaryOpType {
        match token_type {
            TokenType::Plus => BinaryOpType::Add,
            TokenType::Minus => BinaryOpType::Sub,
            TokenType::Multiply => BinaryOpType::Mul,
            TokenType::Divide => BinaryOpType::Div,
            TokenType::Modulo => BinaryOpType::Mod,
            TokenType::Equal => BinaryOpType::Equal,
            TokenType::NotEqual => BinaryOpType::NotEqual,
            TokenType::Less => BinaryOpType::Less,
            TokenType::Greater => BinaryOpType::Greater,
            TokenType::LessEqual => BinaryOpType::LessEqual,
            TokenType::GreaterEqual => BinaryOpType::GreaterEqual,
            TokenType::LogicalAnd => BinaryOpType::LogicalAnd,
            TokenType::LogicalOr => BinaryOpType::LogicalOr,
            _ => unreachable!("Invalid binary operator token"),
        }
    }

    pub fn map_unary_op(token_type: TokenType) -> UnaryOpType {
        match token_type {
            TokenType::LogicalNot => UnaryOpType::LogicalNot,
            TokenType::Minus => UnaryOpType::Minus,
            TokenType::Plus => UnaryOpType::Plus,
            _ => unreachable!("Invalid unary operator token"),
        }
    }

    pub fn is_lvalue(node: &Node) -> bool {
        matches!(
            node.node_type,
            NodeType::Identifier | NodeType::MemberAccess | NodeType::ArrayAccess
        )
    }

    pub fn is_primitive_type(token_type: &TokenType) -> bool {
        matches!(
            token_type,
            TokenType::KwInt
                | TokenType::KwFloat
                | TokenType::KwDouble
                | TokenType::KwChar
                | TokenType::KwString
                | TokenType::KwBool
        )
    }

    pub fn is_assignment_op(token_type: &TokenType) -> bool {
        matches!(
            token_type,
            TokenType::Assign
                | TokenType::PlusAssign
                | TokenType::MinusAssign
                | TokenType::MultiplyAssign
                | TokenType::DivideAssign
        )
    }

    pub fn get_operator_precedence(token_type: &TokenType) -> u8 {
        match token_type {
            TokenType::LogicalOr => 1,
            TokenType::LogicalAnd => 2,
            TokenType::Equal | TokenType::NotEqual => 3,
            TokenType::Less | TokenType::Greater
            | TokenType::LessEqual | TokenType::GreaterEqual => 4,
            TokenType::Plus | TokenType::Minus => 5,
            TokenType::Multiply | TokenType::Divide | TokenType::Modulo => 6,
            _ => 0,
        }
    }
}