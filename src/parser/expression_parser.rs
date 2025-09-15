use crate::ast::*;
use crate::tokenizer::{Token, TokenType};
use super::{ParseError, ParseResult, utils::ParserUtils, type_parser::TypeParser, token_utils::TokenUtils};

pub struct ExpressionParser;

impl ExpressionParser {
    pub fn parse_expression(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        Self::parse_assignment(tokens, current)
    }

    pub fn parse_assignment(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let left = Self::parse_ternary(tokens, current)?;

        if TokenUtils::is_assignment_token(tokens, *current) {
            let op_tok = TokenUtils::advance(tokens, current).clone();
            let rhs = Self::parse_assignment(tokens, current)?;

            if !ParserUtils::is_lvalue(&left) {
                return Err(ParseError::UnexpectedToken {
                    found: op_tok.token_type,
                    expected: Some("assignable left-hand side".to_string()),
                    line: op_tok.line,
                });
            }

            return Ok(Node::new(
                NodeType::Assignment,
                NodeData::Assignment {
                    target: Box::new(left),
                    op: ParserUtils::map_assign_op(op_tok.token_type),
                    value: Box::new(rhs),
                },
                op_tok.line,
                op_tok.column,
            ));
        }

        Ok(left)
    }

    pub fn parse_ternary(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let mut expr = Self::parse_logical_or(tokens, current)?;

        if TokenUtils::match_token(tokens, current, &[TokenType::Question]) {
            let line = expr.line;
            let column = expr.column;
            let true_expr = Self::parse_expression(tokens, current)?;
            TokenUtils::consume(tokens, current, TokenType::Colon, "Expected ':' after ternary true expression")?;
            let false_expr = Self::parse_expression(tokens, current)?;

            expr = Node::new(
                NodeType::TernaryOp,
                NodeData::TernaryOp {
                    condition: Box::new(expr),
                    true_expr: Box::new(true_expr),
                    false_expr: Box::new(false_expr),
                },
                line,
                column,
            );
        }

        Ok(expr)
    }

    pub fn parse_logical_or(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let mut expr = Self::parse_logical_and(tokens, current)?;

        while TokenUtils::match_token(tokens, current, &[TokenType::LogicalOr]) {
            let op = BinaryOpType::LogicalOr;
            let right = Self::parse_logical_and(tokens, current)?;
            let line = expr.line;
            let column = expr.column;
            expr = Node::create_binary_op(op, Box::new(expr), Box::new(right), line, column);
        }

        Ok(expr)
    }

    pub fn parse_logical_and(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let mut expr = Self::parse_equality(tokens, current)?;

        while TokenUtils::match_token(tokens, current, &[TokenType::LogicalAnd]) {
            let op = BinaryOpType::LogicalAnd;
            let right = Self::parse_equality(tokens, current)?;
            let line = expr.line;
            let column = expr.column;
            expr = Node::create_binary_op(op, Box::new(expr), Box::new(right), line, column);
        }

        Ok(expr)
    }

    pub fn parse_equality(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let mut expr = Self::parse_comparison(tokens, current)?;

        while TokenUtils::match_token(tokens, current, &[TokenType::Equal, TokenType::NotEqual]) {
            let op_token = TokenUtils::previous(tokens, *current);
            let op = ParserUtils::map_binary_op(op_token.token_type);
            let right = Self::parse_comparison(tokens, current)?;
            let line = expr.line;
            let column = expr.column;
            expr = Node::create_binary_op(op, Box::new(expr), Box::new(right), line, column);
        }

        Ok(expr)
    }

    pub fn parse_comparison(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let mut expr = Self::parse_term(tokens, current)?;

        while TokenUtils::match_token(tokens, current, &[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let op_token = TokenUtils::previous(tokens, *current);
            let op = ParserUtils::map_binary_op(op_token.token_type);
            let right = Self::parse_term(tokens, current)?;
            let line = expr.line;
            let column = expr.column;
            expr = Node::create_binary_op(op, Box::new(expr), Box::new(right), line, column);
        }

        Ok(expr)
    }

    pub fn parse_term(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let mut expr = Self::parse_factor(tokens, current)?;

        while TokenUtils::match_token(tokens, current, &[TokenType::Minus, TokenType::Plus]) {
            let op_token = TokenUtils::previous(tokens, *current);
            let op = ParserUtils::map_binary_op(op_token.token_type);
            let right = Self::parse_factor(tokens, current)?;
            let line = expr.line;
            let column = expr.column;
            expr = Node::create_binary_op(op, Box::new(expr), Box::new(right), line, column);
        }

        Ok(expr)
    }

    pub fn parse_factor(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let mut expr = Self::parse_unary(tokens, current)?;

        while TokenUtils::match_token(tokens, current, &[TokenType::Divide, TokenType::Multiply, TokenType::Modulo]) {
            let op_token = TokenUtils::previous(tokens, *current);
            let op = ParserUtils::map_binary_op(op_token.token_type);
            let right = Self::parse_unary(tokens, current)?;
            let line = expr.line;
            let column = expr.column;
            expr = Node::create_binary_op(op, Box::new(expr), Box::new(right), line, column);
        }

        Ok(expr)
    }

    pub fn parse_unary(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        if TokenUtils::match_token(tokens, current, &[TokenType::LogicalNot, TokenType::Minus, TokenType::Plus]) {
            let prev = TokenUtils::previous(tokens, *current).clone();
            let op = ParserUtils::map_unary_op(prev.token_type);
            let operand = Self::parse_unary(tokens, current)?;
            return Ok(Node::create_unary_op(
                op,
                Box::new(operand),
                prev.line,
                prev.column,
            ));
        }

        Self::parse_postfix(tokens, current)
    }

    pub fn parse_postfix(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let mut expr = Self::parse_primary(tokens, current)?;

        loop {
            if TokenUtils::match_token(tokens, current, &[TokenType::Increment]) {
                let prev = TokenUtils::previous(tokens, *current);
                expr = Node::create_unary_op(
                    UnaryOpType::PostIncrement,
                    Box::new(expr),
                    prev.line,
                    prev.column,
                );
            } else if TokenUtils::match_token(tokens, current, &[TokenType::Decrement]) {
                let prev = TokenUtils::previous(tokens, *current);
                expr = Node::create_unary_op(
                    UnaryOpType::PostDecrement,
                    Box::new(expr),
                    prev.line,
                    prev.column,
                );
            } else if TokenUtils::match_token(tokens, current, &[TokenType::LeftBracket]) {
                expr = Self::parse_array_access(tokens, current, expr)?;
            } else if TokenUtils::match_token(tokens, current, &[TokenType::Dot]) {
                expr = Self::parse_member_access(tokens, current, expr)?;
            } else if TokenUtils::match_token(tokens, current, &[TokenType::LeftParen]) {
                expr = Self::parse_function_call(tokens, current, expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_array_access(
        tokens: &[Token],
        current: &mut usize,
        array_expr: Node,
    ) -> ParseResult<Node> {
        let mut indices = Vec::new();
        indices.push(Box::new(Self::parse_expression(tokens, current)?));
        TokenUtils::consume(tokens, current, TokenType::RightBracket, "Expected ']'")?;

        while TokenUtils::match_token(tokens, current, &[TokenType::LeftBracket]) {
            indices.push(Box::new(Self::parse_expression(tokens, current)?));
            TokenUtils::consume(tokens, current, TokenType::RightBracket, "Expected ']'")?;
        }

        let line = array_expr.line;
        let column = array_expr.column;
        Ok(Node::new(
            NodeType::ArrayAccess,
            NodeData::ArrayAccess {
                array: Box::new(array_expr),
                indices,
            },
            line,
            column,
        ))
    }

    fn parse_member_access(
        tokens: &[Token],
        current: &mut usize,
        object_expr: Node,
    ) -> ParseResult<Node> {
        TokenUtils::consume(tokens, current, TokenType::Identifier, "Expected member name after '.'")?;
        let member_name = TokenUtils::previous(tokens, *current).lexeme.clone();

        let line = object_expr.line;
        let column = object_expr.column;
        Ok(Node::new(
            NodeType::MemberAccess,
            NodeData::MemberAccess {
                object: Box::new(object_expr),
                member_name,
            },
            line,
            column,
        ))
    }

    fn parse_function_call(
        tokens: &[Token],
        current: &mut usize,
        func_expr: Node,
    ) -> ParseResult<Node> {
        let mut args = Vec::new();
        if !TokenUtils::check_token(tokens, *current, TokenType::RightParen) {
            loop {
                args.push(Box::new(Self::parse_expression(tokens, current)?));
                if !TokenUtils::match_token(tokens, current, &[TokenType::Comma]) {
                    break;
                }
            }
        }
        TokenUtils::consume(tokens, current, TokenType::RightParen, "Expected ')' after arguments")?;

        if let NodeData::Identifier { name } = &func_expr.data {
            let line = func_expr.line;
            let column = func_expr.column;
            Ok(Node::new(
                NodeType::FunctionCall,
                NodeData::FunctionCall {
                    name: name.clone(),
                    args,
                },
                line,
                column,
            ))
        } else {
            Ok(func_expr)
        }
    }

    pub fn parse_primary(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        if TokenUtils::match_token(tokens, current, &[TokenType::KwNew]) {
            return Self::parse_constructor_call(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::KwTrue]) {
            let prev = TokenUtils::previous(tokens, *current);
            return Ok(Node::create_literal(
                DataType::Bool,
                LiteralValue::Bool(true),
                prev.line,
                prev.column,
            ));
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::KwFalse]) {
            let prev = TokenUtils::previous(tokens, *current);
            return Ok(Node::create_literal(
                DataType::Bool,
                LiteralValue::Bool(false),
                prev.line,
                prev.column,
            ));
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::Int]) {
            return Self::parse_int_literal(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::Float]) {
            return Self::parse_float_literal(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::String]) {
            return Self::parse_string_literal(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::Char]) {
            return Self::parse_char_literal(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::Identifier]) {
            let prev = TokenUtils::previous(tokens, *current);
            return Ok(Node::create_identifier(
                prev.lexeme.clone(),
                prev.line,
                prev.column,
            ));
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::LeftParen]) {
            return Self::parse_parenthesized_or_cast(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::LeftBracket]) {
            return Self::parse_array_literal(tokens, current);
        }

        Err(ParseError::UnexpectedToken {
            found: TokenUtils::peek(tokens, *current).token_type,
            expected: Some("primary expression".to_string()),
            line: TokenUtils::peek(tokens, *current).line,
        })
    }

    fn parse_constructor_call(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current).clone();

        TokenUtils::consume(tokens, current, TokenType::Identifier, "Expected class name after 'new'")?;
        let class_name = TokenUtils::previous(tokens, *current).lexeme.clone();

        TokenUtils::consume(tokens, current, TokenType::LeftParen, "Expected '(' after class name")?;

        TokenUtils::skip_newlines_and_indents(tokens, current);

        let mut field_values = Vec::new();

        if !TokenUtils::check_token(tokens, *current, TokenType::RightParen) {
            loop {
                TokenUtils::skip_newlines_and_indents(tokens, current);

                if TokenUtils::check_token(tokens, *current, TokenType::RightParen) {
                    break;
                }

                let value = Box::new(Self::parse_assignment(tokens, current)?);

                field_values.push(FieldValue::new(String::new(), value));

                let has_comma = TokenUtils::match_token(tokens, current, &[TokenType::Comma]);
                if has_comma {
                    TokenUtils::skip_newlines_and_indents(tokens, current);
                }

                if !has_comma {
                    break;
                }
            }
        }

        TokenUtils::skip_newlines_and_indents(tokens, current);
        TokenUtils::consume(tokens, current, TokenType::RightParen, "Expected ')' after class fields")?;

        Ok(Node::new(
            NodeType::StructLiteral,
            NodeData::StructLiteral {
                struct_name: class_name,
                field_values,
            },
            prev.line,
            prev.column,
        ))
    }

    fn parse_int_literal(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current);
        let value = prev
            .lexeme
            .parse::<i64>()
            .map_err(|_| ParseError::UnexpectedToken {
                found: prev.token_type,
                expected: Some("valid integer".to_string()),
                line: prev.line,
            })?;
        Ok(Node::create_literal(
            DataType::Int,
            LiteralValue::Int(value),
            prev.line,
            prev.column,
        ))
    }

    fn parse_float_literal(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current);
        let value = prev
            .lexeme
            .parse::<f64>()
            .map_err(|_| ParseError::UnexpectedToken {
                found: prev.token_type,
                expected: Some("valid float".to_string()),
                line: prev.line,
            })?;
        Ok(Node::create_literal(
            DataType::Float,
            LiteralValue::Float(value),
            prev.line,
            prev.column,
        ))
    }

    fn parse_string_literal(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current);
        let mut value = prev.lexeme.clone();
        if value.len() >= 2 {
            value = value[1..value.len() - 1].to_string();
        }
        Ok(Node::create_literal(
            DataType::String,
            LiteralValue::String(value),
            prev.line,
            prev.column,
        ))
    }

    fn parse_char_literal(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current);
        let value = prev.lexeme.clone();
        if value.len() >= 3 {
            let ch = value.chars().nth(1).unwrap_or('\0');
            return Ok(Node::create_literal(
                DataType::Char,
                LiteralValue::Char(ch),
                prev.line,
                prev.column,
            ));
        }

        Err(ParseError::UnexpectedToken {
            found: prev.token_type,
            expected: Some("valid character literal".to_string()),
            line: prev.line,
        })
    }

    fn parse_parenthesized_or_cast(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        if TypeParser::check_type(tokens, *current) {
            let target_type = TypeParser::parse_type(tokens, current)?;
            TokenUtils::consume(tokens, current, TokenType::RightParen, "Expected ')' after cast type")?;
            let expr = Self::parse_unary(tokens, current)?;
            let line = expr.line;
            let column = expr.column;
            return Ok(Node::new(
                NodeType::Cast,
                NodeData::Cast {
                    target_type,
                    expr: Box::new(expr),
                },
                line,
                column,
            ));
        } else {
            let expr = Self::parse_expression(tokens, current)?;
            TokenUtils::consume(tokens, current, TokenType::RightParen, "Expected ')' after expression")?;
            return Ok(expr);
        }
    }

    fn parse_array_literal(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current);
        let line = prev.line;
        let column = prev.column;

        let mut elements = Vec::new();

        TokenUtils::skip_newlines_and_indents(tokens, current);

        if !TokenUtils::check_token(tokens, *current, TokenType::RightBracket) {
            loop {
                TokenUtils::skip_newlines_and_indents(tokens, current);
                elements.push(Box::new(Self::parse_expression(tokens, current)?));
                TokenUtils::skip_newlines_and_indents(tokens, current);

                if !TokenUtils::match_token(tokens, current, &[TokenType::Comma]) {
                    break;
                }

                TokenUtils::skip_newlines_and_indents(tokens, current);

                if TokenUtils::check_token(tokens, *current, TokenType::RightBracket) {
                    break;
                }
            }
        }

        TokenUtils::skip_newlines_and_indents(tokens, current);
        TokenUtils::consume(tokens, current, TokenType::RightBracket, "Expected ']' after array elements")?;

        Ok(Node::new(
            NodeType::ArrayLiteral,
            NodeData::ArrayLiteral { elements },
            line,
            column,
        ))
    }
}