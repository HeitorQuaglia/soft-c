use crate::ast::DataType;
use crate::tokenizer::{Token, TokenType};
use super::{ParseResult, token_utils::TokenUtils};

pub struct TypeParser;

impl TypeParser {
    pub fn parse_type(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<DataType> {
        let base_type = Self::parse_base_type(tokens, current)?;

        if TokenUtils::check_token(tokens, *current, TokenType::LeftBracket) {
            Self::parse_array_type(tokens, current, base_type)
        } else {
            Ok(base_type)
        }
    }

    fn parse_base_type(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<DataType> {
        if TokenUtils::match_token(tokens, current, &[TokenType::KwInt]) {
            Ok(DataType::Int)
        } else if TokenUtils::match_token(tokens, current, &[TokenType::KwFloat]) {
            Ok(DataType::Float)
        } else if TokenUtils::match_token(tokens, current, &[TokenType::KwDouble]) {
            Ok(DataType::Double)
        } else if TokenUtils::match_token(tokens, current, &[TokenType::KwChar]) {
            Ok(DataType::Char)
        } else if TokenUtils::match_token(tokens, current, &[TokenType::KwString]) {
            Ok(DataType::String)
        } else if TokenUtils::match_token(tokens, current, &[TokenType::KwBool]) {
            Ok(DataType::Bool)
        } else if TokenUtils::check_token(tokens, *current, TokenType::Identifier) {
            let type_name = TokenUtils::advance(tokens, current).lexeme.clone();
            Ok(DataType::Class(type_name))
        } else {
            Err(TokenUtils::unexpected_token_error(tokens, *current, Some("type keyword")))
        }
    }

    fn parse_array_type(
        tokens: &[Token],
        current: &mut usize,
        base_type: DataType,
    ) -> ParseResult<DataType> {
        let mut dimensions = Vec::new();

        while TokenUtils::match_token(tokens, current, &[TokenType::LeftBracket]) {
            if TokenUtils::check_token(tokens, *current, TokenType::RightBracket) {
                dimensions.push(0);
            } else {
                // TODO: Handle sized arrays like int[10]
                dimensions.push(0);
            }
            TokenUtils::consume(tokens, current, TokenType::RightBracket, "Expected ']'")?;
        }

        Ok(DataType::Array(Box::new(base_type), dimensions))
    }

    pub fn check_type(tokens: &[Token], current: usize) -> bool {
        TokenUtils::is_type_token(tokens, current)
    }
}