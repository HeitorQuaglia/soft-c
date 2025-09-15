use crate::tokenizer::{Token, TokenType};
use super::{ParseError, ParseResult};

pub struct TokenUtils;

impl TokenUtils {
    pub fn peek(tokens: &[Token], current: usize) -> &Token {
        if current >= tokens.len() {
            &tokens[tokens.len() - 1]
        } else {
            &tokens[current]
        }
    }

    pub fn previous(tokens: &[Token], current: usize) -> &Token {
        &tokens[current - 1]
    }

    pub fn advance<'a>(tokens: &'a [Token], current: &mut usize) -> &'a Token {
        if *current < tokens.len() - 1 {
            *current += 1;
        }
        &tokens[*current - 1]
    }

    pub fn is_at_end(tokens: &[Token], current: usize) -> bool {
        current >= tokens.len() || tokens[current].token_type == TokenType::Eof
    }

    pub fn check_token(tokens: &[Token], current: usize, token_type: TokenType) -> bool {
        if current >= tokens.len() {
            false
        } else {
            tokens[current].token_type == token_type
        }
    }

    pub fn match_token(tokens: &[Token], current: &mut usize, types: &[TokenType]) -> bool {
        for token_type in types {
            if Self::check_token(tokens, *current, *token_type) {
                Self::advance(tokens, current);
                return true;
            }
        }
        false
    }

    pub fn consume(
        tokens: &[Token],
        current: &mut usize,
        token_type: TokenType,
        message: &str,
    ) -> ParseResult<Token> {
        if Self::check_token(tokens, *current, token_type) {
            Ok(Self::advance(tokens, current).clone())
        } else {
            Err(ParseError::UnexpectedToken {
                found: Self::peek(tokens, *current).token_type,
                expected: Some(message.to_string()),
                line: Self::peek(tokens, *current).line,
            })
        }
    }

    pub fn skip_newlines(tokens: &[Token], current: &mut usize) {
        while Self::match_token(tokens, current, &[TokenType::Newline]) {}
    }

    pub fn skip_newlines_and_indents(tokens: &[Token], current: &mut usize) {
        while Self::match_token(tokens, current, &[TokenType::Newline, TokenType::Indent, TokenType::Dedent]) {}
    }

    pub fn is_primitive_type_token(tokens: &[Token], current: usize) -> bool {
        matches!(
            Self::peek(tokens, current).token_type,
            TokenType::KwInt
                | TokenType::KwFloat
                | TokenType::KwDouble
                | TokenType::KwChar
                | TokenType::KwString
                | TokenType::KwBool
        )
    }

    pub fn is_type_token(tokens: &[Token], current: usize) -> bool {
        matches!(
            Self::peek(tokens, current).token_type,
            TokenType::KwInt
                | TokenType::KwFloat
                | TokenType::KwDouble
                | TokenType::KwChar
                | TokenType::KwString
                | TokenType::KwBool
                | TokenType::Identifier
        )
    }

    pub fn is_assignment_token(tokens: &[Token], current: usize) -> bool {
        matches!(
            Self::peek(tokens, current).token_type,
            TokenType::Assign
                | TokenType::PlusAssign
                | TokenType::MinusAssign
                | TokenType::MultiplyAssign
                | TokenType::DivideAssign
        )
    }

    pub fn current_position(tokens: &[Token], current: usize) -> (u32, u32) {
        let token = Self::peek(tokens, current);
        (token.line, token.column)
    }

    pub fn unexpected_token_error(
        tokens: &[Token],
        current: usize,
        expected: Option<&str>,
    ) -> ParseError {
        let token = Self::peek(tokens, current);
        ParseError::UnexpectedToken {
            found: token.token_type,
            expected: expected.map(|s| s.to_string()),
            line: token.line,
        }
    }

    pub fn get_token(tokens: &[Token], index: usize) -> Option<&Token> {
        tokens.get(index)
    }

    pub fn lookahead(tokens: &[Token], current: usize, offset: usize) -> Option<&Token> {
        Self::get_token(tokens, current + offset)
    }

    pub fn has_tokens_remaining(tokens: &[Token], current: usize, count: usize) -> bool {
        current + count <= tokens.len()
    }
}