use crate::ast::Node;
use crate::tokenizer::{Token, TokenType};
use super::{ParseResult, ParseResultMulti, ParseErrors, statement_parser::StatementParser};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> ParseResult<Node> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            self.skip_newlines();
            if self.is_at_end() {
                break;
            }

            let stmt = StatementParser::parse_statement(&self.tokens, &mut self.current)?;
            statements.push(stmt);
        }

        Ok(Node::create_program(statements))
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() || self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        if self.current >= self.tokens.len() {
            &self.tokens[self.tokens.len() - 1]
        } else {
            &self.tokens[self.current]
        }
    }

    fn skip_newlines(&mut self) {
        while self.match_token(&[TokenType::Newline]) {}
    }

    fn match_token(&mut self, types: &[TokenType]) -> bool {
        for token_type in types {
            if self.check(*token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().token_type == token_type
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        &self.tokens[self.current - 1]
    }

    pub fn parse_with_error_recovery(&mut self) -> ParseResultMulti<Node> {
        let mut statements = Vec::new();
        let mut errors = ParseErrors::new();

        while !self.is_at_end() {
            self.skip_newlines();
            if self.is_at_end() {
                break;
            }

            match StatementParser::parse_statement(&self.tokens, &mut self.current) {
                Ok(stmt) => statements.push(stmt),
                Err(parse_error) => {
                    errors.add(parse_error);
                    self.recover_to_next_statement();
                }
            }
        }

        if errors.has_errors() {
            Err(errors)
        } else {
            Ok(Node::create_program(statements))
        }
    }

    fn recover_to_next_statement(&mut self) {
        if !self.is_at_end() {
            self.advance();
        }

        while !self.is_at_end() {
            match self.peek().token_type {
                TokenType::KwInt | TokenType::KwFloat | TokenType::KwDouble |
                TokenType::KwChar | TokenType::KwString | TokenType::KwBool |
                TokenType::KwIf | TokenType::KwWhile | TokenType::KwFor |
                TokenType::KwReturn | TokenType::KwBreak | TokenType::KwContinue |
                TokenType::KwPrint | TokenType::KwImport | TokenType::KwExport |
                TokenType::KwModule | TokenType::KwStruct | TokenType::KwClass => {
                    break;
                }
                TokenType::Semicolon => {
                    self.advance();
                    break;
                }
                TokenType::RightBrace | TokenType::LeftBrace => {
                    break;
                }
                TokenType::Eof => {
                    break;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }
}