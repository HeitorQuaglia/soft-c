use crate::ast::*;
use crate::tokenizer::{Token, TokenType};
use super::{ParseError, ParseResult, expression_parser::ExpressionParser, type_parser::TypeParser, utils::ParserUtils, token_utils::TokenUtils};

pub struct StatementParser;

impl StatementParser {
    pub fn parse_statement(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        if TokenUtils::match_token(tokens, current, &[TokenType::KwImport]) {
            return Self::parse_import_statement(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::KwExport]) {
            return Self::parse_export_statement(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::KwModule]) {
            return Self::parse_module_declaration(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::KwType]) {
            return Self::parse_struct_definition(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::KwStruct]) {
            return Self::parse_struct_definition(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::KwClass]) {
            return Self::parse_class_definition(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::KwPrint]) {
            return Self::parse_print_statement(tokens, current);
        }

        if ParserUtils::is_primitive_type(&TokenUtils::peek(tokens, *current).token_type) {
            return Self::parse_typed_statement(tokens, current);
        }

        if TokenUtils::check_token(tokens, *current, TokenType::Identifier) {
            return Self::parse_identifier_statement(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::KwIf]) {
            return Self::parse_if_statement(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::KwWhile]) {
            return Self::parse_while_statement(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::KwFor]) {
            return Self::parse_for_statement(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::KwDo]) {
            return Self::parse_do_while_statement(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::KwSwitch]) {
            return Self::parse_switch_statement(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::KwBreak]) {
            return Self::parse_break_statement(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::KwContinue]) {
            return Self::parse_continue_statement(tokens, current);
        }

        if TokenUtils::match_token(tokens, current, &[TokenType::KwReturn]) {
            return Self::parse_return_statement(tokens, current);
        }

        Self::parse_expression_statement(tokens, current)
    }

    fn parse_typed_statement(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let type_token = TokenUtils::advance(tokens, current).clone();
        let data_type = Self::token_to_data_type(type_token.token_type);

        if TokenUtils::check_token(tokens, *current, TokenType::Identifier) {
            let saved_pos = *current;
            TokenUtils::advance(tokens, current);

            if TokenUtils::check_token(tokens, *current, TokenType::LeftParen) {
                *current = saved_pos - 1;
                return Self::parse_function_definition(tokens, current);
            }
            *current = saved_pos;
        }

        Self::parse_var_declaration_with_type(tokens, current, data_type)
    }

    fn parse_identifier_statement(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let saved_pos = *current;
        TokenUtils::advance(tokens, current);

        if TokenUtils::check_token(tokens, *current, TokenType::Identifier) {
            let type_name = tokens[saved_pos].lexeme.clone();
            let data_type = DataType::Class(type_name);
            return Self::parse_var_declaration_with_type(tokens, current, data_type);
        }

        *current = saved_pos;
        Self::parse_expression_statement(tokens, current)
    }

    fn token_to_data_type(token_type: TokenType) -> DataType {
        match token_type {
            TokenType::KwInt => DataType::Int,
            TokenType::KwFloat => DataType::Float,
            TokenType::KwDouble => DataType::Double,
            TokenType::KwChar => DataType::Char,
            TokenType::KwString => DataType::String,
            TokenType::KwBool => DataType::Bool,
            _ => unreachable!("Invalid type token"),
        }
    }

    pub fn parse_function_definition(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let return_type = TypeParser::parse_type(tokens, current)?;
        let name_token = TokenUtils::consume(tokens, current, TokenType::Identifier, "Expected function name")?;
        let name = name_token.lexeme.clone();
        let line = name_token.line;
        let column = name_token.column;

        TokenUtils::consume(tokens, current, TokenType::LeftParen, "Expected '(' after function name")?;

        let mut parameters = Vec::new();
        if !TokenUtils::check_token(tokens, *current, TokenType::RightParen) {
            loop {
                let param_type = TypeParser::parse_type(tokens, current)?;
                let param_name = TokenUtils::consume(tokens, current, TokenType::Identifier, "Expected parameter name")?;

                parameters.push(Parameter {
                    data_type: param_type,
                    name: param_name.lexeme.clone(),
                });

                if !TokenUtils::match_token(tokens, current, &[TokenType::Comma]) {
                    break;
                }
            }
        }

        TokenUtils::consume(tokens, current, TokenType::RightParen, "Expected ')' after parameters")?;
        TokenUtils::consume(tokens, current, TokenType::LeftBrace, "Expected '{' after function signature")?;

        let body = Self::parse_block_with_braces(tokens, current)?;

        Ok(Node::new(
            NodeType::FunctionDef,
            NodeData::FunctionDef {
                return_type,
                name,
                parameters,
                body: Box::new(body),
            },
            line,
            column,
        ))
    }

    pub fn parse_var_declaration_with_type(
        tokens: &[Token],
        current: &mut usize,
        data_type: DataType,
    ) -> ParseResult<Node> {
        let name_token = TokenUtils::consume(tokens, current, TokenType::Identifier, "Expected variable name")?;
        let name = name_token.lexeme.clone();
        let line = name_token.line;
        let column = name_token.column;

        let mut array_sizes = None;
        if TokenUtils::check_token(tokens, *current, TokenType::LeftBracket) {
            let mut sizes = Vec::new();
            while TokenUtils::match_token(tokens, current, &[TokenType::LeftBracket]) {
                let size = ExpressionParser::parse_expression(tokens, current)?;
                sizes.push(Box::new(size));
                TokenUtils::consume(tokens, current, TokenType::RightBracket, "Expected ']'")?;
            }
            array_sizes = Some(sizes);
        }

        let init_expr = if TokenUtils::match_token(tokens, current, &[TokenType::Assign]) {
            Some(Box::new(ExpressionParser::parse_expression(tokens, current)?))
        } else {
            None
        };

        TokenUtils::consume(tokens, current, TokenType::Semicolon, "Expected ';' after variable declaration")?;

        Ok(Node::new(
            NodeType::VarDecl,
            NodeData::VarDecl {
                data_type,
                name,
                array_sizes,
                init_expr,
            },
            line,
            column,
        ))
    }

    pub fn parse_if_statement(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current).clone();
        TokenUtils::consume(tokens, current, TokenType::LeftParen, "Expected '(' after 'if'")?;
        let condition = ExpressionParser::parse_expression(tokens, current)?;
        TokenUtils::consume(tokens, current, TokenType::RightParen, "Expected ')' after if condition")?;
        TokenUtils::consume(tokens, current, TokenType::LeftBrace, "Expected '{' after if condition")?;

        let then_stmt = Self::parse_block_with_braces(tokens, current)?;

        TokenUtils::skip_newlines_and_indents(tokens, current);

        let else_stmt = if TokenUtils::match_token(tokens, current, &[TokenType::KwElse]) {
            if TokenUtils::match_token(tokens, current, &[TokenType::KwIf]) {
                Some(Box::new(Self::parse_if_statement(tokens, current)?))
            } else {
                TokenUtils::consume(tokens, current, TokenType::LeftBrace, "Expected '{' after 'else'")?;
                Some(Box::new(Self::parse_block_with_braces(tokens, current)?))
            }
        } else {
            None
        };

        Ok(Node::new(
            NodeType::IfStmt,
            NodeData::IfStmt {
                condition: Box::new(condition),
                then_stmt: Box::new(then_stmt),
                else_stmt,
            },
            prev.line,
            prev.column,
        ))
    }

    pub fn parse_while_statement(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current).clone();
        TokenUtils::consume(tokens, current, TokenType::LeftParen, "Expected '(' after 'while'")?;
        let condition = ExpressionParser::parse_expression(tokens, current)?;
        TokenUtils::consume(tokens, current, TokenType::RightParen, "Expected ')' after while condition")?;
        TokenUtils::consume(tokens, current, TokenType::LeftBrace, "Expected '{' after while condition")?;

        let body = Self::parse_block_with_braces(tokens, current)?;

        Ok(Node::new(
            NodeType::WhileStmt,
            NodeData::WhileStmt {
                condition: Box::new(condition),
                body: Box::new(body),
            },
            prev.line,
            prev.column,
        ))
    }

    pub fn parse_for_statement(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current).clone();
        TokenUtils::consume(tokens, current, TokenType::LeftParen, "Expected '(' after 'for'")?;

        let init = if TokenUtils::check_token(tokens, *current, TokenType::Semicolon) || TokenUtils::check_token(tokens, *current, TokenType::RightParen) {
            None
        } else {
            Some(Box::new(Self::parse_statement(tokens, current)?))
        };
        TokenUtils::consume(tokens, current, TokenType::Semicolon, "Expected ';' after for initializer")?;

        let condition = if TokenUtils::check_token(tokens, *current, TokenType::Semicolon) {
            None
        } else {
            Some(Box::new(ExpressionParser::parse_expression(tokens, current)?))
        };
        TokenUtils::consume(tokens, current, TokenType::Semicolon, "Expected ';' after for condition")?;

        let update = if TokenUtils::check_token(tokens, *current, TokenType::RightParen) {
            None
        } else {
            Some(Box::new(ExpressionParser::parse_expression(tokens, current)?))
        };

        TokenUtils::consume(tokens, current, TokenType::RightParen, "Expected ')' after for clauses")?;
        TokenUtils::consume(tokens, current, TokenType::LeftBrace, "Expected '{' after for(...)")?;
        let body = Self::parse_block_with_braces(tokens, current)?;

        Ok(Node::new(
            NodeType::ForStmt,
            NodeData::ForStmt {
                init,
                condition,
                update,
                body: Box::new(body),
            },
            prev.line,
            prev.column,
        ))
    }

    pub fn parse_do_while_statement(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current).clone();
        TokenUtils::consume(tokens, current, TokenType::Colon, "Expected ':' after 'do'")?;
        TokenUtils::skip_newlines(tokens, current);

        let body = Self::parse_block(tokens, current)?;

        TokenUtils::consume(tokens, current, TokenType::KwWhile, "Expected 'while' after do body")?;
        TokenUtils::consume(tokens, current, TokenType::LeftParen, "Expected '(' after 'while'")?;
        let condition = ExpressionParser::parse_expression(tokens, current)?;
        TokenUtils::consume(tokens, current, TokenType::RightParen, "Expected ')' after while condition")?;
        TokenUtils::skip_newlines(tokens, current);

        Ok(Node::new(
            NodeType::DoWhileStmt,
            NodeData::DoWhileStmt {
                body: Box::new(body),
                condition: Box::new(condition),
            },
            prev.line,
            prev.column,
        ))
    }

    pub fn parse_switch_statement(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current).clone();
        TokenUtils::consume(tokens, current, TokenType::LeftParen, "Expected '(' after 'switch'")?;
        let expr = ExpressionParser::parse_expression(tokens, current)?;
        TokenUtils::consume(tokens, current, TokenType::RightParen, "Expected ')' after switch expression")?;
        TokenUtils::consume(tokens, current, TokenType::Colon, "Expected ':' after switch expression")?;
        TokenUtils::skip_newlines(tokens, current);

        let mut cases = Vec::new();

        while TokenUtils::match_token(tokens, current, &[TokenType::KwCase, TokenType::KwDefault]) {
            let case_prev = TokenUtils::previous(tokens, *current).clone();
            let is_default = case_prev.token_type == TokenType::KwDefault;

            let value = if is_default {
                None
            } else {
                Some(Box::new(ExpressionParser::parse_expression(tokens, current)?))
            };

            TokenUtils::consume(tokens, current, TokenType::Colon, "Expected ':' after case value")?;
            TokenUtils::skip_newlines(tokens, current);

            let mut statements = Vec::new();
            while !TokenUtils::check_token(tokens, *current, TokenType::KwCase)
                && !TokenUtils::check_token(tokens, *current, TokenType::KwDefault)
                && !TokenUtils::is_at_end(tokens, *current)
            {
                if TokenUtils::check_token(tokens, *current, TokenType::Newline) {
                    TokenUtils::advance(tokens, current);
                    continue;
                }
                statements.push(Self::parse_statement(tokens, current)?);
            }

            cases.push(Node::new(
                NodeType::CaseStmt,
                NodeData::CaseStmt { value, statements },
                case_prev.line,
                case_prev.column,
            ));
        }

        Ok(Node::new(
            NodeType::SwitchStmt,
            NodeData::SwitchStmt {
                expr: Box::new(expr),
                cases,
            },
            prev.line,
            prev.column,
        ))
    }

    pub fn parse_break_statement(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current).clone();
        TokenUtils::consume(tokens, current, TokenType::Semicolon, "Expected ';' after 'break'")?;
        Ok(Node::new(
            NodeType::BreakStmt,
            NodeData::BreakStmt,
            prev.line,
            prev.column,
        ))
    }

    pub fn parse_continue_statement(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current).clone();
        TokenUtils::consume(tokens, current, TokenType::Semicolon, "Expected ';' after 'continue'")?;
        Ok(Node::new(
            NodeType::ContinueStmt,
            NodeData::ContinueStmt,
            prev.line,
            prev.column,
        ))
    }

    pub fn parse_return_statement(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current).clone();
        let value = if TokenUtils::check_token(tokens, *current, TokenType::Semicolon) || TokenUtils::is_at_end(tokens, *current) {
            None
        } else {
            Some(Box::new(ExpressionParser::parse_expression(tokens, current)?))
        };

        TokenUtils::consume(tokens, current, TokenType::Semicolon, "Expected ';' after return statement")?;

        Ok(Node::new(
            NodeType::ReturnStmt,
            NodeData::ReturnStmt { value },
            prev.line,
            prev.column,
        ))
    }

    pub fn parse_struct_definition(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current).clone();

        TokenUtils::consume(tokens, current, TokenType::Identifier, "Expected struct name")?;
        let struct_name = TokenUtils::previous(tokens, *current).lexeme.clone();

        TokenUtils::consume(tokens, current, TokenType::Colon, "Expected ':' after struct name")?;
        TokenUtils::skip_newlines(tokens, current);

        TokenUtils::consume(tokens, current, TokenType::Indent, "Expected indent after struct declaration")?;

        let mut fields = Vec::new();

        loop {
            TokenUtils::skip_newlines(tokens, current);

            if TokenUtils::check_token(tokens, *current, TokenType::Dedent) {
                break;
            }

            let field_type = if ParserUtils::is_primitive_type(&TokenUtils::peek(tokens, *current).token_type) {
                TokenUtils::advance(tokens, current);
                Self::token_to_data_type(TokenUtils::previous(tokens, *current).token_type)
            } else if TokenUtils::check_token(tokens, *current, TokenType::Identifier) {
                let type_name = TokenUtils::advance(tokens, current).lexeme.clone();
                DataType::Struct(type_name)
            } else {
                return Err(ParseError::UnexpectedToken {
                    found: TokenUtils::peek(tokens, *current).token_type,
                    expected: Some("field type".to_string()),
                    line: TokenUtils::peek(tokens, *current).line,
                });
            };

            TokenUtils::consume(tokens, current, TokenType::Identifier, "Expected field name")?;
            let field_name = TokenUtils::previous(tokens, *current).lexeme.clone();

            fields.push(Parameter {
                name: field_name,
                data_type: field_type,
            });

            TokenUtils::skip_newlines(tokens, current);
        }

        TokenUtils::consume(tokens, current, TokenType::Dedent, "Expected dedent after struct body")?;

        Ok(Node::new(
            NodeType::StructDef,
            NodeData::StructDef {
                name: struct_name,
                fields,
            },
            prev.line,
            prev.column,
        ))
    }

    pub fn parse_class_definition(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current).clone();

        TokenUtils::consume(tokens, current, TokenType::Identifier, "Expected class name")?;
        let class_name = TokenUtils::previous(tokens, *current).lexeme.clone();

        TokenUtils::consume(tokens, current, TokenType::LeftBrace, "Expected '{' after class name")?;
        TokenUtils::skip_newlines(tokens, current);

        let mut fields = Vec::new();
        let mut constructor = None;

        while !TokenUtils::check_token(tokens, *current, TokenType::RightBrace) && !TokenUtils::is_at_end(tokens, *current) {
            TokenUtils::skip_newlines_and_indents(tokens, current);

            if TokenUtils::check_token(tokens, *current, TokenType::RightBrace) {
                break;
            }

            if TokenUtils::match_token(tokens, current, &[TokenType::KwConstructor]) {
                constructor = Some(Box::new(Self::parse_constructor_definition(tokens, current)?));
                continue;
            }

            let field_type = if ParserUtils::is_primitive_type(&TokenUtils::peek(tokens, *current).token_type) {
                TokenUtils::advance(tokens, current);
                Self::token_to_data_type(TokenUtils::previous(tokens, *current).token_type)
            } else if TokenUtils::check_token(tokens, *current, TokenType::Identifier) {
                let type_name = TokenUtils::advance(tokens, current).lexeme.clone();
                DataType::Class(type_name)
            } else {
                return Err(ParseError::UnexpectedToken {
                    found: TokenUtils::peek(tokens, *current).token_type,
                    expected: Some("field type".to_string()),
                    line: TokenUtils::peek(tokens, *current).line,
                });
            };

            TokenUtils::consume(tokens, current, TokenType::Identifier, "Expected field name")?;
            let field_name = TokenUtils::previous(tokens, *current).lexeme.clone();

            fields.push(Parameter {
                name: field_name,
                data_type: field_type,
            });

            TokenUtils::consume(tokens, current, TokenType::Semicolon, "Expected ';' after field declaration")?;
            TokenUtils::skip_newlines(tokens, current);
        }

        TokenUtils::consume(tokens, current, TokenType::RightBrace, "Expected '}' after class body")?;

        Ok(Node::new(
            NodeType::ClassDef,
            NodeData::ClassDef {
                name: class_name,
                fields,
                constructor,
            },
            prev.line,
            prev.column,
        ))
    }

    pub fn parse_constructor_definition(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current).clone();

        TokenUtils::consume(tokens, current, TokenType::LeftParen, "Expected '(' after 'constructor'")?;

        let mut parameters = Vec::new();
        if !TokenUtils::check_token(tokens, *current, TokenType::RightParen) {
            loop {
                let param_type = TypeParser::parse_type(tokens, current)?;
                let param_name = TokenUtils::consume(tokens, current, TokenType::Identifier, "Expected parameter name")?;

                parameters.push(Parameter {
                    data_type: param_type,
                    name: param_name.lexeme.clone(),
                });

                if !TokenUtils::match_token(tokens, current, &[TokenType::Comma]) {
                    break;
                }
            }
        }

        TokenUtils::consume(tokens, current, TokenType::RightParen, "Expected ')' after parameters")?;
        TokenUtils::consume(tokens, current, TokenType::LeftBrace, "Expected '{' after constructor signature")?;

        let body = Self::parse_block_with_braces(tokens, current)?;

        Ok(Node::new(
            NodeType::Constructor,
            NodeData::Constructor {
                parameters,
                body: Box::new(body),
            },
            prev.line,
            prev.column,
        ))
    }

    pub fn parse_import_statement(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current).clone();

        TokenUtils::consume(tokens, current, TokenType::Identifier, "Expected symbol name after 'import'")?;
        let symbol_name = TokenUtils::previous(tokens, *current).lexeme.clone();

        TokenUtils::consume(tokens, current, TokenType::KwFrom, "Expected 'from' after import symbol")?;

        TokenUtils::consume(tokens, current, TokenType::Identifier, "Expected module name after 'from'")?;
        let module_name = TokenUtils::previous(tokens, *current).lexeme.clone();

        TokenUtils::consume(tokens, current, TokenType::Semicolon, "Expected ';' after import statement")?;

        Ok(Node::new(
            NodeType::ImportStmt,
            NodeData::ImportStmt {
                import_type: crate::ast::ImportType::Aliased(module_name, symbol_name),
            },
            prev.line,
            prev.column,
        ))
    }

    pub fn parse_export_statement(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let line = TokenUtils::previous(tokens, *current).line;
        let column = TokenUtils::previous(tokens, *current).column;

        let export_type = if TokenUtils::match_token(tokens, current, &[TokenType::Multiply]) {
            TokenUtils::consume(tokens, current, TokenType::KwFrom, "Expected 'from' after '*'")?;
            let module_name = TokenUtils::consume(tokens, current, TokenType::Identifier, "Expected module name")?.lexeme;
            crate::ast::ExportType::Reexport(module_name)
        } else if TokenUtils::match_token(tokens, current, &[TokenType::LeftBrace]) {
            let mut symbols = Vec::new();

            if !TokenUtils::check_token(tokens, *current, TokenType::RightBrace) {
                loop {
                    let symbol = TokenUtils::consume(tokens, current, TokenType::Identifier, "Expected symbol name")?.lexeme;
                    symbols.push(symbol);

                    if !TokenUtils::match_token(tokens, current, &[TokenType::Comma]) {
                        break;
                    }
                }
            }

            TokenUtils::consume(tokens, current, TokenType::RightBrace, "Expected '}'")?;
            crate::ast::ExportType::List(symbols)
        } else if TypeParser::check_type(tokens, *current) {
            let data_type = TypeParser::parse_type(tokens, current)?;
            let name = TokenUtils::consume(tokens, current, TokenType::Identifier, "Expected variable name")?.lexeme;

            let init_expr = if TokenUtils::match_token(tokens, current, &[TokenType::Assign]) {
                Some(Box::new(ExpressionParser::parse_expression(tokens, current)?))
            } else {
                None
            };

            crate::ast::ExportType::Variable(name, data_type, init_expr)
        } else {
            return Err(ParseError::UnexpectedToken {
                found: TokenUtils::peek(tokens, *current).token_type,
                expected: Some("export declaration".to_string()),
                line: TokenUtils::peek(tokens, *current).line,
            });
        };

        TokenUtils::consume(tokens, current, TokenType::Semicolon, "Expected ';' after export statement")?;

        Ok(Node::new(
            NodeType::ExportStmt,
            NodeData::ExportStmt { export_type },
            line,
            column,
        ))
    }

    pub fn parse_module_declaration(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let line = TokenUtils::previous(tokens, *current).line;
        let column = TokenUtils::previous(tokens, *current).column;

        let name = TokenUtils::consume(tokens, current, TokenType::Identifier, "Expected module name")?.lexeme;

        if TokenUtils::check_token(tokens, *current, TokenType::Newline) || TokenUtils::is_at_end(tokens, *current) {
            TokenUtils::skip_newlines(tokens, current);
            let statements = Vec::new();
            let body = Node::create_block(statements, line, column);

            return Ok(Node::new(
                NodeType::ModuleDecl,
                NodeData::ModuleDecl {
                    name,
                    body: Box::new(body),
                },
                line,
                column,
            ));
        }

        TokenUtils::consume(tokens, current, TokenType::Colon, "Expected ':' after module name")?;

        let body = Self::parse_block(tokens, current)?;

        Ok(Node::new(
            NodeType::ModuleDecl,
            NodeData::ModuleDecl {
                name,
                body: Box::new(body),
            },
            line,
            column,
        ))
    }

    pub fn parse_print_statement(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let prev = TokenUtils::previous(tokens, *current).clone();

        TokenUtils::consume(tokens, current, TokenType::LeftParen, "Expected '(' after 'print'")?;

        let mut arguments = Vec::new();
        if !TokenUtils::check_token(tokens, *current, TokenType::RightParen) {
            loop {
                arguments.push(Box::new(ExpressionParser::parse_expression(tokens, current)?));
                if !TokenUtils::match_token(tokens, current, &[TokenType::Comma]) {
                    break;
                }
            }
        }

        TokenUtils::consume(tokens, current, TokenType::RightParen, "Expected ')' after print arguments")?;
        TokenUtils::consume(tokens, current, TokenType::Semicolon, "Expected ';' after print statement")?;

        Ok(Node::new(
            NodeType::PrintStmt,
            NodeData::PrintStmt { arguments },
            prev.line,
            prev.column,
        ))
    }

    pub fn parse_expression_statement(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let expr = ExpressionParser::parse_expression(tokens, current)?;
        let line = expr.line;
        let column = expr.column;
        TokenUtils::consume(tokens, current, TokenType::Semicolon, "Expected ';' after expression statement")?;

        Ok(Node::new(
            NodeType::ExprStmt,
            NodeData::ExprStmt {
                expr: Box::new(expr),
            },
            line,
            column,
        ))
    }

    pub fn parse_block(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let mut statements = Vec::new();
        let line = TokenUtils::peek(tokens, *current).line;
        let column = TokenUtils::peek(tokens, *current).column;

        if !TokenUtils::match_token(tokens, current, &[TokenType::Indent]) {
            return if TokenUtils::match_token(tokens, current, &[TokenType::Ellipsis]) {
                TokenUtils::skip_newlines(tokens, current);
                Ok(Node::new(
                    NodeType::Block,
                    NodeData::Block { statements },
                    line,
                    column,
                ))
            } else {
                statements.push(Self::parse_statement(tokens, current)?);
                Ok(Node::new(
                    NodeType::Block,
                    NodeData::Block { statements },
                    line,
                    column,
                ))
            };
        }

        while !TokenUtils::is_at_end(tokens, *current) && !TokenUtils::check_token(tokens, *current, TokenType::Dedent) && !TokenUtils::check_token(tokens, *current, TokenType::Eof) {
            TokenUtils::skip_newlines_and_indents(tokens, current);
            if TokenUtils::is_at_end(tokens, *current) || TokenUtils::check_token(tokens, *current, TokenType::Dedent) {
                break;
            }

            statements.push(Self::parse_statement(tokens, current)?);
        }

        if TokenUtils::check_token(tokens, *current, TokenType::Dedent) {
            TokenUtils::advance(tokens, current);
        }

        Ok(Node::create_block(statements, line, column))
    }

    pub fn parse_block_with_braces(
        tokens: &[Token],
        current: &mut usize,
    ) -> ParseResult<Node> {
        let mut statements = Vec::new();
        let line = TokenUtils::peek(tokens, *current).line;
        let column = TokenUtils::peek(tokens, *current).column;

        TokenUtils::skip_newlines(tokens, current);

        while !TokenUtils::is_at_end(tokens, *current) && !TokenUtils::check_token(tokens, *current, TokenType::RightBrace) && !TokenUtils::check_token(tokens, *current, TokenType::Eof) {
            TokenUtils::skip_newlines(tokens, current);
            if TokenUtils::is_at_end(tokens, *current) || TokenUtils::check_token(tokens, *current, TokenType::RightBrace) {
                break;
            }

            statements.push(Self::parse_statement(tokens, current)?);
        }

        TokenUtils::consume(tokens, current, TokenType::RightBrace, "Expected '}' after block")?;

        Ok(Node::create_block(statements, line, column))
    }

}