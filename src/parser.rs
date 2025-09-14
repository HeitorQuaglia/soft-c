use crate::ast::*;
use crate::tokenizer::{Token, TokenType};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        found: TokenType,
        expected: Option<String>,
        line: u32,
    },
    UnexpectedEof,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                found,
                expected,
                line,
            } => {
                if let Some(exp) = expected {
                    write!(
                        f,
                        "Unexpected token {:?} at line {}, expected {}",
                        found, line, exp
                    )
                } else {
                    write!(f, "Unexpected token {:?} at line {}", found, line)
                }
            }
            ParseError::UnexpectedEof => write!(f, "Unexpected end of file"),
        }
    }
}

impl std::error::Error for ParseError {}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Node, ParseError> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            self.skip_newlines();
            if self.is_at_end() {
                break;
            }

            let stmt = self.statement()?;
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

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().token_type == token_type
        }
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

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<Token, ParseError> {
        if self.check(token_type) {
            Ok(self.advance().clone())
        } else {
            Err(ParseError::UnexpectedToken {
                found: self.peek().token_type,
                expected: Some(message.to_string()),
                line: self.peek().line,
            })
        }
    }

    fn skip_newlines(&mut self) {
        while self.match_token(&[TokenType::Newline]) {}
    }

    fn skip_newlines_and_indents(&mut self) {
        while self.match_token(&[TokenType::Newline, TokenType::Indent, TokenType::Dedent]) {}
    }

    fn statement(&mut self) -> Result<Node, ParseError> {
        if self.match_token(&[TokenType::KwImport]) {
            return self.import_statement();
        }

        if self.match_token(&[TokenType::KwExport]) {
            return self.export_statement();
        }

        if self.match_token(&[TokenType::KwModule]) {
            return self.module_declaration();
        }

        if self.match_token(&[TokenType::KwType]) {
            return self.struct_definition();
        }

        if self.match_token(&[
            TokenType::KwInt,
            TokenType::KwFloat,
            TokenType::KwDouble,
            TokenType::KwChar,
            TokenType::KwString,
            TokenType::KwBool,
        ]) {
            let type_token = self.previous().clone();
            let data_type = match type_token.token_type {
                TokenType::KwInt => DataType::Int,
                TokenType::KwFloat => DataType::Float,
                TokenType::KwDouble => DataType::Double,
                TokenType::KwChar => DataType::Char,
                TokenType::KwString => DataType::String,
                TokenType::KwBool => DataType::Bool,
                _ => unreachable!(),
            };

            if self.check(TokenType::Identifier) {
                let saved_pos = self.current;
                self.advance();
                if self.check(TokenType::LeftParen) {
                    self.current -= 2;
                    return self.function_definition();
                }
                self.current = saved_pos;
            }
            return self.var_declaration_with_type(data_type);
        }

        if self.check(TokenType::Identifier) {
            let saved_pos = self.current;
            self.advance();
            if self.check(TokenType::Identifier) {
                let type_name = self.tokens[saved_pos].lexeme.clone();
                let data_type = DataType::Class(type_name);
                return self.var_declaration_with_type(data_type);
            }
            self.current = saved_pos;
        }

        if self.match_token(&[TokenType::KwIf]) {
            return self.if_statement();
        }

        if self.match_token(&[TokenType::KwWhile]) {
            return self.while_statement();
        }

        if self.match_token(&[TokenType::KwFor]) {
            return self.for_statement();
        }

        if self.match_token(&[TokenType::KwDo]) {
            return self.do_while_statement();
        }

        if self.match_token(&[TokenType::KwSwitch]) {
            return self.switch_statement();
        }

        if self.match_token(&[TokenType::KwBreak]) {
            let prev = self.previous().clone();
            self.consume(TokenType::Semicolon, "Expected ';' after 'break'")?;
            return Ok(Node::new(
                NodeType::BreakStmt,
                NodeData::BreakStmt,
                prev.line,
                prev.column,
            ));
        }

        if self.match_token(&[TokenType::KwContinue]) {
            let prev = self.previous().clone();
            self.consume(TokenType::Semicolon, "Expected ';' after 'continue'")?;
            return Ok(Node::new(
                NodeType::ContinueStmt,
                NodeData::ContinueStmt,
                prev.line,
                prev.column,
            ));
        }

        if self.match_token(&[TokenType::KwReturn]) {
            return self.return_statement();
        }

        if self.match_token(&[TokenType::KwStruct]) {
            return self.struct_definition();
        }

        if self.match_token(&[TokenType::KwClass]) {
            return self.class_definition();
        }

        if self.match_token(&[TokenType::KwImport]) {
            return self.import_statement();
        }

        if self.match_token(&[TokenType::KwPrint]) {
            return self.print_statement();
        }

        self.expression_statement()
    }

    fn function_definition(&mut self) -> Result<Node, ParseError> {
        let return_type = self.parse_type()?;
        let name_token = self.consume(TokenType::Identifier, "Expected function name")?;
        let name = name_token.lexeme.clone();
        let line = name_token.line;
        let column = name_token.column;

        self.consume(TokenType::LeftParen, "Expected '(' after function name")?;

        let mut parameters = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                let param_type = self.parse_type()?;
                let param_name = self.consume(TokenType::Identifier, "Expected parameter name")?;

                parameters.push(Parameter {
                    data_type: param_type,
                    name: param_name.lexeme.clone(),
                });

                if !self.match_token(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expected ')' after parameters")?;
        self.consume(
            TokenType::LeftBrace,
            "Expected '{' after function signature",
        )?;

        let body = self.block_with_braces()?;

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

    fn var_declaration_with_type(&mut self, data_type: DataType) -> Result<Node, ParseError> {
        let name_token = self.consume(TokenType::Identifier, "Expected variable name")?;
        let name = name_token.lexeme.clone();
        let line = name_token.line;
        let column = name_token.column;

        let mut array_sizes = None;
        if self.check(TokenType::LeftBracket) {
            let mut sizes = Vec::new();
            while self.match_token(&[TokenType::LeftBracket]) {
                let size = self.expression()?;
                sizes.push(Box::new(size));
                self.consume(TokenType::RightBracket, "Expected ']'")?;
            }
            array_sizes = Some(sizes);
        }

        let init_expr = if self.match_token(&[TokenType::Assign]) {
            Some(Box::new(self.expression()?))
        } else {
            None
        };

        self.consume(
            TokenType::Semicolon,
            "Expected ';' after variable declaration",
        )?;

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

    fn if_statement(&mut self) -> Result<Node, ParseError> {
        let prev = self.previous().clone();
        self.consume(TokenType::LeftParen, "Expected '(' after 'if'")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')' after if condition")?;
        self.consume(TokenType::LeftBrace, "Expected '{' after if condition")?;

        let then_stmt = self.block_with_braces()?;

        self.skip_newlines_and_indents();

        let else_stmt = if self.match_token(&[TokenType::KwElse]) {
            if self.match_token(&[TokenType::KwIf]) {
                Some(Box::new(self.if_statement()?))
            } else {
                self.consume(TokenType::LeftBrace, "Expected '{' after 'else'")?;
                Some(Box::new(self.block_with_braces()?))
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

    fn while_statement(&mut self) -> Result<Node, ParseError> {
        let prev = self.previous().clone();
        self.consume(TokenType::LeftParen, "Expected '(' after 'while'")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')' after while condition")?;
        self.consume(TokenType::LeftBrace, "Expected '{' after while condition")?;

        let body = self.block_with_braces()?;

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

    fn for_statement(&mut self) -> Result<Node, ParseError> {
        let prev = self.previous().clone();
        self.consume(TokenType::LeftParen, "Expected '(' after 'for'")?;

        let init = if self.check(TokenType::Semicolon) || self.check(TokenType::RightParen) {
            None
        } else {
            Some(Box::new(self.statement()?))
        };
        self.consume(TokenType::Semicolon, "Expected ';' after for initializer")?;

        let condition = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(Box::new(self.expression()?))
        };
        self.consume(TokenType::Semicolon, "Expected ';' after for condition")?;

        let update = if self.check(TokenType::RightParen) {
            None
        } else {
            Some(Box::new(self.expression()?))
        };

        self.consume(TokenType::RightParen, "Expected ')' after for clauses")?;
        self.consume(TokenType::LeftBrace, "Expected '{' after for(...)")?;
        let body = self.block_with_braces()?;

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

    fn do_while_statement(&mut self) -> Result<Node, ParseError> {
        let prev = self.previous().clone();
        self.consume(TokenType::Colon, "Expected ':' after 'do'")?;
        self.skip_newlines();

        let body = self.block()?;

        self.consume(TokenType::KwWhile, "Expected 'while' after do body")?;
        self.consume(TokenType::LeftParen, "Expected '(' after 'while'")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')' after while condition")?;
        self.skip_newlines();

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

    fn switch_statement(&mut self) -> Result<Node, ParseError> {
        let prev = self.previous().clone();
        self.consume(TokenType::LeftParen, "Expected '(' after 'switch'")?;
        let expr = self.expression()?;
        self.consume(
            TokenType::RightParen,
            "Expected ')' after switch expression",
        )?;
        self.consume(TokenType::Colon, "Expected ':' after switch expression")?;
        self.skip_newlines();

        let mut cases = Vec::new();

        while self.match_token(&[TokenType::KwCase, TokenType::KwDefault]) {
            let case_prev = self.previous().clone();
            let is_default = case_prev.token_type == TokenType::KwDefault;

            let value = if is_default {
                None
            } else {
                Some(Box::new(self.expression()?))
            };

            self.consume(TokenType::Colon, "Expected ':' after case value")?;
            self.skip_newlines();

            let mut statements = Vec::new();
            while !self.check(TokenType::KwCase)
                && !self.check(TokenType::KwDefault)
                && !self.is_at_end()
            {
                if self.check(TokenType::Newline) {
                    self.advance();
                    continue;
                }
                statements.push(self.statement()?);
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

    fn return_statement(&mut self) -> Result<Node, ParseError> {
        let prev = self.previous().clone();
        let value = if self.check(TokenType::Semicolon) || self.is_at_end() {
            None
        } else {
            Some(Box::new(self.expression()?))
        };

        self.consume(TokenType::Semicolon, "Expected ';' after return statement")?;

        Ok(Node::new(
            NodeType::ReturnStmt,
            NodeData::ReturnStmt { value },
            prev.line,
            prev.column,
        ))
    }

    fn struct_definition(&mut self) -> Result<Node, ParseError> {
        let prev = self.previous().clone();

        self.consume(TokenType::Identifier, "Expected struct name")?;
        let struct_name = self.previous().lexeme.clone();

        self.consume(TokenType::Colon, "Expected ':' after struct name")?;
        self.skip_newlines();

        self.consume(
            TokenType::Indent,
            "Expected indent after struct declaration",
        )?;

        let mut fields = Vec::new();

        loop {
            self.skip_newlines();

            if self.check(TokenType::Dedent) {
                break;
            }

            if !self.match_token(&[
                TokenType::KwInt,
                TokenType::KwFloat,
                TokenType::KwDouble,
                TokenType::KwChar,
                TokenType::KwString,
                TokenType::KwBool,
            ]) {
                if self.check(TokenType::Identifier) {
                    self.advance();
                } else {
                    return Err(ParseError::UnexpectedToken {
                        found: self.peek().token_type,
                        expected: Some("field type".to_string()),
                        line: self.peek().line,
                    });
                }
            }

            let type_token = self.previous().clone();
            let data_type = match type_token.token_type {
                TokenType::KwInt => DataType::Int,
                TokenType::KwFloat => DataType::Float,
                TokenType::KwDouble => DataType::Double,
                TokenType::KwChar => DataType::Char,
                TokenType::KwString => DataType::String,
                TokenType::KwBool => DataType::Bool,
                TokenType::Identifier => DataType::Struct(type_token.lexeme.clone()),
                _ => unreachable!(),
            };

            self.consume(TokenType::Identifier, "Expected field name")?;
            let field_name = self.previous().lexeme.clone();

            fields.push(Parameter {
                name: field_name,
                data_type,
            });

            self.skip_newlines();
        }

        self.consume(TokenType::Dedent, "Expected dedent after struct body")?;

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

    fn class_literal(&mut self) -> Result<Node, ParseError> {
        let prev = self.previous().clone();

        self.consume(TokenType::Identifier, "Expected class name after 'new'")?;
        let class_name = self.previous().lexeme.clone();

        self.consume(TokenType::LeftParen, "Expected '(' after class name")?;

        self.skip_newlines_and_indents();

        let mut field_values = Vec::new();

        if !self.check(TokenType::RightParen) {
            loop {
                self.skip_newlines_and_indents();

                if self.check(TokenType::RightParen) {
                    break;
                }

                let value = Box::new(self.assignment()?);

                field_values.push(FieldValue {
                    field_name: String::new(),
                    value,
                });

                let has_comma = self.match_token(&[TokenType::Comma]);
                if has_comma {
                    self.skip_newlines_and_indents();
                }

                if !has_comma {
                    break;
                }
            }
        }

        self.skip_newlines_and_indents();
        self.consume(TokenType::RightParen, "Expected ')' after class fields")?;

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

    fn import_statement(&mut self) -> Result<Node, ParseError> {
        let prev = self.previous().clone();

        self.consume(TokenType::Identifier, "Expected symbol name after 'import'")?;
        let symbol_name = self.previous().lexeme.clone();

        self.consume(TokenType::KwFrom, "Expected 'from' after import symbol")?;

        self.consume(TokenType::Identifier, "Expected module name after 'from'")?;
        let module_name = self.previous().lexeme.clone();

        self.consume(TokenType::Semicolon, "Expected ';' after import statement")?;

        Ok(Node::new(
            NodeType::ImportStmt,
            NodeData::ImportStmt {
                import_type: crate::ast::ImportType::Aliased(module_name, symbol_name),
            },
            prev.line,
            prev.column,
        ))
    }

    fn print_statement(&mut self) -> Result<Node, ParseError> {
        let prev = self.previous().clone();

        self.consume(TokenType::LeftParen, "Expected '(' after 'print'")?;

        let mut arguments = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                arguments.push(Box::new(self.expression()?));
                if !self.match_token(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expected ')' after print arguments")?;
        self.consume(TokenType::Semicolon, "Expected ';' after print statement")?;

        Ok(Node::new(
            NodeType::PrintStmt,
            NodeData::PrintStmt { arguments },
            prev.line,
            prev.column,
        ))
    }

    fn expression_statement(&mut self) -> Result<Node, ParseError> {
        let expr = self.expression()?;
        let line = expr.line;
        let column = expr.column;
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after expression statement",
        )?;

        Ok(Node::new(
            NodeType::ExprStmt,
            NodeData::ExprStmt {
                expr: Box::new(expr),
            },
            line,
            column,
        ))
    }

    fn block(&mut self) -> Result<Node, ParseError> {
        let mut statements = Vec::new();
        let line = self.peek().line;
        let column = self.peek().column;

        if !self.match_token(&[TokenType::Indent]) {
            return if self.match_token(&[TokenType::Ellipsis]) {
                self.skip_newlines();
                Ok(Node::new(
                    NodeType::Block,
                    NodeData::Block { statements },
                    line,
                    column,
                ))
            } else {
                statements.push(self.statement()?);
                Ok(Node::new(
                    NodeType::Block,
                    NodeData::Block { statements },
                    line,
                    column,
                ))
            };
        }

        while !self.is_at_end() && !self.check(TokenType::Dedent) && !self.check(TokenType::Eof) {
            self.skip_newlines_and_indents();
            if self.is_at_end() || self.check(TokenType::Dedent) {
                break;
            }

            statements.push(self.statement()?);
        }

        if self.check(TokenType::Dedent) {
            self.advance();
        }

        Ok(Node::create_block(statements, line, column))
    }

    fn block_with_braces(&mut self) -> Result<Node, ParseError> {
        let mut statements = Vec::new();
        let line = self.peek().line;
        let column = self.peek().column;

        self.skip_newlines_and_indents();

        while !self.is_at_end() && !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof)
        {
            self.skip_newlines_and_indents();
            if self.is_at_end() || self.check(TokenType::RightBrace) {
                break;
            }

            statements.push(self.statement()?);
        }

        self.consume(TokenType::RightBrace, "Expected '}' after block")?;

        Ok(Node::create_block(statements, line, column))
    }

    fn expression(&mut self) -> Result<Node, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Node, ParseError> {
        let left = self.ternary()?;

        if self.match_token(&[
            TokenType::Assign,
            TokenType::PlusAssign,
            TokenType::MinusAssign,
            TokenType::MultiplyAssign,
            TokenType::DivideAssign,
        ]) {
            let op_tok = self.previous().clone();
            let rhs = self.assignment()?;

            if !Self::is_lvalue(&left) {
                return Err(ParseError::UnexpectedToken {
                    found: op_tok.token_type.clone(),
                    expected: Some("assignable left-hand side".to_string()),
                    line: op_tok.line,
                });
            }

            return Ok(Node::new(
                NodeType::Assignment,
                NodeData::Assignment {
                    target: Box::new(left),
                    op: Self::map_assign_op(op_tok.token_type),
                    value: Box::new(rhs),
                },
                op_tok.line,
                op_tok.column,
            ));
        }

        Ok(left)
    }

    fn is_lvalue(n: &Node) -> bool {
        matches!(
            n.node_type,
            NodeType::Identifier | NodeType::MemberAccess | NodeType::ArrayAccess
        )
    }

    fn map_assign_op(t: TokenType) -> AssignOpType {
        match t {
            TokenType::Assign => AssignOpType::Assign,
            TokenType::PlusAssign => AssignOpType::AddAssign,
            TokenType::MinusAssign => AssignOpType::SubAssign,
            TokenType::MultiplyAssign => AssignOpType::MulAssign,
            TokenType::DivideAssign => AssignOpType::DivAssign,
            _ => unreachable!(),
        }
    }

    fn ternary(&mut self) -> Result<Node, ParseError> {
        let mut expr = self.logical_or()?;

        if self.match_token(&[TokenType::Question]) {
            let line = expr.line;
            let column = expr.column;
            let true_expr = self.expression()?;
            self.consume(
                TokenType::Colon,
                "Expected ':' after ternary true expression",
            )?;
            let false_expr = self.expression()?;

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

    fn logical_or(&mut self) -> Result<Node, ParseError> {
        let mut expr = self.logical_and()?;

        while self.match_token(&[TokenType::LogicalOr]) {
            let op = BinaryOpType::LogicalOr;
            let right = self.logical_and()?;
            let line = expr.line;
            let column = expr.column;
            expr = Node::create_binary_op(op, Box::new(expr), Box::new(right), line, column);
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Node, ParseError> {
        let mut expr = self.equality()?;

        while self.match_token(&[TokenType::LogicalAnd]) {
            let op = BinaryOpType::LogicalAnd;
            let right = self.equality()?;
            let line = expr.line;
            let column = expr.column;
            expr = Node::create_binary_op(op, Box::new(expr), Box::new(right), line, column);
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Node, ParseError> {
        let mut expr = self.comparison()?;

        while self.match_token(&[TokenType::Equal, TokenType::NotEqual]) {
            let op = match self.previous().token_type {
                TokenType::Equal => BinaryOpType::Equal,
                TokenType::NotEqual => BinaryOpType::NotEqual,
                _ => unreachable!(),
            };
            let right = self.comparison()?;
            let line = expr.line;
            let column = expr.column;
            expr = Node::create_binary_op(op, Box::new(expr), Box::new(right), line, column);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Node, ParseError> {
        let mut expr = self.term()?;

        while self.match_token(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let op = match self.previous().token_type {
                TokenType::Greater => BinaryOpType::Greater,
                TokenType::GreaterEqual => BinaryOpType::GreaterEqual,
                TokenType::Less => BinaryOpType::Less,
                TokenType::LessEqual => BinaryOpType::LessEqual,
                _ => unreachable!(),
            };
            let right = self.term()?;
            let line = expr.line;
            let column = expr.column;
            expr = Node::create_binary_op(op, Box::new(expr), Box::new(right), line, column);
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Node, ParseError> {
        let mut expr = self.factor()?;

        while self.match_token(&[TokenType::Minus, TokenType::Plus]) {
            let op = match self.previous().token_type {
                TokenType::Minus => BinaryOpType::Sub,
                TokenType::Plus => BinaryOpType::Add,
                _ => unreachable!(),
            };
            let right = self.factor()?;
            let line = expr.line;
            let column = expr.column;
            expr = Node::create_binary_op(op, Box::new(expr), Box::new(right), line, column);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Node, ParseError> {
        let mut expr = self.unary()?;

        while self.match_token(&[TokenType::Divide, TokenType::Multiply, TokenType::Modulo]) {
            let op = match self.previous().token_type {
                TokenType::Divide => BinaryOpType::Div,
                TokenType::Multiply => BinaryOpType::Mul,
                TokenType::Modulo => BinaryOpType::Mod,
                _ => unreachable!(),
            };
            let right = self.unary()?;
            let line = expr.line;
            let column = expr.column;
            expr = Node::create_binary_op(op, Box::new(expr), Box::new(right), line, column);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Node, ParseError> {
        if self.match_token(&[TokenType::LogicalNot, TokenType::Minus, TokenType::Plus]) {
            let prev = self.previous().clone();
            let op = match prev.token_type {
                TokenType::LogicalNot => UnaryOpType::LogicalNot,
                TokenType::Minus => UnaryOpType::Minus,
                TokenType::Plus => UnaryOpType::Plus,
                _ => unreachable!(),
            };
            let operand = self.unary()?;
            return Ok(Node::create_unary_op(
                op,
                Box::new(operand),
                prev.line,
                prev.column,
            ));
        }

        self.postfix()
    }

    fn postfix(&mut self) -> Result<Node, ParseError> {
        let mut expr = self.primary()?;

        loop {
            if self.match_token(&[TokenType::Increment]) {
                let prev = self.previous();
                expr = Node::create_unary_op(
                    UnaryOpType::PostIncrement,
                    Box::new(expr),
                    prev.line,
                    prev.column,
                );
            } else if self.match_token(&[TokenType::Decrement]) {
                let prev = self.previous();
                expr = Node::create_unary_op(
                    UnaryOpType::PostDecrement,
                    Box::new(expr),
                    prev.line,
                    prev.column,
                );
            } else if self.match_token(&[TokenType::LeftBracket]) {
                let mut indices = Vec::new();
                indices.push(Box::new(self.expression()?));
                self.consume(TokenType::RightBracket, "Expected ']'")?;

                while self.match_token(&[TokenType::LeftBracket]) {
                    indices.push(Box::new(self.expression()?));
                    self.consume(TokenType::RightBracket, "Expected ']'")?;
                }

                let line = expr.line;
                let column = expr.column;
                expr = Node::new(
                    NodeType::ArrayAccess,
                    NodeData::ArrayAccess {
                        array: Box::new(expr),
                        indices,
                    },
                    line,
                    column,
                );
            } else if self.match_token(&[TokenType::Dot]) {
                self.consume(TokenType::Identifier, "Expected member name after '.'")?;
                let member_name = self.previous().lexeme.clone();

                let line = expr.line;
                let column = expr.column;
                expr = Node::new(
                    NodeType::MemberAccess,
                    NodeData::MemberAccess {
                        object: Box::new(expr),
                        member_name,
                    },
                    line,
                    column,
                );
            } else if self.match_token(&[TokenType::LeftParen]) {
                let mut args = Vec::new();
                if !self.check(TokenType::RightParen) {
                    loop {
                        args.push(Box::new(self.expression()?));
                        if !self.match_token(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }
                self.consume(TokenType::RightParen, "Expected ')' after arguments")?;

                if let NodeData::Identifier { name } = &expr.data {
                    let line = expr.line;
                    let column = expr.column;
                    expr = Node::new(
                        NodeType::FunctionCall,
                        NodeData::FunctionCall {
                            name: name.clone(),
                            args,
                        },
                        line,
                        column,
                    );
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Node, ParseError> {
        if self.match_token(&[TokenType::KwNew]) {
            return self.class_literal();
        }

        if self.match_token(&[TokenType::KwTrue]) {
            let prev = self.previous();
            return Ok(Node::create_literal(
                DataType::Bool,
                LiteralValue::Bool(true),
                prev.line,
                prev.column,
            ));
        }

        if self.match_token(&[TokenType::KwFalse]) {
            let prev = self.previous();
            return Ok(Node::create_literal(
                DataType::Bool,
                LiteralValue::Bool(false),
                prev.line,
                prev.column,
            ));
        }

        if self.match_token(&[TokenType::Int]) {
            let prev = self.previous();
            let value = prev
                .lexeme
                .parse::<i64>()
                .map_err(|_| ParseError::UnexpectedToken {
                    found: prev.token_type,
                    expected: Some("valid integer".to_string()),
                    line: prev.line,
                })?;
            return Ok(Node::create_literal(
                DataType::Int,
                LiteralValue::Int(value),
                prev.line,
                prev.column,
            ));
        }

        if self.match_token(&[TokenType::Float]) {
            let prev = self.previous();
            let value = prev
                .lexeme
                .parse::<f64>()
                .map_err(|_| ParseError::UnexpectedToken {
                    found: prev.token_type,
                    expected: Some("valid float".to_string()),
                    line: prev.line,
                })?;
            return Ok(Node::create_literal(
                DataType::Float,
                LiteralValue::Float(value),
                prev.line,
                prev.column,
            ));
        }

        if self.match_token(&[TokenType::String]) {
            let prev = self.previous();
            let mut value = prev.lexeme.clone();
            if value.len() >= 2 {
                value = value[1..value.len() - 1].to_string();
            }
            return Ok(Node::create_literal(
                DataType::String,
                LiteralValue::String(value),
                prev.line,
                prev.column,
            ));
        }

        if self.match_token(&[TokenType::Char]) {
            let prev = self.previous();
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
        }

        if self.match_token(&[TokenType::Identifier]) {
            let prev = self.previous();
            return Ok(Node::create_identifier(
                prev.lexeme.clone(),
                prev.line,
                prev.column,
            ));
        }

        if self.match_token(&[TokenType::LeftParen]) {
            if self.check_type() {
                let target_type = self.parse_type()?;
                self.consume(TokenType::RightParen, "Expected ')' after cast type")?;
                let expr = self.unary()?;
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
                let expr = self.expression()?;
                self.consume(TokenType::RightParen, "Expected ')' after expression")?;
                return Ok(expr);
            }
        }

        if self.match_token(&[TokenType::LeftBracket]) {
            return self.array_literal();
        }

        Err(ParseError::UnexpectedToken {
            found: self.peek().token_type,
            expected: Some("primary expression".to_string()),
            line: self.peek().line,
        })
    }

    fn array_literal(&mut self) -> Result<Node, ParseError> {
        let line = self.previous().line;
        let column = self.previous().column;

        let mut elements = Vec::new();

        self.skip_newlines_and_indents();

        if !self.check(TokenType::RightBracket) {
            loop {
                self.skip_newlines_and_indents();
                elements.push(Box::new(self.expression()?));
                self.skip_newlines_and_indents();

                if !self.match_token(&[TokenType::Comma]) {
                    break;
                }

                self.skip_newlines_and_indents();

                if self.check(TokenType::RightBracket) {
                    break;
                }
            }
        }

        self.skip_newlines_and_indents();
        self.consume(TokenType::RightBracket, "Expected ']' after array elements")?;

        Ok(Node::new(
            NodeType::ArrayLiteral,
            NodeData::ArrayLiteral { elements },
            line,
            column,
        ))
    }

    fn check_type(&self) -> bool {
        matches!(
            self.peek().token_type,
            TokenType::KwInt
                | TokenType::KwFloat
                | TokenType::KwDouble
                | TokenType::KwChar
                | TokenType::KwString
                | TokenType::KwBool
        )
    }

    fn parse_type(&mut self) -> Result<DataType, ParseError> {
        let base_type = if self.match_token(&[TokenType::KwInt]) {
            DataType::Int
        } else if self.match_token(&[TokenType::KwFloat]) {
            DataType::Float
        } else if self.match_token(&[TokenType::KwDouble]) {
            DataType::Double
        } else if self.match_token(&[TokenType::KwChar]) {
            DataType::Char
        } else if self.match_token(&[TokenType::KwString]) {
            DataType::String
        } else if self.match_token(&[TokenType::KwBool]) {
            DataType::Bool
        } else if self.check(TokenType::Identifier) {
            let type_name = self.advance().lexeme.clone();
            DataType::Class(type_name)
        } else {
            return Err(ParseError::UnexpectedToken {
                found: self.peek().token_type,
                expected: Some("type keyword".to_string()),
                line: self.peek().line,
            });
        };

        if self.check(TokenType::LeftBracket) {
            let mut dimensions = Vec::new();
            while self.match_token(&[TokenType::LeftBracket]) {
                if self.check(TokenType::RightBracket) {
                    dimensions.push(0);
                } else {
                    // TODO: Handle sized arrays like int[10]
                    dimensions.push(0);
                }
                self.consume(TokenType::RightBracket, "Expected ']'")?;
            }
            Ok(DataType::Array(Box::new(base_type), dimensions))
        } else {
            Ok(base_type)
        }
    }

    fn export_statement(&mut self) -> Result<Node, ParseError> {
        let line = self.previous().line;
        let column = self.previous().column;

        let export_type = if self.match_token(&[TokenType::Multiply]) {
            self.consume(TokenType::KwFrom, "Expected 'from' after '*'")?;
            let module_name = self
                .consume(TokenType::Identifier, "Expected module name")?
                .lexeme;
            ExportType::Reexport(module_name)
        } else if self.match_token(&[TokenType::LeftBrace]) {
            let mut symbols = Vec::new();

            if !self.check(TokenType::RightBrace) {
                loop {
                    let symbol = self
                        .consume(TokenType::Identifier, "Expected symbol name")?
                        .lexeme;
                    symbols.push(symbol);

                    if !self.match_token(&[TokenType::Comma]) {
                        break;
                    }
                }
            }

            self.consume(TokenType::RightBrace, "Expected '}'")?;
            ExportType::List(symbols)
        } else if self.check_type() {
            let data_type = self.parse_type()?;
            let name = self
                .consume(TokenType::Identifier, "Expected variable name")?
                .lexeme;

            let init_expr = if self.match_token(&[TokenType::Assign]) {
                Some(Box::new(self.expression()?))
            } else {
                None
            };

            ExportType::Variable(name, data_type, init_expr)
        } else {
            return Err(ParseError::UnexpectedToken {
                found: self.peek().token_type,
                expected: Some("export declaration".to_string()),
                line: self.peek().line,
            });
        };

        self.consume(TokenType::Semicolon, "Expected ';' after export statement")?;

        Ok(Node::new(
            NodeType::ExportStmt,
            NodeData::ExportStmt { export_type },
            line,
            column,
        ))
    }

    fn module_declaration(&mut self) -> Result<Node, ParseError> {
        let line = self.previous().line;
        let column = self.previous().column;

        let name = self
            .consume(TokenType::Identifier, "Expected module name")?
            .lexeme;

        if self.check(TokenType::Newline) || self.is_at_end() {
            self.skip_newlines();
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

        self.consume(TokenType::Colon, "Expected ':' after module name")?;

        let body = self.block()?;

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

    fn class_definition(&mut self) -> Result<Node, ParseError> {
        let prev = self.previous().clone();

        self.consume(TokenType::Identifier, "Expected class name")?;
        let class_name = self.previous().lexeme.clone();

        self.consume(TokenType::LeftBrace, "Expected '{' after class name")?;
        self.skip_newlines();

        let mut fields = Vec::new();
        let mut constructor = None;

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            self.skip_newlines_and_indents();

            if self.check(TokenType::RightBrace) {
                break;
            }

            if self.match_token(&[TokenType::KwConstructor]) {
                constructor = Some(Box::new(self.constructor_definition()?));
                continue;
            }

            if !self.match_token(&[
                TokenType::KwInt,
                TokenType::KwFloat,
                TokenType::KwDouble,
                TokenType::KwChar,
                TokenType::KwString,
                TokenType::KwBool,
            ]) {
                if self.check(TokenType::Identifier) {
                    self.advance();
                } else {
                    return Err(ParseError::UnexpectedToken {
                        found: self.peek().token_type,
                        expected: Some("field type".to_string()),
                        line: self.peek().line,
                    });
                }
            }

            let type_token = self.previous().clone();
            let data_type = match type_token.token_type {
                TokenType::KwInt => DataType::Int,
                TokenType::KwFloat => DataType::Float,
                TokenType::KwDouble => DataType::Double,
                TokenType::KwChar => DataType::Char,
                TokenType::KwString => DataType::String,
                TokenType::KwBool => DataType::Bool,
                TokenType::Identifier => DataType::Class(type_token.lexeme.clone()),
                _ => unreachable!(),
            };

            self.consume(TokenType::Identifier, "Expected field name")?;
            let field_name = self.previous().lexeme.clone();

            fields.push(Parameter {
                name: field_name,
                data_type,
            });

            self.consume(TokenType::Semicolon, "Expected ';' after field declaration")?;
            self.skip_newlines();
        }

        self.consume(TokenType::RightBrace, "Expected '}' after class body")?;

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

    fn constructor_definition(&mut self) -> Result<Node, ParseError> {
        let prev = self.previous().clone();

        self.consume(TokenType::LeftParen, "Expected '(' after 'constructor'")?;

        let mut parameters = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                let param_type = self.parse_type()?;
                let param_name = self.consume(TokenType::Identifier, "Expected parameter name")?;

                parameters.push(Parameter {
                    data_type: param_type,
                    name: param_name.lexeme.clone(),
                });

                if !self.match_token(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expected ')' after parameters")?;
        self.consume(
            TokenType::LeftBrace,
            "Expected '{' after constructor signature",
        )?;

        let body = self.block_with_braces()?;

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
}
