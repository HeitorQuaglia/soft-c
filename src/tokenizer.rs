use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    Int,
    Float,
    String,
    Char,
    Bool,
    Identifier,
    KwInt,
    KwFloat,
    KwDouble,
    KwChar,
    KwString,
    KwBool,
    KwIf,
    KwElse,
    KwWhile,
    KwFor,
    KwDo,
    KwSwitch,
    KwCase,
    KwDefault,
    KwBreak,
    KwContinue,
    KwReturn,
    KwTrue,
    KwFalse,
    KwPrint,
    KwType,
    KwStruct,
    KwClass,
    KwConstructor,
    KwImport,
    KwExport,
    KwModule,
    KwFrom,
    KwAs,
    KwNew,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Assign,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    Increment,
    Decrement,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Colon,
    Question,
    Comma,
    Semicolon,
    Dot,
    Ellipsis,
    Newline,
    Indent,
    Dedent,
    Eof,
    Comment,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: u32,
    pub column: u32,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: u32, column: u32) -> Self {
        Token {
            token_type,
            lexeme,
            line,
            column,
        }
    }
}

pub struct Tokenizer {
    source: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: u32,
    column: u32,
    keywords: HashMap<String, TokenType>,
    indent_stack: Vec<usize>,
    at_line_start: bool,
    tab_width: usize,
}

impl Tokenizer {
    pub fn new(source: &str) -> Self {
        let mut keywords = HashMap::new();
        keywords.insert("int".to_string(), TokenType::KwInt);
        keywords.insert("float".to_string(), TokenType::KwFloat);
        keywords.insert("double".to_string(), TokenType::KwDouble);
        keywords.insert("char".to_string(), TokenType::KwChar);
        keywords.insert("string".to_string(), TokenType::KwString);
        keywords.insert("bool".to_string(), TokenType::KwBool);
        keywords.insert("if".to_string(), TokenType::KwIf);
        keywords.insert("else".to_string(), TokenType::KwElse);
        keywords.insert("while".to_string(), TokenType::KwWhile);
        keywords.insert("for".to_string(), TokenType::KwFor);
        keywords.insert("do".to_string(), TokenType::KwDo);
        keywords.insert("switch".to_string(), TokenType::KwSwitch);
        keywords.insert("case".to_string(), TokenType::KwCase);
        keywords.insert("default".to_string(), TokenType::KwDefault);
        keywords.insert("break".to_string(), TokenType::KwBreak);
        keywords.insert("continue".to_string(), TokenType::KwContinue);
        keywords.insert("return".to_string(), TokenType::KwReturn);
        keywords.insert("true".to_string(), TokenType::KwTrue);
        keywords.insert("false".to_string(), TokenType::KwFalse);
        keywords.insert("print".to_string(), TokenType::KwPrint);
        keywords.insert("type".to_string(), TokenType::KwType);
        keywords.insert("struct".to_string(), TokenType::KwStruct);
        keywords.insert("class".to_string(), TokenType::KwClass);
        keywords.insert("constructor".to_string(), TokenType::KwConstructor);
        keywords.insert("import".to_string(), TokenType::KwImport);
        keywords.insert("export".to_string(), TokenType::KwExport);
        keywords.insert("module".to_string(), TokenType::KwModule);
        keywords.insert("from".to_string(), TokenType::KwFrom);
        keywords.insert("as".to_string(), TokenType::KwAs);
        keywords.insert("new".to_string(), TokenType::KwNew);

        Tokenizer {
            source: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            column: 1,
            keywords,
            indent_stack: vec![0],
            at_line_start: true,
            tab_width: 4
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        while !self.is_at_end() {
            self.start = self.current;
            
            if self.at_line_start {
                self.handle_indentation()?;
                continue;
            }
            
            self.scan_token()?;
        }

        while self.indent_stack.len() > 1 {
            self.indent_stack.pop();
            self.tokens.push(Token::new(
                TokenType::Dedent,
                String::new(),
                self.line,
                self.column,
            ));
        }

        self.tokens.push(Token::new(
            TokenType::Eof,
            String::new(),
            self.line,
            self.column,
        ));

        Ok(self.tokens.clone())
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn handle_indentation(&mut self) -> Result<(), String> {
        let mut indent_level = 0;
        
        while !self.is_at_end() && (self.peek() == ' ' || self.peek() == '\t') {
            if self.peek() == '\t' {
                indent_level += 4;
            } else {
                indent_level += 1;
            }
            self.advance();
        }
        
        if !self.is_at_end() && self.peek() == '\n' {
            self.advance();
            self.tokens.push(Token::new(TokenType::Newline, String::new(), self.line, self.column));
            self.at_line_start = true;
            return Ok(());
        }

        if !self.is_at_end() && self.peek() == '/' && self.peek_next() == '/' {
            while !self.is_at_end() && self.peek() != '\n' {
                self.advance();
            }
            if !self.is_at_end() && self.peek() == '\n' {
                self.advance();
                self.tokens.push(Token::new(TokenType::Newline, String::new(), self.line, self.column));
            }
            self.at_line_start = true;
            return Ok(());
        }
        
        self.at_line_start = false;
        let current_indent = *self.indent_stack.last().unwrap();
        
        if indent_level > current_indent {
            self.indent_stack.push(indent_level);
            self.tokens.push(Token::new(
                TokenType::Indent,
                String::new(),
                self.line,
                self.column,
            ));
        } else if indent_level < current_indent {
            while let Some(&stack_indent) = self.indent_stack.last() {
                if stack_indent <= indent_level {
                    break;
                }
                self.indent_stack.pop();
                self.tokens.push(Token::new(
                    TokenType::Dedent,
                    String::new(),
                    self.line,
                    self.column,
                ));
            }
            
            if self.indent_stack.last() != Some(&indent_level) {
                return Err(format!("Indentation error at line {}", self.line));
            }
        }
        
        Ok(())
    }


    fn advance(&mut self) -> Option<char> {
        let c = self.source[self.current];
        self.current += 1;
        if c == '\n' {
            self.line += 1;
            self.column = 1;
            self.at_line_start = true;
        } else {
            self.column += 1;
        }
        Some(c)
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source[self.current + 1]
        }
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.source[self.current] != expected {
            false
        } else {
            self.current += 1;
            self.column += 1;
            true
        }
    }

    fn add_token(&mut self, token_type: TokenType) {
        let lexeme: String = self.source[self.start..self.current].iter().collect();
        let token = Token::new(
            token_type,
            lexeme.clone(),
            self.line,
            self.column - lexeme.len() as u32,
        );
        self.tokens.push(token);
    }

    fn scan_token(&mut self) -> Result<(), String> {
        let c = self.advance().unwrap();

        match c {
            ' ' | '\r' | '\t' => {},
            '\n' => {
                self.add_token(TokenType::Newline);
                self.at_line_start = true;
            },
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '[' => self.add_token(TokenType::LeftBracket),
            ']' => self.add_token(TokenType::RightBracket),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ':' => self.add_token(TokenType::Colon),
            '?' => self.add_token(TokenType::Question),
            ';' => self.add_token(TokenType::Semicolon),
            ',' => self.add_token(TokenType::Comma),
            '.' => {
                if self.match_char('.') && self.match_char('.') {
                    self.add_token(TokenType::Ellipsis);
                } else {
                    self.add_token(TokenType::Dot);
                }
            },
            '+' => {
                if self.match_char('+') {
                    self.add_token(TokenType::Increment);
                } else if self.match_char('=') {
                    self.add_token(TokenType::PlusAssign);
                } else {
                    self.add_token(TokenType::Plus);
                }
            },
            '-' => {
                if self.match_char('-') {
                    self.add_token(TokenType::Decrement);
                } else if self.match_char('=') {
                    self.add_token(TokenType::MinusAssign);
                } else {
                    self.add_token(TokenType::Minus);
                }
            },
            '*' => {
                if self.match_char('=') {
                    self.add_token(TokenType::MultiplyAssign);
                } else {
                    self.add_token(TokenType::Multiply);
                }
            },
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.match_char('=') {
                    self.add_token(TokenType::DivideAssign);
                } else {
                    self.add_token(TokenType::Divide);
                }
            },
            '%' => self.add_token(TokenType::Modulo),
            '=' => {
                if self.match_char('=') {
                    self.add_token(TokenType::Equal);
                } else {
                    self.add_token(TokenType::Assign);
                }
            },
            '!' => {
                if self.match_char('=') {
                    self.add_token(TokenType::NotEqual);
                } else {
                    self.add_token(TokenType::LogicalNot);
                }
            },
            '<' => {
                if self.match_char('=') {
                    self.add_token(TokenType::LessEqual);
                } else {
                    self.add_token(TokenType::Less);
                }
            },
            '>' => {
                if self.match_char('=') {
                    self.add_token(TokenType::GreaterEqual);
                } else {
                    self.add_token(TokenType::Greater);
                }
            },
            '&' => {
                if self.match_char('&') {
                    self.add_token(TokenType::LogicalAnd);
                } else {
                    return Err(format!("Unexpected character '&' at line {}, column {}", self.line, self.column));
                }
            },
            '|' => {
                if self.match_char('|') {
                    self.add_token(TokenType::LogicalOr);
                } else {
                    return Err(format!("Unexpected character '|' at line {}, column {}", self.line, self.column));
                }
            },
            '"' => self.scan_string()?,
            '\'' => self.scan_char()?,
            _ => {
                if c.is_ascii_digit() {
                    self.scan_number()?;
                } else if c.is_ascii_alphabetic() || c == '_' {
                    self.scan_identifier();
                } else {
                    return Err(format!("Unexpected character '{}' at line {}, column {}", c, self.line, self.column));
                }
            }
        }

        Ok(())
    }

    fn scan_string(&mut self) -> Result<(), String> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
                self.column = 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(format!("Unterminated string at line {}", self.line));
        }

        self.advance();
        self.add_token(TokenType::String);
        Ok(())
    }

    fn scan_char(&mut self) -> Result<(), String> {
        if self.peek() == '\\' {
            self.advance();
            self.advance();
        } else {
            self.advance();
        }

        if self.peek() != '\'' {
            return Err(format!("Unterminated char at line {}", self.line));
        }

        self.advance();
        self.add_token(TokenType::Char);
        Ok(())
    }

    fn scan_number(&mut self) -> Result<(), String> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();
            
            while self.peek().is_ascii_digit() {
                self.advance();
            }
            
            self.add_token(TokenType::Float);
        } else {
            self.add_token(TokenType::Int);
        }

        Ok(())
    }

    fn scan_identifier(&mut self) {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let text: String = self.source[self.start..self.current].iter().collect();
        let token_type = self.keywords.get(&text)
            .cloned()
            .unwrap_or(TokenType::Identifier);
        
        self.add_token(token_type);
    }
}