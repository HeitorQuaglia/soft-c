use super::{Token, TokenType, TokenizerError, keywords, position::{Position, PositionTracker}};
use super::error::Result;

pub struct Tokenizer {
    source: Vec<char>,
    tokens: Vec<Token>,
    current: usize,
    position_tracker: PositionTracker,
}

impl Tokenizer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            tokens: Vec::new(),
            current: 0,
            position_tracker: PositionTracker::new(4), // 4 spaces per tab
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        while !self.is_at_end() {
            self.scan_token()?;
        }

        self.add_token(TokenType::Eof, "");

        Ok(std::mem::take(&mut self.tokens))
    }

    fn scan_token(&mut self) -> Result<()> {
        let start_position = self.position_tracker.position();
        let ch = self.advance();

        match ch {
            '\n' => {
                self.add_token_at_position(TokenType::Newline, "\n", start_position);
            }
            '/' if self.match_char('/') => {
                self.scan_line_comment()?;
            }
            '/' if self.match_char('*') => {
                self.scan_block_comment()?;
            }
            '(' => self.add_token_at_position(TokenType::LeftParen, "(", start_position),
            ')' => self.add_token_at_position(TokenType::RightParen, ")", start_position),
            '[' => self.add_token_at_position(TokenType::LeftBracket, "[", start_position),
            ']' => self.add_token_at_position(TokenType::RightBracket, "]", start_position),
            '{' => self.add_token_at_position(TokenType::LeftBrace, "{", start_position),
            '}' => self.add_token_at_position(TokenType::RightBrace, "}", start_position),
            ',' => self.add_token_at_position(TokenType::Comma, ",", start_position),
            ';' => self.add_token_at_position(TokenType::Semicolon, ";", start_position),
            ':' => self.add_token_at_position(TokenType::Colon, ":", start_position),
            '?' => self.add_token_at_position(TokenType::Question, "?", start_position),

            // Operators and multi-character tokens
            '+' => self.scan_plus(start_position),
            '-' => self.scan_minus(start_position),
            '*' => self.scan_multiply(start_position),
            '/' => self.scan_divide(start_position),
            '%' => self.scan_modulo(start_position),
            '=' => self.scan_equal(start_position),
            '!' => self.scan_not(start_position),
            '<' => self.scan_less(start_position),
            '>' => self.scan_greater(start_position),
            '&' => self.scan_and(start_position),
            '|' => self.scan_or(start_position),
            '.' => self.scan_dot(start_position),

            // String literals
            '"' => self.scan_string()?,

            // Character literals
            '\'' => self.scan_char()?,

            // Numbers or identifiers
            _ => {
                if ch.is_ascii_digit() {
                    self.scan_number(start_position)?;
                } else if ch.is_ascii_alphabetic() || ch == '_' {
                    self.scan_identifier(start_position);
                } else {
                    return Err(TokenizerError::UnexpectedCharacter {
                        ch,
                        position: start_position,
                    });
                }
            }
        }

        Ok(())
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            let ch = self.source[self.current];
            self.current += 1;
            self.position_tracker.advance(ch);
            ch
        }
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
        if self.peek() == expected {
            self.advance();
            true
        } else {
            false
        }
    }

    fn add_token(&mut self, token_type: TokenType, lexeme: &str) {
        self.add_token_at_position(token_type, lexeme, self.position_tracker.position());
    }

    fn add_token_at_position(&mut self, token_type: TokenType, lexeme: &str, position: Position) {
        let token = Token::new(
            token_type,
            lexeme.to_string(),
            position,
        );
        self.tokens.push(token);
    }

    fn add_token_with_text(&mut self, token_type: TokenType, start: usize, start_position: Position) {
        let lexeme: String = self.source[start..self.current].iter().collect();
        self.add_token_at_position(token_type, &lexeme, start_position);
    }

    fn scan_plus(&mut self, start_position: Position) {
        if self.match_char('=') {
            self.add_token_at_position(TokenType::PlusAssign, "+=", start_position);
        } else if self.match_char('+') {
            self.add_token_at_position(TokenType::Increment, "++", start_position);
        } else {
            self.add_token_at_position(TokenType::Plus, "+", start_position);
        }
    }

    fn scan_minus(&mut self, start_position: Position) {
        if self.match_char('=') {
            self.add_token_at_position(TokenType::MinusAssign, "-=", start_position);
        } else if self.match_char('-') {
            self.add_token_at_position(TokenType::Decrement, "--", start_position);
        } else {
            self.add_token_at_position(TokenType::Minus, "-", start_position);
        }
    }

    fn scan_multiply(&mut self, start_position: Position) {
        if self.match_char('=') {
            self.add_token_at_position(TokenType::MultiplyAssign, "*=", start_position);
        } else {
            self.add_token_at_position(TokenType::Multiply, "*", start_position);
        }
    }

    fn scan_divide(&mut self, start_position: Position) {
        if self.match_char('=') {
            self.add_token_at_position(TokenType::DivideAssign, "/=", start_position);
        } else {
            self.add_token_at_position(TokenType::Divide, "/", start_position);
        }
    }

    fn scan_modulo(&mut self, start_position: Position) {
        self.add_token_at_position(TokenType::Modulo, "%", start_position);
    }

    fn scan_equal(&mut self, start_position: Position) {
        if self.match_char('=') {
            self.add_token_at_position(TokenType::Equal, "==", start_position);
        } else {
            self.add_token_at_position(TokenType::Assign, "=", start_position);
        }
    }

    fn scan_not(&mut self, start_position: Position) {
        if self.match_char('=') {
            self.add_token_at_position(TokenType::NotEqual, "!=", start_position);
        } else {
            self.add_token_at_position(TokenType::LogicalNot, "!", start_position);
        }
    }

    fn scan_less(&mut self, start_position: Position) {
        if self.match_char('=') {
            self.add_token_at_position(TokenType::LessEqual, "<=", start_position);
        } else {
            self.add_token_at_position(TokenType::Less, "<", start_position);
        }
    }

    fn scan_greater(&mut self, start_position: Position) {
        if self.match_char('=') {
            self.add_token_at_position(TokenType::GreaterEqual, ">=", start_position);
        } else {
            self.add_token_at_position(TokenType::Greater, ">", start_position);
        }
    }

    fn scan_and(&mut self, start_position: Position) {
        if self.match_char('&') {
            self.add_token_at_position(TokenType::LogicalAnd, "&&", start_position);
        }
    }

    fn scan_or(&mut self, start_position: Position) {
        if self.match_char('|') {
            self.add_token_at_position(TokenType::LogicalOr, "||", start_position);
        }
    }

    fn scan_dot(&mut self, start_position: Position) {
        if self.match_char('.') && self.match_char('.') {
            self.add_token_at_position(TokenType::Ellipsis, "...", start_position);
        } else {
            self.add_token_at_position(TokenType::Dot, ".", start_position);
        }
    }

    fn scan_string(&mut self) -> Result<()> {
        let start_position = self.position_tracker.position();
        let mut value = String::new();

        while !self.is_at_end() && self.peek() != '"' {
            if self.peek() == '\n' {
                return Err(TokenizerError::UnterminatedString {
                    position: start_position,
                });
            }

            if self.peek() == '\\' {
                self.advance();
                value.push(self.scan_escape_sequence()?);
            } else {
                value.push(self.advance());
            }
        }

        if self.is_at_end() {
            return Err(TokenizerError::UnterminatedString {
                position: start_position,
            });
        }

        self.advance();
        self.add_token(TokenType::String, &value);
        Ok(())
    }

    fn scan_char(&mut self) -> Result<()> {
        let start_position = self.position_tracker.position();

        if self.is_at_end() || self.peek() == '\n' {
            return Err(TokenizerError::UnterminatedCharLiteral {
                position: start_position,
            });
        }

        let ch = if self.peek() == '\\' {
            self.advance();
            self.scan_escape_sequence()?
        } else {
            self.advance()
        };

        if self.is_at_end() || self.peek() != '\'' {
            return Err(TokenizerError::UnterminatedCharLiteral {
                position: start_position,
            });
        }

        self.advance();
        self.add_token(TokenType::Char, &ch.to_string());
        Ok(())
    }

    fn scan_escape_sequence(&mut self) -> Result<char> {
        let start_position = self.position_tracker.position();

        if self.is_at_end() {
            return Err(TokenizerError::InvalidEscapeSequence {
                sequence: "\\".to_string(),
                position: start_position,
            });
        }

        let escaped = self.advance();
        match escaped {
            'n' => Ok('\n'),
            't' => Ok('\t'),
            'r' => Ok('\r'),
            '\\' => Ok('\\'),
            '\'' => Ok('\''),
            '"' => Ok('"'),
            '0' => Ok('\0'),
            _ => Err(TokenizerError::InvalidEscapeSequence {
                sequence: format!("\\{}", escaped),
                position: start_position,
            }),
        }
    }

    fn scan_number(&mut self, start_position: Position) -> Result<()> {
        let start = self.current - 1;

        while self.peek().is_ascii_digit() {
            self.advance();
        }

        let mut is_float = false;
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            is_float = true;
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let token_type = if is_float {
            TokenType::Float
        } else {
            TokenType::Int
        };

        self.add_token_with_text(token_type, start, start_position);
        Ok(())
    }

    fn scan_identifier(&mut self, start_position: Position) {
        let start = self.current - 1;

        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let text: String = self.source[start..self.current].iter().collect();

        let token_type = keywords::get_keyword(&text)
            .unwrap_or(TokenType::Identifier);

        self.add_token_at_position(token_type, &text, start_position);
    }

    fn scan_line_comment(&mut self) -> Result<()> {
        while !self.is_at_end() && self.peek() != '\n' {
            self.advance();
        }
        Ok(())
    }

    fn scan_block_comment(&mut self) -> Result<()> {
        let start_position = self.position_tracker.position();

        while !self.is_at_end() {
            if self.peek() == '*' && self.peek_next() == '/' {
                self.advance();
                self.advance();
                return Ok(());
            }
            self.advance();
        }

        Err(TokenizerError::UnterminatedString {
            position: start_position,
        })
    }
}