use super::token_type::TokenType;
use super::position::Position;

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    position: Position,
    // Legacy compatibility fields
    pub line: u32,
    pub column: u32,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, position: Position) -> Self {
        Self {
            token_type,
            lexeme: lexeme.clone(),
            position,
            line: position.line(),
            column: position.column(),
        }
    }

    pub fn with_position(
        token_type: TokenType,
        lexeme: String,
        line: u32,
        column: u32,
    ) -> Self {
        Self {
            token_type,
            lexeme: lexeme.clone(),
            position: Position::new(line, column),
            line,
            column,
        }
    }

    pub fn token_type(&self) -> TokenType {
        self.token_type
    }

    pub fn lexeme(&self) -> &str {
        &self.lexeme
    }

    pub fn position(&self) -> &Position {
        &self.position
    }

    pub fn is_type(&self, token_type: TokenType) -> bool {
        self.token_type == token_type
    }

    pub fn is_any_of(&self, types: &[TokenType]) -> bool {
        types.contains(&self.token_type)
    }

    pub fn eof(position: Position) -> Self {
        Self {
            token_type: TokenType::Eof,
            lexeme: String::new(),
            position,
            line: position.line(),
            column: position.column(),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}('{}') at {}",
            self.token_type,
            self.lexeme,
            self.position
        )
    }
}