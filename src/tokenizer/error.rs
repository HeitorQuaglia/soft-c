use super::position::Position;
use std::fmt;

#[derive(Debug, Clone)]
pub enum TokenizerError {
    UnexpectedCharacter {
        ch: char,
        position: Position
    },
    UnterminatedString {
        position: Position
    },
    UnterminatedCharLiteral {
        position: Position
    },
    InvalidCharLiteral {
        content: String,
        position: Position
    },
    InvalidNumberFormat {
        content: String,
        position: Position
    },
    InvalidEscapeSequence {
        sequence: String,
        position: Position
    },
    IndentationError {
        expected: usize,
        found: usize,
        position: Position
    },
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenizerError::UnexpectedCharacter { ch, position } => {
                write!(f, "Unexpected character '{}' at {}", ch, position)
            }
            TokenizerError::UnterminatedString { position } => {
                write!(f, "Unterminated string literal at {}", position)
            }
            TokenizerError::UnterminatedCharLiteral { position } => {
                write!(f, "Unterminated character literal at {}", position)
            }
            TokenizerError::InvalidCharLiteral { content, position } => {
                write!(f, "Invalid character literal '{}' at {}", content, position)
            }
            TokenizerError::InvalidNumberFormat { content, position } => {
                write!(f, "Invalid number format '{}' at {}", content, position)
            }
            TokenizerError::InvalidEscapeSequence { sequence, position } => {
                write!(f, "Invalid escape sequence '{}' at {}", sequence, position)
            }
            TokenizerError::IndentationError { expected, found, position } => {
                write!(
                    f,
                    "Indentation error: expected {} spaces, found {} at {}",
                    expected, found, position
                )
            }
        }
    }
}

impl std::error::Error for TokenizerError {}

pub type Result<T> = std::result::Result<T, TokenizerError>;