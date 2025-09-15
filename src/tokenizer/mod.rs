mod token_type;
mod token;
mod keywords;
mod position;
mod error;
mod lexer;

#[cfg(test)]
mod tests;

pub use token_type::TokenType;
pub use token::Token;
pub use error::TokenizerError;
pub use lexer::Tokenizer;

pub type TokenizerResult<T> = Result<T, TokenizerError>;