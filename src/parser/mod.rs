mod error;
mod parser_core;
mod expression_parser;
mod statement_parser;
mod type_parser;
mod utils;
mod token_utils;

#[cfg(test)]
mod tests;

pub use error::{ParseError, ParseErrors};
pub use parser_core::Parser;

pub type ParseResult<T> = Result<T, ParseError>;
pub type ParseResultMulti<T> = Result<T, ParseErrors>;