#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        found: crate::tokenizer::TokenType,
        expected: Option<String>,
        line: u32,
    },
    UnexpectedEof,
}

#[derive(Debug)]
pub struct ParseErrors {
    pub errors: Vec<ParseError>,
}

impl ParseErrors {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
        }
    }

    pub fn add(&mut self, error: ParseError) {
        self.errors.push(error);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn count(&self) -> usize {
        self.errors.len()
    }
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

impl std::fmt::Display for ParseErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.errors.is_empty() {
            write!(f, "No parsing errors")
        } else {
            writeln!(f, "Found {} parsing error(s):", self.errors.len())?;
            for (i, error) in self.errors.iter().enumerate() {
                writeln!(f, "  {}: {}", i + 1, error)?;
            }
            Ok(())
        }
    }
}

impl std::error::Error for ParseErrors {}