#[cfg(test)]
mod tests {
    use crate::tokenizer::{Tokenizer, TokenType, TokenizerError};

    #[test]
    fn test_simple_tokens() {
        let mut tokenizer = Tokenizer::new("+ - * / =");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens.len(), 6); // 5 operators + EOF
        assert!(tokens[0].is_type(TokenType::Plus));
        assert!(tokens[1].is_type(TokenType::Minus));
        assert!(tokens[2].is_type(TokenType::Multiply));
        assert!(tokens[3].is_type(TokenType::Divide));
        assert!(tokens[4].is_type(TokenType::Assign));
        assert!(tokens[5].is_type(TokenType::Eof));
    }

    #[test]
    fn test_keywords() {
        let mut tokenizer = Tokenizer::new("int class if while");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens.len(), 5); // 4 keywords + EOF
        assert!(tokens[0].is_type(TokenType::KwInt));
        assert!(tokens[1].is_type(TokenType::KwClass));
        assert!(tokens[2].is_type(TokenType::KwIf));
        assert!(tokens[3].is_type(TokenType::KwWhile));
    }

    #[test]
    fn test_identifiers() {
        let mut tokenizer = Tokenizer::new("variable_name _private __internal");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens.len(), 4); // 3 identifiers + EOF
        for i in 0..3 {
            assert!(tokens[i].is_type(TokenType::Identifier));
        }
    }

    #[test]
    fn test_numbers() {
        let mut tokenizer = Tokenizer::new("42 3.14 0");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens.len(), 4); // 3 numbers + EOF
        assert!(tokens[0].is_type(TokenType::Int));
        assert_eq!(tokens[0].lexeme(), "42");

        assert!(tokens[1].is_type(TokenType::Float));
        assert_eq!(tokens[1].lexeme(), "3.14");

        assert!(tokens[2].is_type(TokenType::Int));
        assert_eq!(tokens[2].lexeme(), "0");
    }

    #[test]
    fn test_strings() {
        let mut tokenizer = Tokenizer::new(r#""hello world" "with\nescape""#);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens.len(), 3); // 2 strings + EOF
        assert!(tokens[0].is_type(TokenType::String));
        assert_eq!(tokens[0].lexeme(), "hello world");

        assert!(tokens[1].is_type(TokenType::String));
        assert_eq!(tokens[1].lexeme(), "with\nescape");
    }

    #[test]
    fn test_chars() {
        let mut tokenizer = Tokenizer::new(r"'a' '\n' '\\'");
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens.len(), 4); // 3 chars + EOF
        assert!(tokens[0].is_type(TokenType::Char));
        assert_eq!(tokens[0].lexeme(), "a");

        assert!(tokens[1].is_type(TokenType::Char));
        assert_eq!(tokens[1].lexeme(), "\n");

        assert!(tokens[2].is_type(TokenType::Char));
        assert_eq!(tokens[2].lexeme(), "\\");
    }

    #[test]
    fn test_compound_operators() {
        let mut tokenizer = Tokenizer::new("== != <= >= && || += -=");
        let tokens = tokenizer.tokenize().unwrap();

        let expected = [
            TokenType::Equal,
            TokenType::NotEqual,
            TokenType::LessEqual,
            TokenType::GreaterEqual,
            TokenType::LogicalAnd,
            TokenType::LogicalOr,
            TokenType::PlusAssign,
            TokenType::MinusAssign,
            TokenType::Eof,
        ];

        assert_eq!(tokens.len(), expected.len());
        for (token, &expected_type) in tokens.iter().zip(expected.iter()) {
            assert!(token.is_type(expected_type));
        }
    }

    #[test]
    fn test_comments() {
        let mut tokenizer = Tokenizer::new("int x; // this is a comment\nint y;");
        let tokens = tokenizer.tokenize().unwrap();

        // Debug: let's see what tokens we actually get
        println!("Comment test tokens ({}):", tokens.len());
        for (i, token) in tokens.iter().enumerate() {
            println!("  {}: {:?} '{}'", i, token.token_type, token.lexeme);
        }

        // Should skip comment and just tokenize: int x ; \n int y ; EOF
        // We get 8 tokens: int x ; \n int y ; EOF (with semicolon after y)
        assert_eq!(tokens.len(), 8);
        assert!(tokens[0].is_type(TokenType::KwInt));
        assert!(tokens[1].is_type(TokenType::Identifier));
        assert!(tokens[2].is_type(TokenType::Semicolon));
        assert!(tokens[3].is_type(TokenType::Newline));
        assert!(tokens[4].is_type(TokenType::KwInt));
        assert!(tokens[5].is_type(TokenType::Identifier));
        assert!(tokens[6].is_type(TokenType::Semicolon));
        assert!(tokens[7].is_type(TokenType::Eof));
    }

    #[test]
    fn test_position_tracking() {
        let mut tokenizer = Tokenizer::new("int x;\nfloat y;");
        let tokens = tokenizer.tokenize().unwrap();

        // Debug: let's see the positions
        println!("Position test tokens:");
        for (i, token) in tokens.iter().enumerate() {
            println!("  {}: {:?} '{}' at {}:{}", i, token.token_type, token.lexeme, token.line, token.column);
        }

        // First token should be at line 1, column 1
        assert_eq!(tokens[0].line, 1);
        assert_eq!(tokens[0].column, 1);

        // Token after newline should be at line 2
        let float_token = tokens.iter().find(|t| t.is_type(TokenType::KwFloat)).unwrap();
        assert_eq!(float_token.line, 2);
    }

    #[test]
    fn test_error_handling() {
        // Unterminated string
        let mut tokenizer = Tokenizer::new(r#""unterminated string"#);
        let result = tokenizer.tokenize();
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), TokenizerError::UnterminatedString { .. }));

        // Invalid character
        let mut tokenizer = Tokenizer::new("@invalid");
        let result = tokenizer.tokenize();
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), TokenizerError::UnexpectedCharacter { .. }));
    }

    #[test]
    fn test_complex_program() {
        let program = r#"
class Calculator {
    int value;

    constructor(int initial) {
        this.value = initial;
    }
}

int main() {
    Calculator calc = new Calculator(42);
    return 0;
}
"#;

        let mut tokenizer = Tokenizer::new(program);
        let tokens = tokenizer.tokenize().unwrap();

        // Should successfully tokenize without errors
        assert!(!tokens.is_empty());
        assert!(tokens.last().unwrap().is_type(TokenType::Eof));

        // Check that we have the expected keywords
        let has_class = tokens.iter().any(|t| t.is_type(TokenType::KwClass));
        let has_constructor = tokens.iter().any(|t| t.is_type(TokenType::KwConstructor));
        let has_new = tokens.iter().any(|t| t.is_type(TokenType::KwNew));

        assert!(has_class);
        assert!(has_constructor);
        assert!(has_new);
    }
}