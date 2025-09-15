#[cfg(test)]
mod tests {
    use crate::tokenizer::{Tokenizer, Token, TokenType};
    use crate::ast::NodeType;
    use crate::parser::{ParseError, Parser};

    fn tokenize_code(code: &str) -> Vec<Token> {
        let mut tokenizer = Tokenizer::new(code);
        tokenizer.tokenize().unwrap()
    }

    #[test]
    fn test_parse_simple_variable_declaration() {
        let tokens = tokenize_code("int x = 42;");
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(ast.node_type, NodeType::Program);
        if let crate::ast::NodeData::Program { statements } = &ast.data {
            assert_eq!(statements.len(), 1);
            assert_eq!(statements[0].node_type, NodeType::VarDecl);
        }
    }

    #[test]
    fn test_parse_function_call() {
        let tokens = tokenize_code("print(42);");
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(ast.node_type, NodeType::Program);
        if let crate::ast::NodeData::Program { statements } = &ast.data {
            assert_eq!(statements.len(), 1);
            assert_eq!(statements[0].node_type, NodeType::PrintStmt);
        }
    }

    #[test]
    fn test_parse_arithmetic_expression() {
        let tokens = tokenize_code("int result = 1 + 2 * 3;");
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(ast.node_type, NodeType::Program);
    }

    #[test]
    fn test_parse_if_statement() {
        let tokens = tokenize_code("if (x > 0) { print(x); }");
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(ast.node_type, NodeType::Program);
        if let crate::ast::NodeData::Program { statements } = &ast.data {
            assert_eq!(statements.len(), 1);
            assert_eq!(statements[0].node_type, NodeType::IfStmt);
        }
    }

    #[test]
    fn test_parse_function_definition() {
        let tokens = tokenize_code("int add(int a, int b) { return a + b; }");
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(ast.node_type, NodeType::Program);
        if let crate::ast::NodeData::Program { statements } = &ast.data {
            assert_eq!(statements.len(), 1);
            assert_eq!(statements[0].node_type, NodeType::FunctionDef);
        }
    }

    #[test]
    fn test_parse_error_unexpected_token() {
        let tokens = tokenize_code("int x =;");
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_err());
        if let Err(ParseError::UnexpectedToken { found, .. }) = result {
            assert_eq!(found, TokenType::Semicolon);
        }
    }

    #[test]
    fn test_parse_array_declaration() {
        let tokens = tokenize_code("int arr[10];");
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(ast.node_type, NodeType::Program);
        if let crate::ast::NodeData::Program { statements } = &ast.data {
            assert_eq!(statements.len(), 1);
            assert_eq!(statements[0].node_type, NodeType::VarDecl);
        }
    }

    #[test]
    fn test_parse_struct_definition() {
        let code = r#"
struct Point:
    int x
    int y
"#;
        let tokens = tokenize_code(code);
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(ast.node_type, NodeType::Program);
        if let crate::ast::NodeData::Program { statements } = &ast.data {
            assert_eq!(statements.len(), 1);
            assert_eq!(statements[0].node_type, NodeType::StructDef);
        }
    }

    #[test]
    fn test_parse_class_definition() {
        let code = "class Car { string brand; int year; }";
        let tokens = tokenize_code(code);
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(ast.node_type, NodeType::Program);
        if let crate::ast::NodeData::Program { statements } = &ast.data {
            assert_eq!(statements.len(), 1);
            assert_eq!(statements[0].node_type, NodeType::ClassDef);
        }
    }

    #[test]
    fn test_parse_import_statement() {
        let tokens = tokenize_code("import abs from math;");
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(ast.node_type, NodeType::Program);
        if let crate::ast::NodeData::Program { statements } = &ast.data {
            assert_eq!(statements.len(), 1);
            assert_eq!(statements[0].node_type, NodeType::ImportStmt);
        }
    }
}