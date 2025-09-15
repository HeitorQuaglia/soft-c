#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
    // Literals
    Int,
    Float,
    String,
    Char,
    Bool,
    Identifier,

    // Keywords - Data Types
    KwInt,
    KwFloat,
    KwDouble,
    KwChar,
    KwString,
    KwBool,

    // Keywords - Control Flow
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

    // Keywords - Values
    KwTrue,
    KwFalse,

    // Keywords - Object-Oriented
    KwClass,
    KwConstructor,
    KwNew,

    // Keywords - Modules
    KwImport,
    KwExport,
    KwModule,
    KwFrom,
    KwAs,

    // Keywords - Other
    KwPrint,
    KwType,
    KwStruct,

    // Arithmetic Operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,

    // Assignment Operators
    Assign,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,

    // Unary Operators
    Increment,
    Decrement,

    // Comparison Operators
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,

    // Logical Operators
    LogicalAnd,
    LogicalOr,
    LogicalNot,

    // Delimiters
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    // Punctuation
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

impl TokenType {
    pub fn is_keyword(&self) -> bool {
        matches!(self,
            TokenType::KwInt | TokenType::KwFloat | TokenType::KwDouble |
            TokenType::KwChar | TokenType::KwString | TokenType::KwBool |
            TokenType::KwIf | TokenType::KwElse | TokenType::KwWhile |
            TokenType::KwFor | TokenType::KwDo | TokenType::KwSwitch |
            TokenType::KwCase | TokenType::KwDefault | TokenType::KwBreak |
            TokenType::KwContinue | TokenType::KwReturn | TokenType::KwTrue |
            TokenType::KwFalse | TokenType::KwClass | TokenType::KwConstructor |
            TokenType::KwNew | TokenType::KwImport | TokenType::KwExport |
            TokenType::KwModule | TokenType::KwFrom | TokenType::KwAs |
            TokenType::KwPrint | TokenType::KwType | TokenType::KwStruct
        )
    }

    pub fn is_operator(&self) -> bool {
        matches!(self,
            TokenType::Plus | TokenType::Minus | TokenType::Multiply |
            TokenType::Divide | TokenType::Modulo | TokenType::Assign |
            TokenType::PlusAssign | TokenType::MinusAssign |
            TokenType::MultiplyAssign | TokenType::DivideAssign |
            TokenType::Increment | TokenType::Decrement |
            TokenType::Equal | TokenType::NotEqual | TokenType::Less |
            TokenType::Greater | TokenType::LessEqual | TokenType::GreaterEqual |
            TokenType::LogicalAnd | TokenType::LogicalOr | TokenType::LogicalNot
        )
    }

    pub fn is_literal(&self) -> bool {
        matches!(self,
            TokenType::Int | TokenType::Float | TokenType::String |
            TokenType::Char | TokenType::Bool
        )
    }
}