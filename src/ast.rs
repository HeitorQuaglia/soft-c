#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    Program,
    FunctionDef,
    VarDecl,
    Assignment,
    BinaryOp,
    UnaryOp,
    TernaryOp,
    FunctionCall,
    Cast,
    Literal,
    Identifier,
    ArrayLiteral,
    ArrayAccess,
    IfStmt,
    WhileStmt,
    ForStmt,
    DoWhileStmt,
    SwitchStmt,
    CaseStmt,
    BreakStmt,
    ContinueStmt,
    ReturnStmt,
    PrintStmt,
    Block,
    ExprStmt,
    StructDef,
    StructLiteral,
    MemberAccess,
    ImportStmt,
    ExportStmt,
    ModuleDecl,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
    Int,
    Float,
    Double,
    Char,
    String,
    Bool,
    Array(Box<DataType>, Vec<usize>),
    Void,
    Struct(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOpType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOpType {
    Minus,
    Plus,
    LogicalNot,
    PreIncrement,
    PostIncrement,
    PreDecrement,
    PostDecrement,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignOpType {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportType {
    Module(String),
    Wildcard(String),
    Selective(String, Vec<String>),
    Aliased(String, String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExportType {
    Function(String, Box<Node>),
    Variable(String, DataType, Option<Box<Node>>),
    List(Vec<String>),
    Reexport(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub data_type: DataType,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldValue {
    pub field_name: String,
    pub value: Box<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub node_type: NodeType,
    pub data: NodeData,
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeData {
    Program {
        statements: Vec<Node>,
    },
    FunctionDef {
        return_type: DataType,
        name: String,
        parameters: Vec<Parameter>,
        body: Box<Node>,
    },
    VarDecl {
        data_type: DataType,
        name: String,
        array_sizes: Option<Vec<Box<Node>>>,
        init_expr: Option<Box<Node>>,
    },
    Assignment {
        op: AssignOpType,
        target: Box<Node>,
        value: Box<Node>,
    },
    BinaryOp {
        op: BinaryOpType,
        left: Box<Node>,
        right: Box<Node>,
    },
    UnaryOp {
        op: UnaryOpType,
        operand: Box<Node>,
    },
    TernaryOp {
        condition: Box<Node>,
        true_expr: Box<Node>,
        false_expr: Box<Node>,
    },
    FunctionCall {
        name: String,
        args: Vec<Box<Node>>,
    },
    Cast {
        target_type: DataType,
        expr: Box<Node>,
    },
    Literal {
        data_type: DataType,
        value: LiteralValue,
    },
    Identifier {
        name: String,
    },
    ArrayLiteral {
        elements: Vec<Box<Node>>,
    },
    ArrayAccess {
        array: Box<Node>,
        indices: Vec<Box<Node>>,
    },
    IfStmt {
        condition: Box<Node>,
        then_stmt: Box<Node>,
        else_stmt: Option<Box<Node>>,
    },
    WhileStmt {
        condition: Box<Node>,
        body: Box<Node>,
    },
    ForStmt {
        init: Option<Box<Node>>,
        condition: Option<Box<Node>>,
        update: Option<Box<Node>>,
        body: Box<Node>,
    },
    DoWhileStmt {
        body: Box<Node>,
        condition: Box<Node>,
    },
    SwitchStmt {
        expr: Box<Node>,
        cases: Vec<Node>,
    },
    CaseStmt {
        value: Option<Box<Node>>,
        statements: Vec<Node>,
    },
    BreakStmt,
    ContinueStmt,
    ReturnStmt {
        value: Option<Box<Node>>,
    },
    PrintStmt {
        arguments: Vec<Box<Node>>,
    },
    Block {
        statements: Vec<Node>,
    },
    ExprStmt {
        expr: Box<Node>,
    },
    StructDef {
        name: String,
        fields: Vec<Parameter>,
    },
    StructLiteral {
        struct_name: String,
        field_values: Vec<FieldValue>,
    },
    MemberAccess {
        object: Box<Node>,
        member_name: String,
    },
    ImportStmt {
        import_type: ImportType,
    },
    ExportStmt {
        export_type: ExportType,
    },
    ModuleDecl {
        name: String,
        body: Box<Node>,
    },
}

impl Node {
    pub fn new(node_type: NodeType, data: NodeData, line: u32, column: u32) -> Self {
        Node {
            node_type,
            data,
            line,
            column,
        }
    }

    pub fn create_literal(data_type: DataType, value: LiteralValue, line: u32, column: u32) -> Self {
        Node::new(
            NodeType::Literal,
            NodeData::Literal { data_type, value },
            line,
            column,
        )
    }

    pub fn create_identifier(name: String, line: u32, column: u32) -> Self {
        Node::new(
            NodeType::Identifier,
            NodeData::Identifier { name },
            line,
            column,
        )
    }

    pub fn create_binary_op(
        op: BinaryOpType,
        left: Box<Node>,
        right: Box<Node>,
        line: u32,
        column: u32,
    ) -> Self {
        Node::new(
            NodeType::BinaryOp,
            NodeData::BinaryOp { op, left, right },
            line,
            column,
        )
    }

    pub fn create_unary_op(op: UnaryOpType, operand: Box<Node>, line: u32, column: u32) -> Self {
        Node::new(
            NodeType::UnaryOp,
            NodeData::UnaryOp { op, operand },
            line,
            column,
        )
    }

    pub fn create_program(statements: Vec<Node>) -> Self {
        Node::new(
            NodeType::Program,
            NodeData::Program { statements },
            1,
            1,
        )
    }

    pub fn create_block(statements: Vec<Node>, line: u32, column: u32) -> Self {
        Node::new(
            NodeType::Block,
            NodeData::Block { statements },
            line,
            column,
        )
    }
}