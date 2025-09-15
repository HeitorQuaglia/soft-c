use super::{DataType, BinaryOpType, UnaryOpType, AssignOpType, Node};

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
}

impl LiteralValue {
    pub fn data_type(&self) -> DataType {
        match self {
            LiteralValue::Int(_) => DataType::Int,
            LiteralValue::Float(_) => DataType::Float,
            LiteralValue::String(_) => DataType::String,
            LiteralValue::Char(_) => DataType::Char,
            LiteralValue::Bool(_) => DataType::Bool,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub data_type: DataType,
    pub name: String,
}

impl Parameter {
    pub fn new(data_type: DataType, name: String) -> Self {
        Self { data_type, name }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldValue {
    pub field_name: String,
    pub value: Box<Node>,
}

impl FieldValue {
    pub fn new(field_name: String, value: Box<Node>) -> Self {
        Self { field_name, value }
    }
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
pub enum NodeData {
    Program {
        statements: Vec<Node>,
    },
    Block {
        statements: Vec<Node>,
    },
    ModuleDecl {
        name: String,
        body: Box<Node>,
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
    StructDef {
        name: String,
        fields: Vec<Parameter>,
    },
    ClassDef {
        name: String,
        fields: Vec<Parameter>,
        constructor: Option<Box<Node>>,
    },
    Constructor {
        parameters: Vec<Parameter>,
        body: Box<Node>,
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
    MemberAccess {
        object: Box<Node>,
        member_name: String,
    },
    Assignment {
        op: AssignOpType,
        target: Box<Node>,
        value: Box<Node>,
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
    ExprStmt {
        expr: Box<Node>,
    },
    StructLiteral {
        struct_name: String,
        field_values: Vec<FieldValue>,
    },
    ImportStmt {
        import_type: ImportType,
    },
    ExportStmt {
        export_type: ExportType,
    },
}