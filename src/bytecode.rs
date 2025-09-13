#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    LoadInt(i64),
    LoadFloat(f64),
    LoadString(String),
    LoadChar(char),
    LoadBool(bool),
    LoadNull,
    LoadLocal(u16),
    StoreLocal(u16),
    LoadGlobal(String),
    StoreGlobal(String),
    NewArray(u16),
    ArrayLoad,
    ArrayStore,
    ArrayLength,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    PreIncrement,
    PostIncrement,
    PreDecrement,
    PostDecrement,
    CastToInt,
    CastToFloat,
    CastToDouble,
    CastToString,
    CastToBool,
    Jump(u32),
    JumpIfTrue(u32),
    JumpIfFalse(u32),
    Call(String, u8),
    Return,
    ReturnValue,
    Print,
    Pop,
    Dup,
    Swap,
    Halt,
    TernarySelect,
    SwitchTable(Vec<(i64, u32)>, u32),
    Debug(String),
}

#[derive(Debug, Clone)]
pub struct BytecodeProgram {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
    pub globals: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Int(i64),
    Float(f64), 
    String(String),
    Char(char),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub arity: u8,
    pub locals_count: u16,
    pub start_address: u32,
    pub end_address: u32,
}

#[derive(Debug, Clone)]
pub struct LocalVariable {
    pub name: String,
    pub data_type: crate::ast::DataType,
    pub slot: u16,
}

impl BytecodeProgram {
    pub fn new() -> Self {
        BytecodeProgram {
            instructions: Vec::new(),
            constants: Vec::new(),
            functions: Vec::new(),
            globals: Vec::new(),
        }
    }
    
    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
    
    pub fn add_constant(&mut self, constant: Constant) -> u16 {
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    
    pub fn add_function(&mut self, function: Function) {
        self.functions.push(function);
    }
    
    pub fn current_address(&self) -> u32 {
        self.instructions.len() as u32
    }
}

pub trait Compilable {
    fn compile(&self, program: &mut BytecodeProgram) -> Result<(), String>;
}

#[derive(Debug, Clone)]
pub struct DebugInfo {
    pub line: u32,
    pub column: u32,
    pub source_file: String,
}

pub type InstructionPointer = u32;
pub type StackIndex = u16;