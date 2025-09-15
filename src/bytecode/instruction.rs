#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // === LOAD OPERATIONS ===
    LoadInt(i64),
    LoadFloat(f64),
    LoadString(String),
    LoadChar(char),
    LoadBool(bool),
    LoadNull,

    // === VARIABLE OPERATIONS ===
    LoadLocal(u16),
    StoreLocal(u16),
    LoadGlobal(String),
    StoreGlobal(String),

    // === ARRAY OPERATIONS ===
    NewArray(u16),
    ArrayLoad,
    ArrayStore,
    ArrayLength,

    // === ARITHMETIC OPERATIONS ===
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,

    // === COMPARISON OPERATIONS ===
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,

    // === LOGICAL OPERATIONS ===
    LogicalAnd,
    LogicalOr,
    LogicalNot,

    // === ASSIGNMENT OPERATIONS ===
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,

    // === INCREMENT/DECREMENT OPERATIONS ===
    PreIncrement,
    PostIncrement,
    PreDecrement,
    PostDecrement,

    // === TYPE CASTING OPERATIONS ===
    CastToInt,
    CastToFloat,
    CastToDouble,
    CastToString,
    CastToBool,

    // === CONTROL FLOW OPERATIONS ===
    Jump(u32),
    JumpIfTrue(u32),
    JumpIfFalse(u32),

    // === FUNCTION OPERATIONS ===
    Call(String, u8),
    Return,
    ReturnValue,
    ReturnMain,
    CallFunction(String, u8),
    CallMain,

    // === MODULE SYSTEM OPERATIONS ===
    ImportModule(String),
    ImportSymbol(String, String),
    ImportWildcard(String),
    ExportSymbol(String),
    LoadModuleSymbol(String, String),

    // === OBJECT OPERATIONS ===
    NewStruct(String),
    NewObject(String),
    GetField(String),
    SetField(String),
    GetObjectField(String),
    SetObjectField(String),
    CallConstructor(String, u8),

    // === CONTEXT OPERATIONS ===
    LoadThis,
    PushThisContext,
    PopThisContext,

    // === STACK OPERATIONS ===
    Pop,
    Dup,
    Swap,

    // === CONTROL OPERATIONS ===
    TernarySelect,
    SwitchTable(Vec<(i64, u32)>, u32),

    // === UTILITY OPERATIONS ===
    Print,
    Debug(String),
    Halt,
}

impl Instruction {
    pub fn is_load(&self) -> bool {
        matches!(
            self,
            Instruction::LoadInt(_)
            | Instruction::LoadFloat(_)
            | Instruction::LoadString(_)
            | Instruction::LoadChar(_)
            | Instruction::LoadBool(_)
            | Instruction::LoadNull
            | Instruction::LoadLocal(_)
            | Instruction::LoadGlobal(_)
        )
    }

    pub fn is_store(&self) -> bool {
        matches!(
            self,
            Instruction::StoreLocal(_) | Instruction::StoreGlobal(_)
        )
    }

    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self,
            Instruction::Add
            | Instruction::Sub
            | Instruction::Mul
            | Instruction::Div
            | Instruction::Mod
            | Instruction::Neg
        )
    }

    pub fn is_comparison(&self) -> bool {
        matches!(
            self,
            Instruction::Equal
            | Instruction::NotEqual
            | Instruction::Less
            | Instruction::Greater
            | Instruction::LessEqual
            | Instruction::GreaterEqual
        )
    }

    pub fn is_logical(&self) -> bool {
        matches!(
            self,
            Instruction::LogicalAnd | Instruction::LogicalOr | Instruction::LogicalNot
        )
    }

    pub fn is_jump(&self) -> bool {
        matches!(
            self,
            Instruction::Jump(_) | Instruction::JumpIfTrue(_) | Instruction::JumpIfFalse(_)
        )
    }

    pub fn is_call(&self) -> bool {
        matches!(
            self,
            Instruction::Call(_, _) | Instruction::CallFunction(_, _) | Instruction::CallConstructor(_, _) | Instruction::CallMain
        )
    }

    pub fn is_return(&self) -> bool {
        matches!(self, Instruction::Return | Instruction::ReturnValue | Instruction::ReturnMain)
    }

    pub fn is_stack_op(&self) -> bool {
        matches!(self, Instruction::Pop | Instruction::Dup | Instruction::Swap)
    }

    pub fn is_module_op(&self) -> bool {
        matches!(
            self,
            Instruction::ImportModule(_)
            | Instruction::ImportSymbol(_, _)
            | Instruction::ImportWildcard(_)
            | Instruction::ExportSymbol(_)
            | Instruction::LoadModuleSymbol(_, _)
        )
    }

    pub fn is_object_op(&self) -> bool {
        matches!(
            self,
            Instruction::NewStruct(_)
            | Instruction::NewObject(_)
            | Instruction::GetField(_)
            | Instruction::SetField(_)
            | Instruction::GetObjectField(_)
            | Instruction::SetObjectField(_)
            | Instruction::LoadThis
            | Instruction::PushThisContext
            | Instruction::PopThisContext
        )
    }

    pub fn call_arity(&self) -> Option<u8> {
        match self {
            Instruction::Call(_, arity) => Some(*arity),
            Instruction::CallFunction(_, arity) => Some(*arity),
            Instruction::CallConstructor(_, arity) => Some(*arity),
            _ => None,
        }
    }

    pub fn jump_target(&self) -> Option<u32> {
        match self {
            Instruction::Jump(target) => Some(*target),
            Instruction::JumpIfTrue(target) => Some(*target),
            Instruction::JumpIfFalse(target) => Some(*target),
            _ => None,
        }
    }

    pub fn description(&self) -> &'static str {
        match self {
            Instruction::LoadInt(_) => "Load integer constant",
            Instruction::LoadFloat(_) => "Load float constant",
            Instruction::LoadString(_) => "Load string constant",
            Instruction::LoadChar(_) => "Load character constant",
            Instruction::LoadBool(_) => "Load boolean constant",
            Instruction::LoadNull => "Load null value",
            Instruction::LoadLocal(_) => "Load local variable",
            Instruction::StoreLocal(_) => "Store to local variable",
            Instruction::LoadGlobal(_) => "Load global variable",
            Instruction::StoreGlobal(_) => "Store to global variable",
            Instruction::Add => "Add two values",
            Instruction::Sub => "Subtract two values",
            Instruction::Mul => "Multiply two values",
            Instruction::Div => "Divide two values",
            Instruction::Mod => "Modulo operation",
            Instruction::Neg => "Negate value",
            Instruction::Equal => "Test equality",
            Instruction::NotEqual => "Test inequality",
            Instruction::Less => "Test less than",
            Instruction::Greater => "Test greater than",
            Instruction::LessEqual => "Test less than or equal",
            Instruction::GreaterEqual => "Test greater than or equal",
            Instruction::LogicalAnd => "Logical AND",
            Instruction::LogicalOr => "Logical OR",
            Instruction::LogicalNot => "Logical NOT",
            Instruction::Jump(_) => "Unconditional jump",
            Instruction::JumpIfTrue(_) => "Jump if true",
            Instruction::JumpIfFalse(_) => "Jump if false",
            Instruction::Call(_, _) => "Call function",
            Instruction::CallFunction(_, _) => "Call user-defined function",
            Instruction::CallMain => "Call main entry point",
            Instruction::Return => "Return from function",
            Instruction::ReturnValue => "Return value from function",
            Instruction::ReturnMain => "Return from main entry point",
            Instruction::Print => "Print value",
            Instruction::Pop => "Pop from stack",
            Instruction::Dup => "Duplicate top of stack",
            Instruction::Swap => "Swap two top stack values",
            Instruction::Halt => "Halt execution",
            _ => "Other instruction",
        }
    }
}