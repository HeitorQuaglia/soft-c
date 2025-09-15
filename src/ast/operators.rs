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

impl BinaryOpType {
    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self,
            BinaryOpType::Add
            | BinaryOpType::Sub
            | BinaryOpType::Mul
            | BinaryOpType::Div
            | BinaryOpType::Mod
        )
    }

    pub fn is_comparison(&self) -> bool {
        matches!(
            self,
            BinaryOpType::Equal
            | BinaryOpType::NotEqual
            | BinaryOpType::Less
            | BinaryOpType::Greater
            | BinaryOpType::LessEqual
            | BinaryOpType::GreaterEqual
        )
    }

    pub fn is_logical(&self) -> bool {
        matches!(self, BinaryOpType::LogicalAnd | BinaryOpType::LogicalOr)
    }

    pub fn precedence(&self) -> u8 {
        match self {
            BinaryOpType::LogicalOr => 1,
            BinaryOpType::LogicalAnd => 2,
            BinaryOpType::Equal | BinaryOpType::NotEqual => 3,
            BinaryOpType::Less | BinaryOpType::Greater | BinaryOpType::LessEqual | BinaryOpType::GreaterEqual => 4,
            BinaryOpType::Add | BinaryOpType::Sub => 5,
            BinaryOpType::Mul | BinaryOpType::Div | BinaryOpType::Mod => 6,
        }
    }

    pub fn symbol(&self) -> &'static str {
        match self {
            BinaryOpType::Add => "+",
            BinaryOpType::Sub => "-",
            BinaryOpType::Mul => "*",
            BinaryOpType::Div => "/",
            BinaryOpType::Mod => "%",
            BinaryOpType::Equal => "==",
            BinaryOpType::NotEqual => "!=",
            BinaryOpType::Less => "<",
            BinaryOpType::Greater => ">",
            BinaryOpType::LessEqual => "<=",
            BinaryOpType::GreaterEqual => ">=",
            BinaryOpType::LogicalAnd => "&&",
            BinaryOpType::LogicalOr => "||",
        }
    }
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

impl UnaryOpType {
    pub fn is_prefix(&self) -> bool {
        matches!(
            self,
            UnaryOpType::Minus | UnaryOpType::Plus | UnaryOpType::LogicalNot | UnaryOpType::PreIncrement | UnaryOpType::PreDecrement
        )
    }

    pub fn is_postfix(&self) -> bool {
        matches!(self, UnaryOpType::PostIncrement | UnaryOpType::PostDecrement)
    }

    pub fn symbol(&self) -> &'static str {
        match self {
            UnaryOpType::Minus => "-",
            UnaryOpType::Plus => "+",
            UnaryOpType::LogicalNot => "!",
            UnaryOpType::PreIncrement | UnaryOpType::PostIncrement => "++",
            UnaryOpType::PreDecrement | UnaryOpType::PostDecrement => "--",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignOpType {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
}

impl AssignOpType {
    pub fn symbol(&self) -> &'static str {
        match self {
            AssignOpType::Assign => "=",
            AssignOpType::AddAssign => "+=",
            AssignOpType::SubAssign => "-=",
            AssignOpType::MulAssign => "*=",
            AssignOpType::DivAssign => "/=",
        }
    }

    pub fn to_binary_op(&self) -> Option<BinaryOpType> {
        match self {
            AssignOpType::Assign => None,
            AssignOpType::AddAssign => Some(BinaryOpType::Add),
            AssignOpType::SubAssign => Some(BinaryOpType::Sub),
            AssignOpType::MulAssign => Some(BinaryOpType::Mul),
            AssignOpType::DivAssign => Some(BinaryOpType::Div),
        }
    }
}