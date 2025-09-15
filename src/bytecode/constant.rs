#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
}

impl Constant {
    pub fn type_name(&self) -> &'static str {
        match self {
            Constant::Int(_) => "int",
            Constant::Float(_) => "float",
            Constant::String(_) => "string",
            Constant::Char(_) => "char",
            Constant::Bool(_) => "bool",
        }
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, Constant::Int(_) | Constant::Float(_))
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Constant::Int(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Constant::Float(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Constant::String(_))
    }

    pub fn is_char(&self) -> bool {
        matches!(self, Constant::Char(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Constant::Bool(_))
    }

    pub fn to_string(&self) -> String {
        match self {
            Constant::Int(i) => i.to_string(),
            Constant::Float(f) => f.to_string(),
            Constant::String(s) => format!("\"{}\"", s),
            Constant::Char(c) => format!("'{}'", c),
            Constant::Bool(b) => b.to_string(),
        }
    }

    pub fn size_bytes(&self) -> usize {
        match self {
            Constant::Int(_) => 8,
            Constant::Float(_) => 8,
            Constant::String(s) => s.len() + 8,
            Constant::Char(_) => 4,
            Constant::Bool(_) => 1,
        }
    }
}