#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
    Int,
    Float,
    Double,
    Char,
    String,
    Bool,
    Void,
    Array(Box<DataType>, Vec<usize>),
    Struct(String),
    Class(String),
}

impl DataType {
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            DataType::Int
            | DataType::Float
            | DataType::Double
            | DataType::Char
            | DataType::String
            | DataType::Bool
        )
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, DataType::Int | DataType::Float | DataType::Double)
    }

    pub fn is_reference(&self) -> bool {
        matches!(self, DataType::Array(_, _) | DataType::Struct(_) | DataType::Class(_))
    }

    pub fn size_bytes(&self) -> Option<usize> {
        match self {
            DataType::Int => Some(8),
            DataType::Float => Some(4),
            DataType::Double => Some(8),
            DataType::Char => Some(1),
            DataType::Bool => Some(1),
            DataType::String => None,
            _ => None,
        }
    }
}