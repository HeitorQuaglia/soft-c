use crate::ast::DataType;

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub arity: u8,
    pub locals_count: u16,
    pub start_address: u32,
    pub end_address: u32,
}

impl Function {
    pub fn new(name: String, arity: u8, locals_count: u16, start_address: u32, end_address: u32) -> Self {
        Self {
            name,
            arity,
            locals_count,
            start_address,
            end_address,
        }
    }

    pub fn instruction_count(&self) -> u32 {
        self.end_address - self.start_address
    }

    pub fn is_nullary(&self) -> bool {
        self.arity == 0
    }

    pub fn is_unary(&self) -> bool {
        self.arity == 1
    }

    pub fn is_binary(&self) -> bool {
        self.arity == 2
    }

    pub fn is_variadic(&self) -> bool {
        // For now, we don't support variadic functions, but this is a placeholder
        false
    }

    pub fn signature(&self) -> String {
        format!("{}({})", self.name, self.arity)
    }
}

#[derive(Debug, Clone)]
pub struct LocalVariable {
    pub name: String,
    pub data_type: DataType,
    pub slot: u16,
}

impl LocalVariable {
    pub fn new(name: String, data_type: DataType, slot: u16) -> Self {
        Self {
            name,
            data_type,
            slot,
        }
    }

    pub fn is_primitive(&self) -> bool {
        self.data_type.is_primitive()
    }

    pub fn is_reference(&self) -> bool {
        self.data_type.is_reference()
    }

    pub fn type_name(&self) -> String {
        format!("{:?}", self.data_type)
    }
}