use crate::ast::{DataType, Node, Parameter};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum SymbolType {
    Function {
        return_type: DataType,
        parameters: Vec<Parameter>,
        body: Box<Node>,
        start_address: Option<u32>,
    },
    Constructor {
        class_name: String,
        parameters: Vec<Parameter>,
        body: Box<Node>,
        start_address: Option<u32>,
    },
    Class {
        fields: Vec<Parameter>,
        constructor: Option<Box<Node>>,
        methods: HashMap<String, SymbolType>,
    },
    Variable {
        data_type: DataType,
        is_global: bool,
        address: Option<u32>,
    },
    Module {
        symbols: HashMap<String, SymbolType>,
    },
}

impl SymbolType {
    pub fn is_callable(&self) -> bool {
        matches!(self, SymbolType::Function { .. } | SymbolType::Constructor { .. })
    }

    pub fn has_body(&self) -> bool {
        matches!(self, SymbolType::Function { .. } | SymbolType::Constructor { .. })
    }

    pub fn arity(&self) -> Option<usize> {
        match self {
            SymbolType::Function { parameters, .. } => Some(parameters.len()),
            SymbolType::Constructor { parameters, .. } => Some(parameters.len()),
            _ => None,
        }
    }

    pub fn start_address(&self) -> Option<u32> {
        match self {
            SymbolType::Function { start_address, .. } => *start_address,
            SymbolType::Constructor { start_address, .. } => *start_address,
            _ => None,
        }
    }

    pub fn set_start_address(&mut self, address: u32) -> Result<(), String> {
        match self {
            SymbolType::Function { start_address, .. } => {
                *start_address = Some(address);
                Ok(())
            },
            SymbolType::Constructor { start_address, .. } => {
                *start_address = Some(address);
                Ok(())
            },
            _ => Err("Cannot set start address for non-callable symbol".to_string()),
        }
    }

    pub fn body(&self) -> Option<&Node> {
        match self {
            SymbolType::Function { body, .. } => Some(body),
            SymbolType::Constructor { body, .. } => Some(body),
            _ => None,
        }
    }

    pub fn parameters(&self) -> Option<&Vec<Parameter>> {
        match self {
            SymbolType::Function { parameters, .. } => Some(parameters),
            SymbolType::Constructor { parameters, .. } => Some(parameters),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub symbol_type: SymbolType,
    pub module_name: Option<String>,
    pub is_public: bool,
}

impl Symbol {
    pub fn new(name: String, symbol_type: SymbolType, module_name: Option<String>, is_public: bool) -> Self {
        Self {
            name,
            symbol_type,
            module_name,
            is_public,
        }
    }

    pub fn function(
        name: String,
        return_type: DataType,
        parameters: Vec<Parameter>,
        body: Box<Node>,
        module_name: Option<String>,
    ) -> Self {
        Self::new(
            name,
            SymbolType::Function {
                return_type,
                parameters,
                body,
                start_address: None,
            },
            module_name,
            true,
        )
    }

    pub fn constructor(
        name: String,
        class_name: String,
        parameters: Vec<Parameter>,
        body: Box<Node>,
        module_name: Option<String>,
    ) -> Self {
        Self::new(
            name,
            SymbolType::Constructor {
                class_name,
                parameters,
                body,
                start_address: None,
            },
            module_name,
            true,
        )
    }

    pub fn class(
        name: String,
        fields: Vec<Parameter>,
        constructor: Option<Box<Node>>,
        module_name: Option<String>,
    ) -> Self {
        Self::new(
            name,
            SymbolType::Class {
                fields,
                constructor,
                methods: HashMap::new(),
            },
            module_name,
            true,
        )
    }

    pub fn variable(
        name: String,
        data_type: DataType,
        is_global: bool,
        module_name: Option<String>,
    ) -> Self {
        Self::new(
            name,
            SymbolType::Variable {
                data_type,
                is_global,
                address: None,
            },
            module_name,
            true,
        )
    }

    pub fn qualified_name(&self) -> String {
        if let Some(module_name) = &self.module_name {
            format!("{}::{}", module_name, self.name)
        } else {
            self.name.clone()
        }
    }

    pub fn is_callable(&self) -> bool {
        self.symbol_type.is_callable()
    }

    pub fn is_function(&self) -> bool {
        matches!(self.symbol_type, SymbolType::Function { .. })
    }

    pub fn is_constructor(&self) -> bool {
        matches!(self.symbol_type, SymbolType::Constructor { .. })
    }

    pub fn is_class(&self) -> bool {
        matches!(self.symbol_type, SymbolType::Class { .. })
    }

    pub fn is_variable(&self) -> bool {
        matches!(self.symbol_type, SymbolType::Variable { .. })
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    pub symbols: HashMap<String, Symbol>,
    pub modules: HashMap<String, HashMap<String, Symbol>>,
    pub current_module: Option<String>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            modules: HashMap::new(),
            current_module: None,
        }
    }

    pub fn enter_module(&mut self, module_name: String) {
        self.current_module = Some(module_name.clone());
        if !self.modules.contains_key(&module_name) {
            self.modules.insert(module_name, HashMap::new());
        }
    }

    pub fn exit_module(&mut self) {
        self.current_module = None;
    }

    fn qualified_name(&self, name: &str) -> String {
        if let Some(module_name) = &self.current_module {
            format!("{}::{}", module_name, name)
        } else {
            name.to_string()
        }
    }

    pub fn add_symbol(&mut self, symbol: Symbol) -> Result<(), String> {
        let symbol_name = symbol.name.clone();
        let qualified_name = self.qualified_name(&symbol_name);

        if self.symbols.contains_key(&qualified_name) {
            return Err(format!("Symbol '{}' already defined", qualified_name));
        }

        if let Some(module_name) = &self.current_module {
            let module_symbols = self.modules.get_mut(module_name).unwrap();
            if module_symbols.contains_key(&symbol_name) {
                return Err(format!("Symbol '{}' already defined in module '{}'", symbol_name, module_name));
            }
            module_symbols.insert(symbol_name, symbol.clone());
        }

        self.symbols.insert(qualified_name, symbol);
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        if let Some(module_name) = &self.current_module {
            let qualified_name = format!("{}::{}", module_name, name);
            if let Some(symbol) = self.symbols.get(&qualified_name) {
                return Some(symbol);
            }
        }

        self.symbols.get(name)
    }

    pub fn lookup_in_module(&self, module_name: &str, symbol_name: &str) -> Option<&Symbol> {
        let qualified_name = format!("{}::{}", module_name, symbol_name);
        self.symbols.get(&qualified_name)
    }

    pub fn get_function_address(&self, name: &str) -> Option<u32> {
        self.lookup(name)
            .and_then(|symbol| symbol.symbol_type.start_address())
    }

    pub fn set_function_address(&mut self, name: &str, address: u32) -> Result<(), String> {
        let qualified_name = self.qualified_name(name);

        if let Some(symbol) = self.symbols.get_mut(&qualified_name) {
            symbol.symbol_type.set_start_address(address)
        } else {
            Err(format!("Function or constructor '{}' not found", name))
        }
    }

    pub fn list_functions(&self) -> Vec<&Symbol> {
        self.symbols
            .values()
            .filter(|symbol| symbol.is_callable())
            .collect()
    }

    pub fn list_constructors(&self) -> Vec<&Symbol> {
        self.symbols
            .values()
            .filter(|symbol| symbol.is_constructor())
            .collect()
    }

    pub fn list_classes(&self) -> Vec<&Symbol> {
        self.symbols
            .values()
            .filter(|symbol| symbol.is_class())
            .collect()
    }

    pub fn list_variables(&self) -> Vec<&Symbol> {
        self.symbols
            .values()
            .filter(|symbol| symbol.is_variable())
            .collect()
    }

    pub fn stats(&self) -> SymbolTableStats {
        let mut stats = SymbolTableStats::default();

        for symbol in self.symbols.values() {
            match &symbol.symbol_type {
                SymbolType::Function { .. } => stats.functions += 1,
                SymbolType::Constructor { .. } => stats.constructors += 1,
                SymbolType::Class { .. } => stats.classes += 1,
                SymbolType::Variable { .. } => stats.variables += 1,
                SymbolType::Module { .. } => stats.modules += 1,
            }
        }

        stats.total_symbols = self.symbols.len();
        stats
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Default)]
pub struct SymbolTableStats {
    pub total_symbols: usize,
    pub functions: usize,
    pub constructors: usize,
    pub classes: usize,
    pub variables: usize,
    pub modules: usize,
}