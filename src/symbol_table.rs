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

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub symbol_type: SymbolType,
    pub module_name: Option<String>,
    pub is_public: bool,
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

    pub fn add_symbol(&mut self, symbol: Symbol) -> Result<(), String> {
        let symbol_name = symbol.name.clone();

        if let Some(module_name) = &self.current_module {
            let module_symbols = self.modules.get_mut(module_name).unwrap();
            if module_symbols.contains_key(&symbol_name) {
                return Err(format!("Symbol '{}' already defined in module '{}'", symbol_name, module_name));
            }
            module_symbols.insert(symbol_name.clone(), symbol.clone());
        }

        let qualified_name = if let Some(module_name) = &self.current_module {
            format!("{}::{}", module_name, symbol_name)
        } else {
            symbol_name.clone()
        };

        if self.symbols.contains_key(&qualified_name) {
            return Err(format!("Symbol '{}' already defined", qualified_name));
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
        if let Some(symbol) = self.lookup(name) {
            match &symbol.symbol_type {
                SymbolType::Function { start_address, .. } => *start_address,
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn set_function_address(&mut self, name: &str, address: u32) -> Result<(), String> {
        let qualified_name = if let Some(module_name) = &self.current_module {
            format!("{}::{}", module_name, name)
        } else {
            name.to_string()
        };

        if let Some(symbol) = self.symbols.get_mut(&qualified_name) {
            match &mut symbol.symbol_type {
                SymbolType::Function { start_address, .. } => {
                    *start_address = Some(address);
                    Ok(())
                },
                SymbolType::Constructor { start_address, .. } => {
                    *start_address = Some(address);
                    Ok(())
                },
                _ => Err(format!("Symbol '{}' is not a function or constructor", name)),
            }
        } else {
            Err(format!("Function or constructor '{}' not found", name))
        }
    }

    pub fn list_functions(&self) -> Vec<&Symbol> {
        self.symbols
            .values()
            .filter(|symbol| matches!(symbol.symbol_type, SymbolType::Function { .. } | SymbolType::Constructor { .. }))
            .collect()
    }

    pub fn list_constructors(&self) -> Vec<&Symbol> {
        self.symbols
            .values()
            .filter(|symbol| matches!(symbol.symbol_type, SymbolType::Constructor { .. }))
            .collect()
    }

    pub fn list_classes(&self) -> Vec<&Symbol> {
        self.symbols
            .values()
            .filter(|symbol| matches!(symbol.symbol_type, SymbolType::Class { .. }))
            .collect()
    }
}