use crate::ast::{DataType, Node};
use crate::bytecode::BytecodeProgram;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq)]
pub enum ImportType {
    Module(String),
    Wildcard(String),
    Selective(String, Vec<String>),
    Aliased(String, String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExportType {
    Function(String, Node),
    Variable(String, DataType, Option<Node>),
    List(Vec<String>),
    Reexport(String),
}

#[derive(Debug, Clone)]
pub struct ImportDeclaration {
    pub import_type: ImportType,
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone)]
pub struct ExportDeclaration {
    pub export_type: ExportType,
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub file_path: PathBuf,
    pub imports: Vec<ImportDeclaration>,
    pub exports: HashMap<String, ExportedSymbol>,
    pub bytecode: BytecodeProgram,
    pub dependencies: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum ExportedSymbol {
    Function {
        name: String,
        arity: u8,
        bytecode_address: u32,
        return_type: DataType,
        parameters: Vec<(String, DataType)>,
    },
    Variable {
        name: String,
        data_type: DataType,
        value: Option<crate::vm::Value>,
        is_constant: bool,
    },
    Type {
        name: String,
        definition: DataType,
    },
}

#[derive(Debug)]
pub struct ModuleRegistry {
    modules: HashMap<String, Module>,
    search_paths: Vec<PathBuf>,
    stdlib_path: PathBuf,
}

impl ModuleRegistry {
    pub fn new() -> Self {
        let mut registry = ModuleRegistry {
            modules: HashMap::new(),
            search_paths: vec![
                PathBuf::from("."),
                PathBuf::from("./modules"),
                PathBuf::from("./lib"),
            ],
            stdlib_path: PathBuf::from("./stdlib"),
        };
        
        registry.load_stdlib_modules();
        registry
    }
    
    pub fn add_search_path(&mut self, path: PathBuf) {
        if !self.search_paths.contains(&path) {
            self.search_paths.push(path);
        }
    }
    
    pub fn resolve_module(&self, module_name: &str) -> Option<PathBuf> {
        if module_name.starts_with("std.") {
            let stdlib_module = module_name.strip_prefix("std.").unwrap();
            let stdlib_path = self.stdlib_path.join(format!("{}.softc", stdlib_module));
            if stdlib_path.exists() {
                return Some(stdlib_path);
            }
        }
        
        for search_path in &self.search_paths {
            let module_path = search_path.join(format!("{}.softc", module_name));
            if module_path.exists() {
                return Some(module_path);
            }
            
            let index_path = search_path.join(module_name).join("index.softc");
            if index_path.exists() {
                return Some(index_path);
            }
        }
        
        None
    }
    
    pub fn load_module(&mut self, module_name: &str) -> Result<&Module, String> {
        if self.modules.contains_key(module_name) {
            return Ok(self.modules.get(module_name).unwrap());
        }
        
        let module_path = self.resolve_module(module_name)
            .ok_or_else(|| format!("Module not found: {}", module_name))?;
        
        let source = fs::read_to_string(&module_path)
            .map_err(|e| format!("Error reading module {}: {}", module_name, e))?;
        
        let module = self.compile_module(module_name, module_path, source)?;
        
        for dependency in &module.dependencies.clone() {
            self.load_module(dependency)?;
        }
        
        self.modules.insert(module_name.to_string(), module);
        
        Ok(self.modules.get(module_name).unwrap())
    }
    
    fn compile_module(&self, name: &str, path: PathBuf, source: String) -> Result<Module, String> {
        use crate::tokenizer::Tokenizer;
        use crate::parser::Parser;
        use crate::multi_pass_compiler::MultiPassCompiler;
        
        let mut tokenizer = Tokenizer::new(&source);
        let tokens = tokenizer.tokenize()
            .map_err(|e| format!("Tokenization error in {}: {}", name, e))?;
        
        let mut parser = Parser::new(tokens);
        let ast = parser.parse()
            .map_err(|e| format!("Parse error in {}: {}", name, e))?;
        
        let (imports, exports, dependencies) = self.extract_module_info(&ast)?;
        
        let mut compiler = MultiPassCompiler::new();
        let (bytecode, _function_addresses) = compiler.compile(&ast)
            .map_err(|e| format!("Compilation error in {}: {}", name, e))?;
        
        Ok(Module {
            name: name.to_string(),
            file_path: path,
            imports,
            exports,
            bytecode,
            dependencies,
        })
    }
    
    fn extract_module_info(&self, ast: &Node) -> Result<(Vec<ImportDeclaration>, HashMap<String, ExportedSymbol>, Vec<String>), String> {
        let imports = Vec::new();
        let exports = HashMap::new();
        let dependencies = Vec::new();
        
        // TODO: Implementar extração real quando AST suportar import/export
        Ok((imports, exports, dependencies))
    }
    
    fn load_stdlib_modules(&mut self) {
        self.create_virtual_math_module();
        self.create_virtual_io_module();
        self.create_virtual_string_module();
    }
    
    fn create_virtual_math_module(&mut self) {
        let mut exports = HashMap::new();
        
        exports.insert("abs".to_string(), ExportedSymbol::Function {
            name: "abs".to_string(),
            arity: 1,
            bytecode_address: 0,
            return_type: DataType::Int,
            parameters: vec![("x".to_string(), DataType::Int)],
        });
        
        exports.insert("max".to_string(), ExportedSymbol::Function {
            name: "max".to_string(),
            arity: 2,
            bytecode_address: 0,
            return_type: DataType::Int,
            parameters: vec![
                ("a".to_string(), DataType::Int),
                ("b".to_string(), DataType::Int),
            ],
        });
        
        exports.insert("min".to_string(), ExportedSymbol::Function {
            name: "min".to_string(),
            arity: 2,
            bytecode_address: 0,
            return_type: DataType::Int,
            parameters: vec![
                ("a".to_string(), DataType::Int),
                ("b".to_string(), DataType::Int),
            ],
        });
        
        exports.insert("sqrt".to_string(), ExportedSymbol::Function {
            name: "sqrt".to_string(),
            arity: 1,
            bytecode_address: 0,
            return_type: DataType::Float,
            parameters: vec![("x".to_string(), DataType::Float)],
        });
        
        exports.insert("pow".to_string(), ExportedSymbol::Function {
            name: "pow".to_string(),
            arity: 2,
            bytecode_address: 0,
            return_type: DataType::Float,
            parameters: vec![
                ("base".to_string(), DataType::Float),
                ("exp".to_string(), DataType::Float),
            ],
        });
        
        exports.insert("PI".to_string(), ExportedSymbol::Variable {
            name: "PI".to_string(),
            data_type: DataType::Float,
            value: Some(crate::vm::Value::Float(std::f64::consts::PI)),
            is_constant: true,
        });
        
        exports.insert("E".to_string(), ExportedSymbol::Variable {
            name: "E".to_string(),
            data_type: DataType::Float,
            value: Some(crate::vm::Value::Float(std::f64::consts::E)),
            is_constant: true,
        });
        
        let math_module = Module {
            name: "std.math".to_string(),
            file_path: PathBuf::from("virtual://std.math"),
            imports: Vec::new(),
            exports,
            bytecode: BytecodeProgram::new(),
            dependencies: Vec::new(),
        };
        
        self.modules.insert("std.math".to_string(), math_module);
    }
    
    fn create_virtual_io_module(&mut self) {
        let mut exports = HashMap::new();
        
        exports.insert("print".to_string(), ExportedSymbol::Function {
            name: "print".to_string(),
            arity: 1,
            bytecode_address: 0,
            return_type: DataType::Void,
            parameters: vec![("value".to_string(), DataType::String)],
        });
        
        exports.insert("println".to_string(), ExportedSymbol::Function {
            name: "println".to_string(),
            arity: 1,
            bytecode_address: 0,
            return_type: DataType::Void,
            parameters: vec![("value".to_string(), DataType::String)],
        });
        
        let io_module = Module {
            name: "std.io".to_string(),
            file_path: PathBuf::from("virtual://std.io"),
            imports: Vec::new(),
            exports,
            bytecode: BytecodeProgram::new(),
            dependencies: Vec::new(),
        };
        
        self.modules.insert("std.io".to_string(), io_module);
    }
    
    fn create_virtual_string_module(&mut self) {
        let mut exports = HashMap::new();
        
        exports.insert("len".to_string(), ExportedSymbol::Function {
            name: "len".to_string(),
            arity: 1,
            bytecode_address: 0,
            return_type: DataType::Int,
            parameters: vec![("s".to_string(), DataType::String)],
        });
        
        let string_module = Module {
            name: "std.string".to_string(),
            file_path: PathBuf::from("virtual://std.string"),
            imports: Vec::new(),
            exports,
            bytecode: BytecodeProgram::new(),
            dependencies: Vec::new(),
        };
        
        self.modules.insert("std.string".to_string(), string_module);
    }
    
    pub fn list_modules(&self) -> Vec<&str> {
        self.modules.keys().map(|s| s.as_str()).collect()
    }
    
    pub fn get_symbol(&self, module_name: &str, symbol_name: &str) -> Option<&ExportedSymbol> {
        self.modules
            .get(module_name)?
            .exports
            .get(symbol_name)
    }
    
    pub fn resolve_symbol<'a>(&'a self, symbol_name: &'a str, current_module: Option<&str>) -> Option<(&'a str, &'a ExportedSymbol)> {
        if let Some(dot_pos) = symbol_name.find('.') {
            let module_name = &symbol_name[..dot_pos];
            let symbol_name = &symbol_name[dot_pos + 1..];
            
            if let Some(symbol) = self.get_symbol(module_name, symbol_name) {
                return Some((module_name, symbol));
            }
        }
        
        if let Some(current_module) = current_module {
            if let Some(module) = self.modules.get(current_module) {
                for import in &module.imports {
                    match &import.import_type {
                        ImportType::Module(mod_name) => {
                            if let Some(symbol) = self.get_symbol(mod_name, symbol_name) {
                                return Some((mod_name, symbol));
                            }
                        },
                        ImportType::Wildcard(mod_name) => {
                            if let Some(symbol) = self.get_symbol(mod_name, symbol_name) {
                                return Some((mod_name, symbol));
                            }
                        },
                        ImportType::Selective(mod_name, symbols) => {
                            if symbols.contains(&symbol_name.to_string()) {
                                if let Some(symbol) = self.get_symbol(mod_name, symbol_name) {
                                    return Some((mod_name, symbol));
                                }
                            }
                        },
                        ImportType::Aliased(mod_name, alias) => {
                            if alias == symbol_name {
                                // TODO: Retornar módulo inteiro como símbolo
                            }
                        },
                    }
                }
            }
        }
        
        None
    }
}