use super::symbol_table::{SymbolTable, SymbolType};

pub struct DebugPrinter;

impl DebugPrinter {
    pub fn print_symbols(symbol_table: &SymbolTable) {
        println!("=== Symbol Table ===");

        for (name, symbol) in &symbol_table.symbols {
            match &symbol.symbol_type {
                SymbolType::Function { return_type, parameters, .. } => {
                    print!("FUNCTION {}: {} -> {:?}", name, parameters.len(), return_type);
                    if let Some(addr) = symbol_table.get_function_address(name) {
                        print!(" @ 0x{:04X}", addr);
                    }
                    println!();
                },
                SymbolType::Constructor { class_name, parameters, .. } => {
                    print!("CONSTRUCTOR {} for class {}: {} params", name, class_name, parameters.len());
                    if let Some(addr) = symbol_table.get_function_address(name) {
                        print!(" @ 0x{:04X}", addr);
                    }
                    println!();
                },
                SymbolType::Class { fields, .. } => {
                    println!("CLASS {}: {} fields", name, fields.len());
                },
                SymbolType::Variable { data_type, is_global, .. } => {
                    println!("VARIABLE {}: {:?} (global: {})", name, data_type, is_global);
                },
                SymbolType::Module { .. } => {
                    println!("MODULE {}", name);
                },
            }
        }

        println!("==================");
        Self::print_function_addresses(symbol_table);
    }

    fn print_function_addresses(symbol_table: &SymbolTable) {
        println!("=== Function Addresses ===");

        for func in symbol_table.list_functions() {
            match &func.symbol_type {
                SymbolType::Function { .. } => {
                    println!("Function: {}", func.name);
                },
                SymbolType::Constructor { class_name, .. } => {
                    println!("Constructor: {} (for class {})", func.name, class_name);
                },
                _ => {}
            }
        }

        println!("========================");
    }

    pub fn print_stats(symbol_table: &SymbolTable) {
        let stats = symbol_table.stats();

        println!("=== Symbol Table Statistics ===");
        println!("Total symbols: {}", stats.total_symbols);
        println!("Functions: {}", stats.functions);
        println!("Constructors: {}", stats.constructors);
        println!("Classes: {}", stats.classes);
        println!("Variables: {}", stats.variables);
        println!("Modules: {}", stats.modules);
        println!("==============================");
    }

    pub fn print_compilation_progress(pass: u32, description: &str) {
        println!("Pass {}: {}...", pass, description);
    }

    pub fn print_function_compilation(name: &str, is_constructor: bool, address: u32) {
        let callable_type = if is_constructor { "constructor" } else { "function" };
        println!("Compiling {} '{}' at address 0x{:04X}", callable_type, name, address);
    }
}