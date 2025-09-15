use std::collections::HashMap;

#[derive(Debug)]
pub struct ScopeManager {
    local_variables: HashMap<String, u16>,
    local_count: u16,
    scope_stack: Vec<ScopeFrame>,
}

#[derive(Debug, Clone)]
struct ScopeFrame {
    variables: HashMap<String, u16>,
    base_count: u16,
}

impl ScopeManager {
    pub fn new() -> Self {
        Self {
            local_variables: HashMap::new(),
            local_count: 0,
            scope_stack: Vec::new(),
        }
    }

    pub fn clear(&mut self) {
        self.local_variables.clear();
        self.local_count = 0;
        self.scope_stack.clear();
    }

    pub fn enter_scope(&mut self) {
        let frame = ScopeFrame {
            variables: self.local_variables.clone(),
            base_count: self.local_count,
        };
        self.scope_stack.push(frame);
    }

    pub fn exit_scope(&mut self) {
        if let Some(frame) = self.scope_stack.pop() {
            self.local_variables = frame.variables;
            self.local_count = frame.base_count;
        }
    }

    pub fn add_local(&mut self, name: String) -> u16 {
        let slot = self.local_count;
        self.local_variables.insert(name, slot);
        self.local_count += 1;
        slot
    }

    pub fn get_local(&self, name: &str) -> Option<u16> {
        self.local_variables.get(name).copied()
    }

    pub fn local_count(&self) -> u16 {
        self.local_count
    }

    pub fn setup_parameters(&mut self, parameters: &[crate::ast::Parameter]) {
        for param in parameters {
            self.add_local(param.name.clone());
        }
    }

    pub fn setup_constructor(&mut self, parameters: &[crate::ast::Parameter]) {
        self.add_local("this".to_string());

        for param in parameters {
            self.add_local(param.name.clone());
        }
    }

    pub fn is_defined(&self, name: &str) -> bool {
        self.local_variables.contains_key(name)
    }

    pub fn scope_depth(&self) -> usize {
        self.scope_stack.len()
    }

    pub fn current_variables(&self) -> &HashMap<String, u16> {
        &self.local_variables
    }
}

impl Default for ScopeManager {
    fn default() -> Self {
        Self::new()
    }
}