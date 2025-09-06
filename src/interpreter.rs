use crate::ast::*;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    UndefinedVariable(String),
    TypeMismatch(String),
    DivisionByZero,
    IndexOutOfBounds,
    InvalidOperation(String),
    Break,
    Continue,
    Return(Value),
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeError::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
            RuntimeError::TypeMismatch(msg) => write!(f, "Type mismatch: {}", msg),
            RuntimeError::DivisionByZero => write!(f, "Division by zero"),
            RuntimeError::IndexOutOfBounds => write!(f, "Index out of bounds"),
            RuntimeError::InvalidOperation(msg) => write!(f, "Invalid operation: {}", msg),
            RuntimeError::Break => write!(f, "Break statement outside loop"),
            RuntimeError::Continue => write!(f, "Continue statement outside loop"),
            RuntimeError::Return(_) => write!(f, "Return statement"),
        }
    }
}

impl std::error::Error for RuntimeError {}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Array(Vec<Value>),
    Struct(HashMap<String, Value>),
    Void,
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Char(c) => *c != '\0',
            Value::Array(a) => !a.is_empty(),
            Value::Struct(s) => !s.is_empty(),
            Value::Void => false,
        }
    }

    pub fn to_int(&self) -> Result<i64, RuntimeError> {
        match self {
            Value::Int(i) => Ok(*i),
            Value::Float(f) => Ok(*f as i64),
            Value::Bool(b) => Ok(if *b { 1 } else { 0 }),
            Value::Char(c) => Ok(*c as i64),
            _ => Err(RuntimeError::TypeMismatch("Cannot convert to int".to_string())),
        }
    }

    pub fn to_float(&self) -> Result<f64, RuntimeError> {
        match self {
            Value::Int(i) => Ok(*i as f64),
            Value::Float(f) => Ok(*f),
            Value::Bool(b) => Ok(if *b { 1.0 } else { 0.0 }),
            Value::Char(c) => Ok(*c as u32 as f64),
            _ => Err(RuntimeError::TypeMismatch("Cannot convert to float".to_string())),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::String(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Array(a) => format!("[{}]", a.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(", ")),
            Value::Struct(_) => "[struct]".to_string(),
            Value::Void => "void".to_string(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub data_type: DataType,
    pub value: Value,
}

#[derive(Debug, Clone)]
pub struct CallFrame {
    pub function_name: String,
    pub variables: HashMap<String, Variable>,
}

impl CallFrame {
    pub fn new(function_name: String) -> Self {
        CallFrame {
            function_name,
            variables: HashMap::new(),
        }
    }
}

pub struct Interpreter {
    call_stack: Vec<CallFrame>,
    functions: HashMap<String, Node>,
    structs: HashMap<String, Node>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut interpreter = Interpreter {
            call_stack: Vec::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
        };
        interpreter.call_stack.push(CallFrame::new("global".to_string()));
        interpreter
    }

    fn current_frame(&mut self) -> &mut CallFrame {
        self.call_stack.last_mut().expect("Call stack should never be empty")
    }

    fn get_variable(&self, name: &str) -> Option<&Variable> {
        for frame in self.call_stack.iter().rev() {
            if let Some(var) = frame.variables.get(name) {
                return Some(var);
            }
        }
        None
    }

    fn set_variable(&mut self, name: String, variable: Variable) {
        self.current_frame().variables.insert(name, variable);
    }

    fn push_call_frame(&mut self, function_name: String) {
        self.call_stack.push(CallFrame::new(function_name));
    }

    fn pop_call_frame(&mut self) {
        if self.call_stack.len() > 1 {
            self.call_stack.pop();
        }
    }

    fn get_variable_mut(&mut self, name: &str) -> Option<&mut Variable> {
        for frame in self.call_stack.iter_mut().rev() {
            if frame.variables.contains_key(name) {
                return frame.variables.get_mut(name);
            }
        }
        None
    }

    pub fn execute(&mut self, node: &Node) -> Result<Value, RuntimeError> {
        if let NodeData::Program { statements } = &node.data {
            for stmt in statements {
                match &stmt.data {
                    NodeData::FunctionDef { name, .. } => {
                        self.functions.insert(name.clone(), stmt.clone());
                    }
                    NodeData::StructDef { name, .. } => {
                        self.structs.insert(name.clone(), stmt.clone());
                    }
                    _ => {}
                }
            }

            if let Some(main_func) = self.functions.get("main").cloned() {
                match self.execute_statement(&main_func) {
                    Ok(value) => return Ok(value),
                    Err(RuntimeError::Return(return_value)) => return Ok(return_value),
                    Err(e) => return Err(e),
                }
            }

            let mut last_value = Value::Void;
            for stmt in statements {
                if !matches!(stmt.data, NodeData::FunctionDef { .. } | NodeData::StructDef { .. }) {
                    last_value = self.execute_statement(stmt)?;
                }
            }
            return Ok(last_value);
        }

        self.execute_statement(node)
    }

    fn execute_statement(&mut self, node: &Node) -> Result<Value, RuntimeError> {
        match &node.data {
            NodeData::Program { statements } => {
                let mut last_value = Value::Void;
                for stmt in statements {
                    last_value = self.execute_statement(stmt)?;
                }
                Ok(last_value)
            }

            NodeData::FunctionDef { body, .. } => {
                self.execute_statement(body)
            }

            NodeData::VarDecl { data_type, name, array_sizes, init_expr } => {
                let value = if let Some(init) = init_expr {
                    self.execute_expression(init)?
                } else if let Some(sizes) = array_sizes {
                    // Create array with specified size
                    if sizes.len() == 1 {
                        // Single dimension array
                        let size_node = &sizes[0];
                        let size_value = self.execute_expression(size_node)?;
                        if let Value::Int(size) = size_value {
                            let default_elem = self.default_value_for_type(data_type);
                            Value::Array(vec![default_elem; size as usize])
                        } else {
                            return Err(RuntimeError::InvalidOperation(
                                "Array size must be an integer".to_string()
                            ));
                        }
                    } else {
                        // Multi-dimensional arrays not implemented yet
                        return Err(RuntimeError::InvalidOperation(
                            "Multi-dimensional arrays not implemented yet".to_string()
                        ));
                    }
                } else {
                    self.default_value_for_type(data_type)
                };

                let var = Variable {
                    name: name.clone(),
                    data_type: data_type.clone(),
                    value,
                };

                self.set_variable(name.clone(), var);
                Ok(Value::Void)
            }

            NodeData::Assignment { op, target, value } => {
                let val = self.execute_expression(value)?;
                self.assign_to_target(target, op, val)
            }

            NodeData::IfStmt { condition, then_stmt, else_stmt } => {
                let cond_value = self.execute_expression(condition)?;
                if cond_value.is_truthy() {
                    self.execute_statement(then_stmt)
                } else if let Some(else_branch) = else_stmt {
                    self.execute_statement(else_branch)
                } else {
                    Ok(Value::Void)
                }
            }

            NodeData::WhileStmt { condition, body } => {
                let mut last_value = Value::Void;
                loop {
                    let cond_value = self.execute_expression(condition)?;
                    if !cond_value.is_truthy() {
                        break;
                    }
                    
                    match self.execute_statement(body) {
                        Ok(value) => last_value = value,
                        Err(RuntimeError::Break) => break,
                        Err(RuntimeError::Continue) => continue,
                        Err(e) => return Err(e),
                    }
                }
                Ok(last_value)
            }

            NodeData::ForStmt { init, condition, update, body } => {
                if let Some(init_stmt) = init {
                    self.execute_statement(init_stmt)?;
                }

                let mut last_value = Value::Void;
                loop {
                    if let Some(cond) = condition {
                        let cond_value = self.execute_expression(cond)?;
                        if !cond_value.is_truthy() {
                            break;
                        }
                    }

                    match self.execute_statement(body) {
                        Ok(value) => last_value = value,
                        Err(RuntimeError::Break) => break,
                        Err(RuntimeError::Continue) => {
                            if let Some(upd) = update {
                                self.execute_expression(upd)?;
                            }
                            continue;
                        },
                        Err(e) => return Err(e),
                    }

                    if let Some(upd) = update {
                        self.execute_expression(upd)?;
                    }
                }
                Ok(last_value)
            }

            NodeData::DoWhileStmt { body, condition } => {
                let mut last_value = Value::Void;
                loop {
                    match self.execute_statement(body) {
                        Ok(value) => last_value = value,
                        Err(RuntimeError::Break) => break,
                        Err(RuntimeError::Continue) => {},
                        Err(e) => return Err(e),
                    }

                    let cond_value = self.execute_expression(condition)?;
                    if !cond_value.is_truthy() {
                        break;
                    }
                }
                Ok(last_value)
            }

            NodeData::SwitchStmt { expr, cases } => {
                let switch_value = self.execute_expression(expr)?;
                
                for case in cases {
                    if let NodeData::CaseStmt { value, statements } = &case.data {
                        let is_match = if let Some(case_value) = value {
                            let case_val = self.execute_expression(case_value)?;
                            self.values_equal(&switch_value, &case_val)?
                        } else {
                            true // default case
                        };

                        if is_match {
                            let mut last_value = Value::Void;
                            for stmt in statements {
                                match self.execute_statement(stmt) {
                                    Ok(value) => last_value = value,
                                    Err(RuntimeError::Break) => return Ok(last_value),
                                    Err(e) => return Err(e),
                                }
                            }
                            return Ok(last_value);
                        }
                    }
                }
                Ok(Value::Void)
            }

            NodeData::BreakStmt => Err(RuntimeError::Break),
            NodeData::ContinueStmt => Err(RuntimeError::Continue),

            NodeData::ReturnStmt { value } => {
                let ret_value = if let Some(val) = value {
                    self.execute_expression(val)?
                } else {
                    Value::Void
                };
                Err(RuntimeError::Return(ret_value))
            }

            NodeData::Block { statements } => {
                let mut last_value = Value::Void;
                for stmt in statements {
                    last_value = self.execute_statement(stmt)?;
                }
                Ok(last_value)
            }

            NodeData::PrintStmt { arguments } => {
                let mut output = Vec::new();
                for arg in arguments {
                    let value = self.execute_expression(arg)?;
                    output.push(self.value_to_string(&value));
                }
                println!("{}", output.join(" "));
                Ok(Value::Void)
            }

            NodeData::ExprStmt { expr } => {
                self.execute_expression(expr)
            }

            _ => {
                self.execute_expression(node)
            }
        }
    }

    fn execute_expression(&mut self, node: &Node) -> Result<Value, RuntimeError> {
        match &node.data {
            NodeData::Literal { value, .. } => {
                Ok(match value {
                    LiteralValue::Int(i) => Value::Int(*i),
                    LiteralValue::Float(f) => Value::Float(*f),
                    LiteralValue::String(s) => Value::String(s.clone()),
                    LiteralValue::Char(c) => Value::Char(*c),
                    LiteralValue::Bool(b) => Value::Bool(*b),
                })
            }

            NodeData::Identifier { name } => {
                self.get_variable(name)
                    .map(|var| var.value.clone())
                    .ok_or_else(|| RuntimeError::UndefinedVariable(name.clone()))
            }

            NodeData::BinaryOp { op, left, right } => {
                let left_val = self.execute_expression(left)?;
                let right_val = self.execute_expression(right)?;
                self.execute_binary_op(op, &left_val, &right_val)
            }

            NodeData::UnaryOp { op, operand } => {
                let operand_val = self.execute_expression(operand)?;
                self.execute_unary_op(op, &operand_val, operand)
            }

            NodeData::TernaryOp { condition, true_expr, false_expr } => {
                let cond_val = self.execute_expression(condition)?;
                if cond_val.is_truthy() {
                    self.execute_expression(true_expr)
                } else {
                    self.execute_expression(false_expr)
                }
            }

            NodeData::FunctionCall { name, args } => {
                if name == "print" {
                    let mut output = String::new();
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            output.push(' ');
                        }
                        let val = self.execute_expression(arg)?;
                        output.push_str(&val.to_string());
                    }
                    println!("{}", output);
                    Ok(Value::Void)
                } else if name == "len" {
                    if args.len() != 1 {
                        return Err(RuntimeError::InvalidOperation(
                            "len() function requires exactly one argument".to_string()
                        ));
                    }
                    
                    let arg_value = self.execute_expression(&args[0])?;
                    match arg_value {
                        Value::Array(ref arr) => Ok(Value::Int(arr.len() as i64)),
                        Value::String(ref s) => Ok(Value::Int(s.len() as i64)),
                        _ => Err(RuntimeError::InvalidOperation(
                            "len() can only be called on arrays or strings".to_string()
                        ))
                    }
                } else {
                    // Custom function call
                    self.call_function(name, args)
                }
            }

            NodeData::Cast { target_type, expr } => {
                let value = self.execute_expression(expr)?;
                self.cast_value(&value, target_type)
            }

            NodeData::ArrayAccess { array, indices } => {
                let mut current_value = self.execute_expression(array)?;
                
                // Process each index sequentially for multi-dimensional access
                for index_node in indices {
                    let index_val = self.execute_expression(index_node)?;
                    let index = index_val.to_int()? as usize;
                    
                    match current_value {
                        Value::Array(arr) => {
                            if index < arr.len() {
                                current_value = arr[index].clone();
                            } else {
                                return Err(RuntimeError::IndexOutOfBounds);
                            }
                        },
                        Value::String(s) => {
                            if index < s.len() {
                                let ch = s.chars().nth(index).unwrap();
                                current_value = Value::Char(ch);
                            } else {
                                return Err(RuntimeError::IndexOutOfBounds);
                            }
                        },
                        _ => {
                            return Err(RuntimeError::TypeMismatch("Not an array or string".to_string()));
                        }
                    }
                }
                
                Ok(current_value)
            }

            NodeData::ArrayLiteral { elements } => {
                let mut array_values = Vec::new();
                for element in elements {
                    array_values.push(self.execute_expression(element)?);
                }
                Ok(Value::Array(array_values))
            }

            NodeData::Assignment { op, target, value } => {
                let val = self.execute_expression(value)?;
                self.assign_to_target(target, op, val)
            }

            _ => Err(RuntimeError::InvalidOperation("Unsupported expression".to_string())),
        }
    }

    fn execute_binary_op(&self, op: &BinaryOpType, left: &Value, right: &Value) -> Result<Value, RuntimeError> {
        match op {
            BinaryOpType::Add => {
                match (left, right) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 + b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + *b as f64)),
                    (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b))),
                    _ => Err(RuntimeError::TypeMismatch("Invalid types for addition".to_string())),
                }
            }

            BinaryOpType::Sub => {
                match (left, right) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 - b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - *b as f64)),
                    _ => Err(RuntimeError::TypeMismatch("Invalid types for subtraction".to_string())),
                }
            }

            BinaryOpType::Mul => {
                match (left, right) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 * b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * *b as f64)),
                    _ => Err(RuntimeError::TypeMismatch("Invalid types for multiplication".to_string())),
                }
            }

            BinaryOpType::Div => {
                match (left, right) {
                    (Value::Int(a), Value::Int(b)) => {
                        if *b == 0 {
                            Err(RuntimeError::DivisionByZero)
                        } else {
                            Ok(Value::Int(a / b))
                        }
                    }
                    (Value::Float(a), Value::Float(b)) => {
                        if *b == 0.0 {
                            Err(RuntimeError::DivisionByZero)
                        } else {
                            Ok(Value::Float(a / b))
                        }
                    }
                    (Value::Int(a), Value::Float(b)) => {
                        if *b == 0.0 {
                            Err(RuntimeError::DivisionByZero)
                        } else {
                            Ok(Value::Float(*a as f64 / b))
                        }
                    }
                    (Value::Float(a), Value::Int(b)) => {
                        if *b == 0 {
                            Err(RuntimeError::DivisionByZero)
                        } else {
                            Ok(Value::Float(a / *b as f64))
                        }
                    }
                    _ => Err(RuntimeError::TypeMismatch("Invalid types for division".to_string())),
                }
            }

            BinaryOpType::Mod => {
                match (left, right) {
                    (Value::Int(a), Value::Int(b)) => {
                        if *b == 0 {
                            Err(RuntimeError::DivisionByZero)
                        } else {
                            Ok(Value::Int(a % b))
                        }
                    }
                    _ => Err(RuntimeError::TypeMismatch("Modulo operation requires integers".to_string())),
                }
            }

            BinaryOpType::Equal => Ok(Value::Bool(self.values_equal(left, right)?)),
            BinaryOpType::NotEqual => Ok(Value::Bool(!self.values_equal(left, right)?)),

            BinaryOpType::Less => {
                match (left, right) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) < *b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(*a < (*b as f64))),
                    _ => Err(RuntimeError::TypeMismatch("Invalid types for comparison".to_string())),
                }
            }

            BinaryOpType::Greater => {
                match (left, right) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) > *b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(*a > (*b as f64))),
                    _ => Err(RuntimeError::TypeMismatch("Invalid types for comparison".to_string())),
                }
            }

            BinaryOpType::LessEqual => {
                match (left, right) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) <= *b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(*a <= (*b as f64))),
                    _ => Err(RuntimeError::TypeMismatch("Invalid types for comparison".to_string())),
                }
            }

            BinaryOpType::GreaterEqual => {
                match (left, right) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) >= *b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(*a >= (*b as f64))),
                    _ => Err(RuntimeError::TypeMismatch("Invalid types for comparison".to_string())),
                }
            }

            BinaryOpType::LogicalAnd => Ok(Value::Bool(left.is_truthy() && right.is_truthy())),
            BinaryOpType::LogicalOr => Ok(Value::Bool(left.is_truthy() || right.is_truthy())),
        }
    }

    fn execute_unary_op(&mut self, op: &UnaryOpType, operand: &Value, operand_node: &Node) -> Result<Value, RuntimeError> {
        match op {
            UnaryOpType::Minus => {
                match operand {
                    Value::Int(i) => Ok(Value::Int(-i)),
                    Value::Float(f) => Ok(Value::Float(-f)),
                    _ => Err(RuntimeError::TypeMismatch("Invalid type for unary minus".to_string())),
                }
            }

            UnaryOpType::Plus => {
                match operand {
                    Value::Int(i) => Ok(Value::Int(*i)),
                    Value::Float(f) => Ok(Value::Float(*f)),
                    _ => Err(RuntimeError::TypeMismatch("Invalid type for unary plus".to_string())),
                }
            }

            UnaryOpType::LogicalNot => Ok(Value::Bool(!operand.is_truthy())),

            UnaryOpType::PreIncrement | UnaryOpType::PostIncrement => {
                if let NodeData::Identifier { name } = &operand_node.data {
                    if let Some(var) = self.get_variable_mut(name) {
                        let new_val = match var.value {
                            Value::Int(ref mut i) => {
                                *i += 1;
                                Value::Int(*i)
                            }
                            Value::Float(ref mut f) => {
                                *f += 1.0;
                                Value::Float(*f)
                            }
                            _ => return Err(RuntimeError::TypeMismatch("Cannot increment this type".to_string())),
                        };
                        var.value = new_val.clone();
                        Ok(if matches!(op, UnaryOpType::PreIncrement) { new_val } else { operand.clone() })
                    } else {
                        Err(RuntimeError::UndefinedVariable(name.clone()))
                    }
                } else {
                    Err(RuntimeError::InvalidOperation("Can only increment variables".to_string()))
                }
            }

            UnaryOpType::PreDecrement | UnaryOpType::PostDecrement => {
                if let NodeData::Identifier { name } = &operand_node.data {
                    if let Some(var) = self.get_variable_mut(name) {
                        let new_val = match var.value {
                            Value::Int(ref mut i) => {
                                *i -= 1;
                                Value::Int(*i)
                            }
                            Value::Float(ref mut f) => {
                                *f -= 1.0;
                                Value::Float(*f)
                            }
                            _ => return Err(RuntimeError::TypeMismatch("Cannot decrement this type".to_string())),
                        };
                        var.value = new_val.clone();
                        Ok(if matches!(op, UnaryOpType::PreDecrement) { new_val } else { operand.clone() })
                    } else {
                        Err(RuntimeError::UndefinedVariable(name.clone()))
                    }
                } else {
                    Err(RuntimeError::InvalidOperation("Can only decrement variables".to_string()))
                }
            }
        }
    }


    fn values_equal(&self, left: &Value, right: &Value) -> Result<bool, RuntimeError> {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(a == b),
            (Value::Float(a), Value::Float(b)) => Ok((a - b).abs() < f64::EPSILON),
            (Value::Int(a), Value::Float(b)) => Ok(((*a as f64) - b).abs() < f64::EPSILON),
            (Value::Float(a), Value::Int(b)) => Ok((a - (*b as f64)).abs() < f64::EPSILON),
            (Value::String(a), Value::String(b)) => Ok(a == b),
            (Value::Char(a), Value::Char(b)) => Ok(a == b),
            (Value::Bool(a), Value::Bool(b)) => Ok(a == b),
            _ => Ok(false),
        }
    }

    fn cast_value(&self, value: &Value, target_type: &DataType) -> Result<Value, RuntimeError> {
        match target_type {
            DataType::Int => Ok(Value::Int(value.to_int()?)),
            DataType::Float | DataType::Double => Ok(Value::Float(value.to_float()?)),
            DataType::String => Ok(Value::String(value.to_string())),
            DataType::Bool => Ok(Value::Bool(value.is_truthy())),
            _ => Err(RuntimeError::TypeMismatch("Unsupported cast".to_string())),
        }
    }

    fn default_value_for_type(&self, data_type: &DataType) -> Value {
        match data_type {
            DataType::Int => Value::Int(0),
            DataType::Float | DataType::Double => Value::Float(0.0),
            DataType::String => Value::String(String::new()),
            DataType::Char => Value::Char('\0'),
            DataType::Bool => Value::Bool(false),
            DataType::Array(_, _) => Value::Array(Vec::new()),
            DataType::Struct(_) => Value::Struct(HashMap::new()),
            DataType::Void => Value::Void,
        }
    }

    fn value_to_string(&self, value: &Value) -> String {
        match value {
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::String(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Array(arr) => {
                let elements: Vec<String> = arr.iter().map(|v| self.value_to_string(v)).collect();
                format!("[{}]", elements.join(", "))
            },
            Value::Struct(_) => "{...}".to_string(),
            Value::Void => "void".to_string(),
        }
    }

    fn call_function(&mut self, name: &str, args: &[Box<Node>]) -> Result<Value, RuntimeError> {
        if let Some(func) = self.functions.get(name).cloned() {
            if let NodeData::FunctionDef { parameters, body, .. } = &func.data {
                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(self.execute_expression(arg)?);
                }

                if parameters.len() != arg_values.len() {
                    return Err(RuntimeError::InvalidOperation(
                        format!("Function '{}' expects {} arguments, got {}", 
                                name, parameters.len(), arg_values.len())
                    ));
                }

                self.push_call_frame(name.to_string());

                for (param, arg_value) in parameters.iter().zip(arg_values.iter()) {
                    let var = Variable {
                        name: param.name.clone(),
                        data_type: param.data_type.clone(),
                        value: arg_value.clone(),
                    };
                    self.set_variable(param.name.clone(), var);
                }

                let result = match self.execute_statement(body) {
                    Err(RuntimeError::Return(value)) => Ok(value),
                    Ok(value) => Ok(value),
                    Err(e) => Err(e),
                };

                self.pop_call_frame();
                
                result
            } else {
                Err(RuntimeError::InvalidOperation(format!("Invalid function: {}", name)))
            }
        } else {
            Err(RuntimeError::UndefinedVariable(format!("Function '{}' not found", name)))
        }
    }

    fn assign_to_target(&mut self, target: &Node, op: &AssignOpType, value: Value) -> Result<Value, RuntimeError> {
        match &target.node_type {
            NodeType::Identifier => {
                if let NodeData::Identifier { name } = &target.data {
                    let current_value = if matches!(op, AssignOpType::Assign) {
                        value
                    } else {
                        // Para operadores como +=, -=, etc., primeiro pegamos o valor atual
                        let var = self.get_variable(name)
                            .ok_or_else(|| RuntimeError::UndefinedVariable(name.clone()))?;
                        self.apply_assign_op(&var.value, op, value)?
                    };

                    // Atualizar variable na call frame atual
                    if let Some(frame) = self.call_stack.last_mut() {
                        if let Some(var) = frame.variables.get_mut(name) {
                            var.value = current_value.clone();
                            return Ok(current_value);
                        }
                    }

                    Err(RuntimeError::UndefinedVariable(format!("Variable '{}' not found", name)))
                } else {
                    Err(RuntimeError::InvalidOperation("Invalid assignment target".to_string()))
                }
            }
            
            _ => Err(RuntimeError::InvalidOperation("Assignment to this target not supported yet".to_string()))
        }
    }

    fn apply_assign_op(&self, current: &Value, op: &AssignOpType, new_value: Value) -> Result<Value, RuntimeError> {
        match op {
            AssignOpType::Assign => Ok(new_value),
            AssignOpType::AddAssign => {
                self.execute_binary_op(&BinaryOpType::Add, current, &new_value)
            }
            AssignOpType::SubAssign => {
                self.execute_binary_op(&BinaryOpType::Sub, current, &new_value)
            }
            AssignOpType::MulAssign => {
                self.execute_binary_op(&BinaryOpType::Mul, current, &new_value)
            }
            AssignOpType::DivAssign => {
                self.execute_binary_op(&BinaryOpType::Div, current, &new_value)
            }
        }
    }
}