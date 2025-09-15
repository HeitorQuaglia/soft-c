#[derive(Debug, Clone)]
pub struct DebugInfo {
    pub line: u32,
    pub column: u32,
    pub source_file: String,
}

impl DebugInfo {
    pub fn new(line: u32, column: u32, source_file: String) -> Self {
        Self {
            line,
            column,
            source_file,
        }
    }

    pub fn from_position(position: (u32, u32), source_file: String) -> Self {
        Self::new(position.0, position.1, source_file)
    }

    pub fn position(&self) -> (u32, u32) {
        (self.line, self.column)
    }

    pub fn location_string(&self) -> String {
        format!("{}:{}:{}", self.source_file, self.line, self.column)
    }

    pub fn same_location(&self, other: &DebugInfo) -> bool {
        self.line == other.line
            && self.column == other.column
            && self.source_file == other.source_file
    }
}