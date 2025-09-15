#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    line: u32,
    column: u32,
}

impl Position {
    pub fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }

    pub fn start() -> Self {
        Self::new(1, 1)
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn column(&self) -> u32 {
        self.column
    }

    pub fn advance_column(&mut self) {
        self.column += 1;
    }

    pub fn next_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }

    pub fn add_columns(&mut self, count: u32) {
        self.column += count;
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug)]
pub struct PositionTracker {
    position: Position,
    tab_width: usize,
}

impl PositionTracker {
    pub fn new(tab_width: usize) -> Self {
        Self {
            position: Position::start(),
            tab_width,
        }
    }

    pub fn position(&self) -> Position {
        self.position
    }

    pub fn advance(&mut self, ch: char) {
        match ch {
            '\n' => self.position.next_line(),
            '\t' => {
                let tab_stop = ((self.position.column - 1) / self.tab_width as u32 + 1) * self.tab_width as u32 + 1;
                self.position.column = tab_stop;
            }
            _ => self.position.advance_column(),
        }
    }

    pub fn advance_by(&mut self, text: &str) {
        for ch in text.chars() {
            self.advance(ch);
        }
    }
}