use std::fmt;

#[derive(Clone, Copy, Debug)]
pub struct Location {
    pub column: u64,
    pub line: u64,
}

impl Location {
    pub fn new(line: u64, column: u64) -> Self {
        Location { line, column }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Line {}, Column {}", self.line, self.column)
    }
}




