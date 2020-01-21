use std::fmt;

/// Represents a position inside some textual document.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct TextPosition {
    /// Row, counting from 0.
    pub row: u64,
    /// Column, counting from 0.
    pub column: u64,
}

impl TextPosition {
    /// Creates a new position initialized to the beginning of the document.
    pub fn new() -> TextPosition {
        TextPosition { row: 0, column: 0 }
    }

    pub fn advance_both(&mut self, lines: u64, columns: u64) {
        self.row += lines;
        if lines > 0 {
            self.column = columns;
        } else {
            self.column += columns;
        }
    }

    /// Advances the position inside a line.
    pub fn advance(&mut self, count: u64) {
        self.column += count;
    }

    /// Advances the position to the beginning of the next line.
    pub fn new_line(&mut self) {
        self.column = 0;
        self.row += 1;
    }
}

impl fmt::Display for TextPosition {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.row + 1, self.column + 1)
    }
}

/// Designates a type which can yield a position inside some textual document.
///
/// This trait is implemented by parsers, lexers and errors.
pub trait Position {
    /// Returns the current position or a position corresponding to the object.
    fn position(&self) -> TextPosition;
}

impl Position for TextPosition {
    #[inline]
    fn position(&self) -> TextPosition {
        *self
    }
}
