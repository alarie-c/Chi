use crate::{file::File, handle::Handle};
use std::{
    cmp::min,
    ops::Range,
};

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Span
// ------------------------------------------------------------------------------------------------------------------ //

/// Used to represent some position in a source file using byte offsets and lengths.
#[derive(Debug, Clone, Copy)]
pub struct Span {
    /// The file this span derives from
    pub file: Handle<File>,
    /// The offset (in bytes) from the beginning of the file
    pub offset: usize,
    /// The length (in bytes) from `offset`
    pub len: usize,
    /// User-facing 1-based index for column number
    pub x: usize,
    /// User-facing 1-based index for line number
    pub y: usize,
}

impl Span {
    /// Creates a new `Span` from the specified data.
    pub const fn new(file: Handle<File>, offset: usize, len: usize, x: usize, y: usize) -> Self {
        Self {
            file,
            offset,
            len,
            x,
            y,
        }
    }

    /// Returns the span as a range noninclusive of `usize` integers; takes the form `start..end`.
    pub const fn as_range(&self) -> Range<usize> {
        self.offset..self.end()
    }

    /// Returns the end index (in bytes) that this span points to.
    pub const fn end(&self) -> usize {
        self.offset + self.len
    }

    /// Returns a new `Span` by combining the two others. The LHS and RHS must be from the same file. This method will
    /// maximize span coverage, using the earliest/smallest start index and the farthest/largest end index.
    pub fn merge(&self, rhs: &Span) -> Self {
        // Make sure these are coming from the same file
        assert!(
            self.file == rhs.file,
            "attempted to add two spans of different file!: `{}` and `{}`",
            self.file.index(),
            rhs.file.index()
        );

        // Get the earliest (x, y) pair
        let (x, y) = if self.y < rhs.y {
            (self.x, self.y)
        } else {
            (rhs.x, rhs.y)
        };

        // Get the earliest offset
        let offset = min(self.offset, rhs.offset);

        // Get the farthest end index
        let end = min(self.end(), rhs.end());

        // Then add em all up
        Self::new(self.file, offset, end - offset + 1, x, y)
    }
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Token Kind
// ------------------------------------------------------------------------------------------------------------------ //

/// Used to represent some specific Token variant, including operators, keywords, and literals.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    /// Represents the end of the token stream.
    Eof = 0,

    //
    // Grouping Operators
    //
    LPar,
    RPar,
    LBrac,
    RBrac,
    LCurl,
    RCurl,

    //
    // Arithmetic Operators
    //
    Plus,
    PlusEq,
    PlusPlus,
    Min,
    MinEq,
    MinMin,
    Star,
    StarEq,
    StarStar,
    StarStarEq,
    Slash,
    SlashEq,
    SlashSlash,
    SlashSlashEq,
    Mod,
    ModEq,

    //
    // Comparison, Equality, and Logical
    //
    Bang,
    BangEq,
    Eq,
    EqEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    And,
    AndAnd,
    Bar,
    BarBar,

    //
    // Misc Operators
    //
    Dot,
    Comma,
    Semicolon,
    Colon,
    Arrow,

    //
    // Literals
    //
    Symbol,
    Int,
    Float,
    Str,
    Label,

    //
    // Keywords
    //
    Let,
    Mutable,
    Function,
    Type,
    Enum,
    While,
    For,
    In,
    If,
    Else,
    Break,
    Continue,
    True,
    False,
    Return,
    Defer,
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Token
// ------------------------------------------------------------------------------------------------------------------ //

/// Models some chunked lexical information from a source file, containing a `Span` and a variant `TokenKind`.
#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// Attempts to match the slice of bytes to a keyword and returns that keyword. Returns `None` if no word was matched.
pub fn match_keyword(word: &[u8]) -> Option<TokenKind> {
    use TokenKind::*;
    match word {
        b"let" => Some(Let),
        b"mutable" => Some(Mutable),
        b"function" => Some(Function),
        b"type" => Some(Type),
        b"enum" => Some(Enum),
        b"while" => Some(While),
        b"for" => Some(For),
        b"in" => Some(In),
        b"if" => Some(If),
        b"else" => Some(Else),
        b"break" => Some(Break),
        b"continue" => Some(Continue),
        b"true" => Some(True),
        b"false" => Some(False),
        b"return" => Some(Return),
        b"defer" => Some(Defer),
        _ => None,
    }
}
