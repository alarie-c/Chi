use crate::{
    error::{Error, ErrorIssue},
    file::File,
    handle::Handle,
    token::{Span, Token, TokenKind, match_keyword},
};
use ErrorIssue::*;

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Lex API
// ------------------------------------------------------------------------------------------------------------------ //

/// Takes a file and it's input and lexes it into a stream of tokens, returning the token stream and the error stream.
/// The lexer is created and destroyed within this function.
pub fn lex<'a>(file: Handle<File>, input: &'a String) -> (Vec<Token>, Vec<Error>) {
    let mut tokens: Vec<Token> = vec![];
    let mut errors: Vec<Error> = vec![];

    let mut lexer = Lexer {
        file,
        input: input.as_bytes(),
        offset: 0,
        x: 1,
        y: 1,
    };

    let mut lexing = true;
    while lexing {
        match lexer.next_token() {
            Ok(token) => {
                lexing = token.kind != TokenKind::Eof;
                tokens.push(token);
            }
            Err(error) => errors.push(error),
        }
    }

    (tokens, errors)
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Lexer
// ------------------------------------------------------------------------------------------------------------------ //

/// Used to model the state of the lexer and facilitate tokenization.
struct Lexer<'a> {
    /// Which file this is lexing, will be used for the creation of all `Span`s.
    file: Handle<File>,
    /// The input stream as a slice of bytes.
    input: &'a [u8],
    /// The current position of the cursor (in bytes) in the lexer.
    offset: usize,
    /// User-facing 1-based index representing column number.
    x: usize,
    /// User-facing 1-based index representing line number.
    y: usize,
}

//
// Helper Methods
//

impl<'a> Lexer<'a> {
    /// Returns the byte `k` bytes ahead of `self.offset`. Will return `0`` if it points out of bounds.
    fn get(&self, k: usize) -> u8 {
        if self.offset + k >= self.input.len() {
            return 0;
        }
        self.input[self.offset + k]
    }

    /// Advances the lexer `k` bytes. Will stop at the `0` byte if this puts it out of bounds.
    /// Also advances the column number.
    fn eat(&mut self, k: usize) {
        // Impossible to eat out of bounds
        if self.offset + k > self.input.len() {
            self.offset = self.input.len();
            return;
        }

        self.offset += k;
        self.x += k;
    }

    /// Advances the lexer once if and only if the **next** byte is equivalent to `b`.
    /// Returns `true` if the lexer advanced, returns `false` if not.
    fn eat_cond(&mut self, b: u8) -> bool {
        if self.get(1) == b {
            self.eat(1);
            return true;
        }
        return false;
    }
}

//
// Lexer Methods
//

impl<'a> Lexer<'a> {
    /// Attempts to lex one token and return it. Will return an `Error` if encountered.
    /// All lexers advance inside of themselves, so there is no advancing to be done by the caller. Every call to
    /// `next_token()` will end with `self.offset` pointing to the next byte to be lexed.
    pub(self) fn next_token(&mut self) -> Result<Token, Error> {
        type Tk = TokenKind;

        // Skip whitespace and capture state
        self.skip_whitespace();
        let (file, start, start_x, start_y) = (self.file, self.offset, self.x, self.y);

        // Quick helper closure to capture state and produce a span from just the length.
        let span = |len| Span::new(file, start, len, start_x, start_y);

        //
        // Match bytes
        //

        let token: Token; // all control flow paths in the following match block must define `Token` or return!
        match self.get(0) {
            0 => {
                token = Token {
                    kind: Tk::Eof,
                    span: span(0),
                };
            }
            b'\n' => {
                self.x = 0; // this will be fixed on the call to eat()
                self.y += 1;
                token = Token {
                    kind: Tk::Eol,
                    span: span(1),
                };
            }

            //
            // Grouping Operators
            //
            b'(' => {
                token = Token {
                    kind: Tk::LPar,
                    span: span(1),
                }
            }
            b')' => {
                token = Token {
                    kind: Tk::RPar,
                    span: span(1),
                }
            }
            b'[' => {
                token = Token {
                    kind: Tk::LBrac,
                    span: span(1),
                }
            }
            b']' => {
                token = Token {
                    kind: Tk::RBrac,
                    span: span(1),
                }
            }
            b'{' => {
                token = Token {
                    kind: Tk::LCurl,
                    span: span(1),
                }
            }
            b'}' => {
                token = Token {
                    kind: Tk::RCurl,
                    span: span(1),
                }
            }

            //
            // Arithmetic Operators
            //
            b'+' => {
                if self.eat_cond(b'+') {
                    token = Token {
                        kind: Tk::PlusPlus,
                        span: span(2),
                    }
                } else if self.eat_cond(b'=') {
                    token = Token {
                        kind: Tk::PlusEq,
                        span: span(2),
                    }
                } else {
                    token = Token {
                        kind: Tk::Plus,
                        span: span(1),
                    }
                }
            }
            b'-' => {
                if self.eat_cond(b'-') {
                    token = Token {
                        kind: Tk::MinMin,
                        span: span(2),
                    }
                } else if self.eat_cond(b'=') {
                    token = Token {
                        kind: Tk::MinEq,
                        span: span(2),
                    }
                } else if self.eat_cond(b'>') {
                    token = Token {
                        kind: Tk::Arrow,
                        span: span(2),
                    }
                } else {
                    token = Token {
                        kind: Tk::Min,
                        span: span(1),
                    }
                }
            }
            b'*' => {
                if self.eat_cond(b'*') {
                    if self.eat_cond(b'=') {
                        token = Token {
                            kind: Tk::StarStarEq,
                            span: span(3),
                        }
                    } else {
                        token = Token {
                            kind: Tk::StarStar,
                            span: span(2),
                        }
                    }
                } else if self.eat_cond(b'=') {
                    token = Token {
                        kind: Tk::StarEq,
                        span: span(2),
                    }
                } else {
                    token = Token {
                        kind: Tk::Star,
                        span: span(1),
                    }
                }
            }
            b'/' => {
                if self.eat_cond(b'/') {
                    if self.eat_cond(b'=') {
                        token = Token {
                            kind: Tk::SlashSlashEq,
                            span: span(3),
                        }
                    } else {
                        token = Token {
                            kind: Tk::SlashSlash,
                            span: span(2),
                        }
                    }
                } else if self.eat_cond(b'=') {
                    token = Token {
                        kind: Tk::SlashEq,
                        span: span(2),
                    }
                } else {
                    token = Token {
                        kind: Tk::Slash,
                        span: span(1),
                    }
                }
            }
            b'%' => {
                if self.eat_cond(b'=') {
                    token = Token {
                        kind: Tk::ModEq,
                        span: span(2),
                    }
                } else {
                    token = Token {
                        kind: Tk::Mod,
                        span: span(1),
                    }
                }
            }

            //
            // Comparison, Equality, and Logical
            //
            b'!' => {
                if self.eat_cond(b'=') {
                    token = Token {
                        kind: Tk::BangEq,
                        span: span(2),
                    }
                } else {
                    token = Token {
                        kind: Tk::Bang,
                        span: span(1),
                    }
                }
            }
            b'=' => {
                if self.eat_cond(b'=') {
                    token = Token {
                        kind: Tk::EqEq,
                        span: span(2),
                    }
                } else {
                    token = Token {
                        kind: Tk::Eq,
                        span: span(1),
                    }
                }
            }
            b'<' => {
                if self.eat_cond(b'=') {
                    token = Token {
                        kind: Tk::LtEq,
                        span: span(2),
                    }
                } else {
                    token = Token {
                        kind: Tk::Lt,
                        span: span(1),
                    }
                }
            }
            b'>' => {
                if self.eat_cond(b'=') {
                    token = Token {
                        kind: Tk::GtEq,
                        span: span(2),
                    }
                } else {
                    token = Token {
                        kind: Tk::Gt,
                        span: span(1),
                    }
                }
            }
            b'&' => {
                if self.eat_cond(b'&') {
                    token = Token {
                        kind: Tk::AndAnd,
                        span: span(2),
                    }
                } else {
                    token = Token {
                        kind: Tk::And,
                        span: span(1),
                    }
                }
            }
            b'|' => {
                if self.eat_cond(b'|') {
                    token = Token {
                        kind: Tk::BarBar,
                        span: span(2),
                    }
                } else {
                    token = Token {
                        kind: Tk::Bar,
                        span: span(1),
                    }
                }
            }

            //
            // Misc Operators
            //
            b'.' => {
                token = Token {
                    kind: Tk::Dot,
                    span: span(1),
                }
            }
            b',' => {
                token = Token {
                    kind: Tk::Comma,
                    span: span(1),
                }
            }
            b';' => {
                token = Token {
                    kind: Tk::Semicolon,
                    span: span(1),
                }
            }
            b':' => {
                token = Token {
                    kind: Tk::Colon,
                    span: span(1),
                }
            }

            //
            // Literals
            //
            b'\'' => return self.lex_label(),
            b'"' => return self.lex_str(),
            b'0'..=b'9' => return Ok(self.lex_digit()),
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => return Ok(self.lex_symbol()),

            _ => {
                // Advance for the next iteration
                self.eat(1);

                // Error about the invalid character
                return Err(Error::new(
                    UnrecognizedChar,
                    span(1),
                    "This character is not recognized by the compiler.".into(),
                ));
            }
        }

        // Advance for the next iteration and return
        self.eat(1);
        Ok(token)
    }

    /// Advances the lexer while the **current** byte is considered whitespace.
    fn skip_whitespace(&mut self) {
        loop {
            match self.get(0) {
                b' ' | b'\t' | b'\r' => self.eat(1),
                _ => break,
            }
        }
    }

    /// Lexes an entire symbol and attempts to make a keyword out of it if it can.
    /// Incapable of failing since `next_token()` ensures at least one valid symbol byte exists.
    fn lex_symbol(&mut self) -> Token {
        let (start, start_x, start_y) = (self.offset, self.x, self.y);

        // Eat while valid symbol
        while self.get(1).is_ascii_alphanumeric() || self.get(1) == b'_' {
            self.eat(1);
        }

        // When this breaks, offset -> last character of the symbol
        let len = self.offset - start + 1;
        let span = Span::new(self.file, start, len, start_x, start_y);

        // Look for a keyword or use a symbol
        let bytes = &self.input[start..start + len];
        let kind = match_keyword(bytes).unwrap_or(TokenKind::Symbol);

        // Advance for next iteration
        self.eat(1);
        Token { kind, span }
    }

    /// Lexes an entire digit, integer or float, and returns it.
    /// Incapable of failing since `next_token()` ensures at least one valid digit byte exists.
    fn lex_digit(&mut self) -> Token {
        let (start, start_x, start_y) = (self.offset, self.x, self.y);
        let mut kind = TokenKind::Int;

        // Eat while valid digit
        while self.get(1).is_ascii_digit() || self.get(1) == b'.' {
            //
            // Special case current == '.'
            //
            if self.get(1) == b'.' {
                // Look for the invalid `.` case
                if kind == TokenKind::Float || !self.get(2).is_ascii_digit() {
                    let len = self.offset - start + 1;
                    let span = Span::new(self.file, start, len, start_x, start_y);

                    // Advance for next iteration
                    self.eat(1);
                    return Token { kind, span };
                }

                // Otherwise, this is a valid `.` case
                kind = TokenKind::Float;
            }

            self.eat(1);
        }

        // When this breaks, offset -> last character of the digit
        let len = self.offset - start + 1;
        let span = Span::new(self.file, start, len, start_x, start_y);

        // Advance for next iteration
        self.eat(1);

        Token { kind, span }
    }

    /// Attempts to lex an entire string literal and will error if it is invalid or missing a delimiter.
    fn lex_str(&mut self) -> Result<Token, Error> {
        let (start, start_x, start_y) = (self.offset, self.x, self.y);

        // Skip first `"`
        self.eat(1);

        // Eat while not `"`
        while self.get(0) != b'"' {
            // Catch EOF
            if self.get(0) == 0 {
                return Err(Error::new(
                    UnterminatedString,
                    Span::new(self.file, start, 1, start_x, start_y),
                    "This string literal is missing a closing `\"`.".into(),
                ));
            }

            // @(todo) string escape sequences
            self.eat(1);
        }

        // When this breaks, offset -> `"`
        let len = self.offset - start + 1;
        let span = Span::new(self.file, start, len, start_x, start_y);

        // Advance for next iteration
        self.eat(1);

        Ok(Token {
            kind: TokenKind::Str,
            span,
        })
    }

    /// Attempts to lex an entire label literal and will error if it is invalid or missing a delimiter.
    fn lex_label(&mut self) -> Result<Token, Error> {
        let (start, start_x, start_y) = (self.offset, self.x, self.y);

        // Skip first `'`
        self.eat(1);

        // Eat while not `'`
        while self.get(0) != b'\'' {
            // Catch EOF
            if self.get(0) == 0 {
                return Err(Error::new(
                    UnterminatedLabel,
                    Span::new(self.file, start, 1, start_x, start_y),
                    "This label is missing a closing `'`.".into(),
                ));
            }

            self.eat(1);
        }

        // When this breaks, offset -> `'`
        let len = self.offset - start + 1;
        let span = Span::new(self.file, start, len, start_x, start_y);

        // Advance for next iteration
        self.eat(1);

        Ok(Token {
            kind: TokenKind::Label,
            span,
        })
    }
}
