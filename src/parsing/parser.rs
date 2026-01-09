use crate::{
    error::{Error, ErrorIssue},
    file::File,
    handle::Handle,
    interner::Interner,
    parsing::ast::Ast,
    token::{Span, Token, TokenKind},
};

type Tk = TokenKind;
type Issue = ErrorIssue;

struct Parser<'a> {
    file: Handle<File>,
    errors: Vec<Error>,
    tokens: &'a [Token],
    interner: &'a mut Interner,
    ast: &'a mut Ast,
    source: &'a [u8],
    cursor: usize,
}

//
// Parser API
//

impl<'a> Parser<'a> {

}

//
// Helper methods
//

impl<'a> Parser<'a> {
    /// Returns the token at the `cursor + k` position, returns the EOF token if that is out of bounds.
    fn get(&self, k: usize) -> &Token {
        let i = (self.cursor + k).min(self.tokens.len() - 1);
        &self.tokens[i]
    }

    /// Returns the token at the `cursor - k` position, returns the first token if that is out of bounds.
    fn get_back(&self, k: usize) -> &Token {
        &self.tokens[self.cursor.saturating_sub(k)]
    }

    /// Returns whether or not the token at the `cursor + k` position is of any of the listed kinds.
    fn check<const N: usize>(&self, k: usize, tks: [Tk; N]) -> bool {
        tks.contains(&self.get(k).kind)
    }

    /// Returns whether or not the token at the `cursor - k` position is of any of the listed kinds.
    fn check_back<const N: usize>(&self, k: usize, tks: [Tk; N]) -> bool {
        tks.contains(&self.get_back(k).kind)
    }

    /// Advances the parser `k` tokens forward. This value is clamped so it can never point out of bounds.
    fn eat(&mut self, k: usize) {
        self.cursor = (self.cursor + k).min(self.tokens.len() - 1);
    }

    /// **`cursor -> T in tks`**
    /// Advances the parser until either the EOF or one of the listed token kinds is found.
    ///
    /// Returns the number of tokens eaten `.0` and whether or not the parser is currently
    /// at the EOF token `.1`.
    ///
    /// Will return of EOF is reached, even if nothing else before it matched. **Do not pass EOF**
    /// in the list `tks`.
    fn eat_until<const N: usize>(&mut self, tks: [Tk; N]) -> (usize, bool) {
        let mut skipped = 0;

        // Eat while
        while !tks.contains(&self.get(0).kind) && self.get(0).kind != Tk::Eof {
            self.eat(1);
            skipped += 1;
        }

        let eof_found = self.is_eof();
        (skipped, eof_found)
    }

    /// **if `eat == true` then `cursor -> T in tks` else `cursor` does not move.**
    /// This "asserts" that the **next** token is one of the types in list provided, and if not,
    /// then it will produce a syntax error stating `what` it was looking for, along with the string
    /// representation of the token it actually found (as it exists in the source code).
    ///
    /// If `eat` is `true`, it will advance the parser if and only if if finds one of the expected tokens.
    fn assert<const N: usize>(
        &mut self,
        what: &'static str,
        tks: [Tk; N],
        eat: bool,
    ) -> Result<(), Error> {
        // Look for the token kind
        if tks.contains(&self.get(0).kind) {
            if eat {
                self.eat(1);
            }
            return Ok(());
        }

        // Figure out what we ACTUALLY found here
        let actual = if self.get(0).kind != Tk::Eof {
            self.substr_at(&self.get(0).span)
        } else {
            "the end of the file".into()
        };

        // Return a syntax error
        Err(Error::new(
            Issue::InvalidSyntax,
            self.get(0).span,
            format!("Expected {what}, found {actual} instead."),
        ))
    }

    /// Returns string slice of whatever bytes are pointed to by `span`. Will panic if the
    /// byte sequence is not a valid string, but this should never happen.
    fn substr_at(&self, span: &Span) -> &str {
        let bytes = &self.source[span.as_range()];
        std::str::from_utf8(bytes).expect("invalid byte sequence for this token!")
    }

    /// Returns whether or not the current token is EOF.
    fn is_eof(&self) -> bool {
        self.get(0).kind == Tk::Eof
    }
}

impl<'a> Parser<'a> {
    
}