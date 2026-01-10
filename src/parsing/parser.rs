use crate::{
    error::{Error, ErrorIssue},
    file::File,
    handle::Handle,
    interner::Interner,
    parsing::{
        ast::Ast,
        expr::{self, Expr},
    },
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

impl<'a> Parser<'a> {}

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
    fn expr(&mut self) -> Result<Handle<Expr>, Error> {
        self.expr_atom()
    }
}

impl<'a> Parser<'a> {
    /// **`cursor -> LCURL`, `cursor -> RPAR + 1`**
    fn expr_block(&mut self) -> Result<expr::Block, Error> {
        let start_span = self.get(0).span;
        let mut exprs: Vec<Handle<Expr>> = vec![];

        // Move past the LCURL
        self.eat(1);

        while !self.check(0, [Tk::RCurl]) {
            let this_span = self.get(0).span;

            // Quick EOF check
            if self.is_eof() {
                return Err(Error::new(
                    Issue::MissingDelimiter,
                    start_span,
                    "This block is missing a closing `}`.".into(),
                ));
            }

            // Try to parse some expression
            let expr = match self.expr() {
                // Use this handle
                Ok(handle) => handle,

                // Tank the damage and absorb this error
                Err(error) => {
                    self.errors.push(error);
                    let _ = self.eat_until([Tk::Semicolon, Tk::RCurl]);
                    if self.check(0, [Tk::Semicolon]) {
                        self.eat(1);
                    }
                    continue;
                }
            };

            // After the expression, cursor -> expr + 1
            // Look for the current thing to be a SEMICOLON or RCURL

            // If it's a semicolon, we have to wrap this expression in a discard expr
            if self.check(0, [Tk::Semicolon]) {
                let span = self.get(0).span.merge(&this_span);
                let discard = Expr::new(span, expr::Data::Discard(expr));
                exprs.push(self.ast.push_expr(discard));

                // Advance past the semicolon
                self.eat(1);
                continue;
            }

            // If its an RCURL, then we're ending the block here, straight up.
            if self.check(0, [Tk::RCurl]) {
                break;
            }

            // If it's literally anything else there is clearly an error.
            // @todo is this error too much?
            self.errors.push(Error::new(Issue::InvalidSyntax, self.get(0).span, "Only the last expression of a block may end without a `;`. Considering inserting a `;` here, or end the block and allow this expression to be used as the value of the entire block.".into()));
            let _ = self.eat_until([Tk::Semicolon, Tk::RCurl]);
            if self.check(0, [Tk::Semicolon]) {
                self.eat(1);
            }
            continue;
        }

        // Move past the RCURL and return the block
        self.eat(1);
        Ok(exprs.into())
    }

    /// **`cursor -> first token of arg`, `cursor -> arg + 1`**
    fn expr_argument(&mut self) -> Result<expr::Argument, Error> {
        let start_span = self.get(0).span;

        // First, check if it's a labeled argument
        let label = if self.get(0).kind == Tk::Label {
            let word = &self.source[start_span.as_range()];
            let handle = Some(self.interner.intern(word));

            // Make sure the next thing is a colon
            self.assert("`:` after argument label", [Tk::Colon], true)?;

            // Then move past the colon
            self.eat(1);
            handle
        } else {
            None
        };

        // Then, get the expression and return
        let value = self.expr()?;
        let span = self.get_back(1).span.merge(&start_span);
        Ok(expr::Argument { span, label, value })
    }
}

impl<'a> Parser<'a> {
    /// **`cursor -> 1st of expr`, `cursor -> expr + 1`**
    /// Parses a single atomic expression, such as a literal.
    fn expr_atom(&mut self) -> Result<Handle<Expr>, Error> {
        let span = self.get(0).span;
        let kind = self.get(0).kind;

        let expr: Expr;
        match kind {
            // Integer literal
            Tk::Int => {
                let str = self.substr_at(&span);
                match str.parse::<i64>() {
                    Ok(val) => expr = Expr::new(span, expr::Data::Int(val)),
                    Err(_) => {
                        // Move past this token
                        self.eat(1);

                        // Return an error (we wanted an expression)
                        return Err(Error::new(
                            Issue::InternalError,
                            span,
                            "Problem parsing this integer literal.".into(),
                        ));
                    }
                }
            }
            // Floating-point literal
            Tk::Float => {
                let str = self.substr_at(&span);
                match str.parse::<f64>() {
                    Ok(val) => expr = Expr::new(span, expr::Data::Float(val)),
                    Err(_) => {
                        // Move past this token
                        self.eat(1);

                        // Return an error (we wanted an expression)
                        return Err(Error::new(
                            Issue::InternalError,
                            span,
                            "Problem parsing this floating-point literal.".into(),
                        ));
                    }
                }
            }
            // String literal
            Tk::Str => {
                let word = &self.source[span.as_range()];
                let handle = self.interner.intern(word);
                expr = Expr::new(span, expr::Data::Str(handle));
            }
            // Symbol literal
            Tk::Symbol => {
                let word = &self.source[span.as_range()];
                let handle = self.interner.intern(word);
                expr = Expr::new(span, expr::Data::Symbol(handle));
            }
            // Failure case
            _ => {
                // Move past this token
                self.eat(1);

                // Return an error (we wanted an expression)
                return Err(Error::new(
                    Issue::ExpectedExpression,
                    span,
                    "Expected an expression here.".into(),
                ));
            }
        }

        // Move past this token and push the expression
        self.eat(1);
        Ok(self.ast.push_expr(expr))
    }

    /// **`cursor -> 1st of expr`, `cursor -> RPAR + 1`**
    /// Parses a function call and it's arguments, including labeled ones.
    fn expr_call(&mut self) -> Result<Handle<Expr>, Error> {
        let span = self.get(0).span;
        let expr: Expr;
        let callee = self.expr_atom()?; // @next_call

        // Now look for the LPAR
        if self.check(0, [Tk::LPar]) {
            let mut args: Vec<expr::Argument> = vec![];

            // Move to the first arg
            self.eat(1);

            // While we can, take arguments
            while !self.check(0, [Tk::RPar]) {
                let arg = match self.expr_argument() {
                    Ok(arg) => arg,
                    Err(err) => {
                        // Internalize the error from this arg specifically and move on
                        self.errors.push(err);
                        let _ = self.eat_until([Tk::Comma, Tk::RPar]);

                        // Move along if we get a comma
                        if self.check(0, [Tk::Comma]) {
                            self.eat(1);
                            continue;
                        }

                        // The RPAR case falls through to here
                        break;
                    }
                };

                args.push(arg);
            }

            // By this point, cursor -> RPAR, so move one past it.
            self.eat(1);

            // Then, create the call expr
            let span = self.get_back(1).span.merge(&span);
            expr = Expr::new(
                span,
                expr::Data::Call {
                    callee,
                    args: args.into(),
                },
            );

        // Otherwise, just return the callee as an expression and move up the stack
        } else {
            return Ok(callee);
        }

        // Return the expression
        Ok(self.ast.push_expr(expr))
    }
}

#[cfg(test)]
mod parser_tests {
    use crate::{
        file::File,
        handle::Handle,
        interner::Interner,
        parsing::{
            ast::Ast,
            expr::{self},
            parser::Parser, printer::pretty_print_ast,
        },
    };

    #[test]
    fn atom_and_call() {
        let text = "parse_int(\"123\", radix: 10)".to_owned();
        let (tokens, _lex_errs) = crate::lexer::lex(Handle::<File>::null(), &text);

        let mut interner = Interner::new(Handle::<File>::null());
        let mut ast = Ast::new(Handle::<File>::null());

        let mut parser = Parser {
            file: Handle::<File>::null(),
            errors: vec![],
            tokens: &tokens,
            interner: &mut interner,
            ast: &mut ast,
            source: text.as_bytes(),
            cursor: 0,
        };

        // Do the parsing
        let a_handle = parser.expr_call().unwrap();

        // Grab the errors from the parser
        let _parse_errs = std::mem::take(&mut parser.errors);

        // Relinquish the borrows here
        drop(parser);

        // Print the AST
        pretty_print_ast(&mut ast, &interner, std::io::stdout());

        // Get the data
        let a = ast.get_expr(a_handle);

        // Check the data
        match &a.data {
            expr::Data::Call { callee, args } => {
                assert!(args.arity() == 2);
                assert!(callee.index() == 0);
            }
            _ => panic!("not a call!"),
        }

        assert!(_parse_errs.len() == 0);
    }
}
