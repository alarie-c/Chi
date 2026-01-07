use crate::{
    error::{Error, ErrorIssue},
    file::File,
    handle::Handle,
    interner::{Interner, Substring},
    operator::Op,
    parsing::{
        ast::Ast,
        decl::{Decl, DeclData, decl_fragments},
        expr::{Expr, ExprData},
        stmt::{Stmt, StmtData, stmt_fragments},
    },
    token::{Token, TokenKind},
};

// Aliases
type Issue = ErrorIssue;
type E = ExprData;
type S = StmtData;
type D = DeclData;
type Tk = TokenKind;

macro_rules! stringify_span {
    ($self:expr, $span:expr) => {{
        let bytes = &($self).source[($span).as_range()];
        String::from_utf8(bytes.iter().map(|b| *b).collect())
            .expect("invalid byte sequence for this token!")
    }};
}

macro_rules! skip_to {
    ($self:expr, $( $tk:pat ),+ $(,)?) => {
        while !matches!(
            ($self).get(0).kind,
            Tk::Eof | $( $tk )|+
        ) {
            ($self).eat(1);
        }
    };
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Parse API
// ------------------------------------------------------------------------------------------------------------------ //

/// Takes a file, source stream, and token stream to produce an AST, an error stream, and intern all relevant strings
/// in the source code.
pub fn parse<'a>(
    file: Handle<File>,
    source: &'a [u8],
    tokens: &'a Vec<Token>,
) -> (Ast, Vec<Error>, Interner) {
    assert!(!tokens.is_empty(), "cannot parse an empty file!");

    let mut interner = Interner::new(file);
    let mut ast = Ast::new(file);

    let mut parser = Parser {
        file,
        tokens,
        errors: vec![],
        interner: &mut interner,
        ast: &mut ast,
        source,
        cursor: 0,
    };

    while !parser.is_at_eof() {
        match parser.parse_decl() {
            Ok(handle) => {
                parser.ast.push_to_root(handle);
                let expr = parser.ast.get_decl(handle);
                eprintln!("{:#?}", expr);
            }
            Err(err) => {
                parser.errors.push(err);
            }
        }
    }

    // Grab the errors from the parser
    let errors = std::mem::take(&mut parser.errors);

    // Drop parser to relinquish its exclusive borrows of the interner and ast.
    drop(parser);

    (ast, errors, interner)
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Parser
// ------------------------------------------------------------------------------------------------------------------ //

/// Represents the internal state of the parser and facilitates parsing.
struct Parser<'a> {
    /// The file this parser is operating on.
    file: Handle<File>,
    /// The error stream.
    errors: Vec<Error>,
    /// The token stream.
    tokens: &'a Vec<Token>,
    /// The interner for this file.
    interner: &'a mut Interner,
    /// The AST to write into.
    ast: &'a mut Ast,
    /// The source code to grab byte data from.
    source: &'a [u8],
    /// The current location of the parser in the token stream.
    cursor: usize,
}

//
// API
//

impl<'a> Parser<'a> {
    /// Attempts to parse one expression.
    pub fn parse_expr(&mut self) -> Result<Handle<Expr>, Error> {
        self.parse_assign()
    }

    /// Returns whether or not only token remaining is the EOF.
    pub fn is_at_eof(&self) -> bool {
        self.tokens
            .get(self.cursor)
            .is_none_or(|token| token.kind == TokenKind::Eof)
    }
}

//
// Helpers
//

impl<'a> Parser<'a> {
    /// Returns the token `k` tokens ahead of `cursor`.
    fn get(&self, k: usize) -> &Token {
        let i = self.cursor + k;
        if i >= self.tokens.len() {
            self.tokens.last().unwrap()
        } else {
            &self.tokens[i]
        }
    }

    /// Returns the token `k` tokens behind of `cursor`.
    fn get_back(&self, k: usize) -> &Token {
        let i = self.cursor.saturating_sub(k);
        if i >= self.tokens.len() {
            self.tokens.first().unwrap()
        } else {
            &self.tokens[i]
        }
    }

    /// Advances the parser `k` tokens forward.
    fn eat(&mut self, k: usize) {
        if self.cursor + k >= self.tokens.len() {
            self.cursor = self.tokens.len();
            return;
        }
        self.cursor += k;
    }

    /// Advances the parser until the `target` token kind is found. Will return the number of tokens it skipped,
    /// as well as a boolean denoted whether or not the parser reached the EOF token during this maneuver.
    fn eat_until(&mut self, target: Tk) -> (usize, bool) {
        let mut i = 0;

        while self.get(0).kind != target {
            if self.get(0).kind == Tk::Eof {
                return (i, true);
            }

            self.eat(1);
            i += 1;
        }

        let eof_found = self.get(0).kind == Tk::Eof;
        (i, eof_found)
    }

    fn expect(&mut self, what: &'static str, tk: TokenKind, eat: bool) -> Result<(), Error> {
        // Look for the token kind.
        if self.get(0).kind == tk {
            if eat {
                self.eat(1);
            }
            return Ok(());
        }

        // Return an error otherwise.
        Err(Error::new(
            Issue::InvalidSyntax,
            self.get(0).span,
            format!(
                "Expected {what}, got `{}` instead.",
                stringify_span!(self, self.get(0).span)
            ),
        ))
    }

    fn expect_many<const N: usize>(
        &mut self,
        what: &'static str,
        tk: [TokenKind; N],
        eat: bool,
    ) -> Result<(), Error> {
        // Look for the token kind
        if tk.contains(&self.get(0).kind) {
            if eat {
                self.eat(1);
            }
            return Ok(());
        }

        // Otherwise return an error
        Err(Error::new(
            Issue::InvalidSyntax,
            self.get(0).span,
            format!(
                "Expected {what}, got `{}` instead.",
                stringify_span!(self, self.get(0).span)
            ),
        ))
    }

    fn expect_terminator(&mut self, skip: bool) -> Result<(), Error> {
        match self.expect_many(
            "`;` or new line to end statement",
            [Tk::Semicolon, Tk::Eof, Tk::Eol],
            true,
        ) {
            Ok(()) => Ok(()),
            Err(error) => {
                if skip {
                    skip_to!(self, Tk::Semicolon, Tk::Eol);
                }
                return Err(error);
            }
        }
    }
}

impl<'a> Parser<'a> {
    // Begins: cursor -> LPAR
    // Ends: cursor -> after RPAR
    fn parse_call_args(&mut self) -> Vec<Handle<Expr>> {
        // Span of the LPAR
        let start_span = self.get(0).span;

        // Empty args case check
        if self.get(1).kind == Tk::RPar {
            self.eat(2);
            return vec![];
        }

        let mut args: Vec<Handle<Expr>> = vec![];

        loop {
            self.eat(1); // advance to the first token of this expression
            match self.parse_expr() {
                Ok(handle) => args.push(handle),
                Err(error) => self.errors.push(error),
            }

            // After the call to `parse_expr()` we are pointing at the token
            // AFTER the argument.

            // Breaking inside this match block simply means returning the `args` vec.
            // No hidden eats or anything like that.
            match self.get(0).kind {
                Tk::RPar => break,
                Tk::Comma => {
                    // Make sure the comma is followed by an argument.
                    if self.get(1).kind == Tk::RPar {
                        self.errors.push(Error::new(
                            Issue::InvalidSyntax,
                            self.get(0).span,
                            "Expected another argument after the comma, got `)` instead.".into(),
                        ));

                        // Advance to the RPAR and then return.
                        self.eat(1);
                        break;

                    // Otherwise, just continue. This is the ONLY case that is a valid continuation of the loop!
                    } else {
                        continue;
                    }
                }
                Tk::Eof => {
                    self.errors.push(Error::new(
                        Issue::UnexpectedEoF,
                        start_span,
                        "This function call is missing a closing `)`.".into(),
                    ));
                    break;
                }
                _ => {
                    self.errors.push(Error::new(
                        Issue::InvalidSyntax,
                        self.get(0).span,
                        "Expected either `)` to end the function call or `,` to continue listing arguments.".into(),
                    ));

                    // Advance until the RPAR is found
                    let (_, eof) = self.eat_until(Tk::RPar);

                    // Handle the EOF case
                    if eof {
                        self.errors.push(Error::new(
                            Issue::UnexpectedEoF,
                            start_span,
                            "This function call is missing a closing `)`.".into(),
                        ));
                    }

                    break;
                }
            }
        }

        self.eat(1);
        args
    }
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Expression Parsers
// ------------------------------------------------------------------------------------------------------------------ //

impl<'a> Parser<'a> {
    /// Parses an atom (namely the literals).
    fn parse_atom(&mut self) -> Result<Handle<Expr>, Error> {
        let (span, kind) = (self.get(0).span, self.get(0).kind);
        let bytes = &self.source[span.as_range()];

        let expr: Expr;
        match kind {
            Tk::Int => {
                let str = String::from_utf8(bytes.iter().map(|b| *b).collect())
                    .expect("invalid byte sequence for integer literal!");
                match str.parse::<i64>() {
                    Ok(value) => {
                        expr = Expr::new(span, E::Int { value });
                    }
                    Err(_) => {
                        self.eat(1);
                        return Err(Error::new(
                            Issue::InternalError,
                            span,
                            "Problem parsing this integer literal.".into(),
                        ));
                    }
                }
            }
            Tk::Float => {
                let str = String::from_utf8(bytes.iter().map(|b| *b).collect())
                    .expect("invalid byte sequence for integer literal!");
                match str.parse::<f64>() {
                    Ok(value) => {
                        expr = Expr::new(span, E::Float { value });
                    }
                    Err(_) => {
                        self.eat(1);
                        return Err(Error::new(
                            Issue::InternalError,
                            span,
                            "Problem parsing this floating point literal.".into(),
                        ));
                    }
                }
            }
            Tk::Str => {
                let substring_handle = self.interner.intern(bytes);
                expr = Expr::new(
                    span,
                    E::Str {
                        value: substring_handle,
                    },
                )
            }
            Tk::Symbol => {
                let substring_handle = self.interner.intern(bytes);
                expr = Expr::new(
                    span,
                    E::Symbol {
                        name: substring_handle,
                    },
                )
            }
            _ => {
                self.eat(1);
                return Err(Error::new(
                    Issue::ExpectedExpression,
                    span,
                    "Expected an expression here.".into(),
                ));
            }
        }

        self.eat(1);
        Ok(self.ast.push_expr(expr))
    }

    fn parse_call(&mut self) -> Result<Handle<Expr>, Error> {
        let (span, _) = (self.get(0).span, self.get(0).kind);
        let expr: Expr;

        // Start by parsing this expression
        let callee = self.parse_atom()?; // @mark NEXT CALL

        // Now look for the call
        if self.get(0).kind == Tk::LPar {
            let args = self.parse_call_args();

            // After the call to `parse_call_args()`,
            // cursor -> after RPAR

            let span = self.get_back(1).span.merge(&span);
            expr = Expr::new(span, E::Call { callee, args });
        } else {
            return Ok(callee);
        }

        // Advance PAST the RPAR
        Ok(self.ast.push_expr(expr))
    }

    /// Parses a prefix unary expression like `!x` or `-x`.
    fn parse_prefix_unary(&mut self) -> Result<Handle<Expr>, Error> {
        let (span, kind) = (self.get(0).span, self.get(0).kind);
        let expr: Expr;
        let op: Op;

        // Look for an prefix unary operator
        match kind {
            Tk::Min => op = Op::Neg,
            Tk::Bang => op = Op::Not,
            _ => return self.parse_call(), // @mark NEXT CALL
        }

        // Go to the next token
        self.eat(1);

        // Then parse an expression on this token
        let operand = self.parse_expr()?;

        // Merge the spans
        let span = span.merge(&self.ast.get_expr(operand).span);

        // Produce an expression
        expr = Expr::new(span, E::UnaryPrefix { operand, op });

        Ok(self.ast.push_expr(expr))
    }

    /// Parses a postfix unary expression like `x++` or `x--`.
    fn parse_postfix_unary(&mut self) -> Result<Handle<Expr>, Error> {
        let (span, _) = (self.get(0).span, self.get(0).kind);
        let expr: Expr;
        let op: Op;

        // Parse the current expression first
        let operand = self.parse_prefix_unary()?; // @mark NEXT CALL
        eprintln!(
            "(postfix unary) after stack back: `{:#?}`",
            self.get(0).kind
        );

        // Then look for a postfix unary operator
        match self.get(0).kind {
            Tk::PlusPlus => op = Op::Inc,
            Tk::MinMin => op = Op::Dec,
            _ => return Ok(operand),
        }

        // Merge the spans
        let span = span.merge(&self.get(0).span);

        // Produce an expression
        expr = Expr::new(span, E::UnaryPostfix { operand, op });

        self.eat(1);
        Ok(self.ast.push_expr(expr))
    }

    /// Parses a binary expression with arithmetic operators like `a + b`.
    fn parse_binary(&mut self) -> Result<Handle<Expr>, Error> {
        let (span, _) = (self.get(0).span, self.get(0).kind);
        let expr: Expr;
        let op: Op;

        // Parse the current expression first
        let lhs = self.parse_postfix_unary()?; // @mark NEXT CALL
        eprintln!("(binary) after stack back: `{:#?}`", self.get(0).kind);

        // Then look for a binary operator
        match self.get(0).kind {
            Tk::Plus => op = Op::Add,
            Tk::Min => op = Op::Sub,
            Tk::Star => op = Op::Mul,
            Tk::Slash => op = Op::Div,
            Tk::Mod => op = Op::Mod,
            _ => return Ok(lhs),
        }

        // Go to the next token
        self.eat(1);

        // Then parse an expression on this token
        let rhs = self.parse_expr()?;

        // Merge the spans
        let span = span.merge(&self.ast.get_expr(rhs).span);

        // Produce an expression
        expr = Expr::new(span, E::BinaryArith { lhs, rhs, op });

        // @todo implement postfixup algo here
        // self.eat(1);
        Ok(self.ast.push_expr(expr))
    }

    fn parse_assign(&mut self) -> Result<Handle<Expr>, Error> {
        let (span, _) = (self.get(0).span, self.get(0).kind);
        let expr: Expr;
        let op: Op;

        // Parse the current expression first
        let lhs = self.parse_binary()?; // @mark NEXT CALL
        eprintln!("(assign) after stack back: `{:#?}`", self.get(0).kind);

        // Then look for a binary operator
        match self.get(0).kind {
            Tk::PlusEq => op = Op::AddAssign,
            Tk::MinEq => op = Op::SubAssign,
            Tk::StarEq => op = Op::MulAssign,
            Tk::SlashEq => op = Op::DivAssign,
            Tk::ModEq => op = Op::ModAssign,
            Tk::StarStarEq => op = Op::PowAssign,
            Tk::SlashSlashEq => op = Op::FloorAssign,
            Tk::Eq => op = Op::Assign,
            _ => return Ok(lhs),
        }

        // Go to the next token
        self.eat(1);

        // Then parse an expression on this token
        let rhs = self.parse_expr()?;

        // Merge the spans
        let span = span.merge(&self.ast.get_expr(rhs).span);

        // Produce an expression
        expr = Expr::new(
            span,
            E::Assign {
                assignee: lhs,
                value: rhs,
                op,
            },
        );

        Ok(self.ast.push_expr(expr))
    }
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Statement Parsers
// ------------------------------------------------------------------------------------------------------------------ //

impl<'a> Parser<'a> {
    fn parse_binding(&mut self) -> Result<Stmt, Error> {
        let (start_span, _) = (self.get(0).span, self.get(0).kind);

        // Check if this is a mutable binding and consume the keyword if it is
        let mutable = self.get(1).kind == Tk::Mutable;
        if mutable {
            self.eat(1);
        }

        // Skip to what should be an symbol token
        self.eat(1);

        // Make sure there is a symbol and error if not
        self.expect("a variable name", Tk::Symbol, false)?;

        // Get the handle of the symbol's substring
        let symbol = self
            .interner
            .intern(&self.source[self.get(0).span.as_range()]);

        // Eat the symbol and get to the equals sign
        self.eat(1);

        // Now expect an equals sign
        self.expect("'=' after variable name", Tk::Eq, true)?;

        // Now get an initializer expression
        let init = self.parse_expr()?;

        // Compute span and return the statement
        let span = self.get_back(1).span.merge(&start_span);
        let binding = stmt_fragments::Binding {
            symbol,
            init,
            mutable,
        };

        Ok(Stmt::new(span, S::Binding(binding)))
    }

    pub fn parse_stmt(&mut self) -> Result<Handle<Stmt>, Error> {
        let (span, kind) = (self.get(0).span, self.get(0).kind);

        let res: Result<Stmt, Error> = match kind {
            Tk::Let => self.parse_binding(),
            //Tk::If => {}

            // Non keyword statement i.e. expression statement
            _ => {
                let expr_handle = self.parse_expr()?;
                let expr = self.ast.get_expr(expr_handle);

                // Make sure the expression is actually effectful
                match &expr.data {
                    E::Call { callee: _, args: _ }
                    | E::Assign {
                        assignee: _,
                        value: _,
                        op: _,
                    } => Ok(Stmt::new(expr.span, StmtData::Expr { expr: expr_handle })),
                    _ => {
                        // @todo add more to this error
                        Err(Error::new(
                            Issue::ExpectedExpression,
                            span,
                            "Expressions must be effectful to exist as statements like this."
                                .into(),
                        ))
                    }
                }
            }
        };

        // Check for errors when getting the statement
        let stmt = match res {
            Ok(stmt) => stmt,
            Err(error) => {
                skip_to!(self, Tk::Semicolon, Tk::Eol);
                return Err(error);
            }
        };

        // Assume that each statement parser ends with
        // cursor -> token AFTER the last token of the statement

        // Check for an EoL or semicolon
        match self.get(0).kind {
            Tk::Semicolon | Tk::Eof | Tk::Eol => {
                eprintln!("current: {:?} next: {:?}", self.get(0), self.get(1));
                self.eat(1);
                Ok(self.ast.push_stmt(stmt))
            }

            // Missing semicolon/newline case:
            _ => {
                let offender_span = self.get(0).span;
                skip_to!(self, Tk::Semicolon, Tk::Eol);

                return Err(Error::new(
                    Issue::InvalidSyntax,
                    offender_span,
                    format!(
                        "Expected a new line or `;` to end this statement, got `{}` instead.",
                        stringify_span!(self, self.get(0).span)
                    ),
                ));
            }
        }
    }
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Declaration Parsers
// ------------------------------------------------------------------------------------------------------------------ //

impl<'a> Parser<'a> {
    fn parse_type_name(&mut self) -> Result<Handle<Expr>, Error> {
        if self.get(0).kind != Tk::Symbol {
            Err(Error::new(
                Issue::ExpectedTypeName,
                self.get(0).span,
                format!(
                    "Expected a type name, got `{}` instead.",
                    stringify_span!(self, self.get(0).span)
                ),
            ))
        } else {
            let substring_handle = self
                .interner
                .intern(&self.source[self.get(0).span.as_range()]);
            Ok(self.ast.push_expr(Expr::new(
                self.get(0).span,
                E::Symbol {
                    name: substring_handle,
                },
            )))
        }
    }

    /// cursor -> LPAR
    /// cursor -> one after RPAR
    fn parse_parameters(&mut self) -> Vec<decl_fragments::Parameter> {
        let (start_span, _) = (self.get(0).span, self.get(0).kind);

        // The no parameter case
        if self.get(1).kind == Tk::RPar {
            self.eat(2);
            return vec![];
        }

        let mut params: Vec<decl_fragments::Parameter> = vec![];

        loop {
            // Move to the beginning of the first parameter
            self.eat(1);

            // Look for mutable
            let mutable = if self.get(0).kind == Tk::Mutable {
                self.eat(1);
                true
            } else {
                false
            };

            // Look for a label for exterior name
            let ext_name_maybe: Option<Handle<Substring>> = if self.get(0).kind == Tk::Label {
                // Take the span and go to the actual name
                let span = self.get(0).span;
                self.eat(1);

                // Compute start/end without the `'`
                let start = span.offset + 1;
                let end = span.end() - 1;

                // Get a handle to that substring
                Some(self.interner.intern(&self.source[start..end]))
            } else {
                // Otherwise set it to nothing (for now)
                None
            };

            // Make sure there exists some interior name token
            let int_name: Handle<Substring> = match self.expect("parameter name", Tk::Symbol, true)
            {
                Ok(()) => self
                    .interner
                    .intern(&self.source[self.get(0).span.as_range()]),

                // If there isn't, either skip to the next param (or end of params)
                Err(error) => {
                    self.errors.push(error);
                    skip_to!(self, Tk::RPar, Tk::Comma);

                    // Break on the RPAR (end params)
                    if self.get(0).kind == Tk::RPar {
                        break;

                    // Or try for another parameter
                    } else {
                        continue;
                    }
                }
            };

            // Now check for a colon and handle the error
            match self.expect(
                "`:` after parameter name to annotate its type",
                Tk::Colon,
                true,
            ) {
                Ok(()) => {}
                Err(error) => {
                    self.errors.push(error);
                    skip_to!(self, Tk::RPar, Tk::Comma);

                    // Break on the RPAR (end params)
                    if self.get(0).kind == Tk::RPar {
                        break;

                    // Or try for another parameter
                    } else {
                        continue;
                    }
                }
            }

            // Look for a type name and handle the error
            let type_name = match self.parse_type_name() {
                Ok(type_expr) => type_expr,
                Err(error) => {
                    self.errors.push(error);
                    skip_to!(self, Tk::RPar, Tk::Comma);

                    // Break on the RPAR (end params)
                    if self.get(0).kind == Tk::RPar {
                        break;

                    // Or try for another parameter
                    } else {
                        continue;
                    }
                }
            };
            // cursor -> after type name expr

            // Look for a equals in case of default
            let default: Option<Handle<Expr>> = if self.get(0).kind == Tk::Eq {
                // First consume that Eq
                self.eat(1);

                // Then try to parse an expression
                match self.parse_expr() {
                    // Get the default expression
                    Ok(expr_handle) => Some(expr_handle),

                    // Handle an error in the default expression
                    Err(error) => {
                        self.errors.push(error);
                        skip_to!(self, Tk::RPar, Tk::Comma);

                        // Break on the RPAR (end params)
                        if self.get(0).kind == Tk::RPar {
                            break;

                        // Or try for another parameter
                        } else {
                            continue;
                        }
                    }
                }
            } else {
                None
            };
            // cursor -> after Eq (probably comma or Rpar)

            // Get the exterior name or use the interior one as a fallback
            let ext_name = ext_name_maybe.unwrap_or(int_name);

            // Create the parameter
            let param = decl_fragments::Parameter {
                ext_name,
                int_name,
                type_name,
                mutable,
                default,
            };

            // Push the parameter
            params.push(param);

            // Expect a comma to come after or close with an RPar
            match self.get(0).kind {
                Tk::RPar => break,
                Tk::Comma => {
                    // Make sure the comma is followed by an argument.
                    if self.get(1).kind == Tk::RPar {
                        self.errors.push(Error::new(
                            Issue::InvalidSyntax,
                            self.get(0).span,
                            "Expected another parameter after the comma, got `)` instead.".into(),
                        ));

                        // Advance to the RPAR and then return.
                        self.eat(1);
                        break;

                    // Otherwise, just continue. This is the ONLY case that is a valid continuation of the loop!
                    } else {
                        continue;
                    }
                }
                Tk::Eof => {
                    self.errors.push(Error::new(
                        Issue::UnexpectedEoF,
                        start_span,
                        "This function call is missing a closing `)`.".into(),
                    ));
                    break;
                }
                _ => {
                    self.errors.push(Error::new(
                        Issue::InvalidSyntax,
                        self.get(0).span,
                        "Expected either `)` to end the function declaration or `,` to continue listing arguments.".into(),
                    ));

                    // Advance until the RPAR is found
                    let (_, eof) = self.eat_until(Tk::RPar);

                    // Handle the EOF case
                    if eof {
                        self.errors.push(Error::new(
                            Issue::UnexpectedEoF,
                            start_span,
                            "This function call is missing a closing `)`.".into(),
                        ));
                    }

                    break;
                }
            }
        }

        self.eat(1);
        params
    }

    /// cursor -> `FUNCTION`
    fn parse_fn(&mut self) -> Result<Handle<Decl>, Error> {
        let (start_span, _) = (self.get(0).span, self.get(0).kind);

        // The next thing should be a name for the function, which in the future could be an expression (like a path),
        // but for now we can just bullshit it
        self.expect("function name", Tk::Symbol, true)?;

        // Extract the name from here
        let name = self
            .interner
            .intern(&self.source[self.get(0).span.as_range()]);

        // Then look for a LPAR
        self.expect("`(` to begin function parameters", Tk::LPar, true)?;

        // Then get the parameters
        let params = self.parse_parameters();

        // Then look for a return type, if it exists
        let return_type: Option<Handle<Expr>> = if self.get(0).kind == Tk::Arrow {
            // First consume that arrow
            self.eat(1);

            // Then try to parse an expression
            match self.parse_expr() {
                // Get the default expression
                Ok(expr_handle) => Some(expr_handle),

                // Handle an error and use `None` as the return type
                Err(error) => {
                    self.errors.push(error);
                    skip_to!(self, Tk::Eol, Tk::LCurl);
                    None
                }
            }
        } else {
            None
        };

        // Then look to see if this is just a declaration or a definition
        if self.get(0).kind != Tk::LCurl {
            let span = self.get(0).span.merge(&start_span);
            let def = D::FnDecl {
                sig: decl_fragments::FnSig {
                    name,
                    params,
                    return_type,
                }
            };
            return Ok(self.ast.push_decl(Decl::new(span, def)))
        }

        unreachable!("def not supported yet chief")
    }

    pub fn parse_decl(&mut self) -> Result<Handle<Decl>, Error> {
        let (_, kind) = (self.get(0).span, self.get(0).kind);

        match kind {
            Tk::Function => self.parse_fn(),
            _ => unreachable!("i dunno what that is..."),
        }
    }
}
