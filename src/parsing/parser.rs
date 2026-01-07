use crate::{
    error::{Error, ErrorIssue},
    file::File,
    handle::Handle,
    interner::{Interner, Substring},
    operator::Op,
    parsing::{
        ast::Ast,
        nodes::{
            decl::{self, Decl},
            expr::{self, Expr, UnaryOper},
            stmt::{self, Stmt},
        },
    },
    token::{Token, TokenKind},
};

// Aliases
type Issue = ErrorIssue;
type Tk = TokenKind;

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Parser Macros
// ------------------------------------------------------------------------------------------------------------------ //

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

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Components Parsers
// ------------------------------------------------------------------------------------------------------------------ //

impl<'a> Parser<'a> {
    fn parse_type_name(&mut self) -> Result<Handle<Expr>, Error> {
        // @temp this just looks for a symbol, but in the future it could be an entire expression i.e. path

        // Handle the error case
        if self.get(0).kind != Tk::Symbol {
            Err(Error::new(
                Issue::ExpectedTypeName,
                self.get(0).span,
                format!(
                    "Expected a type name, got `{}` instead.",
                    stringify_span!(self, self.get(0).span)
                ),
            ))

        // Otherwise get the substring, intern it, take a handle, and create the expression.
        } else {
            let substring_handle = self
                .interner
                .intern(&self.source[self.get(0).span.as_range()]);
            
            // Consume the symbol
            self.eat(1);
            
            // Return the expression handle
            Ok(self.ast.push_expr(Expr::new(
                self.get_back(1).span,
                expr::Data::Symbol(substring_handle),
            )))
        }
    }

    // cursor -> first token of param
    fn parse_parameter(&mut self) -> Result<decl::Parameter, Error> {    
        //
        // Mutability
        //
        let mutable = if self.get(0).kind == Tk::Mutable {
            self.eat(1);
            true
        } else {
            false
        };

        //
        // Labeled exterior name
        //
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

        //
        // Interior name
        //
        self.expect("parameter name", Tk::Symbol, false)?;
        let int_name = self
            .interner
            .intern(&self.source[self.get(0).span.as_range()]);

        // Advance to (what should be) the colon
        self.eat(1);

        //
        // Type name
        //

        // Ensure type name
        self.expect(
            "`:` after parameter name to annotate its type",
            Tk::Colon,
            true,
        )?;

        // Extract type name
        let type_name = self.parse_type_name()?;

        // After getting the type name,
        // cursor -> after type name expr

        //
        // Default value
        //
        let default: Option<Handle<Expr>> = if self.get(0).kind == Tk::Eq {
            self.eat(1);
            Some(self.parse_expr()?)
        } else {
            None
        };

        // After getting the default value,
        // cursor -> after eq (probably comma or rpar)

        // Get the exterior name or use the interior one as a fallback
        let ext_name = ext_name_maybe.unwrap_or(int_name);

        // Return the parameter
        Ok(decl::Parameter {
            ext_name,
            int_name,
            type_name,
            mutable,
            default,
        })
    }

    /// cursor -> LPAR
    /// cursor -> one after RPAR
    fn parse_fn_params(&mut self) -> Vec<decl::Parameter> {
        let (start_span, _) = (self.get(0).span, self.get(0).kind);

        // The no parameter case
        if self.get(1).kind == Tk::RPar {
            self.eat(2);
            return vec![];
        }

        let mut params: Vec<decl::Parameter> = vec![];

        loop {
            self.eat(1);

            //
            // Parse the parameter and make sure it's valid
            //
            let param = match self.parse_parameter() {
                Ok(param) => param,
                Err(error) => {
                    // Internalize the error and then recover
                    self.errors.push(error);

                    // Recover by skipping to the next comma or rpar
                    // We do a check to make sure we dont attempt to parse another parameter on an EOF token.
                    skip_to!(self, Tk::Comma, Tk::RPar);
                    match self.get(0).kind {
                        Tk::RPar => break,
                        Tk::Comma => continue,
                        _ => return params,
                    }
                }
            };

            // Push the parameter
            params.push(param);

            //
            // Validate the next token or recover
            //
            match self.get(0).kind {
                Tk::RPar => break,
                Tk::Comma => continue,

                // EOF case if we went too far or it's missing a closing RPAR
                Tk::Eof => {
                    self.errors.push(Error::new(
                        Issue::UnexpectedEoF,
                        start_span,
                        "This function call is missing a closing `)`.".into(),
                    ));
                    break;
                }

                // Literally anything else is a spectacular failure
                // just give up here and try to recover somewhere safe.
                _ => {
                    self.errors.push(Error::new(
                        Issue::InvalidSyntax,
                        self.get(0).span,
                        "Expected either `)` to end the function declaration or `,` to continue listing parameters.".into(),
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

        // Advance to one past the rpar
        self.eat(1);
        params
    }

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
                        expr = Expr::new(span, expr::Data::Int(value));
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
                        expr = Expr::new(span, expr::Data::Float(value));
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
                expr = Expr::new(span, expr::Data::Str(substring_handle))
            }
            Tk::Symbol => {
                let substring_handle = self.interner.intern(bytes);
                expr = Expr::new(span, expr::Data::Symbol(substring_handle))
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
            let args: expr::Arguments = args.into();
            expr = Expr::new(span, expr::Data::Call { callee, args });
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

        // Merge the spans and make the oper
        let span = span.merge(&self.ast.get_expr(operand).span);
        let oper = UnaryOper { operand, op };
        expr = Expr::new(span, expr::Data::Prefix(oper));

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

        // Merge the spans and make the oper
        let span = span.merge(&self.get(0).span);
        let oper = UnaryOper { operand, op };
        expr = Expr::new(span, expr::Data::Postfix(oper));

        // Advance for the next iteration
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

        // Merge the spans and make the oper
        let span = span.merge(&self.ast.get_expr(rhs).span);
        let oper = expr::BinaryOper { lhs, rhs, op };
        expr = Expr::new(span, expr::Data::Arithmetic(oper));

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

        // Merge the spans and make the oper
        let span = span.merge(&self.ast.get_expr(rhs).span);
        let oper = expr::BinaryOper { lhs, rhs, op };
        expr = Expr::new(span, expr::Data::Assign(oper));

        // Do not advance (call to `parse.expr()` did it for us)
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
        let binding = stmt::Binding {
            symbol,
            init,
            mutable,
        };

        Ok(Stmt::new(span, stmt::Data::Binding(binding)))
    }

    pub fn parse_stmt(&mut self) -> Result<Handle<Stmt>, Error> {
        let (span, kind) = (self.get(0).span, self.get(0).kind);

        //
        // Make a statement based on leading keyword
        //
        let res: Result<Stmt, Error> = match kind {
            Tk::Let => self.parse_binding(),

            //
            // Fallback to expression statement
            //
            _ => {
                let expr_handle = self.parse_expr()?;
                let expr = self.ast.get_expr(expr_handle);

                // Make sure the expression is actually effectful
                match &expr.data {
                    // Effectful expression
                    expr::Data::Call { callee: _, args: _ } | expr::Data::Assign(_) => {
                        Ok(Stmt::new(expr.span, stmt::Data::Expr(expr_handle)))
                    }

                    // Non-effectful expressions (error here)
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
    /// cursor -> `FUNCTION`
    fn parse_fn(&mut self) -> Result<Handle<Decl>, Error> {
        let (start_span, _) = (self.get(0).span, self.get(0).kind);
        self.eat(1);

        // The next thing should be a name for the function, which in the future could be an expression (like a path),
        // but for now we can just bullshit it

        //
        // Ensure function name
        //
        self.expect("function name", Tk::Symbol, false)?;

        // Extract the name here
        let name = self
            .interner
            .intern(&self.source[self.get(0).span.as_range()]);

        // Advance to (what should be) an lpar
        self.eat(1);

        // Then look for a LPAR
        self.expect("`(` to begin function parameters", Tk::LPar, false)?;

        //
        // Function parameters
        //
        let params = self.parse_fn_params();

        //
        // Return type if it exists
        //
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

        //
        // Finish off the declaration
        //
        self.expect_many(
            "either `{` to begin function body, or a new line to complete the function declaration.",
            [Tk::Eol, Tk::Eof, Tk::Semicolon, Tk::LCurl],
            true
        )?;

        // Then look to see if this is just a declaration or a definition
        
        //
        // Function declaration
        //
        if self.get_back(1).kind != Tk::LCurl {
            let span = self.get_back(1).span.merge(&start_span);
            let parameters: decl::Parameters = params.into();
            let signature = decl::FnSignature { name, parameters, return_type };
            let def = decl::Data::FnDecl { signature };
            return Ok(self.ast.push_decl(Decl::new(span, def)));
        }

        //
        // Function definition
        //
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
