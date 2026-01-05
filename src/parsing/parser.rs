use crate::{
    error::{Error, ErrorIssue},
    file::File,
    handle::Handle,
    interner::Interner,
    operator::Op,
    parsing::{
        ast::Ast,
        expr::{Expr, ExprData},
    },
    token::{Token, TokenKind},
};

// Aliases
type Issue = ErrorIssue;
type E = ExprData;
type Tk = TokenKind;

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
        match parser.parse_expr() {
            Ok(handle) => {
                parser.ast.push_to_root(handle);
                let expr = parser.ast.get_expr(handle);
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
        self.parse_binary()
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
        if self.cursor + k >= self.tokens.len() {
            return self.tokens.last().unwrap();
        }
        &self.tokens[self.cursor + k]
    }

    /// Advances the parser `k` tokens forward.
    fn eat(&mut self, k: usize) {
        if self.cursor + k >= self.tokens.len() {
            self.cursor = self.tokens.len();
            return;
        }
        self.cursor += k;
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
                        expr = Expr::new(span, E::ExprInt { value });
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
                        expr = Expr::new(span, E::ExprFloat { value });
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
                    E::ExprStr {
                        value: substring_handle,
                    },
                )
            }
            Tk::Symbol => {
                let substring_handle = self.interner.intern(bytes);
                expr = Expr::new(
                    span,
                    E::ExprSymbol {
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

    /// Parses a prefix unary expression like `!x` or `-x`.
    fn parse_prefix_unary(&mut self) -> Result<Handle<Expr>, Error> {
        let (span, kind) = (self.get(0).span, self.get(0).kind);
        let expr: Expr;
        let op: Op;

        // Look for an prefix unary operator
        match kind {
            Tk::Min => op = Op::Neg,
            Tk::Bang => op = Op::Not,
            _ => return self.parse_atom(), // <-- @(mark) NEXT CALL
        }

        // Go to the next token
        self.eat(1);

        // Then parse an expression on this token
        let operand = self.parse_expr()?;

        // Merge the spans
        let span = span.merge(&self.ast.get_expr(operand).span);

        // Produce an expression
        expr = Expr::new(span, E::ExprPrefixUnary { operand, op });

        self.eat(1);
        Ok(self.ast.push_expr(expr))
    }

    /// Parses a postfix unary expression like `x++` or `x--`.
    fn parse_postfix_unary(&mut self) -> Result<Handle<Expr>, Error> {
        let (span, _) = (self.get(0).span, self.get(0).kind);
        let expr: Expr;
        let op: Op;

        // Parse the current expression first
        let operand = self.parse_prefix_unary()?; // <-- @(mark) NEXT CALL

        // Then look for a postfix unary operator
        match self.get(0).kind {
            Tk::PlusPlus => op = Op::Inc,
            Tk::MinMin => op = Op::Dec,
            _ => return Ok(operand),
        }

        // Merge the spans
        let span = span.merge(&self.get(0).span);

        // Produce an expression
        expr = Expr::new(span, E::ExprPostfixUnary { operand, op });

        self.eat(1);
        Ok(self.ast.push_expr(expr))
    }

    /// Parses a binary expression with arithmetic operators like `a + b`.
    fn parse_binary(&mut self) -> Result<Handle<Expr>, Error> {
        let (span, _) = (self.get(0).span, self.get(0).kind);
        let expr: Expr;
        let op: Op;

        // Parse the current expression first
        let lhs = self.parse_postfix_unary()?; // <-- @(mark) NEXT CALL

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
        expr = Expr::new(span, E::ExprBinary { lhs, rhs, op });

        // @(todo) implement postfixup algo here
        self.eat(1);
        Ok(self.ast.push_expr(expr))
    }
}
