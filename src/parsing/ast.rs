use crate::{file::File, handle::Handle, interner::Interner, parsing::expr::{Expr, ExprData}};

/// Used when printing substrings.
const INVALID_STR: &'static str = "<invalid substring handle>";

// Alias
type E = ExprData;

/// Used when printing substrings to resolve a handle into a string safely.
macro_rules! stringify_handle {
    ($int:expr, $handle:expr) => {
        ($int).get_string($handle).unwrap_or(INVALID_STR.into())
    };
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Abstract Syntax Tree
// ------------------------------------------------------------------------------------------------------------------ //

/// Holds all information for the AST, including being the central storage location for all AST nodes.
#[derive(Debug)]
pub struct Ast {
    /// The file this AST belongs to.
    file: Handle<File>,
    /// A container for all expressions.
    exprs: Vec<Expr>,
    // @(temp) root holds expr handles
    root: Vec<Handle<Expr>>,
}

impl Ast {
    /// Creates a new empty AST.
    pub fn new(file: Handle<File>) -> Self {
        Self {
            file,
            exprs: vec![],
            root: vec![],
        }
    }

    /// Pushes the given handle to the root container.
    pub fn push_to_root(&mut self, handle: Handle<Expr>) {
        self.root.push(handle);
    }

    /// Pushes an expression to the container and returns a handle to that expression.
    pub fn push_expr(&mut self, expr: Expr) -> Handle<Expr> {
        self.exprs.push(expr);
        Handle::<Expr>::new(self.exprs.len() - 1)
    }

    /// Gets the expression from a given handle. This assumes that the handle is valid and will fatally error
    /// if not, since `Handle<Expr>`s are only ever supposed to be created from `push_expr()`.
    pub fn get_expr(&self, handle: Handle<Expr>) -> &Expr {
        assert!(
            handle.index() < self.exprs.len(),
            "expr handle `{}` out of bounds!",
            handle.index()
        );
        &self.exprs[handle]
    }

    /// Prints the AST the `stderr`.
    pub fn pretty_print(&self, int: &Interner) {
        // @(temp) printing just expressions
        for expr in &self.root {
            self.print_expr(0, int, *expr);
        }
    }
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Pretty Printing
// ------------------------------------------------------------------------------------------------------------------ //

impl Ast {
    fn print_expr(&self, i: usize, int: &Interner, expr: Handle<Expr>) {
        let spaces = " ".repeat(i);
        let expr = self.get_expr(expr);
        eprint!("{spaces}");

        match &expr.data {
            //
            // Atoms
            //
            E::ExprInt { value } => eprintln!("INT ({})", value),
            E::ExprFloat { value } => eprintln!("FLOAT ({})", value),
            E::ExprBool { value } => eprintln!("BOOL ({})", value),
            E::ExprStr { value } => eprintln!("STR ({})", stringify_handle!(int, *value)),
            E::ExprSymbol { name } => eprintln!("SYMBOL ({})", stringify_handle!(int, *name)),

            //
            // Compound
            //
            E::ExprPostfixUnary { operand, op } => {
                eprintln!("POSTFIX UNARY ({:?})", op);
                self.print_expr(i + 2, int, *operand);
            }
            E::ExprPrefixUnary { operand, op } => {
                eprintln!("PREFIX UNARY ({:?})", op);
                self.print_expr(i + 2, int, *operand);
            }
            E::ExprBinary { lhs, rhs, op } => {
                eprintln!("BINARY ({:?})", op);
                self.print_expr(i + 2, int, *lhs);
                self.print_expr(i + 2, int, *rhs);
            }
            E::ExprCall { callee, args } => {
                eprintln!("CALL");
                self.print_expr(i + 2, int, *callee);
                eprintln!("{spaces}  ARGS (");
                for arg in args {
                    self.print_expr(i + 4, int, *arg);
                }
                eprintln!("{spaces}  )");
            }

            _ => unreachable!("unknown expression data in pretty printer!"),
        }
    }
}
