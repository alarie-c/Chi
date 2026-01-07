use crate::{
    file::File,
    handle::Handle,
    interner::Interner,
    parsing::{
        nodes::decl::{self, Decl},
        nodes::expr::{self, Expr},
        nodes::stmt::{self, Stmt},
    },
};

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Macros
// ------------------------------------------------------------------------------------------------------------------ //

const INVALID_STR: &'static str = "<invalid substring handle>";

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
    /// A container for all statements.
    stmts: Vec<Stmt>,
    /// A container for all statements.
    decls: Vec<Decl>,

    root: Vec<Handle<Decl>>,
}

impl Ast {
    /// Creates a new empty AST.
    pub fn new(file: Handle<File>) -> Self {
        Self {
            file,
            exprs: vec![],
            stmts: vec![],
            decls: vec![],
            root: vec![],
        }
    }

    /// Pushes the given handle to the root container.
    pub fn push_to_root(&mut self, handle: Handle<Decl>) {
        self.root.push(handle);
    }

    /// Prints the AST the `stderr`.
    pub fn pretty_print(&self, int: &Interner) {
        // @temp printing just expressions
        for decl in &self.root {
            self.print_decl(0, int, *decl);
        }
    }
}

impl Ast {
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
}

impl Ast {
    /// Pushes a statement to the container and returns a handle to that statement.
    pub fn push_stmt(&mut self, stmt: Stmt) -> Handle<Stmt> {
        self.stmts.push(stmt);
        Handle::<Stmt>::new(self.stmts.len() - 1)
    }

    /// Gets the statement from a given handle. This assumes that the handle is valid and will fatally error
    /// if not, since `Handle<Stmt>`s are only ever supposed to be created from `push_stmt()`.
    pub fn get_stmt(&self, handle: Handle<Stmt>) -> &Stmt {
        assert!(
            handle.index() < self.stmts.len(),
            "stmt handle `{}` out of bounds!",
            handle.index()
        );
        &self.stmts[handle]
    }
}

impl Ast {
    /// Pushes an declaration to the container and returns a handle to that declaration.
    pub fn push_decl(&mut self, decl: Decl) -> Handle<Decl> {
        self.decls.push(decl);
        Handle::<Decl>::new(self.decls.len() - 1)
    }

    /// Gets the expression from a given handle. This assumes that the handle is valid and will fatally error
    /// if not, since `Handle<Decl>`s are only ever supposed to be created from `push_decl()`.
    pub fn get_decl(&self, handle: Handle<Decl>) -> &Decl {
        assert!(
            handle.index() < self.decls.len(),
            "decl handle `{}` out of bounds!",
            handle.index()
        );
        &self.decls[handle]
    }
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Pretty Printing
// ------------------------------------------------------------------------------------------------------------------ //

impl Ast {
    fn print_block(&self, i: usize, int: &Interner, block: &stmt::Block) {
        let spaces = " ".repeat(i);
        eprintln!("{spaces} BLOCK (");

        for stmt in &block.stmts {
            self.print_stmt(i + 2, int, *stmt);
        }

        eprintln!("{spaces})");
    }

    fn print_decl(&self, i: usize, int: &Interner, decl: Handle<Decl>) {
        let spaces = " ".repeat(i);
        let decl = self.get_decl(decl);
        eprint!("{spaces}");

        match &decl.data {
            decl::Data::FnDecl { signature } => {
                eprintln!("FUNCTION DECL '{}'", stringify_handle!(int, signature.name));
                
                // Print parameters
                eprintln!("{spaces}PARAMS");
                signature.parameters.iter().for_each(|p| {
                    eprintln!(
                        "{spaces}  '{}' {} {} WITH TYPE",
                        stringify_handle!(int, p.ext_name),
                        stringify_handle!(int, p.int_name),
                        if p.mutable { "MUTABLE" } else { "CONSTANT" }
                    );
                    self.print_expr(i + 4, int, p.type_name);
                    if let Some(def) = p.default {
                        eprintln!("{spaces}  AND DEFAULT");
                        self.print_expr(i + 4, int, def);
                    }
                });
                eprintln!("{spaces}END PARAMS");

                // Print the return type if it exists
                eprintln!("{spaces}RETURN TYPE");
                if let Some(rt) = signature.return_type {
                    self.print_expr(i + 4, int, rt);
                } else {
                    eprintln!("{spaces}  NONE");
                }
            }

            _ => unreachable!("unknown declaration in ast pretty printer!"),
        }
    }

    fn print_stmt(&self, i: usize, int: &Interner, stmt: Handle<Stmt>) {
        let spaces = " ".repeat(i);
        let stmt = self.get_stmt(stmt);
        eprint!("{spaces}");

        match &stmt.data {
            stmt::Data::Binding(binding) => {
                let _str = if binding.mutable {
                    "MUTABLE BINDING"
                } else {
                    "BINDING"
                };
                eprintln!("{_str} '{}'", stringify_handle!(int, binding.symbol));
                self.print_expr(i + 2, int, binding.init);
            }
            stmt::Data::Expr(expr) => {
                eprintln!("EXPRESSION STATEMENT");
                self.print_expr(i + 2, int, *expr);
            }
            stmt::Data::If { cond, if_br, el_br } => {
                eprintln!("IF");
                self.print_expr(i + 2, int, *cond);
                eprintln!("{spaces}THEN");
                self.print_block(i, int, if_br);

                if let Some(block) = el_br {
                    eprintln!("{spaces}ELSE");
                    self.print_block(i, int, block);
                }

                eprintln!("{spaces}END IF");
            }
            //_ => unreachable!("unknown statement in ast pretty printer!"),
        }
    }

    fn print_expr(&self, i: usize, int: &Interner, expr: Handle<Expr>) {
        let spaces = " ".repeat(i);
        let expr = self.get_expr(expr);
        eprint!("{spaces}");

        match &expr.data {
            //
            // Atoms
            //
            expr::Data::Int(value) => eprintln!("INT ({})", value),
            expr::Data::Float(value) => eprintln!("FLOAT ({})", value),
            expr::Data::Bool(value) => eprintln!("BOOL ({})", value),
            expr::Data::Str(value) => eprintln!("STR ({})", stringify_handle!(int, *value)),
            expr::Data::Symbol(name) => eprintln!("SYMBOL ({})", stringify_handle!(int, *name)),

            //
            // Compound
            //
            expr::Data::Postfix(opr) => {
                eprintln!("POSTFIX UNARY ({:?})", opr.op);
                self.print_expr(i + 2, int, opr.operand);
            }
            expr::Data::Prefix(opr) => {
                eprintln!("PREFIX UNARY ({:?})", opr.op);
                self.print_expr(i + 2, int, opr.operand);
            }
            expr::Data::Arithmetic(opr) => {
                eprintln!("BINARY ({:?})", opr.op);
                self.print_expr(i + 2, int, opr.lhs);
                self.print_expr(i + 2, int, opr.rhs);
            }
            expr::Data::Call { callee, args } => {
                eprintln!("CALL");
                self.print_expr(i + 2, int, *callee);
                eprintln!("{spaces}  ARGS");
                args.iter()
                    .for_each(|arg| self.print_expr(i + 4, int, *arg));
                eprintln!("{spaces}  END ARGS");
            }
            expr::Data::Assign(opr) => {
                eprintln!("ASSIGN ({:?})", opr.op);
                self.print_expr(i + 2, int, opr.rhs);
                eprintln!("{spaces}TO");
                self.print_expr(i + 2, int, opr.lhs);
                eprintln!("{spaces}END ASSIGN");
            }

            _ => unreachable!("unknown expression data in pretty printer!"),
        }
    }
}
