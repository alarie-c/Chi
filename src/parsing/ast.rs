use crate::{
    file::File,
    handle::Handle,
    interner::Interner,
    parsing::{
        decl::{self, Decl},
        expr::{self, Expr},
    },
};

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
    decls: Vec<Decl>,

    root: Vec<Handle<Decl>>,
}

impl Ast {
    /// Creates a new empty AST.
    pub fn new(file: Handle<File>) -> Self {
        Self {
            file,
            exprs: vec![],
            decls: vec![],
            root: vec![],
        }
    }

    pub fn iter_root(&mut self) -> std::slice::Iter<'_, Handle<Decl>> {
        self.root.iter()
    }

    /// Pushes the given handle to the root container.
    pub fn push_to_root(&mut self, handle: Handle<Decl>) {
        self.root.push(handle);
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