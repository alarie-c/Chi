use crate::{handle::Handle, interner::Substring, parsing::nodes::expr::Expr, token::Span};

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Componenets
// ------------------------------------------------------------------------------------------------------------------ //

/// Used to store a contiguous sequence of statements in the AST.
#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Handle<Stmt>>,
}

#[derive(Debug)]
pub struct Binding {
    pub symbol: Handle<Substring>,
    pub init: Handle<Expr>,
    pub mutable: bool,
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Stmt Data
// ------------------------------------------------------------------------------------------------------------------ //

/// Used to model the node information for an statement, including it's variants and each variant's data.
#[derive(Debug)]
pub enum Data {
    If {
        cond: Handle<Expr>,
        if_br: Block,
        el_br: Option<Block>,
    },
    Expr(Handle<Expr>),
    Binding(Binding),
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Stmt
// ------------------------------------------------------------------------------------------------------------------ //

#[derive(Debug)]
pub struct Stmt {
    pub span: Span,
    pub data: Data,
}

impl Stmt {
    /// Creates a new statement with the given data.
    pub fn new(span: Span, data: Data) -> Self {
        Self { span, data }
    }
}
