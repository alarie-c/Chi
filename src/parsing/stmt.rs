use crate::{handle::Handle, interner::Substring, parsing::expr::Expr, token::Span};

/// Used to store a contiguous sequence of statements in the AST.
#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Handle<Stmt>>,
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Stmt Data
// ------------------------------------------------------------------------------------------------------------------ //

/// Used to model the node information for an statement, including it's variants and each variant's data.
#[derive(Debug)]
pub enum StmtData {
    Binding {
        symbol: Handle<Substring>,
        init: Handle<Expr>,
        mutable: bool,
    },
    If {
        cond: Handle<Expr>,
        if_br: Block,
        el_br: Option<Block>,
    },
    Expr {
        expr: Handle<Expr>,
    },
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Stmt
// ------------------------------------------------------------------------------------------------------------------ //

#[derive(Debug)]
pub struct Stmt {
    pub span: Span,
    pub data: StmtData,
}

impl Stmt {
    /// Creates a new expression with the given data.
    pub fn new(span: Span, data: StmtData) -> Self {
        Self { span, data }
    }
}
