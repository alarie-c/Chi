use crate::{handle::Handle, interner::Substring, parsing::expr::Expr, token::Span};

/// Used to store a contiguous sequence of statements in the AST.
#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Handle<Stmt>>,
}

pub mod stmt_fragments {
    use super::*;

    #[derive(Debug)]
    pub struct Binding {
        pub symbol: Handle<Substring>,
        pub init: Handle<Expr>,
        pub mutable: bool,
    }
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Stmt Data
// ------------------------------------------------------------------------------------------------------------------ //

/// Used to model the node information for an statement, including it's variants and each variant's data.
#[derive(Debug)]
pub enum StmtData {
    If {
        cond: Handle<Expr>,
        if_br: Block,
        el_br: Option<Block>,
    },
    Expr {
        expr: Handle<Expr>,
    },
    Binding(stmt_fragments::Binding),
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
    /// Creates a new statement with the given data.
    pub fn new(span: Span, data: StmtData) -> Self {
        Self { span, data }
    }
}
