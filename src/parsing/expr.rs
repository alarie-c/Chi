use crate::{
    handle::Handle, interner::Substring, operator::Op, token::Span
};

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Expr Data
// ------------------------------------------------------------------------------------------------------------------ //

/// Used to model the node information for an expression, including it's variants and each variant's data.
#[derive(Debug)]
pub enum ExprData { 
    //
    // Atom
    //
    Int {
        value: i64,
    },
    Float {
        value: f64,
    },
    Bool {
        value: bool,
    },
    Str {
        value: Handle<Substring>,
    },
    Symbol {
        name: Handle<Substring>,
    },

    //
    // Compound
    //
    Call {
        callee: Handle<Expr>,
        args: Vec<Handle<Expr>>,
    },
    UnaryPrefix {
        operand: Handle<Expr>,
        op: Op,
    },
    UnaryPostfix {
        operand: Handle<Expr>,
        op: Op,
    },
    BinaryArith {
        lhs: Handle<Expr>,
        rhs: Handle<Expr>,
        op: Op,
    },
    Assign {
        assignee: Handle<Expr>,
        value: Handle<Expr>,
        op: Op,
    }
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Expr
// ------------------------------------------------------------------------------------------------------------------ //

/// Models some expression in the AST, including underlying data and a location in the file.
#[derive(Debug)]
pub struct Expr {
    pub span: Span,
    pub data: ExprData,
}

impl Expr {
    /// Creates a new expression with the given data.
    pub fn new(span: Span, data: ExprData) -> Self {
        Self { span, data }
    }
}
