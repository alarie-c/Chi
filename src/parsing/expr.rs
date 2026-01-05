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
    ExprInt {
        value: i64,
    },
    ExprFloat {
        value: f64,
    },
    ExprBool {
        value: bool,
    },
    ExprStr {
        value: Handle<Substring>,
    },
    ExprSymbol {
        name: Handle<Substring>,
    },

    //
    // Compound
    //
    ExprCall {
        callee: Handle<Expr>,
        args: Vec<Handle<Expr>>,
    },
    ExprPrefixUnary {
        operand: Handle<Expr>,
        op: Op,
    },
    ExprPostfixUnary {
        operand: Handle<Expr>,
        op: Op,
    },
    ExprBinary {
        lhs: Handle<Expr>,
        rhs: Handle<Expr>,
        op: Op,
    },
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
