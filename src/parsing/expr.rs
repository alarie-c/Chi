use crate::{
    handle::Handle, interner::Substring, operator::Op, token::Span
};

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Componenets
// ------------------------------------------------------------------------------------------------------------------ //

#[derive(Debug)]
pub struct Block(Vec<Handle<Expr>>);

impl Block {
    pub fn count(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Handle<Expr>> {
        self.0.iter()
    }
}

impl From<Vec<Handle<Expr>>> for Block {
    fn from(value: Vec<Handle<Expr>>) -> Self {
        Self(value)
    }
}

#[derive(Debug)]
pub struct Arguments(Vec<Handle<Expr>>);

impl Arguments {
    pub fn arity(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Handle<Expr>> {
        self.0.iter()
    }
}

impl From<Vec<Handle<Expr>>> for Arguments {
    fn from(value: Vec<Handle<Expr>>) -> Self {
        Self(value)
    }
}

#[derive(Debug)]
pub struct BinaryOper {
    pub lhs: Handle<Expr>,
    pub rhs: Handle<Expr>,
    pub op: Op,
}

#[derive(Debug)]
pub struct UnaryOper {
    pub operand: Handle<Expr>,
    pub op: Op,
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Expr Data
// ------------------------------------------------------------------------------------------------------------------ //

/// Used to model the node information for an expression, including it's variants and each variant's data.
#[derive(Debug)]
pub enum Data { 
    //
    // Atom
    //
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(Handle<Substring>),
    Symbol(Handle<Substring>),

    //
    // Compound
    //
    Call {
        callee: Handle<Expr>,
        args: Arguments,
    },

    //
    // Unary Operations
    //
    Prefix(UnaryOper),
    Postfix(UnaryOper),
    
    //
    // Binary Operators
    //
    Arithmetic(BinaryOper),
    Logical(BinaryOper),
    Compare(BinaryOper),
    Equality(BinaryOper),
    Assign(BinaryOper),
    
    //
    //
    //
    Block(Block),
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Expr
// ------------------------------------------------------------------------------------------------------------------ //

/// Models some expression in the AST, including underlying data and a location in the file.
#[derive(Debug)]
pub struct Expr {
    pub span: Span,
    pub data: Data,
}

impl Expr {
    /// Creates a new expression with the given data.
    pub fn new(span: Span, data: Data) -> Self {
        Self { span, data }
    }
}
