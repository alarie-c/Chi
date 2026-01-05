
/// Categorizes operators by functionality and where they may be used semantically.
#[derive(Debug, PartialEq, Eq)]
pub enum OperatorKind {
    Arith,
    Assign,
    Unary,
    Logical,
    Compare,
    Equality,
}

/// Represents one semantically meaningful operation.
#[derive(Debug)]
pub enum Op {
    //
    // Arithmetic
    //
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    //
    // Assignment
    //
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    PowAssign,
    FloorAssign,
    Assign,

    //
    // Unary
    //
    Inc,
    Dec,
    Neg,
    Not,

    //
    // Logical
    //
    And,
    Or,

    //
    // Equality
    //
    Eq,
    Neq,

    //
    // Comparison
    //
    Lt,
    Gt,
    LtEq,
    GtEq,
}

impl Op {
    /// Returns the category of the operator.
    pub fn get_kind(&self) -> OperatorKind {
        match self {
            Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Mod => OperatorKind::Arith,
            Op::Lt | Op::Gt | Op::LtEq | Op::GtEq => OperatorKind::Compare,
            Op::Inc | Op::Dec | Op::Neg | Op::Not => OperatorKind::Unary,
            Op::And | Op::Or => OperatorKind::Logical,
            Op::Eq | Op::Neq => OperatorKind::Equality,
            Op::AddAssign
            | Op::SubAssign
            | Op::MulAssign
            | Op::DivAssign
            | Op::ModAssign
            | Op::PowAssign
            | Op::FloorAssign
            | Op::Assign => OperatorKind::Assign,
        }
    }
}
