// ------------------------------------------------------------------------------------------------------------------ //
// Declarations are the highest level AST structure for any compilation unit.

use crate::{handle::Handle, interner::Substring, parsing::expr::{self, Expr}, token::Span};

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Componenets
// ------------------------------------------------------------------------------------------------------------------ //

#[derive(Debug)]
pub struct Parameter {
    pub label: Option<Handle<Substring>>,
    pub name: Handle<Substring>,
    pub type_name: Handle<Expr>,
    pub mutable: bool,
    pub default: Option<Handle<Expr>>,
}

#[derive(Debug)]
pub struct FnSignature {
    // @todo update this to some kind of "impl Name/Path"
    pub name: Handle<Substring>,
    pub parameters: Parameters,
    pub return_type: Option<Handle<Expr>>,
}

#[derive(Debug)]
pub struct Parameters(Vec<Parameter>);

impl Parameters {
    pub fn arity(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Parameter> {
        self.0.iter()
    }
}

impl From<Vec<Parameter>> for Parameters {
    fn from(value: Vec<Parameter>) -> Self {
        Self(value)
    }
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Decl Data
// ------------------------------------------------------------------------------------------------------------------ //


#[derive(Debug)]
pub enum Data {
    FnDecl {
        signature: FnSignature,
    },
    FnDef {
        signature: FnSignature,
        body: expr::Block,
    },
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Decl
// ------------------------------------------------------------------------------------------------------------------ //

#[derive(Debug)]
pub struct Decl {
    pub span: Span,
    pub data: Data,
}

impl Decl {
    /// Creates a new declaration with the given data.
    pub fn new(span: Span, data: Data) -> Self {
        Self { span, data }
    }
}