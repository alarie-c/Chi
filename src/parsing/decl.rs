use crate::{parsing::stmt::Block, token::Span};

pub mod decl_fragments {
    use crate::{handle::Handle, interner::Substring, parsing::expr::Expr};

    #[derive(Debug)]
    pub struct Parameter {
        pub ext_name: Handle<Substring>,
        pub int_name: Handle<Substring>,
        pub type_name: Handle<Expr>,
        pub mutable: bool,
        pub default: Option<Handle<Expr>>,
    }

    #[derive(Debug)]
    pub struct FnSig {
        // @todo update this to some kind of "impl Name/Path"
        pub name: Handle<Substring>,
        pub params: Vec<Parameter>,
        pub return_type: Option<Handle<Expr>>,
    }
}

#[derive(Debug)]
pub enum DeclData {
    FnDecl {
        sig: decl_fragments::FnSig,
    },
    FnDef {
        sig: decl_fragments::FnSig,
        body: Block,
    },
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Decl
// ------------------------------------------------------------------------------------------------------------------ //

#[derive(Debug)]
pub struct Decl {
    pub span: Span,
    pub data: DeclData,
}

impl Decl {
    /// Creates a new declaration with the given data.
    pub fn new(span: Span, data: DeclData) -> Self {
        Self { span, data }
    }
}