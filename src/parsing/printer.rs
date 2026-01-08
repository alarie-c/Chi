use crate::{
    handle::Handle,
    interner::Interner,
    parsing::{
        ast::Ast,
        decl::{self, Decl},
        expr::{self, Expr},
    },
};

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Macros
// ------------------------------------------------------------------------------------------------------------------ //

const INVALID_STR: &'static str = "<invalid substring handle>";

/// Used when printing substrings to resolve a handle into a string safely.
macro_rules! deref_handle {
    ($int:expr, $handle:expr) => {
        ($int).get_string($handle).unwrap_or(INVALID_STR.into())
    };
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Printer API
// ------------------------------------------------------------------------------------------------------------------ //

pub fn pretty_print_ast<'a, W>(ast: &'a mut Ast, interner: &'a Interner, writer: W)
where
    W: std::io::Write,
{
    let root: Vec<Handle<Decl>> = ast.iter_root().map(|decl| *decl).collect();

    let mut ast_printer = AstPrinter {
        ast,
        interner,
        indent: 0,
        writer,
    };

    for decl in root {
        ast_printer.print_decl(decl).expect("issue printing the AST");
    }
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Pretty Printer
// ------------------------------------------------------------------------------------------------------------------ //

struct AstPrinter<'a, W: std::io::Write> {
    ast: &'a Ast,
    interner: &'a Interner,
    indent: u8,
    writer: W,
}

impl<'a, W: std::io::Write> AstPrinter<'a, W> {
    const INDENT_SPACE_CT: u8 = 2;

    pub fn print_decl(&mut self, handle: Handle<Decl>) -> std::io::Result<()> {
        let spaces = " ".repeat((self.indent * Self::INDENT_SPACE_CT) as usize);
        let decl = self.ast.get_decl(handle);
        write!(self.writer, "{}", spaces)?;

        match &decl.data {
            decl::Data::FnDecl { signature } => self.print_fn_signature(signature)?,
            decl::Data::FnDef { signature, body } => {
                self.print_fn_signature(signature)?;
                self.print_block(body)?;
            }
        }

        self.writer.flush()
    }
}

impl<'a, W: std::io::Write> AstPrinter<'a, W> {
    fn print_block(&mut self, block: &expr::Block) -> std::io::Result<()> {
        for expr in block.iter() {
            self.print_expr(*expr)?;
        }
        Ok(())
    }

    fn print_expr(&mut self, handle: Handle<Expr>) -> std::io::Result<()> {
        let spaces = " ".repeat((self.indent * Self::INDENT_SPACE_CT) as usize);
        let expr = self.ast.get_expr(handle);
        write!(self.writer, "{}", spaces)?;

        match &expr.data {
            expr::Data::Int(v) => write!(self.writer, "Int({v})")?,
            expr::Data::Float(v) => write!(self.writer, "Float({v})")?,
            expr::Data::Bool(v) => write!(self.writer, "Bool({v})")?,
            expr::Data::Str(v) => write!(self.writer, "Str({})", deref_handle!(self.interner, *v))?,
            expr::Data::Symbol(v) => {
                write!(self.writer, "Symbol({})", deref_handle!(self.interner, *v))?
            }

            expr::Data::Call { callee, args } => {
                write!(self.writer, "Call(")?;
                self.print_expr(*callee)?;

                // Print arguments
                for a in args.iter() {
                    write!(self.writer, ", ")?;
                    self.print_expr(*a)?;
                }

                write!(self.writer, ")")?;
            }

            expr::Data::Postfix(oper) => {
                write!(self.writer, "Postfix(")?;
                self.print_expr(oper.operand)?;
                write!(self.writer, ", {:?})", oper.op)?;
            }

            expr::Data::Prefix(oper) => {
                write!(self.writer, "Prefix({:?}", oper.op)?;
                self.print_expr(oper.operand)?;
                write!(self.writer, ")")?;
            }

            expr::Data::Arithmetic(oper) => self.print_binary_oper("Arithmetic", oper)?,
            expr::Data::Compare(oper) => self.print_binary_oper("Compare", oper)?,
            expr::Data::Logical(oper) => self.print_binary_oper("Logical", oper)?,
            expr::Data::Equality(oper) => self.print_binary_oper("Equality", oper)?,
            expr::Data::Assign(oper) => self.print_binary_oper("Assign", oper)?,

            expr::Data::Block(block) => self.print_block(block)?,
        }

        Ok(())
    }

    fn print_binary_oper(
        &mut self,
        kind: &'static str,
        oper: &expr::BinaryOper,
    ) -> std::io::Result<()> {
        write!(self.writer, "{kind}(")?;
        self.print_expr(oper.lhs)?;
        write!(self.writer, ", {:?}, ", oper.op)?;
        self.print_expr(oper.rhs)?;
        write!(self.writer, ")")?;
        Ok(())
    }

    fn print_fn_signature(&mut self, sig: &decl::FnSignature) -> std::io::Result<()> {
        let spaces = " ".repeat((self.indent * Self::INDENT_SPACE_CT) as usize);
        let fn_name = deref_handle!(self.interner, sig.name);

        write!(self.writer, "Function({}) -> ", fn_name)?;

        // Print the return type, if there is one, otherwise print void
        if let Some(e) = sig.return_type {
            self.print_expr(e)?;
        } else {
            write!(self.writer, "void")?;
        }

        // Quick newline to get to parameters
        write!(self.writer, "\n")?;

        // Then print the parameters
        for p in sig.parameters.iter() {
            let name = deref_handle!(self.interner, p.name);

            // Start with the preamble/name
            write!(self.writer, "{spaces}    Parameter({}", name)?;

            // Then its mutability
            let mutable = if p.mutable { "Mutable" } else { "Immutable" };
            write!(self.writer, ", {mutable}")?;

            // Then a label if it exists
            if let Some(l) = p.label {
                let label = deref_handle!(self.interner, l);
                write!(self.writer, ", '{label}'")?;
            }

            // Then a default if it exists
            if let Some(d) = p.default {
                write!(self.writer, ", Default(")?;
                self.print_expr(d)?;
                write!(self.writer, ")")?;
            }

            // Newline to finish it off
            write!(self.writer, ")\n")?;
        }

        Ok(())
    }
}
