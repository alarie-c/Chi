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
        ast_printer
            .print_decl(decl)
            .expect("issue printing the AST");
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
                self.indent();
                self.indent();
                self.print_block(body)?;
                self.dedent();
                self.dedent();
            }
        }

        self.writer.flush()
    }
}

impl<'a, W: std::io::Write> AstPrinter<'a, W> {
    fn print_block(&mut self, block: &expr::Block) -> std::io::Result<()> {
        let spaces = " ".repeat((self.indent * Self::INDENT_SPACE_CT) as usize);
        for (i, expr) in block.iter().enumerate() {
            write!(self.writer, "{spaces}{i}: ")?;
            self.print_expr(*expr)?;
            write!(self.writer, "\n")?;
        }
        Ok(())
    }

    fn print_expr(&mut self, handle: Handle<Expr>) -> std::io::Result<()> {
        let spaces = " ".repeat((self.indent * Self::INDENT_SPACE_CT) as usize);
        let expr = self.ast.get_expr(handle);

        match &expr.data {
            //
            // Atoms
            //
            expr::Data::Int(v) => write!(self.writer, "Int({v})")?,
            expr::Data::Float(v) => write!(self.writer, "Float({v})")?,
            expr::Data::Bool(v) => write!(self.writer, "Bool({v})")?,
            expr::Data::Str(v) => write!(self.writer, "Str({})", deref_handle!(self.interner, *v))?,
            expr::Data::Symbol(v) => {
                write!(self.writer, "Symbol({})", deref_handle!(self.interner, *v))?
            }

            //
            // Unary operations
            //
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

            //
            // Binary operations
            //
            expr::Data::Arithmetic(oper) => self.print_binary_oper("Arithmetic", oper)?,
            expr::Data::Compare(oper) => self.print_binary_oper("Compare", oper)?,
            expr::Data::Logical(oper) => self.print_binary_oper("Logical", oper)?,
            expr::Data::Equality(oper) => self.print_binary_oper("Equality", oper)?,
            expr::Data::Assign(oper) => self.print_binary_oper("Assign", oper)?,

            expr::Data::Block(block) => self.print_block(block)?,
            expr::Data::Discard(expr) => {
                write!(self.writer, "Discard(")?;
                self.print_expr(*expr)?;
                write!(self.writer, ")")?;
            }

            //
            // Effectful expressions
            //
            expr::Data::Binding {
                symbol,
                init,
                mutable,
            } => {
                // Print the name
                write!(
                    self.writer,
                    "Binding({}",
                    deref_handle!(self.interner, *symbol)
                )?;

                // Print mutability
                let mutability = if *mutable { "Mutable" } else { "Immutable" };
                write!(self.writer, ", {mutability} = ")?;

                // Print the initial value and close
                self.print_expr(*init)?;
                write!(self.writer, ")")?;
            }
            expr::Data::Call { callee, args } => {
                write!(self.writer, "Call(")?;
                self.print_expr(*callee)?;
                write!(self.writer, ")\n")?;

                // Print arguments
                for a in args.iter() {
                    write!(self.writer, "{spaces}  Arg(")?;
                    
                    // Print the label
                    if let Some(label) = a.label {
                        write!(self.writer, "'{}', ", deref_handle!(self.interner, label))?;
                    }

                    // Then print the value
                    self.print_expr(*&a.value)?;

                    write!(self.writer, ")\n")?;
                }
            }

            //
            // Control flow
            //
            expr::Data::If {
                cond,
                if_block,
                el_block,
            } => {
                // Print the condition
                write!(self.writer, "If(")?;
                self.print_expr(*cond)?;
                write!(self.writer, ")\n")?;

                // Print the if block
                self.indent();
                self.print_block(if_block)?;
                self.dedent();

                // Print the else block
                if let Some(b) = el_block {
                    write!(self.writer, "Else\n")?;
                    self.indent();
                    self.print_block(b)?;
                    self.dedent();
                }
            }
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
        for (i, p) in sig.parameters.iter().enumerate() {
            let name = deref_handle!(self.interner, p.name);

            // Start with the preamble/name
            write!(self.writer, "{spaces}  {i}: Parameter({}", name)?;

            // Then a label if it exists
            if let Some(l) = p.label {
                let label = deref_handle!(self.interner, l);
                write!(self.writer, ", '{label}'")?;
            }

            // Then its mutability
            let mutable = if p.mutable { "Mutable" } else { "Immutable" };
            write!(self.writer, ", {mutable}")?;

            // Then a type name
            write!(self.writer, ", ")?;
            self.print_expr(p.type_name)?;

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

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn dedent(&mut self) {
        let _ = self.indent.saturating_sub(1);
    }
}
