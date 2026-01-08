use crate::{file::File, handle::Handle, lexer::lex, parsing::{parser::parse, printer::pretty_print_ast}, token::TokenKind};
use std::fs;

pub mod error;
pub mod file;
pub mod handle;
pub mod interner;
pub mod lexer;
pub mod operator;
pub mod parsing;
pub mod token;

fn main() {
    let text = fs::read_to_string("test.x").expect("could not find 'test.x'!");
    let (tokens, errors) = lex(Handle::<File>::null(), &text);

    let bytes = text.as_bytes();
    for tok in &tokens {
        if tok.kind == TokenKind::Eof {
            continue;
        }

        let slice = &bytes[tok.span.offset..tok.span.offset + tok.span.len];
        let lexeme = String::from_utf8(slice.iter().map(|b| *b).collect::<Vec<u8>>())
            .expect("invalid token byte sequence to utf8 string!");
        eprint!("{:?}\n--> '{}'\n", tok, lexeme);
    }

    for err in errors {
        eprintln!("{:#?}", err);
    }

    let (mut ast, _parse_errors, _interner) = parse(Handle::<File>::null(), &text.as_bytes(), &tokens);
    pretty_print_ast(&mut ast, &_interner, std::io::stdout());
    println!("parse_errors: {:#?}", _parse_errors);
}
