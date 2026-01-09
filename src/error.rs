use crate::{token::Span};

// Alias
type Issue = ErrorIssue;

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Error
// ------------------------------------------------------------------------------------------------------------------ //

/// Used to represent some specific diagnostic that the compiler wants to output and display to the user.
/// Usually this is either an error, warning, or note.
#[derive(Debug)]
pub struct Error {
    /// The level or severity of this diagnostic.
    kind: ErrorKind,
    /// The specific enumerated issue this diagnostic is reporting.
    issue: ErrorIssue,
    /// The location of this diagnostic.
    span: Span,
    /// The message to be displayed to the user.
    msg: String,
}

impl Error {
    pub fn new(issue: ErrorIssue, span: Span, msg: String) -> Self { 
        let kind = match issue {
            Issue::InternalWarning => ErrorKind::Warning,
            Issue::InternalNote => ErrorKind::Note,
            _ => ErrorKind::Error,
        };

        Self {
            kind,
            issue,
            span,
            msg,
        }
    }
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Error Kind
// ------------------------------------------------------------------------------------------------------------------ //

/// Represents some level of severity for a diagnostic. Only `Error` is capable of aborting compilation.
#[derive(Debug)]
pub enum ErrorKind {
    Error,
    Warning,
    Note,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Error => "Error",
                Self::Warning => "Warning",
                Self::Note => "Note",
            }
        )
    }
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Error Issue
// ------------------------------------------------------------------------------------------------------------------ //

/// Enumerated specific diagnostic issues that may arise during compilation.
#[derive(Debug)]
pub enum ErrorIssue {
    //
    // Lexical Issues
    //
    UnrecognizedChar,
    UnterminatedString,
    UnterminatedLabel,

    //
    // Parse Issues
    //
    ExpectedExpression,
    ExpectedTypeName,
    InvalidSyntax,
    UnexpectedEoF,
    InvalidExprStmt,
    MissingDelimiter,

    //
    // Internal Issues
    //
    InternalError,
    InternalWarning,
    InternalNote,
}

impl std::fmt::Display for ErrorIssue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Issue::UnrecognizedChar => "Unrecognized Character",
                Issue::UnterminatedString => "Unterminated String",
                Issue::UnterminatedLabel => "Unterminated Label",
                
                //
                // Parse Issues
                //
                Issue::ExpectedExpression => "Expected Expression",
                Issue::ExpectedTypeName => "Expected Type Name",
                Issue::InvalidSyntax => "Invalid Syntax",
                Issue::UnexpectedEoF => "Unexpected EOF (end of file)",
                Issue::InvalidExprStmt => "Invalid Expression as a Statement",
                Issue::MissingDelimiter => "Missing Delimiter",

                //
                // Internal Issues
                //
                Issue::InternalError => "Internal Error",
                Issue::InternalWarning => "Internal Warning",
                Issue::InternalNote => "Internal Note",
            }
        )
    }
}