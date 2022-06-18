use papyrus_compiler_diagnostics::Diagnostic;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::num::{ParseFloatError, ParseIntError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexerDiagnosticsKind {
    UnknownToken,
    ParseIntError(ParseIntError),
    ParseFloatError(ParseFloatError),
    FloatNotFinite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexerDiagnostics {
    kind: LexerDiagnosticsKind,
    span: logos::Span,
}

impl LexerDiagnostics {
    pub fn new(kind: LexerDiagnosticsKind, span: logos::Span) -> Self {
        Self { kind, span }
    }

    #[cfg(test)]
    pub fn kind(&self) -> &LexerDiagnosticsKind {
        &self.kind
    }
}

impl Error for LexerDiagnostics {}

impl Display for LexerDiagnostics {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display())
    }
}

impl Diagnostic for LexerDiagnostics {
    type Span = logos::Span;

    fn id(&self) -> u32 {
        match &self.kind {
            LexerDiagnosticsKind::UnknownToken => 1,
            LexerDiagnosticsKind::ParseIntError(_) => 2,
            LexerDiagnosticsKind::ParseFloatError(_) => 3,
            LexerDiagnosticsKind::FloatNotFinite => 4,
        }
    }

    fn message(&self) -> String {
        match &self.kind {
            LexerDiagnosticsKind::UnknownToken => "Unknown token".to_string(),
            LexerDiagnosticsKind::ParseIntError(err) => format!("{}", err),
            LexerDiagnosticsKind::ParseFloatError(err) => format!("{}", err),
            LexerDiagnosticsKind::FloatNotFinite => "number is not finite".to_string(),
        }
    }

    fn span(&self) -> Self::Span {
        self.span.clone()
    }
}
