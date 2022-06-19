use papyrus_compiler_diagnostics::{error_paint, Diagnostic, SeverityLevel, SourceId, SourceRange};
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
    source_id: SourceId,
    logos_span: logos::Span,
}

impl LexerDiagnostics {
    pub fn new(kind: LexerDiagnosticsKind, source_id: SourceId, logos_span: logos::Span) -> Self {
        Self {
            kind,
            source_id,
            logos_span,
        }
    }

    #[cfg(test)]
    pub fn kind(&self) -> &LexerDiagnosticsKind {
        &self.kind
    }
}

impl Diagnostic for LexerDiagnostics {
    fn prefix(&self) -> &'static str {
        "L"
    }

    fn documentation_section(&self) -> &'static str {
        "Lexer_Diagnostics"
    }

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
            LexerDiagnosticsKind::UnknownToken => format!("{}", error_paint("Unknown token")),
            LexerDiagnosticsKind::ParseIntError(err) => format!("{}", error_paint(err)),
            LexerDiagnosticsKind::ParseFloatError(err) => format!("{}", error_paint(err)),
            LexerDiagnosticsKind::FloatNotFinite => {
                format!("{}", error_paint("Number is not finite"))
            }
        }
    }

    fn level(&self) -> SeverityLevel {
        // all lexer diagnostics are errors
        SeverityLevel::Error
    }

    fn documentation_heading(&self) -> &'static str {
        match &self.kind {
            LexerDiagnosticsKind::UnknownToken => "l001-unknown-token",
            LexerDiagnosticsKind::ParseIntError(_) => "l002-parseinterror",
            LexerDiagnosticsKind::ParseFloatError(_) => "l003-parsefloaterror",
            LexerDiagnosticsKind::FloatNotFinite => "l004-number-is-not-finite",
        }
    }

    fn source_id(&self) -> SourceId {
        self.source_id
    }

    fn range(&self) -> SourceRange {
        self.logos_span.clone()
    }
}
