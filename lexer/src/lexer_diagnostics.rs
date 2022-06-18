use papyrus_compiler_diagnostics::{Diagnostic, Range, SourceId};
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
    fn id(&self) -> u32 {
        match &self.kind {
            LexerDiagnosticsKind::UnknownToken => 1,
            LexerDiagnosticsKind::ParseIntError(_) => 2,
            LexerDiagnosticsKind::ParseFloatError(_) => 3,
            LexerDiagnosticsKind::FloatNotFinite => 4,
        }
    }

    fn prefix(&self) -> &'static str {
        "L"
    }

    fn message(&self) -> String {
        match &self.kind {
            LexerDiagnosticsKind::UnknownToken => "Unknown token".to_string(),
            LexerDiagnosticsKind::ParseIntError(err) => format!("{}", err),
            LexerDiagnosticsKind::ParseFloatError(err) => format!("{}", err),
            LexerDiagnosticsKind::FloatNotFinite => "number is not finite".to_string(),
        }
    }

    fn source_id(&self) -> SourceId {
        self.source_id
    }

    fn range(&self) -> Range {
        self.logos_span.clone()
    }
}
