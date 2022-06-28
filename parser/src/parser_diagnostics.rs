use crate::ParserError;
use papyrus_compiler_diagnostics::{
    error_paint, good_paint, Diagnostic, SeverityLevel, SourceId, SourceRange,
};

pub struct ParserDiagnostic<'source> {
    source_id: SourceId,
    error: ParserError<'source>,
}

impl<'source> ParserDiagnostic<'source> {
    pub fn new(source_id: SourceId, error: ParserError<'source>) -> Self {
        ParserDiagnostic { source_id, error }
    }
}

impl<'source> Diagnostic for ParserDiagnostic<'source> {
    fn prefix(&self) -> &'static str {
        "P"
    }

    fn documentation_section(&self) -> &'static str {
        "Parser_Diagnostics"
    }

    fn id(&self) -> u32 {
        match &self.error {
            ParserError::ExpectedToken { .. } => 1,
            ParserError::UnexpectedEOI => 2,
            ParserError::ExpectedEOI { .. } => 3,
            _ => panic!(),
        }
    }

    fn message(&self) -> String {
        match &self.error {
            ParserError::ExpectedToken { expected, found } => {
                let (found, _) = found;
                format!(
                    "Expected {} found {}",
                    good_paint(expected.error_display()),
                    error_paint(found.error_display())
                )
            }
            ParserError::UnexpectedEOI => "Unexpected EOI".to_string(),
            ParserError::ExpectedEOI { found } => {
                let (found, _) = found;
                format!(
                    "Expected {} found {}",
                    good_paint("EOI"),
                    error_paint(found.error_display())
                )
            }
            _ => panic!(),
        }
    }

    fn level(&self) -> SeverityLevel {
        // parser errors are errors, duh
        SeverityLevel::Error
    }

    fn documentation_heading(&self) -> &'static str {
        ""
    }

    fn source_id(&self) -> SourceId {
        self.source_id
    }

    fn range(&self) -> SourceRange {
        match &self.error {
            ParserError::ExpectedToken { expected: _, found } => {
                let (_, range) = found;
                range.clone()
            }
            ParserError::ExpectedEOI { found } => {
                let (_, range) = found;
                range.clone()
            }
            _ => panic!(),
        }
    }
}
