use papyrus_compiler_diagnostics::{Diagnostic, SeverityLevel, SourceId, SourceRange};
use papyrus_compiler_lexer::syntax::token::Token;
use std::collections::HashSet;

pub enum ParserDiagnosticsKind<'a> {
    ExpectedNothing {
        found: Option<Token<'a>>,
    },
    ExpectedOne {
        found: Option<Token<'a>>,
        expected: Token<'a>,
    },
    ExpectedOneOf {
        found: Option<Token<'a>>,
        expected: HashSet<Token<'a>>,
    },
}

pub struct ParserDiagnostics<'a> {
    kind: ParserDiagnosticsKind<'a>,
    source_id: SourceId,
    source_range: SourceRange,
}

impl<'a> ParserDiagnostics<'a> {
    pub fn new(
        kind: ParserDiagnosticsKind<'a>,
        source_id: SourceId,
        source_range: SourceRange,
    ) -> Self {
        Self {
            kind,
            source_id,
            source_range,
        }
    }
}

fn optional_token_to_string(token: Option<&Token>) -> String {
    match token {
        Some(token) => token.error_display(),
        None => "Nothing".to_string(),
    }
}

impl<'a> Diagnostic for ParserDiagnostics<'a> {
    fn id(&self) -> u32 {
        match &self.kind {
            ParserDiagnosticsKind::ExpectedNothing { .. } => 1,
            ParserDiagnosticsKind::ExpectedOne { .. } => 2,
            ParserDiagnosticsKind::ExpectedOneOf { .. } => 3,
        }
    }

    fn prefix(&self) -> &'static str {
        "P"
    }

    fn message(&self) -> String {
        match &self.kind {
            ParserDiagnosticsKind::ExpectedNothing { found } => format!(
                "Unexpected Token, found {} expected Nothing",
                optional_token_to_string(found.as_ref())
            ),
            ParserDiagnosticsKind::ExpectedOne { found, expected } => format!(
                "Unexpected Token, found {} expected {}",
                optional_token_to_string(found.as_ref()),
                expected.error_display()
            ),
            ParserDiagnosticsKind::ExpectedOneOf { found, expected } => {
                let strings = expected
                    .iter()
                    .map(|token| token.error_display())
                    .collect::<Vec<_>>();

                let capacity = strings
                    .iter()
                    .map(|x| x.len())
                    .reduce(|prev, cur| prev + cur)
                    .unwrap_or(1)
                    + (strings.len() - 1) * 2
                    + 4;

                let mut expected = String::with_capacity(capacity);
                for (i, s) in strings.iter().enumerate() {
                    if i == 0 {
                        expected.push_str(s.as_str());
                    } else if i == strings.len() - 1 {
                        expected.push_str(" or ");
                        expected.push_str(s.as_str());
                    } else {
                        expected.push_str(", ");
                        expected.push_str(s.as_str());
                    }
                }

                format!(
                    "Unexpected Token, found {} expected {}",
                    optional_token_to_string(found.as_ref()),
                    expected
                )
            }
        }
    }

    fn level(&self) -> SeverityLevel {
        // all parser diagnostics are errors
        SeverityLevel::Error
    }

    fn source_id(&self) -> SourceId {
        self.source_id
    }

    fn range(&self) -> SourceRange {
        self.source_range.clone()
    }
}
