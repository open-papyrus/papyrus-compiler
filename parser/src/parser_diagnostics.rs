use papyrus_compiler_diagnostics::{
    error_paint, good_paint, Diagnostic, SeverityLevel, SourceId, SourceRange,
};
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
    fn prefix(&self) -> &'static str {
        "P"
    }

    fn documentation_section(&self) -> &'static str {
        match &self.kind {
            ParserDiagnosticsKind::ExpectedNothing { .. } => "p001-unexpected-token",
            ParserDiagnosticsKind::ExpectedOne { .. } => "p001-unexpected-token",
            ParserDiagnosticsKind::ExpectedOneOf { .. } => "p001-unexpected-token",
        }
    }

    fn id(&self) -> u32 {
        match &self.kind {
            ParserDiagnosticsKind::ExpectedNothing { .. } => 1,
            ParserDiagnosticsKind::ExpectedOne { .. } => 1,
            ParserDiagnosticsKind::ExpectedOneOf { .. } => 1,
        }
    }

    fn message(&self) -> String {
        match &self.kind {
            ParserDiagnosticsKind::ExpectedNothing { found } => format!(
                "Unexpected Token, found {} expected {}",
                error_paint(optional_token_to_string(found.as_ref())),
                good_paint("Nothing")
            ),
            ParserDiagnosticsKind::ExpectedOne { found, expected } => format!(
                "Unexpected Token, found {} expected {}",
                error_paint(optional_token_to_string(found.as_ref())),
                good_paint(expected.error_display())
            ),
            ParserDiagnosticsKind::ExpectedOneOf { found, expected } => {
                let strings = expected
                    .iter()
                    .map(|token| format!("{}", good_paint(token.error_display())))
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
                    error_paint(optional_token_to_string(found.as_ref())),
                    expected
                )
            }
        }
    }

    fn level(&self) -> SeverityLevel {
        // all parser diagnostics are errors
        SeverityLevel::Error
    }

    fn documentation_heading(&self) -> &'static str {
        "Parser_Diagnostics"
    }

    fn source_id(&self) -> SourceId {
        self.source_id
    }

    fn range(&self) -> SourceRange {
        self.source_range.clone()
    }
}
