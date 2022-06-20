use crate::parser_diagnostics::{ParserDiagnostics, ParserDiagnosticsKind};
use papyrus_compiler_diagnostics::{SourceId, SourceRange};
use papyrus_compiler_lexer::syntax::token::Token;
use std::collections::HashSet;

#[derive(Debug)]
pub struct Error<'a> {
    range: SourceRange,
    label: Option<&'static str>,
    expected: HashSet<Token<'a>>,
    found: Option<Token<'a>>,
}

impl<'a> Error<'a> {
    pub fn range(&self) -> SourceRange {
        self.range.clone()
    }

    pub fn label(&self) -> Option<&'static str> {
        self.label
    }

    pub fn expected(&self) -> &HashSet<Token<'a>> {
        &self.expected
    }

    pub fn found(&self) -> Option<&Token<'a>> {
        self.found.as_ref()
    }

    pub fn to_diagnostics(self, id: SourceId) -> ParserDiagnostics<'a> {
        let kind = if self.expected.is_empty() {
            ParserDiagnosticsKind::ExpectedNothing { found: self.found }
        } else if self.expected.len() == 1 {
            let expected = self.expected.into_iter().next().unwrap();
            ParserDiagnosticsKind::ExpectedOne {
                found: self.found,
                expected,
            }
        } else {
            ParserDiagnosticsKind::ExpectedOneOf {
                found: self.found,
                expected: self.expected,
            }
        };

        ParserDiagnostics::new(kind, id, self.range)
    }
}

impl<'a> chumsky::Error<Token<'a>> for Error<'a> {
    type Span = SourceRange;
    type Label = &'static str;

    fn expected_input_found<Iter: IntoIterator<Item = Option<Token<'a>>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<Token<'a>>,
    ) -> Self {
        Self {
            range: span,
            label: None,
            expected: expected.into_iter().flatten().collect(),
            found,
        }
    }

    fn with_label(mut self, label: Self::Label) -> Self {
        self.label.get_or_insert(label);
        self
    }

    fn merge(mut self, other: Self) -> Self {
        for expected in other.expected {
            self.expected.insert(expected);
        }

        self
    }
}
