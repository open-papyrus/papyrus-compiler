use crate::span::Span;
use papyrus_compiler_lexer::syntax::token::Token;

#[derive(Debug)]
pub struct Error<'a> {
    span: Span,
    label: Option<&'static str>,
    expected: Vec<Option<Token<'a>>>,
    found: Option<Token<'a>>
}

impl<'a> Error<'a> {
    pub fn span(&self) -> Span {
        self.span.clone()
    }

    pub fn label(&self) -> Option<&'static str> {
        self.label
    }
    
    pub fn expected(&self) -> &Vec<Option<Token<'a>>> {
        &self.expected
    }
    
    pub fn found(&self) -> Option<&Token<'a>> {
        self.found.as_ref()
    }
}

impl<'a> chumsky::Error<Token<'a>> for Error<'a> {
    type Span = Span;
    type Label = &'static str;

    fn expected_input_found<Iter: IntoIterator<Item = Option<Token<'a>>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<Token<'a>>,
    ) -> Self {
        Self {
            span,
            label: None,
            expected: expected.into_iter().collect(),
            found,
        }
    }

    fn with_label(mut self, label: Self::Label) -> Self {
        self.label.get_or_insert(label);
        self
    }

    fn merge(mut self, mut other: Self) -> Self {
        self.expected.append(&mut other.expected);
        self
    }
}
