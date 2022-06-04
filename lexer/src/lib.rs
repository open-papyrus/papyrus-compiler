use std::fmt::{Display, Formatter};

pub mod syntax;

pub struct Lexer<'a> {
    lex: logos::Lexer<'a, syntax::token::Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        use logos::Logos;

        Self {
            lex: syntax::token::Token::lexer(input),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = syntax::token::Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex.next()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpannedToken<'a> {
    pub token: syntax::token::Token<'a>,
    pub span: logos::Span,
}

impl<'a> Display for SpannedToken<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} ({:?})", self.token, self.span)
    }
}

pub struct SpannedLexer<'a> {
    iter: logos::SpannedIter<'a, syntax::token::Token<'a>>,
}

impl<'a> SpannedLexer<'a> {
    pub fn new(input: &'a str) -> Self {
        use logos::Logos;

        Self {
            iter: syntax::token::Token::lexer(input).spanned(),
        }
    }
}

impl<'a> Iterator for SpannedLexer<'a> {
    type Item = SpannedToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (token, span) = self.iter.next()?;
        Some(SpannedToken { token, span })
    }
}
