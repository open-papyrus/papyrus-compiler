use crate::syntax::token::Token;

pub mod syntax;

pub struct Lexer<'a> {
    lex: logos::Lexer<'a, Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        use logos::Logos;

        Self {
            lex: Token::lexer(input),
        }
    }

    pub fn lex_all(input: &'a str) -> Vec<Token<'a>> {
        let me = Self::new(input);
        let tokens: Vec<Token<'a>> = me.collect();
        tokens
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex.next()
    }
}

pub struct SpannedLexer<'a> {
    iter: logos::SpannedIter<'a, Token<'a>>,
}

impl<'a> SpannedLexer<'a> {
    pub fn new(input: &'a str) -> Self {
        use logos::Logos;

        Self {
            iter: Token::lexer(input).spanned(),
        }
    }

    pub fn lex_all(input: &'a str) -> Vec<SpannedToken<'a>> {
        let me = Self::new(input);
        let tokens: Vec<SpannedToken<'a>> = me.collect();
        tokens
    }
}

pub type SpannedToken<'a> = (Token<'a>, core::ops::Range<usize>);

impl<'a> Iterator for SpannedLexer<'a> {
    type Item = SpannedToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}
