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
