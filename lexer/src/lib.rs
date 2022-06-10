use crate::syntax::token::Token;
use logos::Logos;

pub mod syntax;

pub fn run_lexer<'a>(input: &'a str) -> Vec<(Token<'a>, logos::Span)> {
    let lexer: logos::Lexer<'a, Token<'a>> = Token::lexer(input);
    let iter: logos::SpannedIter<'a, Token<'a>> = lexer.spanned();
    let tokens: Vec<_> = iter.collect();
    tokens
}

#[cfg(test)]
mod test {
    use crate::{run_lexer, Token};

    #[test]
    fn test_run_lexer() {
        let input = "asd^asd";
        let expected = vec![
            (Token::Identifier("asd"), 0..3),
            (Token::Error, 3..4),
            (Token::Identifier("asd"), 4..7),
        ];

        let res = run_lexer(input);
        assert_eq!(res, expected);
    }
}
