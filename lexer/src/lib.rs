extern crate core;

use crate::lexer_diagnostics::{LexerDiagnostics, LexerDiagnosticsKind};
use crate::syntax::token::{LexerExtras, Token};
use logos::Logos;

pub mod lexer_diagnostics;
pub mod syntax;

pub fn run_lexer<'a>(input: &'a str) -> Vec<(Token<'a>, logos::Span)> {
    let lexer: logos::Lexer<'a, Token<'a>> = Token::lexer(input);
    let iter: logos::SpannedIter<'a, Token<'a>> = lexer.spanned();
    let tokens: Vec<_> = iter.collect();
    tokens
}

struct CustomSpannedIterator<'a> {
    pub lexer: logos::Lexer<'a, Token<'a>>,
}

impl<'a> CustomSpannedIterator<'a> {
    pub fn new(lexer: logos::Lexer<'a, Token<'a>>) -> Self {
        Self { lexer }
    }

    pub fn extras(&self) -> &LexerExtras {
        &self.lexer.extras
    }
}

impl<'a> Iterator for CustomSpannedIterator<'a> {
    type Item = (Token<'a>, logos::Span);

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next().map(|token| (token, self.lexer.span()))
    }
}

pub fn run_lexer_with_result<'a>(
    input: &'a str,
) -> Result<Vec<(Token, logos::Span)>, Vec<LexerDiagnostics>> {
    let lexer: logos::Lexer<'a, Token<'a>> = Token::lexer(input);
    let mut spanned_iter = CustomSpannedIterator::new(lexer);

    let mut tokens = Vec::<(Token, logos::Span)>::new();
    let mut diagnostics = Vec::<LexerDiagnostics>::new();

    while let Some((token, span)) = spanned_iter.next() {
        match token {
            Token::Error => {
                let parsing_error = spanned_iter
                    .extras()
                    .parsing_errors
                    .iter()
                    .find(|(_, other_span)| other_span == &span);
                let res = match parsing_error {
                    Some((kind, _)) => LexerDiagnostics::new(kind.clone(), span),
                    _ => LexerDiagnostics::new(LexerDiagnosticsKind::UnknownToken, span),
                };

                diagnostics.push(res);
            }
            _ => tokens.push((token, span)),
        };
    }

    if diagnostics.is_empty() {
        Ok(tokens)
    } else {
        Err(diagnostics)
    }
}

#[cfg(test)]
mod test {
    use crate::{run_lexer, run_lexer_with_result, LexerDiagnostics, LexerDiagnosticsKind, Token};
    use papyrus_compiler_diagnostics::Diagnostic;

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

    #[test]
    fn test_lexer_with_result() {
        let src = "^ 2147483648 3402823470000000000000000000000000000000.0";
        let res = run_lexer_with_result(src).unwrap_err();

        assert_eq!(
            res[0],
            LexerDiagnostics::new(LexerDiagnosticsKind::UnknownToken, 0..1)
        );

        assert!(matches!(
            res[1].kind(),
            LexerDiagnosticsKind::ParseIntError(_)
        ));

        assert_eq!(res[1].span(), 2..12);

        assert_eq!(
            res[2],
            LexerDiagnostics::new(LexerDiagnosticsKind::FloatNotFinite, 13..55)
        )
    }
}
