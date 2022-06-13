use crate::source::*;
use ariadne::{Color, Fmt, Label, Report, ReportKind};
use papyrus_compiler_lexer::syntax::token::Token;
use std::fmt::{Display, Formatter};

pub type CustomReport = Report<SourceSpan>;

pub enum ErrorCode {
    LexerError = 1,
    ParserError = 2,
}

impl Display for ErrorCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorCode::LexerError => write!(f, "E{}", ErrorCode::LexerError as isize),
            ErrorCode::ParserError => write!(f, "E{}", ErrorCode::ParserError as isize),
        }
    }
}

pub type LexerSpan = core::ops::Range<usize>;

pub fn lexer_error_to_report(id: SourceId, span: LexerSpan) -> CustomReport {
    CustomReport::build(ReportKind::Error, id, span.start)
        .with_code(ErrorCode::LexerError)
        .with_message("Unknown Token")
        .with_label(
            Label::new(SourceSpan { id, range: span })
                .with_message("Unknown Token")
                .with_color(Color::Red),
        )
        .finish()
}

fn optional_token_to_string(token: Option<&Token>) -> String {
    match token {
        Some(token) => token.error_display(),
        None => "Nothing".to_string(),
    }
}

pub fn parser_error_to_report(parser_error: papyrus_compiler_parser::error::Error) -> CustomReport {
    let span = parser_error.span();
    let source_id = span.id;
    let found = optional_token_to_string(parser_error.found());

    let message: String = if parser_error.expected().is_empty() {
        format!(
            "Unexpected Token, found {} expected {}",
            found.fg(Color::Red),
            "Nothing".fg(Color::Cyan)
        )
    } else if parser_error.expected().len() == 1 {
        let expected =
            optional_token_to_string(parser_error.expected().iter().next().unwrap().as_ref());
        format!(
            "Unexpected Token, found {} expected {}",
            found.fg(Color::Red),
            expected.fg(Color::Cyan)
        )
    } else {
        let strings = parser_error
            .expected()
            .iter()
            .map(|token| {
                format!(
                    "{}",
                    optional_token_to_string(token.as_ref()).fg(Color::Cyan)
                )
            })
            .collect::<Vec<_>>();

        let capacity = strings
            .iter()
            .map(|x| x.len())
            .reduce(|prev, cur| prev + cur)
            .unwrap()
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
            found.fg(Color::Red),
            expected
        )
    };

    CustomReport::build(ReportKind::Error, source_id, span.range.start)
        .with_code(ErrorCode::ParserError)
        .with_message("Unexpected Token")
        .with_label(
            Label::new(SourceSpan {
                id: source_id,
                range: span.range,
            })
            .with_message(message),
        )
        .finish()
}
