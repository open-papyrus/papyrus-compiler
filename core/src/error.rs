use crate::source::*;
use ariadne::{Color, Label, Report, ReportKind};
use std::fmt::{Display, Formatter};

pub type CustomReport = Report<SourceSpan>;

pub enum ErrorCode {
    LexerError = 1,
}

impl Display for ErrorCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorCode::LexerError => write!(f, "E{}", ErrorCode::LexerError as isize),
        }
    }
}

pub type LexerSpan = core::ops::Range<usize>;

pub fn lexer_error_to_report(id: SourceId, span: LexerSpan) -> CustomReport {
    CustomReport::build(ReportKind::Error, id.clone(), span.start)
        .with_code(ErrorCode::LexerError)
        .with_message("Unexpected Token")
        .with_label(
            Label::new(SourceSpan { id, range: span })
                .with_message("Unexpected Token")
                .with_color(Color::Red),
        )
        .finish()
}
