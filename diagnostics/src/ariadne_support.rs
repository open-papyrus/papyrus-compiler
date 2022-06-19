use crate::{Diagnostic, SeverityLevel, SourceId, SourceRange};
use ariadne::{Label, Report, ReportKind};

pub fn convert_to_report<'a>(
    diagnostic: &'a Box<dyn Diagnostic + 'a>,
) -> Report<(SourceId, SourceRange)> {
    let kind = match diagnostic.level() {
        SeverityLevel::Error => ReportKind::Error,
        SeverityLevel::Warning => ReportKind::Warning,
        SeverityLevel::Suggestion => ReportKind::Advice,
    };

    Report::build(kind, diagnostic.source_id(), diagnostic.range().start)
        .with_code(format!("{}{:03}", diagnostic.prefix(), diagnostic.id()))
        .with_label(
            Label::new((diagnostic.source_id(), diagnostic.range()))
                .with_message(diagnostic.message()),
        )
        .finish()
}
