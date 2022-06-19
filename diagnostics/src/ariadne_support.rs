use crate::{Diagnostic, SeverityLevel, SourceId, SourceRange};
use ariadne::{Label, Report, ReportKind};
use yansi::Color;

const DOCUMENTATION_BASE_URL: &str =
    "https://erri120.github.io/papyrus-compiler/Compiler_Reference/Diagnostics/";

fn create_documentation_url<'a>(diagnostic: &'a Box<dyn Diagnostic + 'a>) -> String {
    format!(
        "{}{}#{}",
        DOCUMENTATION_BASE_URL,
        diagnostic.documentation_section(),
        diagnostic.documentation_heading()
    )
}

pub fn convert_to_report<'a>(
    diagnostic: &'a Box<dyn Diagnostic + 'a>,
) -> Report<(SourceId, SourceRange)> {
    let kind = match diagnostic.level() {
        SeverityLevel::Error => ReportKind::Error,
        SeverityLevel::Warning => ReportKind::Warning,
        SeverityLevel::Suggestion => ReportKind::Advice,
    };

    let label_color = match diagnostic.level() {
        SeverityLevel::Error => Color::Red,
        SeverityLevel::Warning => Color::Yellow,
        SeverityLevel::Suggestion => Color::Cyan,
    };

    Report::build(kind, diagnostic.source_id(), diagnostic.range().start)
        .with_code(format!("{}{:03}", diagnostic.prefix(), diagnostic.id()))
        .with_note(format!(
            "For more information, visit {}",
            create_documentation_url(&diagnostic)
        ))
        .with_label(
            Label::new((diagnostic.source_id(), diagnostic.range()))
                .with_color(label_color)
                .with_message(diagnostic.message()),
        )
        .finish()
}
