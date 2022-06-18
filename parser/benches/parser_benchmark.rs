use criterion::{criterion_group, criterion_main, Criterion};
use papyrus_compiler_parser::ast::script::Script;
use papyrus_compiler_parser::parser_diagnostics::ParserDiagnostics;

fn run_parser(src: &str) -> Result<Script, Vec<ParserDiagnostics>> {
    let lexer_res = papyrus_compiler_lexer::run_lexer(src);
    papyrus_compiler_parser::parse_script(0, lexer_res)
}

fn criterion_benchmark(c: &mut Criterion) {
    let src = include_str!("../../extern/MrOctopus/nl_mcm/main/source/nl_mcm.psc");

    c.bench_function("run parser", |b| {
        b.iter(|| run_parser(src));
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
