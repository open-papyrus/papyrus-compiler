use criterion::{criterion_group, criterion_main, Criterion};

fn criterion_benchmark(c: &mut Criterion) {
    let src = include_str!("../../extern/MrOctopus/nl_mcm/main/source/nl_mcm.psc");

    c.bench_function("run lexer", |b| {
        b.iter(|| papyrus_compiler_lexer::run_lexer(src));
    });

    c.bench_function("run lexer with result", |b| {
        b.iter(|| papyrus_compiler_lexer::run_lexer_with_result(0, src));
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
