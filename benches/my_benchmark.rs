use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use financial_symbols::OptionContract;

fn criterion_iso(c: &mut Criterion) {
    c.bench_function("from_iso_format SPXW231124P04060000", |b| {
        b.iter(|| OptionContract::from_iso_format(black_box("SPXW231124P04060000")))
    });
    c.bench_function("from_iso_format TSLA240119C00066670", |b| {
        b.iter(|| OptionContract::from_iso_format(black_box("TSLA240119C00066670")))
    });
    c.bench_function("from_iso_format A231215C00055000", |b| {
        b.iter(|| OptionContract::from_iso_format(black_box("A231215C00055000")))
    });
}

fn criterion_dx(c: &mut Criterion) {
    c.bench_function("from_dx_feed_symbol .SPXW231124P4060", |b| {
        b.iter(|| OptionContract::from_dx_feed_symbol(black_box(".SPXW231124P4060")))
    });
    c.bench_function("from_dx_feed_symbol .TSLA240119C66.67", |b| {
        b.iter(|| OptionContract::from_dx_feed_symbol(black_box(".TSLA240119C66.67")))
    });
    c.bench_function("from_dx_feed_symbol .A231215C55", |b| {
        b.iter(|| OptionContract::from_dx_feed_symbol(black_box(".A231215C55")))
    });
}

criterion_group!(benches_iso, criterion_iso);
criterion_group!(benches_dx, criterion_dx);
criterion_main!(benches_iso, benches_dx);
