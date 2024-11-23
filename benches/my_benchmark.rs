use criterion::{black_box, criterion_group, criterion_main, Criterion};
use financial_symbols::{OptionContract, Ticker};

fn criterion_iso(c: &mut Criterion) {
    c.bench_function("from_osi SPXW231124P04060000", |b| {
        b.iter(|| OptionContract::from_osi(black_box("SPXW231124P04060000")))
    });
    c.bench_function("from_osi TSLA240119C00066670", |b| {
        b.iter(|| OptionContract::from_osi(black_box("TSLA240119C00066670")))
    });
    c.bench_function("from_osi A231215C00055000", |b| {
        b.iter(|| OptionContract::from_osi(black_box("A231215C00055000")))
    });
}

fn criterion_dx(c: &mut Criterion) {
    c.bench_function("from_dx_feed .SPXW231124P4060", |b| {
        b.iter(|| OptionContract::from_dx_feed(black_box(".SPXW231124P4060")))
    });
    c.bench_function("from_dx_feed .TSLA240119C66.67", |b| {
        b.iter(|| OptionContract::from_dx_feed(black_box(".TSLA240119C66.67")))
    });
    c.bench_function("from_dx_feed .A231215C55", |b| {
        b.iter(|| OptionContract::from_dx_feed(black_box(".A231215C55")))
    });
}

fn criterion_ticker(c: &mut Criterion) {
    c.bench_function("Ticker::try_from SPXW", |b| {
        b.iter(|| Ticker::try_from("SPXW"));
    });

    c.bench_function("Ticker::try_from CWEN.A", |b| {
        b.iter(|| Ticker::try_from(" CWEN.A"));
    });

    c.bench_function("Ticker::try_from GREE1", |b| {
        b.iter(|| Ticker::try_from("GREE1"));
    });

    c.bench_function("Ticker::try_from A", |b| {
        b.iter(|| Ticker::try_from("A"));
    });
}

criterion_group!(benches_ticker, criterion_ticker);
criterion_group!(benches_iso, criterion_iso);
criterion_group!(benches_dx, criterion_dx);

criterion_main!(benches_iso, benches_dx, benches_ticker);
