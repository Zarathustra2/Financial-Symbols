use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rust_decimal::Decimal;

fn criterion_decimal(c: &mut Criterion) {
    c.bench_function("from_i128_with_scale + normalize", |b| {
        b.iter(|| Decimal::from_i128_with_scale(black_box(3058), 3).normalize());
    });

    c.bench_function("set_scale", |b| {
        b.iter(|| {
            let mut strike = Decimal::from(black_box(3058));
            strike.set_scale(3).unwrap();
            strike.normalize_assign();
        });
    });

    c.bench_function("DIV", |b| {
        b.iter(|| Decimal::from(black_box(3058)) / black_box(Decimal::ONE_THOUSAND));
    });
}

criterion_group!(benches_dec, criterion_decimal);

criterion_main!(benches_dec);
