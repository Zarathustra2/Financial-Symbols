# Financial Symbols

Stock symbols & option contract symbols which implement the copy trait.

[![Build status](https://github.com/Zarathustra2/Financial-Symbols/actions/workflows/ci.yaml/badge.svg?branch=main)](https://github.com/Zarathustra2/Financial-Symbols/actions/workflows/ci.yaml)
[![Crates.io](https://img.shields.io/crates/v/financial_symbols)](https://crates.io/crates/financial_symbols)
[![Documentation](https://docs.rs/financial_symbols/badge.svg)](https://docs.rs/financial_symbols)


```rust
use financial_symbols::{OptionContract, OptionType, Ticker};
use rust_decimal::Decimal;
use chrono::NaiveDate;
use std::str::FromStr;

let ticker = Ticker::try_from("AAPL").unwrap();

let ticker_copy = ticker;

assert_eq!(ticker, ticker_copy);
assert_eq!(ticker, "AAPL");

let contract = OptionContract::from_iso_format("SPXW231127C03850000").unwrap();

let contract_copy = contract;
assert_eq!(contract, contract_copy);

assert_eq!(contract.ticker(), "SPXW");
assert_eq!(contract.option_type(), OptionType::Call);
assert_eq!(contract.expiry(), NaiveDate::from_str("2023-11-27").unwrap());
assert_eq!(contract.strike(), Decimal::from(3850));

let contract = OptionContract::from_dx_feed_symbol(".SPXW231127C3850").unwrap();
assert_eq!(contract.to_iso(), "SPXW231127C03850000");
```


## Benchmark 
Tested on Apple M1 Pro 16GB Ram
```bash
from_iso_format SPXW231124P04060000
                        time:   [10.557 ns 10.582 ns 10.629 ns]

from_iso_format TSLA240119C00066670
                        time:   [10.559 ns 10.595 ns 10.667 ns]

from_iso_format A231215C00055000
                        time:   [9.7667 ns 9.7743 ns 9.7830 ns]
```
