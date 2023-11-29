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
assert_eq!(ticker.as_str(), "AAPL");

let contract = OptionContract::from_iso_format("SPXW231127C03850000").unwrap();

let contract_copy = contract;
assert_eq!(contract, contract_copy);

assert_eq!(contract.ticker.as_str(), "SPXW");
assert_eq!(contract.ot_type, OptionType::Call);
assert_eq!(contract.expiry, NaiveDate::from_str("2023-11-27").unwrap());
assert_eq!(contract.strike, Decimal::from(3850));

let contract = OptionContract::from_dx_feed_symbol(".SPXW231127C3850").unwrap();
assert_eq!(contract.as_str(), "SPXW231127C03850000");
```


## Benchmark 
Tested on Apple M1 Pro 16GB Ram
```bash
from_iso_format SPXW231124P04060000
                        time:   [59.047 ns 59.178 ns 59.319 ns]
                        change: [-0.1926% +0.0473% +0.2973%] (p = 0.72 > 0.05)
                        No change in performance detected.
Found 3 outliers among 100 measurements (3.00%)
  2 (2.00%) high mild
  1 (1.00%) high severe

from_iso_format TSLA240119C00066670
                        time:   [74.147 ns 74.391 ns 74.656 ns]
                        change: [-0.8623% -0.4957% -0.1161%] (p = 0.01 < 0.05)
                        Change within noise threshold.
Found 7 outliers among 100 measurements (7.00%)
  6 (6.00%) high mild
  1 (1.00%) high severe

from_iso_format A231215C00055000
                        time:   [55.377 ns 55.555 ns 55.743 ns]
                        change: [+0.9201% +1.1964% +1.4756%] (p = 0.00 < 0.05)
                        Change within noise threshold.
```