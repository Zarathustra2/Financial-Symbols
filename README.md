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
                        time:   [51.028 ns 51.099 ns 51.187 ns]
                        change: [-1.7407% -1.4017% -1.0789%] (p = 0.00 < 0.05)
                        Performance has improved.
Found 9 outliers among 100 measurements (9.00%)
  9 (9.00%) high severe

from_iso_format TSLA240119C00066670
                        time:   [71.124 ns 71.347 ns 71.585 ns]
                        change: [-0.9598% -0.5467% -0.1513%] (p = 0.01 < 0.05)
                        Change within noise threshold.
Found 2 outliers among 100 measurements (2.00%)
  2 (2.00%) high mild

from_iso_format A231215C00055000
                        time:   [48.979 ns 49.028 ns 49.104 ns]
                        change: [-1.1605% -0.8206% -0.4416%] (p = 0.00 < 0.05)
                        Change within noise threshold.
Found 7 outliers among 100 measurements (7.00%)
  7 (7.00%) high severe
```
