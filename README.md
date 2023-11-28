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