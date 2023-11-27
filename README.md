# Financial Symbols

Stock symbols & option contract symbols which implement the copy trait.


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
```