use anyhow::{anyhow, bail, Error};
use chrono::NaiveDate;
use rust_decimal::Decimal;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::str::{from_utf8_unchecked, FromStr};

const TICKER_LENGTH: usize = 7;

#[derive(Clone, Copy)]
pub struct Ticker {
    bytes: [u8; TICKER_LENGTH],
    len: usize,
}

impl Ticker {
    pub const fn from_raw(bytes: [u8; TICKER_LENGTH], len: usize) -> Self {
        Self { bytes, len }
    }

    pub fn as_str(&self) -> &str {
        unsafe { from_utf8_unchecked(self.bytes.get_unchecked(..self.len)) }
    }
}

impl Display for Ticker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl Debug for Ticker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ticker({})", self.as_str())
    }
}

impl Hash for Ticker {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.bytes.hash(state);
    }
}

impl PartialEq for Ticker {
    fn eq(&self, other: &Self) -> bool {
        self.bytes == other.bytes
    }
}

impl PartialOrd for Ticker {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for Ticker {}

impl Ord for Ticker {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.bytes.cmp(&other.bytes)
    }
}

impl TryFrom<&str> for Ticker {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.len() > TICKER_LENGTH {
            bail!(
                "{} has more than {} chars; this is not a valid ticker!",
                value,
                TICKER_LENGTH
            );
        }

        let ticker_bytes = value.as_bytes();
        let mut bytes = [0u8; TICKER_LENGTH];
        let len = ticker_bytes.len();

        for (idx, x) in ticker_bytes.iter().enumerate() {
            bytes[idx] = *x;
        }

        Ok(Self::from_raw(bytes, len))
    }
}

impl TryFrom<String> for Ticker {
    type Error = Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.as_str().try_into()
    }
}

impl TryFrom<&String> for Ticker {
    type Error = Error;

    fn try_from(value: &String) -> Result<Self, Self::Error> {
        value.try_into()
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum OptionType {
    Call,
    Put,
}

impl OptionType {
    pub fn as_str(&self) -> &str {
        match self {
            OptionType::Call => "call",
            OptionType::Put => "put",
        }
    }

    pub fn is_call(&self) -> bool {
        self == &OptionType::Call
    }

    pub fn is_put(&self) -> bool {
        !self.is_call()
    }
}

impl TryFrom<char> for OptionType {
    type Error = Error;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'C' => Ok(OptionType::Call),
            'P' => Ok(OptionType::Put),
            _ => bail!("{value} is not a valid option type"),
        }
    }
}

impl TryFrom<&str> for OptionType {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "C" | "c" | "call" => Ok(Self::Call),
            "P" | "p" | "put" => Ok(Self::Put),
            _ => bail!("{} is not a valid option type", value),
        }
    }
}

impl TryFrom<String> for OptionType {
    type Error = Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.as_str().try_into()
    }
}

impl TryFrom<&String> for OptionType {
    type Error = Error;

    fn try_from(value: &String) -> Result<Self, Self::Error> {
        value.try_into()
    }
}

const EXPIRY_LENGTH: usize = 6;
const OPTION_TYPE_LENGTH: usize = 1;
const STRIKE_LENGTH: usize = 8;
const CONTRACT_LENGTH: usize = TICKER_LENGTH + EXPIRY_LENGTH + OPTION_TYPE_LENGTH + STRIKE_LENGTH;

#[derive(Clone, Copy)]
pub struct OptionContract {
    pub ticker: Ticker,
    pub ot_type: OptionType,
    pub expiry: NaiveDate,
    pub strike: Decimal,
    bytes: [u8; CONTRACT_LENGTH],
    len: usize,
}

impl Display for OptionContract {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl Debug for OptionContract {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OptionContract")
            .field("ticker", &self.ticker)
            .field("ot_type", &self.ot_type)
            .field("expiry", &self.expiry)
            .field("strike", &self.strike)
            .finish()
    }
}

impl Hash for OptionContract {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.bytes.hash(state);
    }
}

impl PartialEq for OptionContract {
    fn eq(&self, other: &Self) -> bool {
        self.bytes == other.bytes
    }
}

impl PartialOrd for OptionContract {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for OptionContract {}

impl Ord for OptionContract {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.bytes.cmp(&other.bytes)
    }
}

impl OptionContract {
    pub fn is_call(&self) -> bool {
        self.ot_type.is_call()
    }

    pub fn is_put(&self) -> bool {
        self.ot_type.is_put()
    }

    pub fn as_str(&self) -> &str {
        unsafe { from_utf8_unchecked(self.bytes.get_unchecked(..self.len)) }
    }

    // pub fn from_dx_format(&self, s: &str) -> Self {

    // }

    pub fn from_iso_format(s: &str) -> Result<Self, Error> {
        if s.len() > CONTRACT_LENGTH {
            bail!(
                "{} has more than {} chars; this is not a valid ISO option contract!",
                s,
                CONTRACT_LENGTH
            );
        }

        let mut strike: [u8; STRIKE_LENGTH] = [0u8; STRIKE_LENGTH];
        let mut ot_type = None;
        let mut ticker_bytes = [0u8; TICKER_LENGTH];

        let len = s.len();
        let strike_offset = len - STRIKE_LENGTH;
        let option_type_offset = strike_offset - OPTION_TYPE_LENGTH;
        let expiry_offset = option_type_offset - EXPIRY_LENGTH;

        let mut year = 0;
        let mut month = 0;
        let mut day = 0;

        for (idx, char) in s.chars().rev().enumerate() {
            let idx = len - 1 - idx;
            if idx >= strike_offset {
                strike[idx - strike_offset] = if char.is_numeric() {
                    char as u8
                } else {
                    // TODO add index, better error msg
                    bail!("{char} is not numeric")
                }
            } else if idx == option_type_offset {
                ot_type = Some(char.try_into()?);
            } else if idx >= expiry_offset {
                const RADIX: u32 = 10;
                let i = char
                    .to_digit(RADIX)
                    .ok_or_else(|| anyhow!("Failed to convert {char} to digit"))?
                    as i32;

                match idx - expiry_offset {
                    5 => day += i,
                    4 => day += 10 * i,
                    3 => month += i,
                    2 => month += 10 * i,
                    1 => year += i,
                    0 => year += 10 * i,
                    _ => bail!("Should not happen, report upstream Char {char}"),
                }
            } else {
                // TODO: Check rest is not over TICKER_LENGTH
                ticker_bytes[idx] = char as u8;
            }
        }

        let strike = Decimal::from_str(unsafe {
            from_utf8_unchecked(strike.get_unchecked(..STRIKE_LENGTH))
        })? / Decimal::ONE_THOUSAND;

        let ot_type = ot_type
            .ok_or_else(|| anyhow!("OptionType has not been found in the given contract"))?;

        let expiry = NaiveDate::from_ymd_opt(2000 + year, month.try_into()?, day.try_into()?)
            .ok_or_else(|| {
                anyhow!(
                    "Failed to construct expiry from year {}, month {} & day {}",
                    year,
                    month,
                    day
                )
            })?;

        let ticker = Ticker::from_raw(
            ticker_bytes,
            len - STRIKE_LENGTH - EXPIRY_LENGTH - OPTION_TYPE_LENGTH,
        );

        let contract_bytes = s.as_bytes();
        let mut bytes = [0u8; CONTRACT_LENGTH];
        let len = contract_bytes.len();

        for (idx, x) in contract_bytes.iter().enumerate() {
            bytes[idx] = *x;
        }

        Ok(Self {
            ticker,
            ot_type,
            expiry,
            strike,
            bytes,
            len,
        })
    }
}

#[cfg(feature = "serde")]
mod serde_feature {
    use super::*;
    use serde::{
        de::{self, Visitor},
        Deserialize, Deserializer, Serialize, Serializer,
    };

    struct TickerVisitor;

    impl<'de> Visitor<'de> for TickerVisitor {
        type Value = Ticker;

        #[inline]
        fn expecting(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("A ticker should be a string")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            v.try_into().map_err(E::custom)
        }
    }

    impl Serialize for Ticker {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.serialize_str(self.as_str())
        }
    }

    impl<'de> Deserialize<'de> for Ticker {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            deserializer.deserialize_str(TickerVisitor)
        }
    }

    struct OptionTypeVisitor;

    impl<'de> Visitor<'de> for OptionTypeVisitor {
        type Value = OptionType;

        #[inline]
        fn expecting(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("An option type should be a string")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            v.try_into().map_err(E::custom)
        }
    }

    impl Serialize for OptionType {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.serialize_str(self.as_str())
        }
    }

    impl<'de> Deserialize<'de> for OptionType {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            deserializer.deserialize_str(OptionTypeVisitor)
        }
    }

    struct OptionContractVisitor;

    impl<'de> Visitor<'de> for OptionContractVisitor {
        type Value = OptionContract;

        #[inline]
        fn expecting(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("An option contract should be a string")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            OptionContract::from_iso_format(v).map_err(E::custom)
        }
    }

    impl Serialize for OptionContract {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.serialize_str(self.as_str())
        }
    }

    impl<'de> Deserialize<'de> for OptionContract {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            deserializer.deserialize_str(OptionContractVisitor)
        }
    }
}

#[cfg(feature = "postgres")]
mod postgres_feature {
    use super::*;
    use postgres_types::FromSql;

    impl<'a> FromSql<'a> for Ticker {
        fn from_sql(
            ty: &postgres_types::Type,
            raw: &'a [u8],
        ) -> Result<Ticker, Box<dyn std::error::Error + Sync + Send>> {
            let s = <&str as FromSql>::from_sql(ty, raw)?;
            Ticker::try_from(s).map_err(|err| err.into())
        }

        fn accepts(ty: &postgres_types::Type) -> bool {
            <&str as FromSql>::accepts(ty)
        }
    }

    impl<'a> FromSql<'a> for OptionType {
        fn from_sql(
            ty: &postgres_types::Type,
            raw: &'a [u8],
        ) -> Result<OptionType, Box<dyn std::error::Error + Sync + Send>> {
            let s = <&str as FromSql>::from_sql(ty, raw)?;
            OptionType::try_from(s).map_err(|err| err.into())
        }

        fn accepts(ty: &postgres_types::Type) -> bool {
            <&str as FromSql>::accepts(ty)
        }
    }

    impl<'a> FromSql<'a> for OptionContract {
        fn from_sql(
            ty: &postgres_types::Type,
            raw: &'a [u8],
        ) -> Result<OptionContract, Box<dyn std::error::Error + Sync + Send>> {
            let s = <&str as FromSql>::from_sql(ty, raw)?;
            OptionContract::from_iso_format(s).map_err(|err| err.into())
        }

        fn accepts(ty: &postgres_types::Type) -> bool {
            <&str as FromSql>::accepts(ty)
        }
    }
}

#[cfg(test)]
mod tests {

    use std::{
        fs::File,
        io::{BufRead, BufReader},
    };

    use anyhow::Context;

    use super::*;

    #[test]
    pub fn can_parse_tickers() {
        let file = File::open("./test_data/tickers.txt").unwrap();
        let reader = BufReader::new(file);

        for (idx, line) in reader.lines().skip(1).enumerate() {
            let s = line.unwrap();
            let context = format!("Line {idx} ticker {s}");
            let ticker = Ticker::try_from(s.as_str()).context(context).unwrap();
            assert_eq!(ticker.as_str(), s.as_str());
        }
    }

    #[test]
    pub fn can_parse_option_contracts() {
        let file = File::open("./test_data/contracts.csv").unwrap();
        let reader = BufReader::new(file);

        for (idx, line) in reader.lines().skip(1).enumerate() {
            // "option_symbol","underlying_symbol","strike","option_type","expires"
            let line = line.unwrap();
            let splits = line.split(',').collect::<Vec<_>>();

            let option_symbol = splits[0];
            let context = format!("Line {idx} contract {option_symbol}");
            let contract = OptionContract::from_iso_format(option_symbol)
                .context(context)
                .unwrap();
            assert_eq!(contract.as_str(), option_symbol);
            assert_eq!(contract.ticker.as_str(), splits[1]);
            assert_eq!(contract.strike, Decimal::from_str(splits[2]).unwrap());
            assert_eq!(contract.ot_type, OptionType::try_from(splits[3]).unwrap());

            match splits[3] {
                "call" => assert!(contract.is_call()),
                "put" => assert!(contract.is_put()),
                other => panic!("{other} is not a valid option type, bad test data?"),
            }

            assert_eq!(contract.expiry, NaiveDate::from_str(splits[4]).unwrap());
        }
    }
}
