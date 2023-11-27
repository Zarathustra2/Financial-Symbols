#![doc = include_str!("../README.md")]

use anyhow::{anyhow, bail, Error};
use chrono::NaiveDate;
use rust_decimal::Decimal;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::str::{from_utf8_unchecked, FromStr};

// The maximum length of a stock ticker.
// According to the NYSE specifications (https://www.nyse.com/publicdocs/nyse/data/NYSE_Symbology_Spec_v1.0c.pdf)
// it is reserving 16 chars for future growth.
//
// We are limitting it for now to 8 since the longest ticker symbol
// is 5. It is 6 if you use `.` for A/B stocks such as CWEN.A
const TICKER_LENGTH: usize = 8;

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

        bytes[..len].copy_from_slice(&ticker_bytes[..len]);

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
    /// Returns the string value of the enum
    ///
    /// # Examples
    /// ```
    /// use financial_symbols::OptionType;
    /// assert_eq!(OptionType::Call.as_str(),  "call");
    /// assert_eq!(OptionType::Put.as_str(),  "put");
    /// ```
    pub const fn as_str(&self) -> &str {
        match self {
            OptionType::Call => "call",
            OptionType::Put => "put",
        }
    }

    /// Returns a 1 char string value of the enum
    ///
    /// # Examples
    /// ```
    /// use financial_symbols::OptionType;
    /// assert_eq!(OptionType::Call.as_short_str(),  "c");
    /// assert_eq!(OptionType::Put.as_short_str(),  "p");
    /// ```
    pub const fn as_short_str(&self) -> &str {
        match self {
            OptionType::Call => "c",
            OptionType::Put => "p",
        }
    }

    /// Returns true if it is a call
    ///
    /// # Examples
    /// ```
    /// use financial_symbols::OptionType;
    /// assert!(OptionType::Call.is_call());
    /// assert!(!OptionType::Put.is_call());
    /// ```
    pub fn is_call(&self) -> bool {
        self == &OptionType::Call
    }

    /// Returns true if it is a put
    ///
    /// # Examples
    /// ```
    /// use financial_symbols::OptionType;
    /// assert!(!OptionType::Call.is_put());
    /// assert!(OptionType::Put.is_put());
    /// ```
    pub fn is_put(&self) -> bool {
        !self.is_call()
    }
}

impl TryFrom<char> for OptionType {
    type Error = Error;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'C' | 'c' => Ok(OptionType::Call),
            'P' | 'p' => Ok(OptionType::Put),
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

    /// Returns the contract in iso format.
    /// See [`from_iso_format`](#method.from_iso_format)
    pub fn as_str(&self) -> &str {
        unsafe { from_utf8_unchecked(self.bytes.get_unchecked(..self.len)) }
    }

    pub fn as_dx_feed_symbol(&self) -> String {
        format!(
            ".{}{}{}{}",
            self.ticker.as_str(),
            self.expiry.format("%y%m%d"),
            if self.is_call() { "C" } else { "P" },
            self.strike
        )
    }

    pub fn from_dx_format(s: &str) -> Result<Self, Error> {
        let mut numbers_seen = 0;
        let mut option_type_seen = false;

        let mut iso_bytes: [u8; 30] = [0u8; 30];
        let mut len = 0;
        let mut idx_offset = 0;

        let mut whole_part_idx = 0;
        let mut whole_part = [0u8; 5];
        let mut is_decimal_part = false;

        let mut decimal_part_idx = 0;
        let mut decimal_part = [0u8; 3];

        let mut decimal_places = 0;

        for (idx, c) in s.chars().enumerate() {
            if idx == 0 {
                idx_offset += 1;
                continue;
            }

            if numbers_seen >= 6 {
                if option_type_seen {
                    if c == '.' {
                        is_decimal_part = true;
                        continue;
                    }
                    if is_decimal_part {
                        decimal_part[decimal_part_idx] = c as u8;
                        decimal_part_idx += 1;
                    } else {
                        whole_part[whole_part_idx] = c as u8;
                        whole_part_idx += 1;
                    }

                    decimal_places += 1;
                } else {
                    if c == 'C' || c == 'P' {
                        option_type_seen = true;
                    }
                    iso_bytes[idx - idx_offset] = c as u8;

                    len += 1;
                }
            } else {
                if c.is_numeric() {
                    numbers_seen += 1;
                }
                iso_bytes[idx - idx_offset] = c as u8;
                len += 1;
            }
        }

        let post_fix_zeros = match decimal_part_idx {
            0 => 3,
            1 => 2,
            2 => 1,
            3 => 0,
            _ => panic!("Should not happen, report upstream"),
        };

        let pre_fix_zeros = 8 - post_fix_zeros - (decimal_places);

        for _ in 0..pre_fix_zeros {
            iso_bytes[len] = b'0';
            len += 1;
        }

        for char in whole_part.iter().take(whole_part_idx) {
            iso_bytes[len] = *char;
            len += 1;
        }

        for char in decimal_part.iter().take(decimal_part_idx) {
            iso_bytes[len] = *char;
            len += 1;
        }

        for _ in 0..post_fix_zeros {
            iso_bytes[len] = b'0';
            len += 1;
        }

        let s = unsafe { from_utf8_unchecked(iso_bytes.get_unchecked(..(len))) };

        Self::from_iso_format(s)
    }

    /// Parses an option contract in the normal iso format.
    ///
    /// The iso format is defined as:
    /// \<TICKER\>\<EXPIRY\>\<OPTION_TYPE\>\<STRIKE\>
    ///
    /// **TICKER** is the underlying symbol, it consists only of alphabetical chars.
    /// For instance the ticker `BRK.B` would be represented as `BRKB`
    ///
    /// **Expiry** is in the format of `YYMMDD`. An expiration of `2023-12-14` would be represented
    /// as `231214`
    ///
    /// **OPTION_TYPE** is a single char which is either `C` or `P`
    ///
    /// **STRIKE** is the strike of the contract multiplied by 1,000.
    /// It will always have a length of 8 chars. If it has less than 8 chars
    /// it will be prefixed with `0` until it has 8 chars.
    ///
    /// # Examples
    /// ```
    /// use financial_symbols::{OptionContract, OptionType};
    /// use rust_decimal::Decimal;
    /// use chrono::NaiveDate;
    /// use std::str::FromStr;
    /// let contract = OptionContract::from_iso_format("SPXW231127C03850000").unwrap();
    /// assert_eq!(contract.ticker.as_str(), "SPXW");
    /// assert_eq!(contract.ot_type, OptionType::Call);
    /// assert_eq!(contract.expiry, NaiveDate::from_str("2023-11-27").unwrap());
    /// assert_eq!(contract.strike, Decimal::from(3850));
    /// ```
    pub fn from_iso_format(s: &str) -> Result<Self, Error> {
        if s.len() > CONTRACT_LENGTH {
            bail!(
                "{} has more than {} chars; this is not a valid ISO option contract!",
                s,
                CONTRACT_LENGTH
            );
        }

        let mut strike: [u8; STRIKE_LENGTH] = [0u8; STRIKE_LENGTH];
        let mut ot_type: Option<OptionType> = None;
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
    use postgres_types::{private::BytesMut, to_sql_checked, FromSql, IsNull, ToSql, Type};

    type SqlErr = Box<dyn std::error::Error + Sync + Send>;

    impl<'a> FromSql<'a> for Ticker {
        fn from_sql(ty: &postgres_types::Type, raw: &'a [u8]) -> Result<Ticker, SqlErr> {
            let s = <&str as FromSql>::from_sql(ty, raw)?;
            Ticker::try_from(s).map_err(|err| err.into())
        }

        fn accepts(ty: &postgres_types::Type) -> bool {
            <&str as FromSql>::accepts(ty)
        }
    }

    impl ToSql for Ticker {
        fn to_sql(&self, ty: &Type, w: &mut BytesMut) -> Result<IsNull, SqlErr>
        where
            Self: Sized,
        {
            <&str as ToSql>::to_sql(&self.as_str(), ty, w)
        }

        fn accepts(ty: &postgres_types::Type) -> bool
        where
            Self: Sized,
        {
            <&str as FromSql>::accepts(ty)
        }

        to_sql_checked!();
    }

    impl<'a> FromSql<'a> for OptionType {
        fn from_sql(ty: &Type, raw: &'a [u8]) -> Result<OptionType, SqlErr> {
            let s = <&str as FromSql>::from_sql(ty, raw)?;
            OptionType::try_from(s).map_err(|err| err.into())
        }

        fn accepts(ty: &postgres_types::Type) -> bool {
            <&str as FromSql>::accepts(ty)
        }
    }

    impl ToSql for OptionType {
        fn to_sql(&self, ty: &Type, w: &mut BytesMut) -> Result<IsNull, SqlErr>
        where
            Self: Sized,
        {
            <&str as ToSql>::to_sql(&self.as_str(), ty, w)
        }

        fn accepts(ty: &postgres_types::Type) -> bool
        where
            Self: Sized,
        {
            <&str as FromSql>::accepts(ty)
        }

        to_sql_checked!();
    }

    impl<'a> FromSql<'a> for OptionContract {
        fn from_sql(ty: &Type, raw: &'a [u8]) -> Result<OptionContract, SqlErr> {
            let s = <&str as FromSql>::from_sql(ty, raw)?;
            OptionContract::from_iso_format(s).map_err(|err| err.into())
        }

        fn accepts(ty: &postgres_types::Type) -> bool {
            <&str as FromSql>::accepts(ty)
        }
    }

    impl ToSql for OptionContract {
        fn to_sql(&self, ty: &Type, w: &mut BytesMut) -> Result<IsNull, SqlErr>
        where
            Self: Sized,
        {
            <&str as ToSql>::to_sql(&self.as_str(), ty, w)
        }

        fn accepts(ty: &Type) -> bool
        where
            Self: Sized,
        {
            <&str as FromSql>::accepts(ty)
        }

        to_sql_checked!();
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
    pub fn can_dx() {
        let contract = OptionContract::from_dx_format(".PANW250117P256.67").unwrap();
        assert_eq!(contract.as_str(), "PANW250117P00256670");

        let contract = OptionContract::from_dx_format(".BOAT230120P24").unwrap();
        assert_eq!(contract.as_str(), "BOAT230120P00024000");

        let contract = OptionContract::from_dx_format(".UPV230421C38").unwrap();
        assert_eq!(contract.as_str(), "UPV230421C00038000");
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
