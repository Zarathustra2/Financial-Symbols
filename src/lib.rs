#![doc = include_str!("../README.md")]

use chrono::NaiveDate;
use rust_decimal::Decimal;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::str::{from_utf8_unchecked, FromStr};

// The maximum length of a stock ticker.
// According to the NYSE specifications (https://www.nyse.com/publicdocs/nyse/data/NYSE_Symbology_Spec_v1.0c.pdf)
// it is reserving 16 chars for future growth.
//
// We are limitting it for now to 7 since the longest ticker symbol
// is 5. It is 6 if you use `.` for A/B stocks such as CWEN.A
const TICKER_LENGTH: usize = 8;

#[derive(Debug)]
pub enum TickerParseErr {
    TooLong(usize),
    Empty,
}

impl Display for TickerParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TickerParseErr::TooLong(len) => write!(
                f,
                "Ticker has {} chars. The maximum amount of chars is {}",
                len,
                TICKER_LENGTH - 1
            ),
            TickerParseErr::Empty => write!(f, "Ticker is empty"),
        }
    }
}

impl std::error::Error for TickerParseErr {}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Ticker {
    bytes: [u8; TICKER_LENGTH],
}

impl Ticker {
    pub const fn from_str_unchecked(s: &str) -> Self {
        let ticker_bytes = s.as_bytes();
        let mut bytes = [0u8; TICKER_LENGTH];
        let len = s.len();
        bytes[0] = len as u8;

        // TODO: Change to
        // std::ptr::copy_nonoverlapping(ticker_bytes.as_ptr(), bytes.as_mut_ptr(), len);
        // once https://github.com/rust-lang/rust/issues/57349 is in.
        let mut i = 0;
        loop {
            if i >= len {
                break;
            }
            bytes[i + 1] = ticker_bytes[i];
            i += 1;
        }

        Self::from_raw(bytes)
    }

    pub const fn from_raw(bytes: [u8; TICKER_LENGTH]) -> Self {
        Self { bytes }
    }

    pub fn as_str(&self) -> &str {
        unsafe { from_utf8_unchecked(self.bytes.get_unchecked(1..1 + self.bytes[0] as usize)) }
    }
}

impl Display for Ticker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl TryFrom<&str> for Ticker {
    type Error = TickerParseErr;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let len = value.len();
        if len > (TICKER_LENGTH - 1) {
            return Err(TickerParseErr::TooLong(len));
        } else if len == 0 {
            return Err(TickerParseErr::Empty);
        }

        let ticker_bytes = value.as_bytes();
        let mut bytes = [0u8; TICKER_LENGTH];
        bytes[0] = len as u8;

        bytes[1..len + 1].copy_from_slice(&ticker_bytes[..len]);

        Ok(Self::from_raw(bytes))
    }
}

impl TryFrom<String> for Ticker {
    type Error = TickerParseErr;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.as_str().try_into()
    }
}

#[derive(Debug)]
pub struct OptionTypeErr;

impl Display for OptionTypeErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Not a valid option type, valid types are 'C', 'c', 'P', 'p', 'call' & 'put'"
        )
    }
}

impl std::error::Error for OptionTypeErr {}

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
    type Error = OptionTypeErr;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'C' | 'c' => Ok(OptionType::Call),
            'P' | 'p' => Ok(OptionType::Put),
            _ => Err(OptionTypeErr),
        }
    }
}

impl TryFrom<&str> for OptionType {
    type Error = OptionTypeErr;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "C" | "c" | "call" => Ok(Self::Call),
            "P" | "p" | "put" => Ok(Self::Put),
            _ => Err(OptionTypeErr),
        }
    }
}

impl TryFrom<String> for OptionType {
    type Error = OptionTypeErr;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.as_str().try_into()
    }
}

impl FromStr for OptionType {
    type Err = OptionTypeErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.try_into()
    }
}

#[derive(Debug)]
pub enum OptionContractErr {
    TooLong(usize),
    TooShort(usize),
    InvalidOptionType(char),
    InvalidExpiry { year: i32, month: u32, day: u32 },
    ExpectedNum { idx: usize, byte: u8 },
    InvalidStrike,
    InvalidTicker,
}

impl Display for OptionContractErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OptionContractErr::TooLong(len) => write!(
                f,
                "Option contract has {} chars. The maximum amount of chars is {}",
                len, CONTRACT_LENGTH
            ),
            OptionContractErr::TooShort(len) => write!(
                f,
                "Option contract has {} chars. The minimum amount of chars is {}",
                len, MIN_CONTRACT_LENGTH
            ),
            OptionContractErr::InvalidOptionType(c) => write!(f, "Invalid option type {c}"),
            OptionContractErr::InvalidExpiry { year, month, day } => {
                write!(f, "{year}-{month}-{day} is not a valid date")
            }
            OptionContractErr::ExpectedNum { idx, byte: b } => {
                write!(f, "Expected a number at idx {idx}, got byte {b}")
            }
            OptionContractErr::InvalidStrike => write!(f, "Invalid strike"),
            OptionContractErr::InvalidTicker => write!(f, "Invalid ticker"),
        }
    }
}

impl std::error::Error for OptionContractErr {}

const EXPIRY_LENGTH: usize = 6;
const OPTION_TYPE_LENGTH: usize = 1;
const STRIKE_LENGTH: usize = 8;
const CONTRACT_LENGTH: usize = TICKER_LENGTH + EXPIRY_LENGTH + OPTION_TYPE_LENGTH + STRIKE_LENGTH;
const MIN_CONTRACT_LENGTH: usize = 1 + EXPIRY_LENGTH + OPTION_TYPE_LENGTH + STRIKE_LENGTH;

// TODO: Or just save ticker, strike as u32, expiry & option type and create string on the fly
#[derive(Clone, Copy, Debug)]
pub struct OptionContract {
    bytes: [u8; CONTRACT_LENGTH],
    len: u8,
}

impl OptionContract {
    fn ticker_len(&self) -> usize {
        self.len as usize - EXPIRY_LENGTH - OPTION_TYPE_LENGTH - STRIKE_LENGTH
    }

    pub fn strike(&self) -> Decimal {
        let mut num = 0u32;

        let bytes = &self.bytes[self.len as usize - 8..self.len as usize];
        for (idx, byte) in bytes.iter().enumerate() {
            num += ((*byte - b'0') as u32) * 10u32.pow((bytes.len() - 1 - idx) as u32);
        }

        Decimal::from_i128_with_scale(num as i128, 3).normalize()
    }

    pub fn expiry(&self) -> NaiveDate {
        let bytes = &self.bytes[self.ticker_len()..self.ticker_len() + EXPIRY_LENGTH];

        let year = (bytes[0] - b'0') as i32 * 10 + (bytes[1] - b'0') as i32;
        let month = (bytes[2] - b'0') as u32 * 10 + (bytes[3] - b'0') as u32;
        let day = (bytes[4] - b'0') as u32 * 10 + (bytes[5] - b'0') as u32;

        NaiveDate::from_ymd_opt(2000 + year, month, day)
            .expect("Only valid dates should be created")
    }

    pub fn ticker(&self) -> Ticker {
        let mut ticker_bytes = [0u8; TICKER_LENGTH];
        ticker_bytes[0] = self.ticker_len() as u8;
        ticker_bytes[1..self.ticker_len() + 1].copy_from_slice(&self.bytes[..self.ticker_len()]);

        Ticker::from_raw(ticker_bytes)
    }

    pub fn option_type(&self) -> OptionType {
        let option_type_byte = self.bytes[self.ticker_len() + EXPIRY_LENGTH];
        if option_type_byte == 80 {
            OptionType::Put
        } else {
            OptionType::Call
        }
    }

    pub fn is_call(&self) -> bool {
        self.option_type().is_call()
    }

    pub fn is_put(&self) -> bool {
        self.option_type().is_put()
    }

    /// Returns the contract in iso format.
    /// See [`from_iso_format`](#method.from_iso_format)
    pub fn as_str(&self) -> &str {
        unsafe { from_utf8_unchecked(self.bytes.get_unchecked(..self.len as usize)) }
    }

    /// Returns the option symbol in the dx feed format.
    /// See [`from_dx_feed_symbol`](#method.from_dx_feed_symbol) for more information.
    /// # Examples
    /// ```
    /// use financial_symbols::{OptionContract, OptionType};
    /// use std::str::FromStr;
    /// let contract = OptionContract::from_iso_format("SPXW231127C03850000").unwrap();
    /// assert_eq!(contract.as_dx_feed_symbol(), ".SPXW231127C3850");
    /// ```
    pub fn as_dx_feed_symbol(&self) -> String {
        format!(
            ".{}{}{}{}",
            self.ticker().as_str(),
            self.expiry().format("%y%m%d"),
            if self.is_call() { "C" } else { "P" },
            self.strike()
        )
    }

    /// Parses an option contract based on the dx feed format.
    ///
    /// The format is based on the option symbols being returned
    /// by the financial data provider DxFeed. The symbols can
    /// be found here: https://tools.dxfeed.com/ipf?TYPE=OPTION
    ///
    /// # Examples
    /// ```
    /// use financial_symbols::{OptionContract, OptionType};
    /// use rust_decimal::Decimal;
    /// use chrono::NaiveDate;
    /// use std::str::FromStr;
    /// let contract = OptionContract::from_dx_feed_symbol(".SPXW231127C3850").unwrap();
    /// assert_eq!(contract.ticker().as_str(), "SPXW");
    /// assert_eq!(contract.option_type(), OptionType::Call);
    /// assert_eq!(contract.expiry(), NaiveDate::from_str("2023-11-27").unwrap());
    /// assert_eq!(contract.strike(), Decimal::from(3850));
    /// assert_eq!(contract.as_str(), "SPXW231127C03850000");
    /// ```
    pub fn from_dx_feed_symbol(s: &str) -> Result<Self, OptionContractErr> {
        if s.len() >= CONTRACT_LENGTH {
            return Err(OptionContractErr::TooLong(s.len()));
        } else if s.is_empty() {
            return Err(OptionContractErr::TooShort(s.len()));
        }

        if !s.starts_with('.') {
            return Err(OptionContractErr::InvalidTicker);
        }

        let mut strike_start: Option<usize> = None;

        for (idx, c) in s.chars().rev().enumerate() {
            if c == 'C' || c == 'P' {
                strike_start = Some(s.len() - idx);
                break;
            }
        }

        let strike_start = strike_start.ok_or(OptionContractErr::InvalidStrike)?;

        if strike_start == s.len() {
            return Err(OptionContractErr::InvalidStrike);
        }

        let (start, strike) = unsafe {
            (
                s.get_unchecked(1..strike_start),
                s.get_unchecked(strike_start..s.len()),
            )
        };

        let strike = if let Some(idx) = strike.find('.') {
            let (up, down) = unsafe {
                (
                    strike.get_unchecked(0..idx),
                    strike.get_unchecked(idx + 1..strike.len()),
                )
            };

            let mut s = String::with_capacity(8);

            let back_zeros = 3 - down.len();
            let front_zeros = 8 - up.len() - back_zeros - down.len();

            for _ in 0..front_zeros {
                s.push('0');
            }

            s.push_str(up);
            s.push_str(down);

            for _ in 0..back_zeros {
                s.push('0');
            }

            s
        } else {
            let back_zeros = 3;
            if (strike.len() + back_zeros) > 8 {
                return Err(OptionContractErr::InvalidStrike);
            }
            let front_zeros = 8 - strike.len() - back_zeros;

            let mut s = String::with_capacity(8);
            for _ in 0..front_zeros {
                s.push('0');
            }

            s.push_str(strike);

            for _ in 0..back_zeros {
                s.push('0');
            }

            s
        };

        let mut iso = start.to_string();
        iso.push_str(strike.as_str());

        Self::from_iso_format(iso.as_str())
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
    /// assert_eq!(contract.ticker().as_str(), "SPXW");
    /// assert_eq!(contract.option_type(), OptionType::Call);
    /// assert_eq!(contract.expiry(), NaiveDate::from_str("2023-11-27").unwrap());
    /// assert_eq!(contract.strike(), Decimal::from(3850));
    /// ```
    pub fn from_iso_format(s: &str) -> Result<Self, OptionContractErr> {
        let len = s.len();

        if len > CONTRACT_LENGTH {
            return Err(OptionContractErr::TooLong(len));
        } else if len < MIN_CONTRACT_LENGTH {
            return Err(OptionContractErr::TooShort(len));
        }

        let mut ticker_bytes = [0u8; TICKER_LENGTH];

        let strike_offset = len - STRIKE_LENGTH;
        let option_type_offset = strike_offset - OPTION_TYPE_LENGTH;
        let expiry_offset = option_type_offset - EXPIRY_LENGTH;

        let mut year: i32 = 0;
        let mut month: u32 = 0;
        let mut day: u32 = 0;

        let contract_bytes = s.as_bytes();
        let mut bytes = [0u8; CONTRACT_LENGTH];

        let mut idx = 0;
        // TODO: Don't need to loop and copy over bytes, just check for each range that valid ticker, expiry, option_type & strike
        loop {
            let byte = contract_bytes[idx];
            bytes[idx] = byte;
            if idx >= strike_offset {
                if !byte.is_ascii_digit() {
                    return Err(OptionContractErr::ExpectedNum { idx, byte });
                }
            } else if idx == option_type_offset {
                if byte != b'C' && byte != b'P' {
                    return Err(OptionContractErr::InvalidOptionType(byte as char));
                }
            } else if idx >= expiry_offset {
                if !byte.is_ascii_digit() {
                    return Err(OptionContractErr::ExpectedNum { idx, byte });
                }

                let i = (byte - b'0') as u32;

                match idx - expiry_offset {
                    5 => day += i,
                    4 => day += 10 * i,
                    3 => month += i,
                    2 => month += 10 * i,
                    1 => year += i as i32,
                    0 => year += 10 * i as i32,
                    _ => unreachable!(),
                }
            } else {
                ticker_bytes[idx + 1] = byte;
            }

            idx += 1;
            if idx == len {
                break;
            }
        }

        if NaiveDate::from_ymd_opt(2000 + year, month, day).is_none() {
            return Err(OptionContractErr::InvalidExpiry { year, month, day });
        };

        Ok(Self {
            bytes,
            len: len as u8,
        })
    }
}

impl Display for OptionContract {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
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

impl TryFrom<&str> for OptionContract {
    type Error = OptionContractErr;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        OptionContract::from_iso_format(value)
    }
}

impl TryFrom<String> for OptionContract {
    type Error = OptionContractErr;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.as_str().try_into()
    }
}

impl FromStr for OptionContract {
    type Err = OptionContractErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.try_into()
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
        collections::HashSet,
        fs::File,
        io::{BufRead, BufReader},
        time::Instant,
    };

    use std::str::FromStr;

    use anyhow::Context;

    use super::*;

    #[test]
    pub fn can_parse_tickers() {
        let file = File::open("./test_data/tickers.txt").unwrap();
        let reader = BufReader::new(file);

        let mut set_str = HashSet::new();
        let mut set_ticker = HashSet::new();

        for (idx, line) in reader.lines().skip(1).enumerate() {
            let s = line.unwrap();
            let context = format!("Line {idx} ticker {s}");
            let ticker = Ticker::try_from(s.as_str()).context(context).unwrap();
            assert_eq!(ticker.as_str(), s.as_str());

            let ticker_unchecked = Ticker::from_str_unchecked(s.as_str());
            assert_eq!(ticker_unchecked.as_str(), s.as_str());
            assert_eq!(ticker_unchecked, ticker);

            set_str.insert(s.to_string());
            set_ticker.insert(ticker);
        }

        assert_eq!(set_str.len(), set_ticker.len());
    }

    #[test]
    pub fn ticker_bad_inputs() {
        assert!(Ticker::try_from("").is_err());
        assert!(Ticker::try_from("TOOLONGGGGGGGGGGG").is_err());
    }

    #[test]
    pub fn foo() {
        // Need proper benchmark
        let num = Decimal::from_str("256.67").unwrap();
        for _ in 0..10 {
            let start = Instant::now();
            let contract = OptionContract::from_iso_format("PANW250117P00256670").unwrap();
            assert_eq!(contract.strike(), num);
            println!("Elapsed {:?}", start.elapsed());
        }
    }

    #[test]
    pub fn dx_feed_format() {
        let contract = OptionContract::from_iso_format("AAPL231229P00202500").unwrap();
        assert_eq!(contract.as_dx_feed_symbol(), ".AAPL231229P202.5");

        let contract = OptionContract::from_dx_feed_symbol(".AAPL231229P202.5").unwrap();
        assert_eq!(contract.as_dx_feed_symbol(), ".AAPL231229P202.5");
    }

    #[test]
    pub fn from_dx_format() {
        let contract = OptionContract::from_dx_feed_symbol(".PANW250117P256.67").unwrap();
        assert_eq!(contract.as_str(), "PANW250117P00256670");

        let contract = OptionContract::from_dx_feed_symbol(".BOAT230120P24").unwrap();
        assert_eq!(contract.as_str(), "BOAT230120P00024000");

        let contract = OptionContract::from_dx_feed_symbol(".UPV230421C38").unwrap();
        assert_eq!(contract.as_str(), "UPV230421C00038000");
    }

    #[test]
    pub fn from_dx_format_bad_format() {
        assert!(OptionContract::from_dx_feed_symbol("").is_err());
        // Missing strike
        assert!(OptionContract::from_dx_feed_symbol(".UPV230421C").is_err());
        // Missing option type
        assert!(OptionContract::from_dx_feed_symbol(".UPV23042138").is_err());
        // Missing bad expiry
        assert!(OptionContract::from_dx_feed_symbol(".UPV231321C38").is_err());
        // Missing starting dot
        assert!(OptionContract::from_dx_feed_symbol("UPV230421C38").is_err());
        // Missing ticker
        assert!(OptionContract::from_dx_feed_symbol(".230421C38").is_err());
        // ticker to long
        assert!(OptionContract::from_dx_feed_symbol(".UPVTOOLONG230421C38").is_err());
    }

    #[test]
    pub fn from_iso_format_bad_format() {
        assert!(OptionContract::from_iso_format("").is_err());

        // Missing strike
        assert!(OptionContract::from_iso_format("UPV230421C").is_err());

        // Malformed strike
        assert!(OptionContract::from_iso_format("UPV230421C0003800").is_err());

        // Missing ticker
        assert!(OptionContract::from_iso_format("230421C00038000").is_err());
        // Bad expiry
        assert!(OptionContract::from_iso_format("UPV231321C00038000").is_err());
        // Bad ticker
        assert!(OptionContract::from_iso_format("UPVWAYTOOLONG230421C00038000").is_err());
    }

    #[test]
    pub fn from_iso_format() {
        let file = File::open("./test_data/contracts.csv").unwrap();
        let reader = BufReader::new(file);

        let mut set_str = HashSet::new();
        let mut set_ticker = HashSet::new();

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
            assert_eq!(contract.ticker().as_str(), splits[1]);
            assert_eq!(contract.strike(), Decimal::from_str(splits[2]).unwrap());
            assert_eq!(
                contract.option_type(),
                OptionType::try_from(splits[3]).unwrap()
            );

            match splits[3] {
                "call" => assert!(contract.is_call()),
                "put" => assert!(contract.is_put()),
                other => panic!("{other} is not a valid option type, bad test data?"),
            }

            assert_eq!(contract.expiry(), NaiveDate::from_str(splits[4]).unwrap());

            set_str.insert(option_symbol.to_string());
            set_ticker.insert(contract);
        }

        assert_eq!(set_str.len(), set_ticker.len());
    }

    #[test]
    fn missing_strike() {
        assert!(OptionContract::from_iso_format("AAPL231229P").is_err());
    }
}
