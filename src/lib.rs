#![doc = include_str!("../README.md")]

use chrono::Datelike;
use chrono::NaiveDate;
use core::str;
use rust_decimal::Decimal;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::str::{from_utf8_unchecked, FromStr};

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

// The maximum length of a stock ticker.
// According to the NYSE specifications (https://www.nyse.com/publicdocs/nyse/data/NYSE_Symbology_Spec_v1.0c.pdf)
// it is reserving 16 chars for future growth.
//
// We are limitting it for now to 7 since the longest ticker symbol
// is 5. It is 6 if you use `.` for A/B stocks such as CWEN.A
//
// We set it to 8 because we save 1 byte for the length of the ticker
const TICKER_LENGTH: usize = 8;

// TODO: We could save it in 6 bytes I think and map each ticker char to a number from 1 to 38 (alphabet + numbers + special chars)
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ticker {
    bytes: [u8; TICKER_LENGTH],
}

// A ticker cannot be empty
#[allow(clippy::len_without_is_empty)]
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

    pub fn len(&self) -> usize {
        self.bytes[0] as usize
    }
}

impl Debug for Ticker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ticker({})", self.as_str())
    }
}

impl FromStr for Ticker {
    type Err = TickerParseErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let len = s.len();
        if len > (TICKER_LENGTH - 1) {
            return Err(TickerParseErr::TooLong(len));
        } else if len == 0 {
            return Err(TickerParseErr::Empty);
        }

        let ticker_bytes = s.as_bytes();
        let mut bytes = [0u8; TICKER_LENGTH];
        bytes[0] = len as u8;

        bytes[1..len + 1].copy_from_slice(&ticker_bytes[..len]);

        Ok(Self::from_raw(bytes))
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
        Self::from_str(value)
    }
}

impl TryFrom<String> for Ticker {
    type Error = TickerParseErr;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.as_str().try_into()
    }
}

impl PartialEq<&str> for Ticker {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<String> for Ticker {
    fn eq(&self, other: &String) -> bool {
        self.as_str() == other.as_str()
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
    pub const fn is_call(&self) -> bool {
        matches!(self, OptionType::Call)
    }

    /// Returns true if it is a put
    ///
    /// # Examples
    /// ```
    /// use financial_symbols::OptionType;
    /// assert!(!OptionType::Call.is_put());
    /// assert!(OptionType::Put.is_put());
    /// ```
    pub const fn is_put(&self) -> bool {
        matches!(self, OptionType::Put)
    }
}

impl Display for OptionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.as_str())
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct OptionContract {
    ticker: Ticker,
    expiry: NaiveDate,
    option_type: OptionType,
    strike: u32,
}

impl OptionContract {
    /// Returns the strike of the contract as a decimal.
    ///
    /// # Examples
    /// ```
    /// use financial_symbols::{OptionContract, OptionType};
    /// use rust_decimal::Decimal;
    /// let contract = OptionContract::from_osi("SPXW231127C03850000").unwrap();
    /// assert_eq!(contract.strike(), Decimal::from(3850));
    /// ```
    pub fn strike(&self) -> Decimal {
        Decimal::from_i128_with_scale(self.strike as i128, 3).normalize()
    }

    /// Returns the expiry date of the contract
    /// # Examples
    /// ```
    /// use financial_symbols::{OptionContract, OptionType};
    /// use chrono::NaiveDate;
    /// use std::str::FromStr;
    /// let contract = OptionContract::from_osi("SPXW231127C03850000").unwrap();
    /// assert_eq!(contract.expiry(), NaiveDate::from_str("2023-11-27").unwrap());
    /// ```
    pub fn expiry(&self) -> NaiveDate {
        self.expiry
    }

    /// Returns the ticker of the contract
    /// # Examples
    /// ```
    /// use financial_symbols::{OptionContract, OptionType};
    /// let contract = OptionContract::from_osi("SPXW231127C03850000").unwrap();
    /// assert_eq!(contract.ticker().as_str(), "SPXW");
    /// ```
    pub fn ticker(&self) -> Ticker {
        self.ticker
    }

    /// Returns the option type of the contract
    /// # Examples
    /// ```
    /// use financial_symbols::{OptionContract, OptionType};
    /// let contract = OptionContract::from_osi("SPXW231127C03850000").unwrap();
    /// assert_eq!(contract.option_type(), OptionType::Call);
    /// let contract = OptionContract::from_osi("SPXW231127P03850000").unwrap();
    /// assert_eq!(contract.option_type(), OptionType::Put);
    pub fn option_type(&self) -> OptionType {
        self.option_type
    }

    /// Returns true if the option is a call
    /// # Examples
    /// ```
    /// use financial_symbols::{OptionContract, OptionType};
    /// let contract = OptionContract::from_osi("SPXW231127C03850000").unwrap();
    /// assert!(contract.is_call());
    /// let contract = OptionContract::from_osi("SPXW231127P03850000").unwrap();
    /// assert!(!contract.is_call());
    /// ```
    pub const fn is_call(&self) -> bool {
        self.option_type.is_call()
    }

    /// Returns true if the option is a put
    /// # Examples
    /// ```
    /// use financial_symbols::{OptionContract, OptionType};
    /// let contract = OptionContract::from_osi("SPXW231127C03850000").unwrap();
    /// assert!(!contract.is_put());
    /// let contract = OptionContract::from_osi("SPXW231127P03850000").unwrap();
    /// assert!(contract.is_put());
    /// ```
    pub const fn is_put(&self) -> bool {
        self.option_type.is_put()
    }

    /// Returns the option symbol in the dx feed format.
    /// See [`from_dx_feed`](#method.from_dx_feed) for more information.
    /// # Examples
    /// ```
    /// use financial_symbols::{OptionContract, OptionType};
    /// let contract = OptionContract::from_osi("SPXW231127C03850000").unwrap();
    /// assert_eq!(contract.to_dx_feed(), ".SPXW231127C3850");
    /// ```
    pub fn to_dx_feed(&self) -> String {
        let strike = self.strike().to_string();

        let mut s = String::with_capacity(1 + self.ticker.len() + EXPIRY_LENGTH + strike.len());

        s.push('.');
        s.push_str(self.ticker.as_str());
        s.push_str(&self.expiry.format("%y%m%d").to_string());
        s.push(if self.option_type.is_call() { 'C' } else { 'P' });
        s.push_str(&strike);

        s
    }

    /// Returns the option symbol in the iso format.
    /// See [`from_osi`](#method.from_osi) for more information about the OSI format.
    pub fn to_osi(&self) -> String {
        let (bytes, total_len) = self.osi_utf8_bytes();
        unsafe { String::from_utf8_unchecked(bytes[..total_len].to_vec()) }
    }

    /// Parses an option contract in the normal iso format.
    ///
    /// The iso format is defined as:
    /// \<TICKER\>\<EXPIRY\>\<OPTION_TYPE\>\<STRIKE\>
    ///
    /// **TICKER** is the underlying symbol, it consists only of alphabetical chars.
    /// For instance the ticker `BRK.B` would be represented as `BRKB`. It can be padded by whitespaces to fill
    /// the 6 chars as definied by the OCC but the shorter version is also valid where the whitespace is omitted.
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
    /// Note: The original specification by the OCC says that the option symbol should be exactly 21 chars long.
    /// For the ticker 6 chars are reserved, 6 for the expiry, 1 for the option type and 8 for the strike.
    /// If the ticker is shorter than 6 chars it should be padded with spaces but `from_osi` also accepts
    /// variants where the whitespace padding is omitted.
    ///
    /// # Examples
    /// ```
    /// use financial_symbols::{OptionContract, OptionType};
    /// use rust_decimal::Decimal;
    /// use chrono::NaiveDate;
    /// use std::str::FromStr;
    /// let contract = OptionContract::from_osi("SPXW231127C03850000").unwrap();
    /// assert_eq!(contract.ticker().as_str(), "SPXW");
    /// assert_eq!(contract.option_type(), OptionType::Call);
    /// assert_eq!(contract.expiry(), NaiveDate::from_str("2023-11-27").unwrap());
    /// assert_eq!(contract.strike(), Decimal::from(3850));
    /// ```
    pub fn from_osi(s: &str) -> Result<Self, OptionContractErr> {
        let len = s.len();

        if len > CONTRACT_LENGTH {
            return Err(OptionContractErr::TooLong(len));
        } else if len < MIN_CONTRACT_LENGTH {
            return Err(OptionContractErr::TooShort(len));
        }

        let ticker_len = len - EXPIRY_LENGTH - OPTION_TYPE_LENGTH - STRIKE_LENGTH;

        let (ticker, rest) = s.split_at(ticker_len);

        let ticker = Ticker::from_str(ticker).map_err(|_| OptionContractErr::InvalidTicker)?;

        let bytes = rest.as_bytes();

        let strike_offset = rest.len() - STRIKE_LENGTH;
        let option_type_offset = strike_offset - OPTION_TYPE_LENGTH;
        let expiry_offset = option_type_offset - EXPIRY_LENGTH;

        for (idx, byte) in bytes.iter().enumerate() {
            if idx >= strike_offset {
                if !byte.is_ascii_digit() {
                    let idx = idx + ticker_len;
                    return Err(OptionContractErr::ExpectedNum { idx, byte: *byte });
                }
            } else if idx == option_type_offset {
                if *byte != b'C' && *byte != b'P' {
                    return Err(OptionContractErr::InvalidOptionType(*byte as char));
                }
            } else if idx >= expiry_offset && !byte.is_ascii_digit() {
                let idx = idx + ticker_len;
                return Err(OptionContractErr::ExpectedNum { idx, byte: *byte });
            }
        }

        let year: i32 = (bytes[0] - b'0') as i32 * 10 + (bytes[1] - b'0') as i32;
        let month: u32 = (bytes[2] - b'0') as u32 * 10 + (bytes[3] - b'0') as u32;
        let day: u32 = (bytes[4] - b'0') as u32 * 10 + (bytes[5] - b'0') as u32;

        let expiry = match NaiveDate::from_ymd_opt(2000 + year, month, day) {
            Some(date) => date,
            None => return Err(OptionContractErr::InvalidExpiry { year, month, day }),
        };

        let option_type = match bytes[6] {
            b'C' => OptionType::Call,
            b'P' => OptionType::Put,
            _ => unreachable!(),
        };

        let strike = (bytes[7] - b'0') as u32 * 10000000
            + (bytes[8] - b'0') as u32 * 1000000
            + (bytes[9] - b'0') as u32 * 100000
            + (bytes[10] - b'0') as u32 * 10000
            + (bytes[11] - b'0') as u32 * 1000
            + (bytes[12] - b'0') as u32 * 100
            + (bytes[13] - b'0') as u32 * 10;

        Ok(Self {
            ticker,
            expiry,
            option_type,
            strike,
        })
    }

    /// Parses an option contract based on the dx feed format.
    /// The format is defined as:
    ///
    /// `. + OPTION ROOT + EXPIRATION DATE in YYMMDD format + OPTION TYPE (C or P) + STRIKE`
    ///
    /// The format is based on the option symbols being returned
    /// by the financial data provider DxFeed. The symbols can
    /// be found here: https://tools.dxfeed.com/ipf?TYPE=OPTION
    ///
    /// For the specification take a look at: https://www.dxfeed.com/downloads/documentation/dxFeed_Symbol_Guide.pdf
    ///
    /// # Examples
    /// ```
    /// use financial_symbols::{OptionContract, OptionType};
    /// use rust_decimal::Decimal;
    /// use chrono::NaiveDate;
    /// use std::str::FromStr;
    /// let contract = OptionContract::from_dx_feed(".SPXW231127C3850").unwrap();
    /// assert_eq!(contract.ticker(), "SPXW");
    /// assert_eq!(contract.option_type(), OptionType::Call);
    /// assert_eq!(contract.expiry(), NaiveDate::from_str("2023-11-27").unwrap());
    /// assert_eq!(contract.strike(), Decimal::from(3850));
    /// assert_eq!(contract.to_osi(), "SPXW231127C03850000");
    /// ```
    pub fn from_dx_feed(s: &str) -> Result<Self, OptionContractErr> {
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

            let mut s = [0u8; 8];

            let back_zeros = 3 - down.len();
            let front_zeros = 8 - up.len() - back_zeros - down.len();

            let mut idx = 0;
            for _ in 0..front_zeros {
                s[idx] = b'0';
                idx += 1;
            }

            // s.push_str(up);
            // s.push_str(down);
            s[idx..idx + up.len()].copy_from_slice(up.as_bytes());
            idx += up.len();

            s[idx..idx + down.len()].copy_from_slice(down.as_bytes());
            idx += down.len();

            for _ in 0..back_zeros {
                s[idx] = b'0';
                idx += 1;
            }

            s
        } else {
            let back_zeros = 3;
            if (strike.len() + back_zeros) > 8 {
                return Err(OptionContractErr::InvalidStrike);
            }
            let front_zeros = 8 - strike.len() - back_zeros;
            let mut s = [0u8; 8];

            let mut idx = 0;
            for _ in 0..front_zeros {
                s[idx] = b'0';
                idx += 1;
            }

            s[idx..idx + strike.len()].copy_from_slice(strike.as_bytes());

            for _ in 0..back_zeros {
                s[idx + strike.len()] = b'0';
                idx += 1;
            }

            s
        };

        let mut iso = start.as_bytes().to_vec();
        iso.extend_from_slice(&strike);

        let iso = unsafe { String::from_utf8_unchecked(iso) };

        Self::from_osi(iso.as_str())
    }

    /// Returns the bytes of the option contract in the OSI format and the length of the resulting
    /// string.
    ///
    /// This can be used to construct a &str from the bytes. In most cases you should just use the
    /// [`to_osi`](#method.to_osi) method instead.
    ///
    /// For more information about the OSI format see [`from_osi`](#method.from_osi).
    ///
    /// # Examples
    /// ```
    /// use financial_symbols::{OptionContract, OptionType};
    /// let contract = OptionContract::from_osi("SPXW231127C03850000").unwrap();
    /// let (bytes, len) = contract.osi_utf8_bytes();
    /// let s = unsafe { std::str::from_utf8_unchecked(&bytes[..len]) };
    /// assert_eq!(s, "SPXW231127C03850000");
    pub fn osi_utf8_bytes(&self) -> ([u8; CONTRACT_LENGTH], usize) {
        let mut bytes = [0u8; CONTRACT_LENGTH];

        let ticker_bytes = &self.ticker.bytes;
        let ticker_len = self.ticker.len();

        bytes[..ticker_len].copy_from_slice(&ticker_bytes[1..ticker_len + 1]);

        let year = self.expiry.year() - 2000;
        let month = self.expiry.month0() + 1;
        let day = self.expiry.day();
        let ot_type = if self.option_type.is_call() {
            b'C'
        } else {
            b'P'
        };
        let strike = self.strike;

        let total_len = ticker_len + EXPIRY_LENGTH + OPTION_TYPE_LENGTH + STRIKE_LENGTH;

        bytes[ticker_len..total_len].copy_from_slice(&[
            (year / 10) as u8 + b'0',
            (year % 10) as u8 + b'0',
            (month / 10) as u8 + b'0',
            (month % 10) as u8 + b'0',
            (day / 10) as u8 + b'0',
            (day % 10) as u8 + b'0',
            ot_type,
            (strike / 10000000) as u8 + b'0',
            ((strike / 1000000) % 10) as u8 + b'0',
            ((strike / 100000) % 10) as u8 + b'0',
            ((strike / 10000) % 10) as u8 + b'0',
            ((strike / 1000) % 10) as u8 + b'0',
            ((strike / 100) % 10) as u8 + b'0',
            ((strike / 10) % 10) as u8 + b'0',
            ((strike) % 10) as u8 + b'0',
        ]);

        (bytes, total_len)
    }
}

impl Display for OptionContract {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_osi())
    }
}

impl TryFrom<&str> for OptionContract {
    type Error = OptionContractErr;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        OptionContract::from_osi(value)
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

    impl<'de> Visitor<'_> for TickerVisitor {
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

    impl<'de> Visitor<'_> for OptionContractVisitor {
        type Value = OptionContract;

        fn expecting(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("An option contract should be a string")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            OptionContract::from_osi(v).map_err(E::custom)
        }
    }

    impl Serialize for OptionContract {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let (bytes, len) = self.osi_utf8_bytes();
            let s = unsafe { str::from_utf8_unchecked(&bytes[..len]) };
            serializer.serialize_str(s)
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
            OptionContract::from_osi(s).map_err(|err| err.into())
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
            let (bytes, len) = self.osi_utf8_bytes();
            let s = unsafe { str::from_utf8_unchecked(&bytes[..len]) };
            <&str as ToSql>::to_sql(&s, ty, w)
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
    };

    use std::str::FromStr;

    use anyhow::Context;

    use super::*;

    #[test]
    fn debug_ticker() {
        let ticker = Ticker::from_str_unchecked("AAPL");
        assert_eq!(format!("{:?}", ticker), "Ticker(AAPL)");
    }

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
    pub fn dx_feed_format() {
        let contract = OptionContract::from_osi("AAPL231229P00202500").unwrap();
        assert_eq!(contract.to_dx_feed(), ".AAPL231229P202.5");

        let contract = OptionContract::from_dx_feed(".AAPL231229P202.5").unwrap();
        assert_eq!(contract.to_dx_feed(), ".AAPL231229P202.5");
    }

    #[test]
    pub fn from_dx_format() {
        let contract = OptionContract::from_dx_feed(".PANW250117P256.67").unwrap();
        assert_eq!(contract.to_osi(), "PANW250117P00256670");

        let contract = OptionContract::from_dx_feed(".BOAT230120P24").unwrap();
        assert_eq!(contract.to_osi(), "BOAT230120P00024000");

        let contract = OptionContract::from_dx_feed(".UPV230421C38").unwrap();
        assert_eq!(contract.to_osi(), "UPV230421C00038000");
    }

    #[test]
    pub fn from_dx_format_bad_format() {
        assert!(OptionContract::from_dx_feed("").is_err());
        // Missing strike
        assert!(OptionContract::from_dx_feed(".UPV230421C").is_err());
        // Missing option type
        assert!(OptionContract::from_dx_feed(".UPV23042138").is_err());
        // Missing bad expiry
        assert!(OptionContract::from_dx_feed(".UPV231321C38").is_err());
        // Missing starting dot
        assert!(OptionContract::from_dx_feed("UPV230421C38").is_err());
        // Missing ticker
        assert!(OptionContract::from_dx_feed(".230421C38").is_err());
        // ticker to long
        assert!(OptionContract::from_dx_feed(".UPVTOOLONG230421C38").is_err());
    }

    #[test]
    pub fn from_osi_format_bad_format() {
        assert!(OptionContract::from_osi("").is_err());

        // Missing strike
        assert!(OptionContract::from_osi("UPV230421C").is_err());

        // Malformed strike
        assert!(OptionContract::from_osi("UPV230421C0003800").is_err());

        // Missing ticker
        assert!(OptionContract::from_osi("230421C00038000").is_err());
        // Bad expiry
        assert!(OptionContract::from_osi("UPV231321C00038000").is_err());
        // Bad ticker
        assert!(OptionContract::from_osi("UPVWAYTOOLONG230421C00038000").is_err());
    }

    #[test]
    pub fn from_osi_format() {
        let file = File::open("./test_data/contracts.csv").unwrap();
        let reader = BufReader::new(file);

        let mut set_str = HashSet::new();
        let mut set_ticker = HashSet::new();

        for (idx, line) in reader.lines().skip(1).enumerate() {
            // "option_symbol","underlying_symbol","strike","option_type","expires"
            let line = line.unwrap();
            let splits = line.split(',').collect::<Vec<_>>();

            let option_symbol = splits[0];
            let bytes = option_symbol.as_bytes();
            let context = format!("Line {idx} contract {option_symbol} bytes {bytes:?}");

            let contract = OptionContract::from_osi(option_symbol)
                .context(context)
                .unwrap();

            assert_eq!(contract.to_osi(), option_symbol);
            assert_eq!(contract.ticker(), splits[1]);
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
        assert!(OptionContract::from_osi("AAPL231229P").is_err());
    }

    #[test]
    fn missing_ticker() {
        assert!(OptionContract::from_osi("231229P00202500").is_err());
    }

    #[test]
    fn missing_option_type() {
        assert!(OptionContract::from_osi("AAPL23122900202500").is_err());
    }

    #[test]
    fn missing_expiry() {
        assert!(OptionContract::from_osi("AAPLP00202500").is_err());
    }

    #[test]
    fn bad_expiry() {
        assert!(OptionContract::from_osi("AAPL231321P00202500").is_err());
        assert!(OptionContract::from_osi("AAPL231399P00202500").is_err());
    }

    #[test]
    fn bad_option_type() {
        assert!(OptionContract::from_osi("AAPL231229X00202500").is_err());
        assert!(OptionContract::from_osi("AAPL231229100202500").is_err());
    }

    #[test]
    fn bad_strike() {
        assert!(OptionContract::from_osi("PANW250117P0X256670").is_err());
        assert!(OptionContract::from_osi("PANW250117P00256670X").is_err());
        assert!(OptionContract::from_osi("PANW250117P002566701").is_err());
        assert!(OptionContract::from_osi("PANW250117P100256670").is_err());
    }

    #[test]
    fn can_handle_osi_long() {
        let contract = OptionContract::from_osi("PANW  250117P00256670").unwrap();
        assert_eq!(contract.to_osi(), "PANW  250117P00256670");
    }
}
