use crate::parse::{E, I, O};
use nom::{
  branch::alt,
  bytes::streaming::tag,
  character::streaming::char,
  combinator::{map, opt},
  error::{ErrorKind, ParseError},
  multi::{many0_count, many1_count, many_m_n},
  sequence::{preceded, tuple},
  AsChar, Err, InputIter, Needed, Slice,
};
use rust_decimal::Decimal as d128;
use smallvec::SmallVec;

fn pn_local_esc<'a>(buf: I<'a>) -> O<'a, char> {
  preceded(
    char('\\'),
    alt((
      char('_'),
      char('~'),
      char('.'),
      char('-'),
      char('!'),
      char('$'),
      char('&'),
      char('\''),
      char('('),
      char(')'),
      char('*'),
      char('+'),
      char(','),
      char(';'),
      char('='),
      char('/'),
      char('?'),
      char('#'),
      char('@'),
      char('%'),
    )),
  )(buf)
}

fn hex<'a>(buf: I<'a>) -> O<'a, char> {
  match buf.iter_elements().next().map(|t| {
    let c = t.as_char();
    let b = c.is_hex_digit();
    (c, b)
  }) {
    None => Err(Err::Incomplete(Needed::Size(1))),
    Some((_, false)) => Err(Err::Error(E::from_error_kind(buf, ErrorKind::HexDigit))),
    Some((c, true)) => Ok((buf.slice(c.len()..), c.as_char())),
  }
}

fn dec<'a>(buf: I<'a>) -> O<'a, char> {
  match buf.iter_elements().next().map(|t| {
    let c = t.as_char();
    let b = c.is_dec_digit();
    (c, b)
  }) {
    None => Err(Err::Incomplete(Needed::Size(1))),
    Some((_, false)) => Err(Err::Error(E::from_error_kind(buf, ErrorKind::HexDigit))),
    Some((c, true)) => Ok((buf.slice(c.len()..), c.as_char())),
  }
}

fn percent<'a>(buf: I<'a>) -> O<'a, char> {
  map(tuple((char('%'), hex, hex)), |(_, c1, c2)| {
    let bytes = [c1 as u8, c2 as u8];
    let s = std::str::from_utf8(&bytes).unwrap();
    u8::from_str_radix(s, 16).unwrap() as char
  })(buf)
}

fn plx<'a>(buf: I<'a>) -> O<'a, char> {
  alt((percent, pn_local_esc))(buf)
}

#[inline]
fn char_range(lo: char, hi: char) -> impl for<'a> Fn(I<'a>) -> O<'a, char> {
  debug_assert!(lo < hi);

  move |buf| match buf.iter_elements().next().map(|t| {
    let c = t.as_char();
    let b = c >= lo && c <= hi;
    (c, b)
  }) {
    None => Err(Err::Incomplete(Needed::Size(1))),
    Some((_, false)) => Err(Err::Error(E::from_error_kind(buf, ErrorKind::HexDigit))),
    Some((c, true)) => Ok((buf.slice(c.len()..), c.as_char())),
  }
}

fn pn_chars_base<'a>(buf: I<'a>) -> O<'a, char> {
  alt((
    char_range('A', 'Z'),
    char_range('a', 'z'),
    char_range('\u{00C0}', '\u{00D6}'),
    char_range('\u{00D8}', '\u{00F6}'),
    char_range('\u{00F8}', '\u{02FF}'),
    char_range('\u{0370}', '\u{037D}'),
    char_range('\u{037F}', '\u{1FFF}'),
    char_range('\u{200C}', '\u{200D}'),
    char_range('\u{2070}', '\u{218F}'),
    char_range('\u{2C00}', '\u{2FEF}'),
    char_range('\u{3001}', '\u{D7FF}'),
    char_range('\u{F900}', '\u{FDCF}'),
    char_range('\u{FDF0}', '\u{FFFD}'),
    char_range('\u{10000}', '\u{EFFFF}'),
  ))(buf)
}

fn pn_chars_u<'a>(buf: I<'a>) -> O<'a, char> {
  alt((pn_chars_base, char('_')))(buf)
}

fn pn_chars<'a>(buf: I<'a>) -> O<'a, char> {
  alt((
    pn_chars_u,
    char('-'),
    dec,
    char('\u{00B7}'),
    char_range('\u{0300}', '\u{036F}'),
    char_range('\u{203F}', '\u{2040}'),
  ))(buf)
}

#[inline]
fn start_mid_end<'a>(
  cstart: impl Fn(I<'a>) -> O<'a, char>,
  cmid: impl Fn(I<'a>) -> O<'a, char>,
  cend: impl Fn(I<'a>) -> O<'a, char>,
) -> impl Fn(I<'a>) -> O<'a, String> {
  move |i: I| {
    let mut acc = String::with_capacity(16);
    let mut i = i.clone();

    // First match the start parser
    match cstart(i.clone()) {
      Err(e) => return Err(e),
      Ok((i1, c)) => {
        i = i1;
        acc.push(c);
      }
    };

    // Then, match the mid parser, as many times as possible,
    // building up a vec of (I, char)
    let mut mid: SmallVec<[_; 16]> = SmallVec::new();
    loop {
      match cmid(i) {
        Err(Err::Error(_)) => break,
        Err(e) => return Err(e),
        Ok((i1, c)) => {
          mid.push((i, c));
          i = i1;
        }
      }
    }

    // Try matching cend. If it fits, great, else we need to backtrack
    match cend(i) {
      Err(Err::Error(_)) => {
        // Walk backwards the mid-list, untill we can match cend
        // eating it in it's entirety if necesary.
        while let Some(last) = mid.pop() {
          match cend(last.0) {
            Err(Err::Error(_)) => (),
            Err(e) => return Err(e),
            Ok((iend, c)) => {
              mid.push((iend, c));
              break;
            }
          }
        }
      }
      Err(e) => return Err(e),
      Ok((_, c1)) => {
        mid.push((i, c1));
      }
    }

    for (_, c) in mid {
      acc.push(c);
    }

    Ok((i, acc))
  }
}

fn pn_prefix<'a>(buf: I<'a>) -> O<'a, String> {
  start_mid_end(pn_chars_base, alt((pn_chars, char('.'))), pn_chars)(buf)
}

fn pn_local<'a>(buf: I<'a>) -> O<'a, String> {
  start_mid_end(
    alt((pn_chars_u, char(':'), dec, plx)),
    alt((pn_chars, char('.'), char(':'), plx)),
    alt((pn_chars, char(':'), plx)),
  )(buf)
}

fn ws<'a>(buf: I<'a>) -> O<'a, char> {
  alt((
    char(0x20 as char), // space
    char(0x9 as char),  // character tabulation
    char(0xd as char),  // carriage return
    char(0xa as char),  // new line
  ))(buf)
}

fn echar<'a>(buf: I<'a>) -> O<'a, char> {
  preceded(
    char('\\'),
    alt((
      map(char('t'), |_| '\t'),
      map(char('b'), |_| '\u{8}'),
      map(char('n'), |_| '\n'),
      map(char('r'), |_| '\r'),
      map(char('f'), |_| '\u{c}'),
      map(char('"'), |_| '\"'),
      map(char('\''), |_| '\''),
      map(char('\\'), |_| '\\'),
    )),
  )(buf)
}

fn uchar<'a>(buf: I<'a>) -> O<'a, char> {
  alt((
    map(
      preceded(tag("\\u"), tuple((hex, hex, hex, hex))),
      |(u1, u2, u3, u4)| {
        let buf = [u1 as u8, u2 as u8, u3 as u8, u4 as u8];
        let s = std::str::from_utf8(&buf).unwrap();
        // TODO: This should be error, not panic
        std::char::from_u32(u32::from_str_radix(s, 16).unwrap()).unwrap()
      },
    ),
    map(
      preceded(tag("\\U"), tuple((hex, hex, hex, hex, hex, hex, hex, hex))),
      |(u1, u2, u3, u4, u5, u6, u7, u8)| {
        let buf = [
          u1 as u8, u2 as u8, u3 as u8, u4 as u8, u5 as u8, u6 as u8, u7 as u8, u8 as u8,
        ];
        let s = std::str::from_utf8(&buf).unwrap();
        // TODO: This should be error, not panic
        std::char::from_u32(u32::from_str_radix(s, 16).unwrap()).unwrap()
      },
    ),
  ))(buf)
}

#[inline]
fn long_str<'a>(quote_char: char) -> impl Fn(I<'a>) -> O<'a, String> {
  preceded(many_m_n(3, 3, char(quote_char)), move |i: I| {
    let mut acc = String::with_capacity(128);
    let mut i = i.clone();

    loop {
      // first, check for string end
      match many_m_n(3, 3, char(quote_char))(i.clone()) {
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
        Ok((i1, _)) => return Ok((i1, acc)),
      }

      // next, check for escaped character sequence
      match alt((echar, uchar))(i.clone()) {
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
        Ok((i1, c)) => {
          acc.push(c);
          i = i1;
          continue;
        }
      }

      // next; check for backslash, as that will indicate
      // an invalid escape sequence.
      match char('\\')(i.clone()) {
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
        Ok((i1, _)) => {
          return match i1.iter_elements().next() {
            None => Err(Err::Incomplete(Needed::Size(1))),
            Some(c) => Err(Err::Error(E::from_char(i1, c))),
          }
        }
      }

      // next; accept any single character that's not matched
      // any of our previous steps
      match i.iter_elements().next() {
        None => return Err(Err::Incomplete(Needed::Size(1))),
        Some(c) => {
          i = i.slice(c.len()..);
          acc.push(c);
        }
      }
    }
  })
}

pub(super) fn string_literal_long_quote<'a>(buf: I<'a>) -> O<'a, String> {
  long_str('"')(buf)
}

pub(super) fn string_literal_long_single_quote<'a>(buf: I<'a>) -> O<'a, String> {
  long_str('\'')(buf)
}

#[inline]
fn simple_str<'a>(quote_char: char) -> impl Fn(I<'a>) -> O<'a, String> {
  preceded(char(quote_char), move |i: I| {
    let mut acc = String::with_capacity(32);
    let mut i = i.clone();

    loop {
      // first, check for string end
      match char(quote_char)(i.clone()) {
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
        Ok((i1, _)) => return Ok((i1, acc)),
      }

      // next, check for illegal characters
      match alt((char('\n'), char('\r')))(i.clone()) {
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
        Ok((i1, c)) => return Err(Err::Error(E::from_char(i1, c))),
      }

      // next, check for escaped character sequence
      match alt((echar, uchar))(i.clone()) {
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
        Ok((i1, c)) => {
          acc.push(c);
          i = i1;
          continue;
        }
      }

      // next; check for backslash, as that will indicate
      // an invalid escape sequence.
      match char('\\')(i.clone()) {
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
        Ok((i1, _)) => {
          return match i1.iter_elements().next() {
            None => Err(Err::Incomplete(Needed::Size(1))),
            Some(c) => Err(Err::Error(E::from_char(i1, c))),
          }
        }
      }

      // next; accept any single character that's not matched
      // any of our previous steps
      match i.iter_elements().next() {
        None => return Err(Err::Incomplete(Needed::Size(1))),
        Some(c) => {
          i = i.slice(c.len()..);
          acc.push(c);
        }
      }
    }
  })
}

pub(super) fn string_literal_quote<'a>(buf: I<'a>) -> O<'a, String> {
  simple_str('"')(buf)
}

pub(super) fn string_literal_single_quote<'a>(buf: I<'a>) -> O<'a, String> {
  simple_str('\'')(buf)
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum UnaryOp {
  Plus,
  Minus,
}

fn unary_op<'a>(buf: I<'a>) -> O<'a, UnaryOp> {
  let parser = opt(alt((char('+'), char('-'))));

  match parser(buf) {
    Err(e) => Err(e),
    Ok((i, Some('-'))) => Ok((i, UnaryOp::Minus)),
    Ok((i, _)) => Ok((i, UnaryOp::Plus)),
  }
}

#[inline]
fn void<'a, T>(f: impl Fn(I<'a>) -> O<'a, T>) -> impl Fn(I<'a>) -> O<'a, ()> {
  map(f, |_| ())
}

#[inline]
fn matched_str<'a, T>(f: impl Fn(I<'a>) -> O<'a, T>) -> impl Fn(I<'a>) -> O<'a, &'a str> {
  move |i: I| match f(i) {
    Err(e) => Err(e),
    Ok((i2, _)) => Ok((i2, i.str_to(&i2))),
  }
}

fn exponent<'a>(buf: I<'a>) -> O<'a, &'a str> {
  matched_str(preceded(
    alt((char('e'), char('E'))),
    tuple((unary_op, many1_count(dec))),
  ))(buf)
}

pub(super) fn double<'a>(buf: I<'a>) -> O<'a, f64> {
  // [0-9]+ '.' [0-9]* EXPONENT
  let arm1 = void(tuple((
    many1_count(dec),
    char('.'),
    many0_count(dec),
    exponent,
  )));

  // '.' [0-9]+ EXPONENT
  let arm2 = void(tuple((char('.'), many1_count(dec), exponent)));

  // [0-9]+ EXPONENT
  let arm3 = void(tuple((many1_count(dec), exponent)));

  let parser = map(
    matched_str(tuple((unary_op, alt((arm1, arm2, arm3))))),
    |s| s.parse::<f64>(),
  );

  match parser(buf) {
    Err(e) => Err(e),
    Ok((i, Err(_))) => {
      let inner_error = E::from_error_kind(i, ErrorKind::Digit);
      let outer_error = E::add_context(i, "error parsing as f64", inner_error);
      Err(Err::Error(outer_error))
    }
    Ok((i, Ok(v))) => Ok((i, v)),
  }
}

pub(super) fn decimal<'a>(buf: I<'a>) -> O<'a, d128> {
  let parser = map(
    matched_str(tuple((
      unary_op,
      many0_count(dec),
      char('.'),
      many1_count(dec),
    ))),
    |s| s.parse::<d128>(),
  );

  match parser(buf) {
    Err(e) => Err(e),
    Ok((i, Err(_))) => {
      let inner_error = E::from_error_kind(i, ErrorKind::Digit);
      let outer_error = E::add_context(i, "error parsing as d128", inner_error);
      Err(Err::Error(outer_error))
    }
    Ok((i, Ok(v))) => Ok((i, v)),
  }
}

pub(super) fn integer<'a>(buf: I<'a>) -> O<'a, i64> {
  let parser = map(matched_str(tuple((unary_op, many1_count(dec)))), |s| {
    s.parse::<i64>()
  });

  match parser(buf) {
    Err(e) => Err(e),
    Ok((i, Err(_))) => {
      let inner_error = E::from_error_kind(i, ErrorKind::Digit);
      let outer_error = E::add_context(i, "error parsing as i64", inner_error);
      Err(Err::Error(outer_error))
    }
    Ok((i, Ok(v))) => Ok((i, v)),
  }
}

pub(super) fn langtag<'a>(buf: I<'a>) -> O<'a, &'a str> {
  let start = alt((char_range('a', 'z'), char_range('A', 'Z')));

  let rest = alt((
    char_range('a', 'z'),
    char_range('A', 'Z'),
    char_range('0', '9'),
  ));

  let lang_tag_ident_part = tuple((
    many1_count(start),
    many0_count(tuple((char('-'), many1_count(rest)))),
  ));

  preceded(char('@'), matched_str(lang_tag_ident_part))(buf)
}

pub(super) fn blank_node_label<'a>(buf: I<'a>) -> O<'a, String> {
  preceded(
    tag("_:"),
    start_mid_end(
      alt((pn_chars_u, char_range('0', '9'))),
      alt((pn_chars, char('.'))),
      pn_chars,
    ),
  )(buf)
}

pub(super) fn pname_ns<'a>(buf: I<'a>) -> O<'a, String> {
  map(
    tuple((opt(pn_prefix), char(':'))),
    |(prefix, _)| match prefix {
      Some(p) => p,
      None => String::new(),
    },
  )(buf)
}

pub(super) fn pname_ln<'a>(buf: I<'a>) -> O<'a, (String, String)> {
  tuple((pname_ns, pn_local))(buf)
}

pub(super) fn iriref<'a>(buf: I<'a>) -> O<'a, String> {
  preceded(char('<'), move |i: I| {
    let mut acc = String::with_capacity(256);
    let mut i = i.clone();

    loop {
      // first, check for string end
      match char('>')(i.clone()) {
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
        Ok((i1, _)) => return Ok((i1, acc)),
      }

      // next, check for illegal characters
      match alt((
        char_range('\u{0}', '\u{20}'),
        char('<'),
        char('"'),
        char('{'),
        char('}'),
        char('|'),
        char('^'),
        char('`'),
      ))(i.clone())
      {
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
        Ok((i1, c)) => return Err(Err::Error(E::from_char(i1, c))),
      }

      // next, check for escaped character sequence
      match uchar(i.clone()) {
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
        Ok((i1, c)) => {
          acc.push(c);
          i = i1;
          continue;
        }
      }

      // next; check for backslash, as that will indicate
      // an invalid escape sequence.
      match char('\\')(i.clone()) {
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
        Ok((i1, _)) => {
          return match i1.iter_elements().next() {
            None => Err(Err::Incomplete(Needed::Size(1))),
            Some(c) => Err(Err::Error(E::from_char(i1, c))),
          }
        }
      }

      // next; accept any single character that's not matched
      // any of our previous steps
      match i.iter_elements().next() {
        None => return Err(Err::Incomplete(Needed::Size(1))),
        Some(c) => {
          i = i.slice(c.len()..);
          acc.push(c);
        }
      }
    }
  })(buf)
}

fn comment<'a>(buf: I<'a>) -> O<'a, ()> {
  fn eat_line<'a>(buf: I<'a>) -> O<'a, ()> {
    let mut i = buf.clone();
    loop {
      // first check for comment end
      match alt((
        map(tuple((char('\r'), char('\n'))), |_| '\n'),
        char('\r'),
        char('\n'),
        char('\0'),
      ))(i)
      {
        Ok((_, '\0')) => return Ok((i, ())),
        Ok((i1, _)) => return Ok((i1, ())),
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
      }

      // next; accept any single character that's not matched
      // any of our previous steps
      match i.iter_elements().next() {
        None => return Err(Err::Incomplete(Needed::Size(1))),
        Some(c) => {
          i = i.slice(c.len()..);
        }
      }
    }
  }

  preceded(char('#'), eat_line)(buf)
}

pub(super) fn wsc<'a>(buf: I<'a>) -> O<'a, bool> {
  let ws_many = map(many1_count(ws), |_| ());

  map(many0_count(alt((ws_many, comment))), |c| c > 0)(buf)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[inline]
  fn test_parser<'a, P, R>(parser: P, str_val: &'a str, expected: R)
  where
    P: Fn(I) -> O<R>,
    R: std::fmt::Debug + PartialEq<R>,
  {
    // We insert `\0` at the end of the string, so as to
    // not triger an incomplete response.

    let mut string = String::with_capacity(str_val.len() + 1);
    string.push_str(str_val);
    string.push('\0');
    let counter = Default::default();
    let buf = I::new_str(&string, &counter);
    let (rest, val) = parser(buf).expect("parsed");
    assert_eq!(rest.as_ref(), "\0");
    assert_eq!(val, expected, "str={}", str_val);
  }

  #[test]
  fn test_doubles() {
    fn test_val(str_val: &str, expected: f64) {
      test_parser(double, str_val, expected)
    }

    test_val("0.0E0", 0.0);
    test_val(".0E0", 0.0);
    test_val("0E0", 0.0);

    test_val("0.0e0", 0.0);
    test_val(".0e0", 0.0);
    test_val("0e0", 0.0);

    test_val("10.10E0", 10.10);
    test_val(".10E0", 0.10);
    test_val("10E0", 10.0);

    test_val("10.10E2", 10.10 * 100.0);
    test_val(".10E2", 0.10 * 100.0);
    test_val("10E2", 10.0 * 100.0);

    test_val("10.10E-2", 10.10e-2);
    test_val(".10E-2", 0.10e-2);
    test_val("10E-2", 10.0e-2);

    test_val("-10.10E-2", -10.10e-2);
    test_val("-.10E-2", -0.10e-2);
    test_val("-10E-2", -10.0e-2);
  }

  #[test]
  fn test_decimal() {
    fn test_val(str_val: &str, expected: f64) {
      use rust_decimal::prelude::*;
      test_parser(decimal, str_val, d128::from_f64(expected).unwrap())
    }

    test_val("0.0", 0.0);
    test_val(".0", 0.0);

    test_val("10.10", 10.10);
    test_val(".10", 0.10);

    test_val("1010.0", 1010.0);
    test_val("-1010.0", -1010.0);

    test_val("-.1010", -0.1010);
    test_val("-10.0001", -10.0001);
  }
}
