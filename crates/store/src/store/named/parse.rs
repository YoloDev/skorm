use evitable::*;
use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::char,
  combinator::{map, opt},
  error::{ErrorKind, ParseError},
  multi::many0_count,
  sequence::{preceded, terminated, tuple},
  AsChar, Err, InputIter, InputLength, Needed, Slice,
};
use smallvec::SmallVec;
use std::sync::Arc;

type I<'a> = super::input::InputBuf<'a>;
type E<'a> = nom::error::VerboseError<super::input::InputBuf<'a>>;
type O<'a, T> = nom::IResult<I<'a>, T, E<'a>>;

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
    None | Some((_, false)) => Err(Err::Error(E::from_error_kind(buf, ErrorKind::HexDigit))),
    Some((c, true)) => Ok((buf.slice(c.len()..), c.as_char())),
  }
}

fn dec<'a>(buf: I<'a>) -> O<'a, char> {
  match buf.iter_elements().next().map(|t| {
    let c = t.as_char();
    let b = c.is_dec_digit();
    (c, b)
  }) {
    None | Some((_, false)) => Err(Err::Error(E::from_error_kind(buf, ErrorKind::HexDigit))),
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
    None | Some((_, false)) => Err(Err::Error(E::from_error_kind(buf, ErrorKind::HexDigit))),
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

pub(super) fn iriref<'a>(i: I<'a>) -> O<'a, String> {
  let (i, enclosed) = opt(char('<'))(i)?;

  let mut acc = String::with_capacity(256);
  let mut i = i.clone();

  loop {
    if i.input_len() == 0 {
      if let None = &enclosed {
        return Ok((i, acc));
      } else {
        return Err(Err::Error(E::from_char(i, '>')));
      }
    }

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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Name {
  Iri(Arc<str>),
  Prefixed(Arc<str>, Arc<str>),
}

#[evitable(description("Could not parse name '{}'", name))]
pub struct NameContext {
  name: String,
}

pub(super) fn parse_name(value: &str) -> NameResult<Name> {
  let buf = I::new(value);
  let parser = preceded(
    many0_count(ws),
    terminated(
      alt((
        map(pname_ln, |(ns, s)| {
          Name::Prefixed(Arc::<str>::from(ns.as_ref()), Arc::<str>::from(s.as_ref()))
        }),
        map(pname_ns, |s| {
          Name::Prefixed(Arc::from(""), Arc::<str>::from(s.as_ref()))
        }),
        map(iriref, |s| Name::Iri(Arc::<str>::from(s.as_ref()))),
      )),
      many0_count(ws),
    ),
  );

  match parser(buf) {
    Ok((i, res)) => {
      ensure!(
        i.input_len() == 0,
        NameContext {
          name: value.to_owned(),
        }
      );
      Ok(res)
    }
    _ => fail!(NameContext {
      name: value.to_owned(),
    }),
  }
}
