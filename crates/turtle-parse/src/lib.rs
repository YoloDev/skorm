#![feature(trait_alias)]
#![feature(trace_macros)]

mod error;
pub(crate) mod parse;
mod ty;

use bytes::{Buf, BufMut};
use skorm_parse::*;
use std::sync::atomic::AtomicU16;
use ty::ToAbsolute;
use url::Url;

pub struct TurtleParser {
  base: Url,
  state: parse::ParserState,
  blank: AtomicU16,
}

impl TurtleParser {
  pub fn new(base: Url) -> Self {
    Self {
      base,
      state: Default::default(),
      blank: AtomicU16::from(1),
    }
  }
}

impl RdfParser for TurtleParser {
  type Item = ty::Statement;
  type Error = error::Error;

  fn next(&mut self, buf: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
    if buf.is_empty() || buf.len() == 1 && buf[0] == 0 {
      return Ok(None);
    }

    loop {
      let state = std::mem::replace(&mut self.state, Default::default());
      //let input = parse::input(&buf, &self.blank)?;
      let (state, len, result) = parse::parse_next(&buf, &self.blank, state)?;
      std::mem::replace(&mut self.state, state);
      buf.advance(len);
      match result {
        Some(ty::Statement::Base(b)) => {
          let url = Url::parse(&b)?;
          std::mem::replace(&mut self.base, url);
        }

        Some(stmt) => return Ok(Some(stmt.to_absolute(&self.base)?)),

        None => return Ok(None),
      }
    }
  }

  fn end(&mut self, buf: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
    buf.reserve(1);
    buf.put_u8('\0' as u8);
    let result = self.next(buf);
    if !buf.is_empty() && buf[buf.len() - 1] == '\0' as u8 {
      buf.truncate(buf.len() - 1);
    }
    result
  }
}
