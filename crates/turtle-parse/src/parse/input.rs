use nom::{
  error::{ErrorKind, ParseError},
  Compare, CompareResult, Err, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition,
  Slice,
};
use std::fmt;
use std::ops::RangeFrom;
use std::sync::atomic::{AtomicU16, Ordering};

pub(super) trait InputLocate {
  fn offset(&self) -> usize;
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct InputBuf<'a> {
  /// Pos of the input - 0 = first byte
  pos: usize,

  /// End of the input (for subslicing)
  end: usize,

  /// Data
  buf: &'a str,

  /// Counter for unnamed blank nodes.
  blank: &'a AtomicU16,
}

impl<'a> InputBuf<'a> {
  pub(super) fn new(buf: &'a [u8], blank: &'a AtomicU16) -> Result<Self, std::str::Utf8Error> {
    match std::str::from_utf8(buf.as_ref()) {
      Err(e) => Err(e),
      Ok(buf) => Ok(Self {
        pos: 0,
        end: buf.len(),
        buf,
        blank,
      }),
    }
  }

  #[cfg(test)]
  pub(super) fn new_str(buf: &'a str, blank: &'a AtomicU16) -> Self {
    Self {
      pos: 0,
      end: buf.len(),
      buf,
      blank,
    }
  }

  pub(super) fn next_blank_name(&self) -> u16 {
    let num = self.blank.fetch_add(1, Ordering::Relaxed);
    num
  }

  #[inline]
  fn slice(&self) -> &'a str {
    &self.buf[self.pos..self.end]
  }

  #[inline]
  pub(super) fn str_to(&self, end: &Self) -> &'a str {
    if end.pos < self.pos {
      panic!("end pos was smaller than start pos.");
    }

    &self.buf[self.pos..end.pos]
  }

  #[inline]
  pub(crate) fn pos(&self) -> usize {
    self.pos
  }
}

impl<'a> AsRef<str> for InputBuf<'a> {
  #[inline]
  fn as_ref(&self) -> &str {
    self.slice()
  }
}

impl<'a> fmt::Display for InputBuf<'a> {
  #[inline]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{:?}", self.slice())
  }
}

impl<'a> PartialEq for InputBuf<'a> {
  #[inline]
  fn eq(&self, other: &InputBuf<'a>) -> bool {
    self.slice() == other.slice()
  }
}

impl<'a> InputLocate for InputBuf<'a> {
  #[inline]
  fn offset(&self) -> usize {
    self.pos
  }
}

impl<'a, U> Compare<U> for InputBuf<'a>
where
  &'a str: Compare<U>,
{
  #[inline]
  fn compare(&self, t: U) -> CompareResult {
    self.slice().compare(t)
  }

  #[inline]
  fn compare_no_case(&self, t: U) -> CompareResult {
    self.slice().compare_no_case(t)
  }
}

impl<'a> InputLength for InputBuf<'a> {
  #[inline]
  fn input_len(&self) -> usize {
    self.slice().input_len()
  }
}

impl<'a> InputIter for InputBuf<'a> {
  type Item = <&'a str as InputIter>::Item;
  type Iter = <&'a str as InputIter>::Iter;
  type IterElem = <&'a str as InputIter>::IterElem;

  #[inline]
  fn iter_indices(&self) -> Self::Iter {
    self.slice().iter_indices()
  }

  #[inline]
  fn iter_elements(&self) -> Self::IterElem {
    self.slice().iter_elements()
  }

  #[inline]
  fn position<P>(&self, predicate: P) -> Option<usize>
  where
    P: Fn(Self::Item) -> bool,
  {
    self.slice().position(predicate)
  }

  #[inline]
  fn slice_index(&self, count: usize) -> Option<usize> {
    self.slice().slice_index(count)
  }
}

impl<'a> InputTake for InputBuf<'a> {
  #[inline]
  fn take(&self, count: usize) -> Self {
    let new_end = std::cmp::min(self.pos + count, self.end);

    Self {
      pos: self.pos,
      end: new_end,
      buf: self.buf,
      blank: self.blank,
    }
  }

  #[inline]
  fn take_split(&self, count: usize) -> (Self, Self) {
    let split = std::cmp::min(self.pos + count, self.end);
    (
      Self {
        pos: split,
        end: self.end,
        buf: self.buf,
        blank: self.blank,
      },
      Self {
        pos: self.pos,
        end: split,
        buf: self.buf,
        blank: self.blank,
      },
    )
  }
}

impl<'a> InputTakeAtPosition for InputBuf<'a> {
  type Item = <&'a str as InputIter>::Item;

  /// looks for the first element of the input type for which the condition returns true,
  /// and returns the input up to this position
  ///
  /// *streaming version*: if no element is found matching the condition, this will return `Incomplete`
  fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
  where
    P: Fn(Self::Item) -> bool,
  {
    match self.slice().position(predicate) {
      Some(n) => Ok(self.take_split(n)),
      None => Err(Err::Incomplete(nom::Needed::Size(1))),
    }
  }

  /// looks for the first element of the input type for which the condition returns true
  /// and returns the input up to this position
  ///
  /// fails if the produced slice is empty
  ///
  /// *streaming version*: if no element is found matching the condition, this will return `Incomplete`
  fn split_at_position1<P, E: ParseError<Self>>(
    &self,
    predicate: P,
    e: ErrorKind,
  ) -> IResult<Self, Self, E>
  where
    P: Fn(Self::Item) -> bool,
  {
    match self.slice().position(predicate) {
      Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
      Some(n) => Ok(self.take_split(n)),
      None => Err(Err::Incomplete(nom::Needed::Size(1))),
    }
  }

  /// looks for the first element of the input type for which the condition returns true,
  /// and returns the input up to this position
  ///
  /// *complete version*: if no element is found matching the condition, this will return the whole input
  fn split_at_position_complete<P, E: ParseError<Self>>(
    &self,
    predicate: P,
  ) -> IResult<Self, Self, E>
  where
    P: Fn(Self::Item) -> bool,
  {
    match self.slice().position(predicate) {
      Some(n) => Ok(self.take_split(n)),
      None => Ok(self.take_split(self.input_len())),
    }
  }

  /// looks for the first element of the input type for which the condition returns true
  /// and returns the input up to this position
  ///
  /// fails if the produced slice is empty
  ///
  /// *complete version*: if no element is found matching the condition, this will return the whole input
  fn split_at_position1_complete<P, E: ParseError<Self>>(
    &self,
    predicate: P,
    e: ErrorKind,
  ) -> IResult<Self, Self, E>
  where
    P: Fn(Self::Item) -> bool,
  {
    match self.slice().position(predicate) {
      Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
      Some(n) => Ok(self.take_split(n)),
      None => {
        let len = self.input_len();
        if len == 0 {
          Err(Err::Error(E::from_error_kind(self.clone(), e)))
        } else {
          Ok(self.take_split(len))
        }
      }
    }
  }
}

impl<'a> Slice<RangeFrom<usize>> for InputBuf<'a> {
  #[inline]
  fn slice(&self, range: RangeFrom<usize>) -> Self {
    let start = self.pos + range.start;
    if start > self.end {
      panic!("Index out of bounds.");
    }

    Self {
      pos: start,
      end: self.end,
      buf: self.buf,
      blank: self.blank,
    }
  }
}
