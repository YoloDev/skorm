mod input;
mod productions;
mod terminals;

pub(crate) use crate::parse::input::InputBuf;
use crate::ty::*;
use evitable::EvitableError;
use nom::{error::VerboseError, Err, IResult};
pub(crate) use productions::{root, root::ParserState};
use std::sync::atomic::AtomicU16;

fn input<'a>(
  data: &'a impl AsRef<[u8]>,
  blank: &'a AtomicU16,
) -> crate::error::Result<InputBuf<'a>> {
  Ok(InputBuf::new(data.as_ref(), blank)?)
}

pub(super) fn parse_next<'a>(
  data: &'a impl AsRef<[u8]>,
  blank: &'a AtomicU16,
  state: ParserState,
) -> crate::error::Result<(ParserState, usize, Option<Statement>)> {
  use nom::character::streaming::char;
  use nom::sequence::precededc;
  let input = input(data, blank)?;
  if let Ok((i, _)) = precededc(input, terminals::wsc, char('\0')) {
    return Ok((state, i.pos() - 1, None));
  }

  match root(state.clone())(input) {
    Ok((i, ListResult::Inner(inner))) => {
      let triple = Triple::new(inner.subj, inner.pred, inner.obj);
      Ok((inner.state, i.pos(), Some(triple.into())))
    }

    Ok((i, ListResult::Item(state, item))) => Ok((state, i.pos(), Some(item))),

    Ok((i, ListResult::Done)) => Ok((Default::default(), i.pos(), None)),

    Err(Err::Incomplete(_)) => Ok((state, 0, None)),

    Err(Err::Failure(e)) | Err(Err::Error(e)) => Err(crate::error::Error::new(
      crate::error::Context::Parse,
      Some(input.convert_error(e).into()),
    )),
  }
}

struct InnerResult<S> {
  state: S,
  subj: Subject,
  pred: Predicate,
  obj: Object,
}

impl<S> InnerResult<S> {
  #[inline]
  fn map<O>(self, f: impl FnOnce(S) -> O) -> InnerResult<O> {
    InnerResult {
      state: f(self.state),
      subj: self.subj,
      pred: self.pred,
      obj: self.obj,
    }
  }
}

enum SubResult<T, S> {
  Item(T),
  Inner(InnerResult<S>),
}

impl<T, S> From<T> for SubResult<T, S> {
  #[inline]
  fn from(value: T) -> Self {
    Self::Item(value)
  }
}

enum ListResult<T, S> {
  Item(S, T),
  Done,
  Inner(InnerResult<S>),
}

type I<'a> = InputBuf<'a>;
type E<'a> = VerboseError<InputBuf<'a>>;
type O<'a, T> = IResult<I<'a>, T, E<'a>>;
type OValue<'a, T, S> = IResult<I<'a>, SubResult<T, S>, E<'a>>;
type OList<'a, T, S> = IResult<I<'a>, ListResult<T, S>, E<'a>>;
