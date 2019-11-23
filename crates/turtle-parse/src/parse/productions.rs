use crate::parse::terminals::*;
use crate::parse::{InnerResult, ListResult, OList, OValue, SubResult, E, I, O};
use crate::ty::*;
use lazy_static::lazy_static;
use nom::{
  branch::alt,
  bytes::streaming::{tag, tag_no_case},
  character::streaming::char,
  combinator::{map, opt},
  error::{ErrorKind, ParseError},
  sequence::{preceded, tuple},
  Err,
};
use std::sync::Arc;

// const NIL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";
// const FIRST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
// const REST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";

lazy_static! {
  static ref TYPE: Arc<str> = Arc::from("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
  static ref NIL: Arc<str> = Arc::from("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil");
  static ref FIRST: Arc<str> = Arc::from("http://www.w3.org/1999/02/22-rdf-syntax-ns#first");
  static ref REST: Arc<str> = Arc::from("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest");
}

fn next_blank_node<'a>(i: I<'a>) -> BlankNode {
  BlankNode::anon(i.next_blank_name())
}

enum NamedOrBlankNode {
  Named(NamedNode),
  Blank(BlankNode),
}

impl From<NamedNode> for NamedOrBlankNode {
  #[inline]
  fn from(n: NamedNode) -> Self {
    Self::Named(n)
  }
}

impl From<BlankNode> for NamedOrBlankNode {
  #[inline]
  fn from(n: BlankNode) -> Self {
    Self::Blank(n)
  }
}

impl From<NamedOrBlankNode> for Subject {
  #[inline]
  fn from(n: NamedOrBlankNode) -> Self {
    match n {
      NamedOrBlankNode::Named(n) => n.into(),
      NamedOrBlankNode::Blank(n) => n.into(),
    }
  }
}

impl From<NamedOrBlankNode> for Object {
  #[inline]
  fn from(n: NamedOrBlankNode) -> Self {
    match n {
      NamedOrBlankNode::Named(n) => n.into(),
      NamedOrBlankNode::Blank(n) => n.into(),
    }
  }
}

trait WrapInner<I>: Sized {
  type Output;

  fn wrap(self, result: I) -> Self::Output;

  #[inline]
  fn wrap_result<T>(self, result: InnerResult<I>) -> SubResult<T, Self::Output> {
    SubResult::Inner(result.map(move |v| self.wrap(v)))
  }

  #[inline]
  fn wrap_list<T>(self, result: InnerResult<I>) -> ListResult<T, Self::Output> {
    ListResult::Inner(result.map(move |v| self.wrap(v)))
  }
}

//trace_macros!(true);
macro_rules! _parser_state_state_enum {
  // exit rule.
  (
    @collect_state_variants ($name:ident),
    ($(,)*) -> ($($variants:tt)*)
  ) => {
    #[derive(Clone, Debug)]
    pub(crate) enum $name {
      None,
      $($variants)*
    }

    impl Default for $name {
      #[inline]
      fn default() -> Self {
        Self::None
      }
    }
  };

  // consume yield variant.
  (
    @collect_state_variants $fixed:tt,
    (inner $_var:ident($_inner:ty), $($tail:tt)*) -> ($($variants:tt)*)
  ) => {
    _parser_state_state_enum! {
      @collect_state_variants $fixed,
      ($($tail)*) -> ($($variants)*)
    }
  };

  // collect variant without payload.
  (
    @collect_state_variants $fixed:tt,
    (state $var:ident, $($tail:tt)*) -> ($($variants:tt)*)
  ) => {
    _parser_state_state_enum! {
      @collect_state_variants $fixed,
      ($($tail)*) -> ($($variants)* $var,)
    }
  };

  // collect variant with payload.
  (
    @collect_state_variants $fixed:tt,
    (state $var:ident $struct:tt, $($tail:tt)*) -> ($($variants:tt)*)
  ) => {
    _parser_state_state_enum! {
      @collect_state_variants $fixed,
      ($($tail)*) -> ($($variants)* $var $struct,)
    }
  };

  // entry rule
  (enum $name:ident { $($body:tt)* }) => {
    _parser_state_state_enum! {
      @collect_state_variants ($name),
      ($($body)*,) -> ()
    }
  };
}

macro_rules! _parser_state_parser_enum {
  // exit rule.
  (
    @collect_state_variants ($name:ident $state:ident),
    ($(,)*) -> ($($variants:tt)*) ($(($var_name:ident $var_ty:ty),)*)
  ) => {
    #[derive(Clone, Debug)]
    pub(crate) enum $name {
      State($state),
      $($variants)*
    }

    impl Default for $name {
      #[inline]
      fn default() -> Self {
        Self::State(Default::default())
      }
    }

    impl From<$state> for $name {
      #[inline]
      fn from(state: $state) -> Self {
        Self::State(state)
      }
    }

    $(
      impl WrapInner<$var_ty> for $state {
        type Output = $name;

        #[inline]
        fn wrap(self, inner: $var_ty) -> Self::Output {
          ParserState::$var_name(self, Box::new(inner))
        }
      }
    )*

    impl $name {
      #[inline]
      pub(super) fn into_result<T>(
        self,
        subj: impl Into<Subject>,
        pred: impl Into<Predicate>,
        obj: impl Into<Object>
      ) -> SubResult<T, Self> {
        SubResult::Inner(InnerResult {
          state: self,
          subj: subj.into(),
          pred: pred.into(),
          obj: obj.into(),
        })
      }
    }

    impl $state {
      #[inline]
      pub(super) fn into_result<T>(
        self,
        subj: impl Into<Subject>,
        pred: impl Into<Predicate>,
        obj: impl Into<Object>
      ) -> SubResult<T, $name> {
        $name::from(self).into_result(subj, pred, obj)
      }
    }
  };

  // collect yield variant.
  (
    @collect_state_variants ($name:ident $state:ident),
    (inner $var:ident($inner:ty), $($tail:tt)*) -> ($($variants:tt)*) ($($var_names:tt)*)
  ) => {
    _parser_state_parser_enum! {
      @collect_state_variants ($name $state),
      ($($tail)*) -> ($($variants)* $var($state, Box<$inner>),) ($($var_names)* ($var $inner),)
    }
  };

  // consume variant without payload.
  (
    @collect_state_variants $fixed:tt,
    (state $var:ident, $($tail:tt)*) -> $variants:tt $var_names:tt
  ) => {
    _parser_state_parser_enum! {
      @collect_state_variants $fixed,
      ($($tail)*) -> $variants $var_names
    }
  };

  // consume variant with payload.
  (
    @collect_state_variants $fixed:tt,
    (state $var:ident $struct:tt, $($tail:tt)*) -> $variants:tt $var_names:tt
  ) => {
    _parser_state_parser_enum! {
      @collect_state_variants $fixed,
      ($($tail)*) -> $variants $var_names
    }
  };

  // entry rule
  (enum $name:ident ($state:ident) { $($body:tt)* }) => {
    _parser_state_parser_enum! {
      @collect_state_variants ($name $state),
      ($($body)*,) -> () ()
    }
  };
}

macro_rules! parser_state {
  ($name:ident $body:tt) => {
    #[allow(dead_code)]
    pub(crate) mod $name {
      use super::*;
      _parser_state_state_enum! { enum State $body }
      _parser_state_parser_enum! { enum ParserState(State) $body }
    }
  };
}

fn anon<'a>(buf: I<'a>) -> O<'a, ()> {
  map(tuple((char('['), wsc, char(']'))), |_| ())(buf)
}

#[inline]
fn wrapped<'a, T>(f: impl FnOnce(I<'a>) -> O<'a, T>) -> impl FnOnce(I<'a>) -> O<'a, T> {
  move |i: I| {
    let (i, _) = wsc(i)?;
    let (i, r) = f(i)?;
    let (i, _) = wsc(i)?;
    Ok((i, r))
  }
}

fn blank_node<'a>(i: I<'a>) -> O<'a, BlankNode> {
  alt((
    map(blank_node_label, |v| BlankNode::named(v)),
    map(anon, |_| next_blank_node(i)),
  ))(i)
}

fn prefix_name<'a>(i: I<'a>) -> O<'a, (String, String)> {
  alt((pname_ln, map(pname_ns, |n| (n, String::new()))))(i)
}

fn iri<'a>(i: I<'a>) -> O<'a, NamedNode> {
  alt((
    map(iriref, NamedNode::iri),
    map(prefix_name, NamedNode::namespaced),
  ))(i)
}

fn string<'a>(i: I<'a>) -> O<'a, String> {
  alt((
    string_literal_long_quote,
    string_literal_long_single_quote,
    string_literal_quote,
    string_literal_single_quote,
  ))(i)
}

fn boolean_literal<'a>(i: I<'a>) -> O<'a, Literal> {
  alt((
    map(tag("true"), |_| Literal::boolean(true)),
    map(tag("false"), |_| Literal::boolean(false)),
  ))(i)
}

fn rdf_literal<'a>(i: I<'a>) -> O<'a, Literal> {
  enum Tail {
    LangTag(String),
    Type(NamedNode),
  }
  let tail = opt(alt((
    map(langtag, |s| Tail::LangTag(s.into())),
    map(preceded(tag("^^"), iri), |n| Tail::Type(n)),
  )));

  map(tuple((string, tail)), |(s, t)| match t {
    None => Literal::simple(s),
    Some(Tail::LangTag(t)) => Literal::lang_tagged((s, t)),
    Some(Tail::Type(t)) => Literal::typed((s, t)),
  })(i)
}

fn numeric_literal<'a>(i: I<'a>) -> O<'a, Literal> {
  alt((
    map(double, Literal::double),
    map(decimal, Literal::decimal),
    map(integer, Literal::integer),
  ))(i)
}

parser_state! {
  collection {
    state Ready {
      prev: BlankNode,
      root: BlankNode,
    },
    state Parsed {
      curr: BlankNode,
      value: Object,
      root: BlankNode,
    },
    state End {
      root: BlankNode,
    },
    inner Object(object::ParserState),
  }
}

fn collection<'a>(
  state: collection::ParserState,
) -> impl FnOnce(I<'a>) -> OValue<'a, NamedOrBlankNode, collection::ParserState> {
  use collection::*;
  move |mut i: I<'a>| {
    // syntax: '(' object* ')'
    match state {
      ParserState::State(State::None) => {
        // if we're at the start, first match the opening paren.
        match tuple((char('('), wsc))(i) {
          Err(e) => return Err(e),
          Ok((i2, _)) => i = i2,
        }
      }

      _ => (),
    }

    let (state, inner) = match state {
      // we previously yielded a new node in our linked list, yield it's value.
      ParserState::State(State::Parsed { curr, value, root }) => {
        return Ok((
          i,
          State::Ready {
            prev: curr.clone(),
            root,
          }
          .into_result(curr, NamedNode::iri(FIRST.clone()), value),
        ))
      }

      // we previously yielded the end of our linked list, return the root node.
      ParserState::State(State::End { root }) => {
        return Ok((i, NamedOrBlankNode::from(root).into()))
      }

      // if we're not already in a delegated state.
      ParserState::State(s) => {
        // check if we've reached the end of the collection.
        match tuple((wsc, char(')')))(i) {
          Err(Err::Error(_)) => (s, Default::default()),
          Err(e) => return Err(e),
          Ok((i2, _)) => {
            return Ok((
              i2,
              match s {
                State::None => NamedOrBlankNode::from(NamedNode::iri(NIL.clone())).into(),
                State::Ready { prev, root } => State::End { root }.into_result(
                  prev,
                  NamedNode::iri(REST.clone()),
                  NamedNode::iri(NIL.clone()),
                ),
                _ => unreachable!(),
              },
            ))
          }
        }
      }

      ParserState::Object(s, i) => (s, *i),
    };

    // parse a single object, returning it with state.
    match wrapped(object(inner))(i) {
      Err(e) => Err(e),
      Ok((i2, SubResult::Inner(inner))) => Ok((i2, state.wrap_result(inner))),
      Ok((i2, SubResult::Item(obj))) => Ok((
        i2,
        match state {
          State::None => {
            // we have no previous object - ie. this is the first item in the list.
            // emit <BLANK_ID> FIRST <VALUE> and set ready to parse nest object.
            let root = next_blank_node(i2);
            State::Ready {
              prev: root.clone(),
              root: root.clone(),
            }
            .into_result(root, NamedNode::iri(FIRST.clone()), obj)
          }

          State::Ready { prev, root } => {
            // we have a previous generagted node, meaning that the value we
            // just parsed is the next in a chain. here we emit <PREV> TAIL <NEW>,
            // and store the resently parsed value in state for emitting next
            // time.
            let curr = next_blank_node(i2);
            State::Parsed {
              curr: curr.clone(),
              value: obj,
              root,
            }
            .into_result(prev, NamedNode::iri(REST.clone()), curr)
          }

          _ => unreachable!(),
        },
      )),
    }
  }
}

parser_state! {
  blank_node_property_list {
    state Ready {
      root: BlankNode,
    },
    inner PredicateObjectList(predicate_object_list::ParserState),
  }
}

fn blank_node_property_list<'a>(
  state: blank_node_property_list::ParserState,
) -> impl FnOnce(I<'a>) -> OValue<'a, BlankNode, blank_node_property_list::ParserState> {
  use blank_node_property_list::*;
  move |mut i: I| {
    // syntax: '[' predicateObjectList ']'
    match state {
      ParserState::State(State::None) => {
        // if we're at the start, first match the opening paren.
        match tuple((char('['), wsc))(i) {
          Err(e) => return Err(e),
          Ok((i2, _)) => i = i2,
        }
      }

      _ => (),
    }

    // note: there's a separate rule for empty blank_node_property_lists
    let (state, inner) = match state {
      ParserState::State(s) => (s, Default::default()),
      ParserState::PredicateObjectList(s, i) => (s, *i),
    };

    match wrapped(predicate_object_list(inner))(i) {
      Err(e) => Err(e),
      Ok((i2, ListResult::Inner(inner))) => Ok((i2, state.wrap_result(inner))),
      Ok((i2, ListResult::Done)) => {
        // no more predicate objects to parse.
        // if we've not got any til now anyways,
        // this is an error.
        match state {
          State::None => {
            let inner_error = E::from_error_kind(i2, ErrorKind::IsA);
            let outer_error = E::add_context(i2, "predicate_object_list", inner_error);
            Err(Err::Error(outer_error))
          }

          State::Ready { root } => {
            // match the closing bracket.
            match tuple((wsc, char(']')))(i) {
              Err(e) => Err(e),
              Ok((i3, _)) => Ok((i3, root.into())),
            }
          }
        }
      }
      Ok((i2, ListResult::Item(inner, pair))) => {
        let (state, subj) = match state {
          State::Ready { root } => (State::Ready { root: root.clone() }, root),
          State::None => {
            let root = next_blank_node(i2);
            (State::Ready { root: root.clone() }, root)
          }
        };

        Ok((i2, state.wrap(inner).into_result(subj, pair.pred, pair.obj)))
      }
    }
  }
}

fn literal<'a>(i: I<'a>) -> O<'a, Literal> {
  alt((rdf_literal, numeric_literal, boolean_literal))(i)
}

parser_state! {
  object {
    inner Collection(collection::ParserState),
    inner BlankNodePropertyList(blank_node_property_list::ParserState),
  }
}

fn object<'a>(
  state: object::ParserState,
) -> impl FnOnce(I<'a>) -> OValue<'a, Object, object::ParserState> {
  use object::*;
  move |i: I| {
    // syntax: iri | BlankNode | collection | blankNodePropertyList | literal

    // we only attempt the simple cases if we're at start.
    if let ParserState::State(State::None) = &state {
      // first, try the simple ones
      // 1. iri
      match iri(i) {
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
        Ok((i2, node)) => {
          let obj: Object = node.into();
          return Ok((i2, obj.into()));
        }
      }

      // 2. blank node (named or ANON)
      match blank_node(i) {
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
        Ok((i2, node)) => {
          let obj: Object = node.into();
          return Ok((i2, obj.into()));
        }
      }

      // 3. literal
      match literal(i) {
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
        Ok((i2, node)) => {
          let obj: Object = node.into();
          return Ok((i2, obj.into()));
        }
      }
    }

    // if not, move on to the complex cases.
    match state {
      ParserState::State(State::None) => {
        // 4. collection
        match collection(Default::default())(i) {
          Err(Err::Error(_)) => (),
          Err(e) => return Err(e),
          Ok((i2, SubResult::Inner(inner))) => return Ok((i2, State::None.wrap_result(inner))),
          Ok((i2, SubResult::Item(obj))) => return Ok((i2, Object::from(obj).into())),
        }

        // 5. blank_node_property_list
        match blank_node_property_list(Default::default())(i) {
          // TODO: Add context to error probably
          Err(e) => Err(e),
          Ok((i2, SubResult::Inner(inner))) => Ok((i2, State::None.wrap_result(inner))),
          Ok((i2, SubResult::Item(obj))) => Ok((i2, Object::from(obj).into())),
        }
      }

      ParserState::Collection(_, inner) => match collection(*inner)(i) {
        Err(e) => Err(e),
        Ok((i2, SubResult::Inner(inner))) => Ok((i2, State::None.wrap_result(inner))),
        Ok((i2, SubResult::Item(obj))) => Ok((i2, Object::from(obj).into())),
      },

      ParserState::BlankNodePropertyList(_, inner) => match blank_node_property_list(*inner)(i) {
        Err(e) => Err(e),
        Ok((i2, SubResult::Inner(inner))) => Ok((i2, State::None.wrap_result(inner))),
        Ok((i2, SubResult::Item(obj))) => Ok((i2, Object::from(obj).into())),
      },
    }
  }
}

fn predicate<'a>(i: I<'a>) -> O<'a, Predicate> {
  map(iri, Into::into)(i)
}

parser_state! {
  subject {
    inner Collection(collection::ParserState),
  }
}

fn subject<'a>(
  state: subject::ParserState,
) -> impl FnOnce(I<'a>) -> OValue<'a, Subject, subject::ParserState> {
  use subject::*;
  move |i: I| {
    // syntax: iri | BlankNode | collection

    // we only attempt the simple cases if we're at start.
    if let ParserState::State(State::None) = &state {
      // first, try the simple ones
      // 1. iri
      match iri(i) {
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
        Ok((i2, node)) => {
          let obj: Subject = node.into();
          return Ok((i2, obj.into()));
        }
      }

      // 2. blank node (named or ANON)
      match blank_node(i) {
        Err(Err::Error(_)) => (),
        Err(e) => return Err(e),
        Ok((i2, node)) => {
          let obj: Subject = node.into();
          return Ok((i2, obj.into()));
        }
      }
    }

    // if not, move on to the complex cases.
    match state {
      ParserState::State(State::None) => {
        // 3. collection
        match collection(Default::default())(i) {
          // TODO: Add context to error probably
          Err(e) => Err(e),
          Ok((i2, SubResult::Inner(inner))) => Ok((i2, State::None.wrap_result(inner))),
          Ok((i2, SubResult::Item(obj))) => Ok((i2, Subject::from(obj).into())),
        }
      }

      ParserState::Collection(_, inner) => match collection(*inner)(i) {
        Err(e) => Err(e),
        Ok((i2, SubResult::Inner(inner))) => Ok((i2, State::None.wrap_result(inner))),
        Ok((i2, SubResult::Item(obj))) => Ok((i2, Subject::from(obj).into())),
      },
    }
  }
}

fn verb<'a>(i: I<'a>) -> O<'a, Predicate> {
  alt((
    map(char('a'), |_| NamedNode::iri(TYPE.clone()).into()),
    predicate,
  ))(i)
}

parser_state! {
  object_list {
    state Comma,
    inner Object(object::ParserState),
  }
}

fn object_list<'a>(
  state: object_list::ParserState,
) -> impl FnOnce(I<'a>) -> OList<'a, Object, object_list::ParserState> {
  use object_list::*;
  move |mut i: I| {
    // syntax: object (',' object)*
    let inner = match state {
      ParserState::State(State::Comma) => match char(',')(i) {
        Err(Err::Error(_)) => return Ok((i, ListResult::Done)),
        Err(e) => return Err(e),
        Ok((i2, _)) => {
          i = i2;
          Default::default()
        }
      },

      ParserState::State(State::None) => Default::default(),
      ParserState::Object(State::None, s) => *s,
      ParserState::Object(State::Comma, _) => unreachable!(),
    };

    match wrapped(object(inner))(i)? {
      (i2, SubResult::Inner(inner)) => Ok((i2, State::None.wrap_list(inner))),
      (i2, SubResult::Item(obj)) => Ok((i2, ListResult::Item(State::Comma.into(), obj))),
    }
  }
}

struct PredicateObjectPair {
  pred: Predicate,
  obj: Object,
}

impl PredicateObjectPair {
  #[inline]
  fn with_subj(self, subject: Subject) -> Triple {
    Triple::new(subject, self.pred, self.obj)
  }
}

parser_state! {
  predicate_object_list {
    state Verb {
      verb: Predicate,
    },
    inner ObjectList(object_list::ParserState),
  }
}

fn predicate_object_list<'a>(
  state: predicate_object_list::ParserState,
) -> impl FnOnce(I<'a>) -> OList<'a, PredicateObjectPair, predicate_object_list::ParserState> {
  use predicate_object_list::*;
  move |mut i: I| {
    // verb objectList (';' (verb objectList)?)*
    let mut state = state;
    loop {
      let (verb, inner) = match state {
        ParserState::State(State::Verb { verb }) => (verb, Default::default()),
        ParserState::ObjectList(State::Verb { verb }, inner) => (verb, *inner),
        ParserState::State(State::None) => {
          let (i2, verb) = wrapped(verb)(i)?;
          i = i2;
          (verb, Default::default())
        }
        s => unreachable!("{:?}", s),
      };

      match wrapped(object_list(inner))(i)? {
        (i2, ListResult::Inner(inner)) => return Ok((i2, State::Verb { verb }.wrap_list(inner))),
        (i2, ListResult::Item(inner, obj)) => {
          return Ok((
            i2,
            ListResult::Item(
              State::Verb { verb: verb.clone() }.wrap(inner),
              PredicateObjectPair { pred: verb, obj },
            ),
          ))
        }
        (i2, ListResult::Done) => i = i2,
      }

      match char(';')(i) {
        Err(Err::Error(_)) => return Ok((i, ListResult::Done)),
        Err(e) => return Err(e),
        Ok((i2, _)) => i = i2,
      }

      state = Default::default();
    }
  }
}

parser_state! {
  triples {
    state Subject {
      subj: Subject,
    },
    state Blank {
      subj: Subject,
    },
    inner Subject(subject::ParserState),
    inner Blank(blank_node_property_list::ParserState),
    inner PredicateObjectList(predicate_object_list::ParserState),
  }
}

fn triples<'a>(
  state: triples::ParserState,
) -> impl FnOnce(I<'a>) -> OList<'a, Triple, triples::ParserState> {
  use triples::*;
  move |i: I| {
    // subject predicateObjectList | blankNodePropertyList predicateObjectList?

    // first, try a subject or blankNodePropertyList
    let (i, state, inner) = match state {
      ParserState::State(State::None) => match subject(Default::default())(i) {
        Ok((i2, SubResult::Inner(inner))) => return Ok((i2, State::None.wrap_list(inner))),
        Ok((i2, SubResult::Item(subj))) => {
          (i2, State::Subject { subj: subj.into() }, Default::default())
        }
        Err(Err::Error(_)) => match blank_node_property_list(Default::default())(i) {
          Ok((i2, SubResult::Inner(inner))) => return Ok((i2, State::None.wrap_list(inner))),
          Ok((i2, SubResult::Item(subj))) => {
            (i2, State::Blank { subj: subj.into() }, Default::default())
          }
          Err(e) => return Err(e),
        },
        Err(e) => return Err(e),
      },
      ParserState::Subject(_, inner) => match subject(*inner)(i) {
        Ok((i2, SubResult::Inner(inner))) => return Ok((i2, State::None.wrap_list(inner))),
        Ok((i2, SubResult::Item(subj))) => {
          (i2, State::Subject { subj: subj.into() }, Default::default())
        }
        Err(e) => return Err(e),
      },
      ParserState::Blank(_, inner) => match blank_node_property_list(*inner)(i) {
        Ok((i2, SubResult::Inner(inner))) => return Ok((i2, State::None.wrap_list(inner))),
        Ok((i2, SubResult::Item(subj))) => {
          (i2, State::Blank { subj: subj.into() }, Default::default())
        }
        Err(e) => return Err(e),
      },
      ParserState::PredicateObjectList(s, inner) => (i, s, *inner),
      _ => unreachable!(),
    };

    match wrapped(predicate_object_list(inner))(i) {
      Ok((i2, ListResult::Inner(inner))) => Ok((i2, state.wrap_list(inner))),
      Ok((i2, ListResult::Item(inner, pair))) => {
        let subj = match &state {
          State::Subject { subj } => subj.clone(),
          State::Blank { subj } => subj.clone(),
          _ => unreachable!(),
        };
        Ok((
          i2,
          ListResult::Item(state.wrap(inner), pair.with_subj(subj)),
        ))
      }
      Ok((i2, ListResult::Done)) => Ok((i2, ListResult::Done)),
      Err(Err::Error(e)) => match state {
        State::Subject { .. } => Err(Err::Error(e)),
        State::Blank { .. } => Ok((i, ListResult::Done)),
        _ => unreachable!(),
      },
      Err(e) => Err(e),
    }
  }
}

fn sparql_prefix<'a>(i: I<'a>) -> O<'a, Prefix> {
  let (i, _) = tag_no_case("PREFIX")(i)?;
  let (i, _) = wsc(i)?;
  let (i, prefix) = pname_ns(i)?;
  let (i, _) = wsc(i)?;
  let (i, uri) = iriref(i)?;
  Ok((i, Prefix::new(prefix, uri)))
}

fn sparql_base<'a>(i: I<'a>) -> O<'a, RcString> {
  let (i, _) = tag_no_case("BASE")(i)?;
  let (i, _) = wsc(i)?;
  let (i, base) = iriref(i)?;
  Ok((i, base.into()))
}

fn base<'a>(i: I<'a>) -> O<'a, RcString> {
  let (i, _) = tag("@base")(i)?;
  let (i, _) = wsc(i)?;
  let (i, base) = iriref(i)?;
  let (i, _) = wsc(i)?;
  let (i, _) = char('.')(i)?;
  Ok((i, base.into()))
}

fn prefix_id<'a>(i: I<'a>) -> O<'a, Prefix> {
  let (i, _) = tag("@prefix")(i)?;
  let (i, _) = wsc(i)?;
  let (i, prefix) = pname_ns(i)?;
  let (i, _) = wsc(i)?;
  let (i, uri) = iriref(i)?;
  let (i, _) = wsc(i)?;
  let (i, _) = char('.')(i)?;
  Ok((i, Prefix::new(prefix, uri)))
}

fn directive<'a>(i: I<'a>) -> O<'a, Statement> {
  alt((
    map(prefix_id, Into::into),
    map(base, Statement::base),
    map(sparql_prefix, Into::into),
    map(sparql_base, Statement::base),
  ))(i)
}

parser_state! {
  statement {
    state Done,
    inner Triples(triples::ParserState),
  }
}

fn statement<'a>(
  state: statement::ParserState,
) -> impl FnOnce(I<'a>) -> OList<'a, Statement, statement::ParserState> {
  use statement::*;
  move |i: I| {
    // syntax directive | triples '.'
    match state {
      ParserState::State(State::Done) => Ok((i, ListResult::Done)),

      ParserState::Triples(_, inner) => match triples(*inner)(i) {
        Ok((i, ListResult::Done)) => {
          let (i, _) = char('.')(i)?;
          Ok((i, ListResult::Done))
        }
        Ok((i, ListResult::Item(inner, item))) => {
          Ok((i, ListResult::Item(State::None.wrap(inner), item.into())))
        }
        Ok((i, ListResult::Inner(inner))) => Ok((i, State::None.wrap_list(inner))),
        Err(e) => Err(e),
      },

      ParserState::State(State::None) => match directive(i) {
        Ok((i, dir)) => Ok((i, ListResult::Item(State::Done.into(), dir.into()))),
        Err(Err::Error(_)) => match triples(Default::default())(i) {
          Ok((i, ListResult::Done)) => Ok((i, ListResult::Done)),
          Ok((i, ListResult::Item(inner, item))) => {
            Ok((i, ListResult::Item(State::None.wrap(inner), item.into())))
          }
          Ok((i, ListResult::Inner(inner))) => Ok((i, State::None.wrap_list(inner))),
          Err(e) => Err(e),
        },
        Err(e) => Err(e),
      },
    }
  }
}

parser_state! {
  root {
    inner Statement(statement::ParserState),
  }
}

pub(super) fn root<'a>(
  state: root::ParserState,
) -> impl FnOnce(I<'a>) -> OList<'a, Statement, root::ParserState> {
  use root::*;
  move |mut input: I| {
    let mut state = state;

    loop {
      match state {
        ParserState::State(State::None) => {
          let (i, _) = wsc(input)?;
          match statement(Default::default())(i) {
            Ok((i, ListResult::Inner(inner))) => return Ok((i, State::None.wrap_list(inner))),
            Ok((i, ListResult::Item(inner, item))) => {
              return Ok((i, ListResult::Item(State::None.wrap(inner), item)))
            }
            Ok((i, ListResult::Done)) => {
              let (i2, _) = wsc(i)?;
              input = i2;
              state = Default::default();
            }
            Err(_) => return Ok((char('\0')(i)?.0, ListResult::Done)),
          }
        }

        ParserState::Statement(_, inner) => match statement(*inner)(input) {
          Ok((i, ListResult::Inner(inner))) => return Ok((i, State::None.wrap_list(inner))),
          Ok((i, ListResult::Item(inner, item))) => {
            return Ok((i, ListResult::Item(State::None.wrap(inner), item)))
          }
          Ok((i, ListResult::Done)) => {
            let (i2, _) = wsc(i)?;
            input = i2;
            state = Default::default();
          }
          Err(e) => return Err(e),
        },
      }
    }
  }
}
