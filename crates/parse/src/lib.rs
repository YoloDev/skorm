#![feature(type_alias_impl_trait)]

mod ty;

pub use bytes::BytesMut;
pub use ty::*;

#[derive(Clone)]
pub enum Statement<P, T>
where
  P: AsRdfPrefix + Clone,
  T: AsRdfTriple + Clone,
{
  Prefix(P),
  Triple(T),
}

impl<'s, P, T> AsRdfStatement for Statement<P, T>
where
  P: AsRdfPrefix + Clone,
  T: AsRdfTriple + Clone,
{
  type Prefix = P;
  type Triple = T;

  #[inline]
  fn as_statement(&self) -> Statement<P, T> {
    self.clone()
  }
}

pub trait AsRdfStatement {
  type Prefix: AsRdfPrefix + Clone;
  type Triple: AsRdfTriple + Clone;

  fn as_statement(&self) -> Statement<Self::Prefix, Self::Triple>;
}

pub trait FromRdfStatement {
  fn from_statement(statement: &impl AsRdfStatement) -> Self;
}

pub trait AsRdfPrefix {
  fn as_prefix(&self) -> Prefix;
}

pub trait FromRdfPrefix {
  fn from_prefix(prefix: &impl AsRdfPrefix) -> Self;
}

pub trait AsRdfSubject {
  fn as_subject(&self) -> Subject;
}

pub trait FromRdfSubject {
  fn from_subject(subject: &impl AsRdfSubject) -> Self;
}

pub trait AsRdfPredicate {
  fn as_predicate(&self) -> Predicate;
}

pub trait FromRdfPredicate {
  fn from_predicate(predicate: &impl AsRdfPredicate) -> Self;
}

pub trait AsRdfObject {
  fn as_object(&self) -> Object;
}

pub trait FromRdfObject {
  fn from_object(object: &impl AsRdfObject) -> Self;
}

pub trait AsRdfTriple {
  type Subject: AsRdfSubject;
  type Predicate: AsRdfPredicate;
  type Object: AsRdfObject;

  fn subject(&self) -> Self::Subject;
  fn predicate(&self) -> Self::Predicate;
  fn object(&self) -> Self::Object;
}

pub trait FromRdfTriple {
  fn from_triple(triple: &impl AsRdfTriple) -> Self;
}

pub trait RdfParser {
  type Item: AsRdfStatement;
  type Error: From<std::io::Error> + std::error::Error + Send + Sync + 'static;

  fn next(&mut self, buf: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error>;
  fn end(&mut self, buf: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
    match self.next(buf)? {
      Some(item) => Ok(Some(item)),
      None if buf.is_empty() => Ok(None),
      None => {
        Err(std::io::Error::new(std::io::ErrorKind::Other, "bytes remaining on stream").into())
      }
    }
  }
}

/// Trait implemented by stores that want to
/// accept rdf triples from multiple parsers.
///
/// When dealing with multiple parsers, one has
/// to care about the fact that they all can use
/// the same blank id name, but not reffer to the
/// same blank node. Each sink produced by `as_sink`
/// in this trait, typically has it's own mapping
/// of blank node name => id.
pub trait AsRdfParserSink<'a> {
  type Sink: RdfParserSink + 'a;

  fn as_sink(&'a mut self) -> Self::Sink;
}

pub trait RdfParserSink {
  type Error;

  fn insert(&mut self, item: &impl AsRdfStatement) -> Result<(), Self::Error>;
}

pub trait RdfParserExt: RdfParser + Sized {
  type Iter: Iterator<Item = Result<Self::Item, Self::Error>>;

  fn into_iter(self, data: BytesMut) -> Self::Iter;
}

impl<P: RdfParser> RdfParserExt for P {
  type Iter = ParserIter<P>;

  #[inline]
  fn into_iter(self, data: BytesMut) -> Self::Iter {
    ParserIter {
      parser: self,
      data,
      done: false,
    }
  }
}

pub struct ParserIter<P: RdfParser> {
  parser: P,
  data: BytesMut,
  done: bool,
}

impl<P: RdfParser> Iterator for ParserIter<P> {
  type Item = Result<<P as RdfParser>::Item, <P as RdfParser>::Error>;

  fn next(&mut self) -> Option<Self::Item> {
    if self.done {
      None
    } else {
      match self.parser.end(&mut self.data) {
        Ok(item) => item.map(Ok),
        Err(e) => {
          self.done = true;
          Some(Err(e))
        }
      }
    }
  }
}
