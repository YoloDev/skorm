mod ty;

pub use bytes::BytesMut;
pub use ty::*;

pub enum Statement<'a, P, T>
where
  P: AsRdfPrefix,
  T: AsRdfTriple,
{
  Prefix(&'a P),
  Triple(&'a T),
}

impl<'a, P, T> Clone for Statement<'a, P, T>
where
  P: AsRdfPrefix,
  T: AsRdfTriple,
{
  #[inline]
  fn clone(&self) -> Self {
    *self
  }
}

impl<'a, P, T> Copy for Statement<'a, P, T>
where
  P: AsRdfPrefix,
  T: AsRdfTriple,
{
}

impl<'s, P, T> AsRdfStatement for Statement<'s, P, T>
where
  P: AsRdfPrefix,
  T: AsRdfTriple,
{
  type Prefix = P;
  type Triple = T;

  #[inline]
  fn as_statement<'a>(&'a self) -> Statement<'a, P, T> {
    *self
  }
}

pub trait AsRdfStatement {
  type Prefix: AsRdfPrefix;
  type Triple: AsRdfTriple;

  fn as_statement<'a>(&'a self) -> Statement<'a, Self::Prefix, Self::Triple>;
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
