pub mod display;

use std::num::NonZeroU32;
use std::sync::Arc;

pub const STRING_TYPE: &str = "http://www.w3.org/2001/XMLSchema#string";
pub const LANG_STRING_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString";

/// Represents a named RDF node.
pub trait NamedNode: Clone {
  /// Get's the iri of the node.
  fn iri(&self) -> &str;

  #[inline]
  fn iri_arc(&self) -> Arc<str> {
    self.iri().into()
  }
}

impl NamedNode for ! {
  #[inline]
  fn iri(&self) -> &str {
    unreachable!()
  }
}

/// Represents a blank RDF node.
pub trait BlankNode: Clone {
  /// Get's the blank node identifier.
  fn id(&self) -> NonZeroU32;
}

impl BlankNode for ! {
  #[inline]
  fn id(&self) -> NonZeroU32 {
    unreachable!()
  }
}

#[derive(Clone, Copy)]
pub enum RdfSubject<N, B>
where
  N: NamedNode,
  B: BlankNode,
{
  Named(N),
  Blank(B),
}

pub trait Subject: Clone {
  type Named: NamedNode;
  type Blank: BlankNode;

  fn get(&self) -> RdfSubject<Self::Named, Self::Blank>;

  fn as_named(&self) -> Option<Self::Named> {
    match self.get() {
      RdfSubject::Named(n) => Some(n),
      RdfSubject::Blank(_) => None,
    }
  }

  fn as_blank(&self) -> Option<Self::Blank> {
    match self.get() {
      RdfSubject::Named(_) => None,
      RdfSubject::Blank(n) => Some(n),
    }
  }
}

pub trait Literal: Clone {
  type Type: NamedNode;

  fn value(&self) -> &str;

  #[inline]
  fn value_arc(&self) -> Arc<str> {
    self.value().into()
  }

  fn ty(&self) -> Self::Type;

  fn lang(&self) -> Option<&str>;

  #[inline]
  fn lang_arc(&self) -> Option<Arc<str>> {
    self.lang().map(Into::into)
  }
}

impl Literal for ! {
  type Type = !;

  #[inline]
  fn value(&self) -> &str {
    unreachable!()
  }

  #[inline]
  fn ty(&self) -> Self::Type {
    unreachable!()
  }
  #[inline]
  fn lang(&self) -> Option<&str> {
    unreachable!()
  }
}

#[derive(Clone, Copy)]
pub enum RdfObject<N, B, O>
where
  N: NamedNode,
  B: BlankNode,
  O: Literal,
{
  Named(N),
  Blank(B),
  Literal(O),
}

pub trait Object: Clone {
  type Named: NamedNode;
  type Blank: BlankNode;
  type Literal: Literal;

  fn get(&self) -> RdfObject<Self::Named, Self::Blank, Self::Literal>;

  fn as_named(&self) -> Option<Self::Named> {
    match self.get() {
      RdfObject::Named(n) => Some(n),
      RdfObject::Blank(_) => None,
      RdfObject::Literal(_) => None,
    }
  }

  fn as_blank(&self) -> Option<Self::Blank> {
    match self.get() {
      RdfObject::Named(_) => None,
      RdfObject::Blank(n) => Some(n),
      RdfObject::Literal(_) => None,
    }
  }

  fn as_literal(&self) -> Option<Self::Literal> {
    match self.get() {
      RdfObject::Named(_) => None,
      RdfObject::Blank(_) => None,
      RdfObject::Literal(n) => Some(n),
    }
  }
}

pub trait Predicate: Clone {
  type Named: NamedNode;

  fn get(&self) -> Self::Named;
}

pub trait Triple: Clone {
  type Subject: Subject;
  type Predicate: Predicate;
  type Object: Object;

  fn subj(&self) -> Self::Subject;
  fn pred(&self) -> Self::Predicate;
  fn obj(&self) -> Self::Object;
}
