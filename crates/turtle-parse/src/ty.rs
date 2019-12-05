use crate::error::*;
use rust_decimal::Decimal as d128;
use skorm_parse::{
  AsRdfObject, AsRdfPredicate, AsRdfPrefix, AsRdfStatement, AsRdfSubject, AsRdfTriple,
  BlankNode as RdfBlankNode, Literal as RdfLiteral, NamedNode as RdfNamedNode, Object as RdfObject,
  Predicate as RdfPredicate, Prefix as RdfPrefix, Statement as RdfStatement, Subject as RdfSubject,
};
use std::sync::Arc;
use url::Url;

pub(crate) type RcString = Arc<str>;

pub(crate) trait ToAbsolute: Sized {
  fn to_absolute(self, base: &Url) -> Result<Self>;
}

impl<T: ToAbsolute> ToAbsolute for Option<T> {
  fn to_absolute(self, base: &Url) -> Result<Self> {
    Ok(match self {
      Some(inner) => Some(inner.to_absolute(base)?),
      None => None,
    })
  }
}

impl ToAbsolute for String {
  fn to_absolute(self, base: &Url) -> Result<String> {
    let abs = base.join(&self)?;
    let s = abs.as_ref().to_owned();
    Ok(s)
  }
}

impl ToAbsolute for RcString {
  #[inline]
  fn to_absolute(self, base: &Url) -> Result<RcString> {
    self.as_ref().to_owned().to_absolute(base).map(Into::into)
  }
}

#[derive(Clone, Debug)]
pub enum NamedNode {
  Namespaced(RcString, RcString),
  Iri(RcString),
}

impl NamedNode {
  fn as_rdf(&self) -> RdfNamedNode {
    match self {
      NamedNode::Namespaced(ns, n) => RdfNamedNode::prefixed(ns.clone(), n.clone()),
      NamedNode::Iri(n) => RdfNamedNode::iri(n.clone()),
    }
  }

  pub(crate) fn iri(value: impl Into<RcString>) -> Self {
    Self::Iri(value.into())
  }

  pub(crate) fn namespaced(value: (impl Into<RcString>, impl Into<RcString>)) -> Self {
    Self::Namespaced(value.0.into(), value.1.into())
  }
}

impl ToAbsolute for NamedNode {
  fn to_absolute(self, base: &Url) -> Result<Self> {
    Ok(match self {
      NamedNode::Iri(n) => NamedNode::Iri(n.to_absolute(base)?),
      n => n,
    })
  }
}

#[derive(Clone, Debug)]
pub enum BlankNode {
  Named(RcString),
  Anon(u16, RcString),
}

impl BlankNode {
  pub(crate) fn named(name: impl Into<RcString>) -> Self {
    BlankNode::Named(name.into())
  }

  pub(crate) fn anon(id: u16) -> Self {
    BlankNode::Anon(id, format!("anon-{}", id).into())
  }

  fn as_rdf(&self) -> RdfBlankNode {
    match self {
      Self::Named(n) => RdfBlankNode::new(n.clone()),
      Self::Anon(n, _) => RdfBlankNode::new_anon(*n as u32),
    }
  }
}

impl ToAbsolute for BlankNode {
  fn to_absolute(self, _base: &Url) -> Result<Self> {
    Ok(self)
  }
}

#[derive(Clone, Debug)]
pub enum Literal {
  Simple(RcString),
  Typed(RcString, NamedNode),
  LangTagged(RcString, RcString),
  Double(f64),
  Decimal(d128),
  Integer(i64),
  Boolean(bool),
}

impl Literal {
  fn as_rdf(&self) -> RdfLiteral {
    match self {
      Literal::Simple(n) => RdfLiteral::simple(n.clone()),
      Literal::Typed(n, t) => RdfLiteral::typed(n.clone(), t.as_rdf()),
      Literal::LangTagged(n, l) => RdfLiteral::lang_tagged(n.clone(), l.clone()),
      _ => unimplemented!(),
    }
  }

  pub(crate) fn simple(value: impl Into<RcString>) -> Self {
    Self::Simple(value.into())
  }

  pub(crate) fn typed(value: (impl Into<RcString>, NamedNode)) -> Self {
    Self::Typed(value.0.into(), value.1)
  }

  pub(crate) fn lang_tagged(value: (impl Into<RcString>, impl Into<RcString>)) -> Self {
    Self::LangTagged(value.0.into(), value.1.into())
  }

  pub(crate) fn double(value: f64) -> Self {
    Self::Double(value)
  }

  pub(crate) fn decimal(value: d128) -> Self {
    Self::Decimal(value)
  }

  pub(crate) fn integer(value: i64) -> Self {
    Self::Integer(value)
  }

  pub(crate) fn boolean(value: bool) -> Self {
    Self::Boolean(value)
  }
}

impl ToAbsolute for Literal {
  fn to_absolute(self, base: &Url) -> Result<Self> {
    Ok(match self {
      Literal::Typed(s, n) => Literal::Typed(s, n.to_absolute(base)?),
      lit => lit,
    })
  }
}

#[derive(Clone, Debug)]
pub enum Subject {
  Named(NamedNode),
  Blank(BlankNode),
}

impl From<NamedNode> for Subject {
  #[inline]
  fn from(inner: NamedNode) -> Self {
    Self::Named(inner)
  }
}

impl From<BlankNode> for Subject {
  #[inline]
  fn from(inner: BlankNode) -> Self {
    Self::Blank(inner)
  }
}

impl ToAbsolute for Subject {
  fn to_absolute(self, base: &Url) -> Result<Self> {
    Ok(match self {
      Subject::Named(n) => Subject::Named(n.to_absolute(base)?),
      Subject::Blank(n) => Subject::Blank(n.to_absolute(base)?),
    })
  }
}

impl AsRdfSubject for Subject {
  fn as_subject(&self) -> RdfSubject {
    match self {
      Subject::Named(n) => n.as_rdf().into(),
      Subject::Blank(n) => n.as_rdf().into(),
    }
  }
}

#[derive(Clone, Debug)]
pub struct Predicate(NamedNode);

impl From<NamedNode> for Predicate {
  #[inline]
  fn from(inner: NamedNode) -> Self {
    Self(inner)
  }
}

impl ToAbsolute for Predicate {
  fn to_absolute(self, base: &Url) -> Result<Self> {
    Ok(Self(self.0.to_absolute(base)?))
  }
}

impl AsRdfPredicate for Predicate {
  fn as_predicate(&self) -> RdfPredicate {
    self.0.as_rdf().into()
  }
}

#[derive(Clone, Debug)]
pub enum Object {
  Named(NamedNode),
  Blank(BlankNode),
  Literal(Literal),
}

impl From<NamedNode> for Object {
  #[inline]
  fn from(value: NamedNode) -> Self {
    Object::Named(value)
  }
}

impl From<BlankNode> for Object {
  #[inline]
  fn from(value: BlankNode) -> Self {
    Object::Blank(value)
  }
}

impl From<Literal> for Object {
  #[inline]
  fn from(value: Literal) -> Self {
    Object::Literal(value)
  }
}

impl ToAbsolute for Object {
  fn to_absolute(self, base: &Url) -> Result<Self> {
    Ok(match self {
      Object::Named(n) => Object::Named(n.to_absolute(base)?),
      Object::Blank(n) => Object::Blank(n.to_absolute(base)?),
      Object::Literal(n) => Object::Literal(n.to_absolute(base)?),
    })
  }
}

impl AsRdfObject for Object {
  fn as_object(&self) -> RdfObject {
    match self {
      Object::Named(n) => n.as_rdf().into(),
      Object::Blank(n) => n.as_rdf().into(),
      Object::Literal(n) => n.as_rdf().into(),
    }
  }
}

#[derive(Clone, Debug)]
pub struct Triple {
  subject: Subject,
  predicate: Predicate,
  object: Object,
}

impl Triple {
  #[inline]
  pub(crate) fn new(
    subject: impl Into<Subject>,
    predicate: impl Into<Predicate>,
    object: impl Into<Object>,
  ) -> Self {
    Self {
      subject: subject.into(),
      predicate: predicate.into(),
      object: object.into(),
    }
  }
}

impl ToAbsolute for Triple {
  fn to_absolute(self, base: &Url) -> Result<Self> {
    Ok(Self {
      subject: self.subject.to_absolute(base)?,
      predicate: self.predicate.to_absolute(base)?,
      object: self.object.to_absolute(base)?,
    })
  }
}

impl AsRdfTriple for Triple {
  type Subject = Subject;
  type Predicate = Predicate;
  type Object = Object;

  #[inline]
  fn subject(&self) -> Self::Subject {
    self.subject.clone()
  }

  #[inline]
  fn predicate(&self) -> Self::Predicate {
    self.predicate.clone()
  }

  #[inline]
  fn object(&self) -> Self::Object {
    self.object.clone()
  }
}

#[derive(Clone, Debug)]
pub struct Prefix(RcString, RcString);

impl Prefix {
  #[inline]
  pub(crate) fn new(prefix: impl Into<RcString>, uri: impl Into<RcString>) -> Self {
    Self(prefix.into(), uri.into())
  }
}

impl ToAbsolute for Prefix {
  fn to_absolute(self, base: &Url) -> Result<Self> {
    Ok(Self(self.0, self.1.to_absolute(base)?))
  }
}

impl AsRdfPrefix for Prefix {
  fn as_prefix(&self) -> RdfPrefix {
    RdfPrefix::new(self.0.clone(), self.1.clone())
  }
}

#[derive(Clone, Debug)]
pub enum Statement {
  Base(RcString),
  Prefix(Prefix),
  Triple(Triple),
}

impl Statement {
  #[inline]
  pub(crate) fn base(url: impl Into<RcString>) -> Self {
    Self::Base(url.into())
  }
}

impl From<Prefix> for Statement {
  #[inline]
  fn from(prefix: Prefix) -> Self {
    Self::Prefix(prefix)
  }
}

impl From<Triple> for Statement {
  #[inline]
  fn from(triple: Triple) -> Self {
    Self::Triple(triple)
  }
}

impl ToAbsolute for Statement {
  fn to_absolute(self, base: &Url) -> Result<Self> {
    Ok(match self {
      Statement::Base(u) => Statement::Base(u),
      Statement::Prefix(p) => Statement::Prefix(p.to_absolute(base)?),
      Statement::Triple(t) => Statement::Triple(t.to_absolute(base)?),
    })
  }
}

impl AsRdfStatement for Statement {
  type Prefix = Prefix;
  type Triple = Triple;

  fn as_statement(&self) -> RdfStatement<Prefix, Triple> {
    match self {
      Statement::Base(_) => unreachable!(),
      Statement::Prefix(p) => RdfStatement::Prefix(p.clone()),
      Statement::Triple(t) => RdfStatement::Triple(t.clone()),
    }
  }
}
