use crate::{
  AsRdfObject, AsRdfPredicate, AsRdfPrefix, AsRdfStatement, AsRdfSubject, AsRdfTriple, Statement,
};
use lazy_static::lazy_static;
use std::sync::Arc;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Never {}

impl AsRdfTriple for Never {
  type Subject = Never;
  type Predicate = Never;
  type Object = Never;

  fn subject(&self) -> Self::Subject {
    unreachable!()
  }
  fn predicate(&self) -> Self::Predicate {
    unreachable!()
  }

  fn object(&self) -> Self::Object {
    unreachable!()
  }
}

impl AsRdfSubject for Never {
  fn as_subject(&self) -> Subject {
    unreachable!()
  }
}

impl AsRdfPredicate for Never {
  fn as_predicate(&self) -> Predicate {
    unreachable!()
  }
}

impl AsRdfObject for Never {
  fn as_object(&self) -> Object {
    unreachable!()
  }
}

macro_rules! display {
  (impl Display for $name:ident {
    fn fmt(&$self:ident, $f:ident: &mut Formatter) -> Result $code:block
  }) => {
    impl ::std::fmt::Display for $name {
      fn fmt($self: &Self, $f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result $code
    }

    impl ::std::fmt::Debug for $name {
      fn fmt($self: &Self, $f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result $code
    }
  };
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Prefix {
  pub prefix: Arc<str>,
  pub uri: Arc<str>,
}

impl Prefix {
  #[inline]
  pub const fn new(prefix: Arc<str>, uri: Arc<str>) -> Self {
    Self { prefix, uri }
  }
}

display! {
  impl Display for Prefix {
    fn fmt(&self, f: &mut Formatter) -> Result {
      write!(f, "@prefix {} <{}> .", &self.prefix, &self.uri)
    }
  }
}

impl AsRdfPrefix for Prefix {
  #[inline]
  fn as_prefix(&self) -> Prefix {
    self.clone()
  }
}

impl AsRdfStatement for Prefix {
  type Prefix = Self;
  type Triple = Never;

  #[inline]
  fn as_statement(&self) -> Statement<Self::Prefix, Self::Triple> {
    Statement::Prefix(self.clone())
  }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum NamedNode {
  Iri(Arc<str>),
  Prefixed(Arc<str>, Arc<str>),
}

impl AsRdfSubject for NamedNode {
  #[inline]
  fn as_subject(&self) -> Subject {
    Subject::named(self.clone())
  }
}

impl AsRdfPredicate for NamedNode {
  #[inline]
  fn as_predicate(&self) -> Predicate {
    Predicate::new(self.clone())
  }
}

impl AsRdfObject for NamedNode {
  #[inline]
  fn as_object(&self) -> Object {
    Object::named(self.clone())
  }
}

impl NamedNode {
  #[inline]
  pub const fn iri(value: Arc<str>) -> Self {
    Self::Iri(value)
  }

  #[inline]
  pub const fn prefixed(namespace: Arc<str>, value: Arc<str>) -> Self {
    Self::Prefixed(namespace, value)
  }
}

display! {
  impl Display for NamedNode {
    fn fmt(&self, f: &mut Formatter) -> Result {
      match self {
        Self::Iri(s) => write!(f, "<{}>", s),
        Self::Prefixed(ns, s) => write!(f, "{}:{}", ns, s),
      }
    }
  }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum BlankNode {
  Named(Arc<str>),
  Anonymous(u32),
}

impl AsRdfSubject for BlankNode {
  #[inline]
  fn as_subject(&self) -> Subject {
    Subject::blank(self.clone())
  }
}

impl AsRdfObject for BlankNode {
  #[inline]
  fn as_object(&self) -> Object {
    Object::blank(self.clone())
  }
}

impl BlankNode {
  #[inline]
  pub const fn new(name: Arc<str>) -> Self {
    Self::Named(name)
  }

  #[inline]
  pub const fn new_anon(id: u32) -> Self {
    Self::Anonymous(id)
  }
}

display! {
  impl Display for BlankNode {
    fn fmt(&self, f: &mut Formatter) -> Result {
      match self {
        Self::Named(n) => write!(f, "_:{}", n),
        Self::Anonymous(n) => write!(f, "_:anon-{}", n),
      }
    }
  }
}

lazy_static! {
  static ref STRING_TYPE: NamedNode =
    NamedNode::iri("http://www.w3.org/2001/XMLSchema#string".into());
  static ref LANG_STRING_TYPE: NamedNode =
    NamedNode::iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#langString".into());
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Literal {
  Simple(Arc<str>),
  Typed(Arc<str>, NamedNode),
  LangTagged(Arc<str>, Arc<str>),
}

impl AsRdfObject for Literal {
  #[inline]
  fn as_object(&self) -> Object {
    Object::literal(self.clone())
  }
}

impl Literal {
  #[inline]
  pub const fn simple(value: Arc<str>) -> Self {
    Self::Simple(value)
  }

  #[inline]
  pub const fn typed(value: Arc<str>, ty: NamedNode) -> Self {
    Self::Typed(value, ty)
  }

  #[inline]
  pub const fn lang_tagged(value: Arc<str>, lang: Arc<str>) -> Self {
    Self::LangTagged(value, lang)
  }

  #[inline]
  pub fn value(&self) -> &str {
    match self {
      Self::Simple(v) => &v,
      Self::Typed(v, _) => &v,
      Self::LangTagged(v, _) => &v,
    }
  }

  #[inline]
  pub fn datatype(&self) -> NamedNode {
    match self {
      Self::Simple(_) => STRING_TYPE.clone(),
      Self::Typed(_, t) => t.clone(),
      Self::LangTagged(_, _) => LANG_STRING_TYPE.clone(),
    }
  }

  #[inline]
  pub fn lang(&self) -> Option<&str> {
    match self {
      Self::Simple(_) => None,
      Self::Typed(_, _) => None,
      Self::LangTagged(_, l) => Some(&l),
    }
  }
}

display! {
  impl Display for Literal {
    fn fmt(&self, f: &mut Formatter) -> Result {
      match self {
        Self::Simple(s) => write!(f, "\"{}\"", s),
        Self::Typed(s, t) => write!(f, "\"{}\"^^{}", s, t),
        Self::LangTagged(s, l) => write!(f, "\"{}\"@{}", s, l),
      }
    }
  }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Subject {
  Named(NamedNode),
  Blank(BlankNode),
}

impl AsRdfSubject for Subject {
  #[inline]
  fn as_subject(&self) -> Subject {
    self.clone()
  }
}

impl Subject {
  #[inline]
  pub const fn named(n: NamedNode) -> Self {
    Self::Named(n)
  }

  #[inline]
  pub const fn blank(n: BlankNode) -> Self {
    Self::Blank(n)
  }

  #[inline]
  pub fn is_named(&self) -> bool {
    match &self {
      Self::Named(_) => true,
      Self::Blank(_) => false,
    }
  }

  #[inline]
  pub fn is_blank(&self) -> bool {
    match &self {
      Self::Named(_) => false,
      Self::Blank(_) => true,
    }
  }

  #[inline]
  pub fn as_named(&self) -> Option<NamedNode> {
    match &self {
      Self::Named(n) => Some(n.clone()),
      Self::Blank(_) => None,
    }
  }

  #[inline]
  pub fn as_blank(&self) -> Option<BlankNode> {
    match &self {
      Self::Named(_) => None,
      Self::Blank(n) => Some(n.clone()),
    }
  }
}

impl From<NamedNode> for Subject {
  #[inline]
  fn from(n: NamedNode) -> Self {
    Self::Named(n)
  }
}

impl From<BlankNode> for Subject {
  #[inline]
  fn from(n: BlankNode) -> Self {
    Self::Blank(n)
  }
}

display! {
  impl Display for Subject {
    fn fmt(&self, f: &mut Formatter) -> Result {
      match self {
        Self::Named(n) => std::fmt::Display::fmt(n, f),
        Self::Blank(n) => std::fmt::Display::fmt(n, f),
      }
    }
  }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Predicate(pub NamedNode);

impl AsRdfPredicate for Predicate {
  #[inline]
  fn as_predicate(&self) -> Predicate {
    self.clone()
  }
}

impl Predicate {
  #[inline]
  pub const fn new(n: NamedNode) -> Self {
    Self(n)
  }
}

impl From<NamedNode> for Predicate {
  #[inline]
  fn from(n: NamedNode) -> Self {
    Self(n)
  }
}

display! {
  impl Display for Predicate {
    fn fmt(&self, f: &mut Formatter) -> Result {
      std::fmt::Display::fmt(&self.0, f)
    }
  }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Object {
  Named(NamedNode),
  Blank(BlankNode),
  Literal(Literal),
}

impl AsRdfObject for Object {
  #[inline]
  fn as_object(&self) -> Object {
    self.clone()
  }
}

impl Object {
  #[inline]
  pub const fn named(n: NamedNode) -> Self {
    Self::Named(n)
  }

  #[inline]
  pub const fn blank(n: BlankNode) -> Self {
    Self::Blank(n)
  }

  #[inline]
  pub const fn literal(n: Literal) -> Self {
    Self::Literal(n)
  }
}

impl From<NamedNode> for Object {
  #[inline]
  fn from(n: NamedNode) -> Self {
    Self::Named(n)
  }
}

impl From<BlankNode> for Object {
  #[inline]
  fn from(n: BlankNode) -> Self {
    Self::Blank(n)
  }
}

impl From<Literal> for Object {
  #[inline]
  fn from(n: Literal) -> Self {
    Self::Literal(n)
  }
}

display! {
  impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> Result {
      match self {
        Self::Named(n) => std::fmt::Display::fmt(n, f),
        Self::Blank(n) => std::fmt::Display::fmt(n, f),
        Self::Literal(n) => std::fmt::Display::fmt(n, f),
      }
    }
  }
}
