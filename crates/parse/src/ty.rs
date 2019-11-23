use crate::{
  AsRdfObject, AsRdfPredicate, AsRdfPrefix, AsRdfStatement, AsRdfSubject, AsRdfTriple, Statement,
};

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
  (impl<'a> Display for $name:ident<'a> {
    fn fmt(&$self:ident, $f:ident: &mut Formatter) -> Result $code:block
  }) => {
    impl<'a> ::std::fmt::Display for $name<'a> {
      fn fmt($self: &Self, $f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result $code
    }

    impl<'a> ::std::fmt::Debug for $name<'a> {
      fn fmt($self: &Self, $f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result $code
    }
  };
}

#[derive(Clone, Copy)]
pub struct Prefix<'a> {
  pub prefix: &'a str,
  pub uri: &'a str,
}

impl<'a> Prefix<'a> {
  #[inline]
  pub const fn new(prefix: &'a str, uri: &'a str) -> Self {
    Self { prefix, uri }
  }
}

display! {
  impl<'a> Display for Prefix<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
      write!(f, "@prefix {} <{}> .", &self.prefix, &self.uri)
    }
  }
}

impl<'a> AsRdfPrefix for Prefix<'a> {
  #[inline]
  fn as_prefix(&self) -> Prefix {
    *self
  }
}

impl<'a> AsRdfStatement for Prefix<'a> {
  type Prefix = Self;
  type Triple = Never;

  #[inline]
  fn as_statement<'s>(&'s self) -> Statement<'s, Self::Prefix, Self::Triple> {
    Statement::Prefix(self)
  }
}

#[derive(Clone, Copy)]
pub enum NamedNode<'a> {
  Iri(&'a str),
  Namespaced(&'a str, &'a str),
}

impl<'a> AsRdfSubject for NamedNode<'a> {
  #[inline]
  fn as_subject(&self) -> Subject {
    Subject::named(*self)
  }
}

impl<'a> AsRdfPredicate for NamedNode<'a> {
  #[inline]
  fn as_predicate(&self) -> Predicate {
    Predicate::new(*self)
  }
}

impl<'a> AsRdfObject for NamedNode<'a> {
  #[inline]
  fn as_object(&self) -> Object {
    Object::named(*self)
  }
}

impl<'a> NamedNode<'a> {
  #[inline]
  pub const fn iri(value: &'a str) -> Self {
    Self::Iri(value)
  }

  #[inline]
  pub const fn namespaced(namespace: &'a str, value: &'a str) -> Self {
    Self::Namespaced(namespace, value)
  }

  #[inline]
  pub fn is_iri(&self) -> bool {
    match self {
      Self::Iri(_) => true,
      Self::Namespaced(_, _) => false,
    }
  }

  #[inline]
  pub fn is_namespaced(&self) -> bool {
    match self {
      Self::Iri(_) => true,
      Self::Namespaced(_, _) => false,
    }
  }

  #[inline]
  pub fn as_iri(&self) -> Option<&'a str> {
    match self {
      Self::Iri(s) => Some(s),
      Self::Namespaced(_, _) => None,
    }
  }

  #[inline]
  pub fn as_namespaced(&self) -> Option<(&'a str, &'a str)> {
    match self {
      Self::Iri(_) => None,
      Self::Namespaced(ns, s) => Some((ns, s)),
    }
  }
}

display! {
  impl<'a> Display for NamedNode<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
      match self {
        Self::Iri(s) => write!(f, "<{}>", s),
        Self::Namespaced(ns, s) => write!(f, "{}:{}", ns, s),
      }
    }
  }
}

#[derive(Clone, Copy)]
pub struct BlankNode<'a>(pub &'a str);

impl<'a> AsRdfSubject for BlankNode<'a> {
  #[inline]
  fn as_subject(&self) -> Subject {
    Subject::blank(*self)
  }
}

impl<'a> AsRdfObject for BlankNode<'a> {
  #[inline]
  fn as_object(&self) -> Object {
    Object::blank(*self)
  }
}

impl<'a> BlankNode<'a> {
  #[inline]
  pub fn new(name: &'a str) -> Self {
    Self(name)
  }
}

display! {
  impl<'a> Display for BlankNode<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
      write!(f, "_:{}", &self.0)
    }
  }
}

const STRING_TYPE: NamedNode<'static> = NamedNode::iri("http://www.w3.org/2001/XMLSchema#string");
const LANG_STRING_TYPE: NamedNode<'static> =
  NamedNode::iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#langString");

#[derive(Clone, Copy)]
pub enum Literal<'a> {
  Simple(&'a str),
  Typed(&'a str, NamedNode<'a>),
  LangTagged(&'a str, &'a str),
}

impl<'a> AsRdfObject for Literal<'a> {
  #[inline]
  fn as_object(&self) -> Object {
    Object::literal(*self)
  }
}

impl<'a> Literal<'a> {
  #[inline]
  pub const fn simple(value: &'a str) -> Self {
    Self::Simple(value)
  }

  #[inline]
  pub const fn typed(value: &'a str, ty: NamedNode<'a>) -> Self {
    Self::Typed(value, ty)
  }

  #[inline]
  pub const fn lang_tagged(value: &'a str, lang: &'a str) -> Self {
    Self::LangTagged(value, lang)
  }

  #[inline]
  pub fn value(&self) -> &'a str {
    match self {
      Self::Simple(v) => v,
      Self::Typed(v, _) => v,
      Self::LangTagged(v, _) => v,
    }
  }

  #[inline]
  pub fn datatype(&self) -> NamedNode<'a> {
    match self {
      Self::Simple(_) => STRING_TYPE,
      Self::Typed(_, t) => *t,
      Self::LangTagged(_, _) => LANG_STRING_TYPE,
    }
  }

  #[inline]
  pub fn lang(&self) -> Option<&'a str> {
    match self {
      Self::Simple(_) => None,
      Self::Typed(_, _) => None,
      Self::LangTagged(_, l) => Some(l),
    }
  }

  #[inline]
  pub fn is_simple(&self) -> bool {
    match self {
      Self::Simple(_) => true,
      Self::Typed(_, _) => false,
      Self::LangTagged(_, _) => false,
    }
  }

  #[inline]
  pub fn is_typed(&self) -> bool {
    match self {
      Self::Simple(_) => false,
      Self::Typed(_, _) => true,
      Self::LangTagged(_, _) => false,
    }
  }

  #[inline]
  pub fn is_lang_tagged(&self) -> bool {
    match self {
      Self::Simple(_) => false,
      Self::Typed(_, _) => false,
      Self::LangTagged(_, _) => true,
    }
  }

  #[inline]
  pub fn as_simple(&self) -> Option<&'a str> {
    match self {
      Self::Simple(v) => Some(v),
      Self::Typed(_, _) => None,
      Self::LangTagged(_, _) => None,
    }
  }

  #[inline]
  pub fn as_typed(&self) -> Option<(&'a str, NamedNode<'a>)> {
    match self {
      Self::Simple(_) => None,
      Self::Typed(v, t) => Some((v, *t)),
      Self::LangTagged(_, _) => None,
    }
  }

  #[inline]
  pub fn as_lang_tagged(&self) -> Option<(&'a str, &'a str)> {
    match self {
      Self::Simple(_) => None,
      Self::Typed(_, _) => None,
      Self::LangTagged(v, l) => Some((v, l)),
    }
  }
}

display! {
  impl<'a> Display for Literal<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
      match self {
        Self::Simple(s) => write!(f, "\"{}\"", s),
        Self::Typed(s, t) => write!(f, "\"{}\"^^{}", s, t),
        Self::LangTagged(s, l) => write!(f, "\"{}\"@{}", s, l),
      }
    }
  }
}

#[derive(Clone, Copy)]
pub enum Subject<'a> {
  Named(NamedNode<'a>),
  Blank(BlankNode<'a>),
}

impl<'a> AsRdfSubject for Subject<'a> {
  #[inline]
  fn as_subject(&self) -> Subject {
    *self
  }
}

impl<'a> Subject<'a> {
  #[inline]
  pub const fn named(n: NamedNode<'a>) -> Self {
    Self::Named(n)
  }

  #[inline]
  pub const fn blank(n: BlankNode<'a>) -> Self {
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
  pub fn as_named(&self) -> Option<NamedNode<'a>> {
    match &self {
      Self::Named(n) => Some(*n),
      Self::Blank(_) => None,
    }
  }

  #[inline]
  pub fn as_blank(&self) -> Option<BlankNode<'a>> {
    match &self {
      Self::Named(_) => None,
      Self::Blank(n) => Some(*n),
    }
  }
}

impl<'a> From<NamedNode<'a>> for Subject<'a> {
  #[inline]
  fn from(n: NamedNode<'a>) -> Self {
    Self::Named(n)
  }
}

impl<'a> From<BlankNode<'a>> for Subject<'a> {
  #[inline]
  fn from(n: BlankNode<'a>) -> Self {
    Self::Blank(n)
  }
}

display! {
  impl<'a> Display for Subject<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
      match self {
        Self::Named(n) => std::fmt::Display::fmt(n, f),
        Self::Blank(n) => std::fmt::Display::fmt(n, f),
      }
    }
  }
}

#[derive(Clone, Copy)]
pub struct Predicate<'a>(pub NamedNode<'a>);

impl<'a> AsRdfPredicate for Predicate<'a> {
  #[inline]
  fn as_predicate(&self) -> Predicate {
    *self
  }
}

impl<'a> Predicate<'a> {
  #[inline]
  pub const fn new(n: NamedNode<'a>) -> Self {
    Self(n)
  }
}

impl<'a> From<NamedNode<'a>> for Predicate<'a> {
  #[inline]
  fn from(n: NamedNode<'a>) -> Self {
    Self(n)
  }
}

display! {
  impl<'a> Display for Predicate<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
      std::fmt::Display::fmt(&self.0, f)
    }
  }
}

#[derive(Clone, Copy)]
pub enum Object<'a> {
  Named(NamedNode<'a>),
  Blank(BlankNode<'a>),
  Literal(Literal<'a>),
}

impl<'a> AsRdfObject for Object<'a> {
  #[inline]
  fn as_object(&self) -> Object {
    *self
  }
}

impl<'a> Object<'a> {
  #[inline]
  pub const fn named(n: NamedNode<'a>) -> Self {
    Self::Named(n)
  }

  #[inline]
  pub const fn blank(n: BlankNode<'a>) -> Self {
    Self::Blank(n)
  }

  #[inline]
  pub const fn literal(n: Literal<'a>) -> Self {
    Self::Literal(n)
  }

  #[inline]
  pub fn is_named(&self) -> bool {
    match self {
      Self::Named(_) => true,
      Self::Blank(_) => false,
      Self::Literal(_) => false,
    }
  }

  #[inline]
  pub fn is_blank(&self) -> bool {
    match self {
      Self::Named(_) => false,
      Self::Blank(_) => true,
      Self::Literal(_) => false,
    }
  }

  #[inline]
  pub fn is_literal(&self) -> bool {
    match self {
      Self::Named(_) => false,
      Self::Blank(_) => false,
      Self::Literal(_) => true,
    }
  }

  #[inline]
  pub fn as_named(&self) -> Option<NamedNode<'a>> {
    match self {
      Self::Named(n) => Some(*n),
      Self::Blank(_) => None,
      Self::Literal(_) => None,
    }
  }

  #[inline]
  pub fn as_blank(&self) -> Option<BlankNode<'a>> {
    match self {
      Self::Named(_) => None,
      Self::Blank(n) => Some(*n),
      Self::Literal(_) => None,
    }
  }

  #[inline]
  pub fn as_literal(&self) -> Option<Literal<'a>> {
    match self {
      Self::Named(_) => None,
      Self::Blank(_) => None,
      Self::Literal(n) => Some(*n),
    }
  }
}

impl<'a> From<NamedNode<'a>> for Object<'a> {
  #[inline]
  fn from(n: NamedNode<'a>) -> Self {
    Self::Named(n)
  }
}

impl<'a> From<BlankNode<'a>> for Object<'a> {
  #[inline]
  fn from(n: BlankNode<'a>) -> Self {
    Self::Blank(n)
  }
}

impl<'a> From<Literal<'a>> for Object<'a> {
  #[inline]
  fn from(n: Literal<'a>) -> Self {
    Self::Literal(n)
  }
}

display! {
  impl<'a> Display for Object<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
      match self {
        Self::Named(n) => std::fmt::Display::fmt(n, f),
        Self::Blank(n) => std::fmt::Display::fmt(n, f),
        Self::Literal(n) => std::fmt::Display::fmt(n, f),
      }
    }
  }
}
