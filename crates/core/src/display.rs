use crate::*;
use std::fmt::{Formatter, Result};

pub fn named_node(node: &impl NamedNode, f: &mut Formatter) -> Result {
  write!(f, "<{}>", node.iri())
}

pub fn blank_node(node: &impl BlankNode, f: &mut Formatter) -> Result {
  write!(f, "_:anon-{}", node.id())
}

pub fn literal(node: &impl Literal, f: &mut Formatter) -> Result {
  match (node.lang(), node.ty().iri()) {
    (Some(l), _) => write!(f, "\"{}\"@{}", node.value(), l),
    (None, t) if t != STRING_TYPE => write!(f, "\"{}\"^^{}", node.value(), t),
    _ => write!(f, "\"{}\"", node.value()),
  }
}

pub fn subject(node: &impl Subject, f: &mut Formatter) -> Result {
  match node.get() {
    RdfSubject::Named(n) => named_node(&n, f),
    RdfSubject::Blank(n) => blank_node(&n, f),
  }
}

pub fn predicate(node: &impl Predicate, f: &mut Formatter) -> Result {
  named_node(&node.get(), f)
}

pub fn object(node: &impl Object, f: &mut Formatter) -> Result {
  match node.get() {
    RdfObject::Named(n) => named_node(&n, f),
    RdfObject::Blank(n) => blank_node(&n, f),
    RdfObject::Literal(n) => literal(&n, f),
  }
}

pub fn triple(node: &impl Triple, f: &mut Formatter) -> Result {
  subject(&node.subj(), f)?;
  f.write_str(" ")?;
  predicate(&node.pred(), f)?;
  f.write_str(" ")?;
  object(&node.obj(), f)?;
  f.write_str(" .")?;

  Ok(())
}
