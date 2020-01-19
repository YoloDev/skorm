#![feature(custom_test_frameworks)]
#![test_runner(datatest::runner)]

use bytes::BytesMut;
use glob::*;
use serde::{Deserialize, Serialize};
use skorm_parse::*;
use skorm_turtle_parse::*;
use std::fs::{create_dir_all, read, write};
use std::path::Path;
use std::sync::Arc;
use url::Url;

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Prefix {
  prefix: Arc<str>,
  url: Arc<str>,
}

impl FromRdfPrefix for Prefix {
  fn from_prefix(prefix: &impl skorm_parse::AsRdfPrefix) -> Self {
    let prefix = prefix.as_prefix();
    Self {
      prefix: prefix.prefix.to_owned(),
      url: prefix.uri.to_owned(),
    }
  }
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum NamedNode {
  Iri(Arc<str>),
  Namespaced(Arc<str>, Arc<str>),
}

impl<'a> From<skorm_parse::NamedNode> for NamedNode {
  fn from(n: skorm_parse::NamedNode) -> Self {
    match n {
      skorm_parse::NamedNode::Iri(i) => Self::Iri(i.to_owned()),
      skorm_parse::NamedNode::Prefixed(n, i) => Self::Namespaced(n.to_owned(), i.to_owned()),
    }
  }
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum BlankNode {
  Named(Arc<str>),
  Anonymous(u32),
}

impl<'a> From<skorm_parse::BlankNode> for BlankNode {
  fn from(n: skorm_parse::BlankNode) -> Self {
    match n {
      skorm_parse::BlankNode::Named(n) => BlankNode::Named(n),
      skorm_parse::BlankNode::Anonymous(n) => BlankNode::Anonymous(n),
    }
  }
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum Literal {
  Simple(Arc<str>),
  Typed(Arc<str>, NamedNode),
  LangTagged(Arc<str>, Arc<str>),
}

impl<'a> From<skorm_parse::Literal> for Literal {
  fn from(lit: skorm_parse::Literal) -> Self {
    match lit {
      skorm_parse::Literal::Simple(s) => Literal::Simple(s.to_owned()),
      skorm_parse::Literal::Typed(s, n) => Literal::Typed(s.to_owned(), n.into()),
      skorm_parse::Literal::LangTagged(s, t) => Literal::LangTagged(s.to_owned(), t.to_owned()),
    }
  }
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum Subject {
  Named(NamedNode),
  Blank(BlankNode),
}

impl FromRdfSubject for Subject {
  fn from_subject(subject: &impl skorm_parse::AsRdfSubject) -> Self {
    let subject = subject.as_subject();
    match subject {
      skorm_parse::Subject::Named(n) => Self::Named(NamedNode::from(n)),
      skorm_parse::Subject::Blank(n) => Self::Blank(BlankNode::from(n)),
    }
  }
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum Predicate {
  Named(NamedNode),
}

impl FromRdfPredicate for Predicate {
  fn from_predicate(predicate: &impl skorm_parse::AsRdfPredicate) -> Self {
    let predicate = predicate.as_predicate();
    Predicate::Named(NamedNode::from(predicate.0))
  }
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum Object {
  Named(NamedNode),
  Blank(BlankNode),
  Literal(Literal),
}

impl FromRdfObject for Object {
  fn from_object(object: &impl skorm_parse::AsRdfObject) -> Self {
    match object.as_object() {
      skorm_parse::Object::Named(n) => Object::Named(NamedNode::from(n)),
      skorm_parse::Object::Blank(n) => Object::Blank(BlankNode::from(n)),
      skorm_parse::Object::Literal(n) => Object::Literal(Literal::from(n)),
    }
  }
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Triple {
  subject: Subject,
  predicate: Predicate,
  object: Object,
}

impl FromRdfTriple for Triple {
  fn from_triple(triple: &impl AsRdfTriple) -> Self {
    Triple {
      subject: Subject::from_subject(&triple.subject()),
      predicate: Predicate::from_predicate(&triple.predicate()),
      object: Object::from_object(&triple.object()),
    }
  }
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum Statement {
  Triple(Triple),
  Prefix(Prefix),
}

impl FromRdfStatement for Statement {
  fn from_statement(statement: &impl AsRdfStatement) -> Self {
    match statement.as_statement() {
      skorm_parse::Statement::Prefix(p) => Statement::Prefix(Prefix::from_prefix(&p)),
      skorm_parse::Statement::Triple(t) => Statement::Triple(Triple::from_triple(&t)),
    }
  }
}

#[datatest::files("tests/cases", {
  input in r"^(.*)\.ttl$",
  output = r"${1}.ron",
})]
fn test_case(input: &[u8], output: &Path) {
  let mut bytes = BytesMut::from(input);
  let mut parser = TurtleParser::new(Url::parse("http://example.com/").unwrap());
  let mut statements = Vec::new();
  while let Some(statement) = parser.end(&mut bytes).unwrap() {
    let converted = statement.as_statement();
    statements.push(Statement::from_statement(&converted));
  }

  if output.is_file() {
    let content = read(&output).unwrap();
    let parsed: Vec<Statement> = ron::de::from_bytes(&content).unwrap();
    assert_eq!(statements, parsed);
  } else {
    let serialized =
      ron::ser::to_string_pretty(&statements, ron::ser::PrettyConfig::default()).unwrap();
    write(output, serialized).unwrap();
  }
}
