use im::HashSet;
use std::num::NonZeroU32;
use std::sync::Arc;

macro_rules! intern_id {
  ($name:ident, $lookup:ident, $inner:ident) => {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub(crate) struct $name(salsa::InternId);

    impl $name {
      #[inline]
      #[allow(dead_code)]
      pub(crate) fn lookup(self, db: &impl TripleDatabase) -> $inner {
        db.$lookup(self)
      }
    }

    impl salsa::InternKey for $name {
      #[inline]
      fn from_intern_id(v: salsa::InternId) -> Self {
        Self(v)
      }

      #[inline]
      fn as_intern_id(&self) -> salsa::InternId {
        self.0
      }
    }
  };
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct StoredUri(pub(crate) Arc<str>);

impl From<Arc<str>> for StoredUri {
  #[inline]
  fn from(s: Arc<str>) -> Self {
    Self(s)
  }
}

intern_id!(Uri, lookup_uri, StoredUri);
intern_id!(Lit, lookup_lit, StoredLiteral);
intern_id!(Trip, lookup_triple, StoredTriple);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) enum StoredLiteral {
  Simple(Arc<str>),
  Typed(Arc<str>, Uri),
  LangTagged(Arc<str>, Arc<str>),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub(crate) struct StoredBlankNode(pub(crate) NonZeroU32);

impl From<std::num::NonZeroU32> for StoredBlankNode {
  #[inline]
  fn from(n: std::num::NonZeroU32) -> Self {
    Self(n)
  }
}

impl From<u32> for StoredBlankNode {
  #[inline]
  fn from(n: u32) -> Self {
    Self(NonZeroU32::new(n).unwrap())
  }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub(crate) enum StoredSubject {
  Named(Uri),
  Blank(StoredBlankNode),
}

impl From<Uri> for StoredSubject {
  #[inline]
  fn from(uri: Uri) -> Self {
    Self::Named(uri)
  }
}

impl From<StoredBlankNode> for StoredSubject {
  #[inline]
  fn from(value: StoredBlankNode) -> Self {
    Self::Blank(value)
  }
}

impl From<NonZeroU32> for StoredSubject {
  #[inline]
  fn from(value: NonZeroU32) -> Self {
    Self::Blank(StoredBlankNode::from(value))
  }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub(crate) struct StoredPredicate(pub(crate) Uri);

impl From<Uri> for StoredPredicate {
  #[inline]
  fn from(uri: Uri) -> Self {
    Self(uri)
  }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub(crate) enum StoredObject {
  Named(Uri),
  Blank(StoredBlankNode),
  Literal(Lit),
}

impl From<Uri> for StoredObject {
  #[inline]
  fn from(uri: Uri) -> Self {
    Self::Named(uri)
  }
}

impl From<StoredBlankNode> for StoredObject {
  #[inline]
  fn from(blank: StoredBlankNode) -> Self {
    Self::Blank(blank)
  }
}

impl From<NonZeroU32> for StoredObject {
  #[inline]
  fn from(blank: NonZeroU32) -> Self {
    Self::Blank(StoredBlankNode::from(blank))
  }
}

impl From<Lit> for StoredObject {
  #[inline]
  fn from(lit: Lit) -> Self {
    Self::Literal(lit)
  }
}

impl From<StoredSubject> for StoredObject {
  #[inline]
  fn from(subj: StoredSubject) -> Self {
    match subj {
      StoredSubject::Blank(s) => Self::Blank(s),
      StoredSubject::Named(s) => Self::Named(s),
    }
  }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct StoredTriple {
  pub(crate) subject: StoredSubject,
  pub(crate) predicate: StoredPredicate,
  pub(crate) object: StoredObject,
}

#[salsa::query_group(TripleDatabaseStorage)]
pub(crate) trait TripleDatabase {
  #[salsa::interned]
  fn uri(&self, uri: StoredUri) -> Uri;

  #[salsa::interned]
  fn lit(&self, lit: StoredLiteral) -> Lit;

  #[salsa::interned]
  fn triple(&self, triple: StoredTriple) -> Trip;

  #[salsa::input]
  fn triples(&self) -> HashSet<Trip>;

  // simple iterators
  fn subjects(&self) -> HashSet<StoredSubject>;
  fn predicates(&self) -> HashSet<StoredPredicate>;
  fn objects(&self) -> HashSet<StoredObject>;

  // all triples for
  fn triples_for_subject(&self, subject: StoredSubject) -> HashSet<Trip>;
  fn triples_for_predicate(&self, predicate: StoredPredicate) -> HashSet<Trip>;
  fn triples_for_object(&self, object: StoredObject) -> HashSet<Trip>;

  fn triples_for_subject_predicate(
    &self,
    subject: StoredSubject,
    predicate: StoredPredicate,
  ) -> HashSet<Trip>;
  fn triples_for_predicate_object(
    &self,
    predicate: StoredPredicate,
    object: StoredObject,
  ) -> HashSet<Trip>;
}

fn subjects(db: &impl TripleDatabase) -> HashSet<StoredSubject> {
  let mut set = HashSet::new();
  for triple in db.triples() {
    set.insert(triple.lookup(db).subject);
  }

  set
}

fn predicates(db: &impl TripleDatabase) -> HashSet<StoredPredicate> {
  let mut set = HashSet::new();
  for triple in db.triples() {
    set.insert(triple.lookup(db).predicate);
  }

  set
}

fn objects(db: &impl TripleDatabase) -> HashSet<StoredObject> {
  let mut set = HashSet::new();
  for triple in db.triples() {
    set.insert(triple.lookup(db).object);
  }

  set
}

#[inline]
fn triples_matching(
  db: &impl TripleDatabase,
  filter: impl Fn(&StoredTriple) -> bool,
) -> HashSet<Trip> {
  db.triples()
    .iter()
    .map(|id| (id, id.lookup(db)))
    .filter(|(_, v)| filter(v))
    .map(|(id, _)| *id)
    .collect()
}

fn triples_for_subject(db: &impl TripleDatabase, subject: StoredSubject) -> HashSet<Trip> {
  triples_matching(db, |t| t.subject == subject)
}

fn triples_for_predicate(db: &impl TripleDatabase, predicate: StoredPredicate) -> HashSet<Trip> {
  triples_matching(db, |t| t.predicate == predicate)
}

fn triples_for_object(db: &impl TripleDatabase, object: StoredObject) -> HashSet<Trip> {
  triples_matching(db, |t| t.object == object)
}

fn triples_for_subject_predicate(
  db: &impl TripleDatabase,
  subject: StoredSubject,
  predicate: StoredPredicate,
) -> HashSet<Trip> {
  db.triples_for_subject(subject)
    .iter()
    .map(|id| (id, id.lookup(db)))
    .filter(|(_, v)| v.predicate == predicate)
    .map(|(id, _)| *id)
    .collect()
}

fn triples_for_predicate_object(
  db: &impl TripleDatabase,
  predicate: StoredPredicate,
  object: StoredObject,
) -> HashSet<Trip> {
  db.triples_for_object(object)
    .iter()
    .map(|id| (id, id.lookup(db)))
    .filter(|(_, v)| v.predicate == predicate)
    .map(|(id, _)| *id)
    .collect()
}
