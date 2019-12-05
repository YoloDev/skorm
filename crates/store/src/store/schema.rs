use super::db::*;
use im::HashSet;
use lazy_static::lazy_static;
use std::collections::VecDeque;
use std::sync::Arc;

lazy_static! {
  static ref PROPERTY: Arc<str> = "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property".into();
  static ref CLASS: Arc<str> = "http://www.w3.org/2000/01/rdf-schema#Class".into();
  static ref TYPE: Arc<str> = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type".into();
  static ref SUBCLASS: Arc<str> = "http://www.w3.org/2000/01/rdf-schema#subClassOf".into();
  static ref RANGE: Arc<str> = "http://www.w3.org/2000/01/rdf-schema#range".into();
  static ref DOMAIN: Arc<str> = "http://www.w3.org/2000/01/rdf-schema#domain".into();
}

#[salsa::query_group(SchemaDatabaseStorage)]
pub(crate) trait SchemaDatabase: TripleDatabase {
  fn direct_subclasses_of(&self, class: StoredSubject) -> HashSet<StoredSubject>;
  fn subclasses_of(&self, class: StoredSubject) -> HashSet<StoredSubject>;

  fn direct_superclasses_of(&self, class: StoredSubject) -> HashSet<StoredSubject>;
  fn superclasses_of(&self, class: StoredSubject) -> HashSet<StoredSubject>;

  fn direct_instances_of(&self, class: StoredSubject) -> HashSet<StoredSubject>;
  fn instances_of(&self, class: StoredSubject) -> HashSet<StoredSubject>;

  fn classes(&self) -> HashSet<StoredSubject>;
  fn properties(&self) -> HashSet<StoredPredicate>;

  fn direct_properties_of(&self, class: StoredSubject) -> HashSet<StoredPredicate>;
  fn properties_of(&self, class: StoredSubject) -> HashSet<StoredPredicate>;

  fn direct_properties_with(&self, class: StoredSubject) -> HashSet<StoredPredicate>;
  fn properties_with(&self, class: StoredSubject) -> HashSet<StoredPredicate>;
}

fn direct_subclasses_of(db: &impl SchemaDatabase, class: StoredSubject) -> HashSet<StoredSubject> {
  let subclass = db.uri(SUBCLASS.clone().into());
  db.triples_for_predicate_object(subclass.into(), class.into())
    .iter()
    .map(|t| t.lookup(db).subject)
    .collect()
}

fn subclasses_of(db: &impl SchemaDatabase, class: StoredSubject) -> HashSet<StoredSubject> {
  let mut todo = VecDeque::with_capacity(16);
  let mut result = HashSet::new();
  todo.push_back(class);

  while let Some(next) = todo.pop_front() {
    let direct_subclasses = db.direct_subclasses_of(next);
    for subclass in direct_subclasses {
      match result.insert(subclass) {
        None => todo.push_back(subclass),
        _ => (),
      }
    }
  }

  result
}

fn direct_superclasses_of(
  db: &impl SchemaDatabase,
  class: StoredSubject,
) -> HashSet<StoredSubject> {
  let subclass = db.uri(SUBCLASS.clone().into());
  db.triples_for_subject_predicate(class.into(), subclass.into())
    .iter()
    .map(|t| t.lookup(db).subject)
    .collect()
}

fn superclasses_of(db: &impl SchemaDatabase, class: StoredSubject) -> HashSet<StoredSubject> {
  let mut todo = VecDeque::with_capacity(16);
  let mut result = HashSet::new();
  todo.push_back(class);

  while let Some(next) = todo.pop_front() {
    let direct_superclasses = db.direct_superclasses_of(next);
    for superclass in direct_superclasses {
      match result.insert(superclass) {
        None => todo.push_back(superclass),
        _ => (),
      }
    }
  }

  result
}

fn direct_instances_of(db: &impl SchemaDatabase, class: StoredSubject) -> HashSet<StoredSubject> {
  let ty = db.uri(TYPE.clone().into());
  db.triples_for_predicate_object(ty.into(), class.into())
    .iter()
    .map(|t| t.lookup(db).subject)
    .collect()
}

fn instances_of(db: &impl SchemaDatabase, class: StoredSubject) -> HashSet<StoredSubject> {
  let mut instances = db.direct_instances_of(class);
  for subclass in db.subclasses_of(class) {
    instances = instances.union(db.direct_instances_of(subclass));
  }

  instances
}

fn classes(db: &impl SchemaDatabase) -> HashSet<StoredSubject> {
  let class = db.uri(CLASS.clone().into());
  db.instances_of(class.into())
}

fn properties(db: &impl SchemaDatabase) -> HashSet<StoredPredicate> {
  let prop = db.uri(PROPERTY.clone().into());
  db.instances_of(prop.into())
    .iter()
    .filter_map(|p| match p {
      StoredSubject::Blank(_) => None,
      StoredSubject::Named(n) => Some(StoredPredicate::from(n.clone())),
    })
    .collect()
}

fn direct_properties_of(
  db: &impl SchemaDatabase,
  class: StoredSubject,
) -> HashSet<StoredPredicate> {
  let domain = db.uri(DOMAIN.clone().into());
  db.triples_for_predicate_object(domain.into(), class.into())
    .iter()
    .map(|t| t.lookup(db).subject)
    .filter_map(|p| match p {
      StoredSubject::Blank(_) => None,
      StoredSubject::Named(n) => Some(StoredPredicate::from(n.clone())),
    })
    .collect()
}

fn properties_of(db: &impl SchemaDatabase, class: StoredSubject) -> HashSet<StoredPredicate> {
  let mut properties = db.direct_properties_of(class);
  for superclass in db.superclasses_of(class) {
    properties = properties.union(db.direct_properties_of(superclass));
  }

  properties
}

fn direct_properties_with(
  db: &impl SchemaDatabase,
  class: StoredSubject,
) -> HashSet<StoredPredicate> {
  let range = db.uri(RANGE.clone().into());
  db.triples_for_predicate_object(range.into(), class.into())
    .iter()
    .map(|t| t.lookup(db).subject)
    .filter_map(|p| match p {
      StoredSubject::Blank(_) => None,
      StoredSubject::Named(n) => Some(StoredPredicate::from(n.clone())),
    })
    .collect()
}

fn properties_with(db: &impl SchemaDatabase, class: StoredSubject) -> HashSet<StoredPredicate> {
  let mut properties = db.direct_properties_with(class);
  for subclass in db.subclasses_of(class) {
    properties = properties.union(db.direct_properties_with(subclass));
  }

  properties
}
