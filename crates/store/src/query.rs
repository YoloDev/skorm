pub mod schema;

use crate::store::db::TripleDatabase;
use crate::store::named::NameLookupDatabase;
use crate::store::{Lit, StoredLiteral, StoredObject, StoredPredicate, StoredSubject, Trip, Uri};
use crate::Store;
use evitable::*;
use lazy_static::lazy_static;
use std::marker::PhantomData;
use std::num::NonZeroU32;
use std::sync::Arc;

lazy_static! {
  static ref STRING_TYPE: Arc<str> = Arc::from(skorm_core::STRING_TYPE);
  static ref LANG_STRING_TYPE: Arc<str> = Arc::from(skorm_core::LANG_STRING_TYPE);
}

mod sealed {
  pub trait Sealed {}
  impl Sealed for super::Store {}
}

#[derive(Clone)]
pub struct NamedNodeRef<'a> {
  stored: Uri,
  name: Arc<str>,
  db: &'a Store,
}

impl<'a> NamedNodeRef<'a> {
  fn new(stored: Uri, db: &'a Store) -> Self {
    let name = stored.lookup(&db.inner).0;
    Self { stored, name, db }
  }
}

impl<'a> skorm_core::NamedNode for NamedNodeRef<'a> {
  #[inline]
  fn iri(&self) -> &str {
    &self.name
  }

  #[inline]
  fn iri_arc(&self) -> Arc<str> {
    self.name.clone()
  }
}

impl<'a> skorm_core::Predicate for NamedNodeRef<'a> {
  type Named = Self;

  #[inline]
  fn get(&self) -> Self {
    self.clone()
  }
}

impl<'a> skorm_core::Subject for NamedNodeRef<'a> {
  type Named = Self;
  type Blank = !;

  #[inline]
  fn get(&self) -> skorm_core::RdfSubject<Self, !> {
    skorm_core::RdfSubject::Named(self.clone())
  }
}

impl<'a> skorm_core::Object for NamedNodeRef<'a> {
  type Named = Self;
  type Blank = !;
  type Literal = !;

  #[inline]
  fn get(&self) -> skorm_core::RdfObject<Self, !, !> {
    skorm_core::RdfObject::Named(self.clone())
  }
}

#[derive(Clone, Copy)]
pub struct BlankNodeRef<'a> {
  stored: NonZeroU32,
  db: PhantomData<&'a ()>,
}

impl<'a> BlankNodeRef<'a> {
  fn new(stored: NonZeroU32) -> Self {
    Self {
      stored,
      db: PhantomData,
    }
  }
}

impl<'a> skorm_core::BlankNode for BlankNodeRef<'a> {
  #[inline]
  fn id(&self) -> NonZeroU32 {
    self.stored
  }
}

impl<'a> skorm_core::Subject for BlankNodeRef<'a> {
  type Named = !;
  type Blank = Self;

  #[inline]
  fn get(&self) -> skorm_core::RdfSubject<!, Self> {
    skorm_core::RdfSubject::Blank(self.clone())
  }
}

impl<'a> skorm_core::Object for BlankNodeRef<'a> {
  type Named = !;
  type Blank = Self;
  type Literal = !;

  #[inline]
  fn get(&self) -> skorm_core::RdfObject<!, Self, !> {
    skorm_core::RdfObject::Blank(self.clone())
  }
}

#[derive(Clone)]
pub struct LiteralRef<'a> {
  stored: Lit,
  value: Arc<str>,
  ty: NamedNodeRef<'a>,
  lang: Option<Arc<str>>,
}

impl<'a> LiteralRef<'a> {
  fn new(stored: Lit, db: &'a Store) -> Self {
    let value = stored.lookup(&db.inner);
    match value {
      StoredLiteral::Simple(value) => {
        let ty = db.inner.uri(STRING_TYPE.clone().into());
        let ty = NamedNodeRef::new(ty, db);
        Self {
          stored,
          value,
          ty,
          lang: None,
        }
      }

      StoredLiteral::LangTagged(value, lang) => {
        let ty = db.inner.uri(LANG_STRING_TYPE.clone().into());
        let ty = NamedNodeRef::new(ty, db);
        Self {
          stored,
          value,
          ty,
          lang: Some(lang),
        }
      }

      StoredLiteral::Typed(value, ty) => {
        let ty = NamedNodeRef::new(ty, db);
        Self {
          stored,
          value,
          ty,
          lang: None,
        }
      }
    }
  }
}

impl<'a> skorm_core::Literal for LiteralRef<'a> {
  type Type = NamedNodeRef<'a>;

  #[inline]
  fn value(&self) -> &str {
    &self.value
  }

  #[inline]
  fn value_arc(&self) -> Arc<str> {
    self.value.clone()
  }

  #[inline]
  fn ty(&self) -> Self::Type {
    self.ty.clone()
  }

  #[inline]
  fn lang(&self) -> Option<&str> {
    match &self.lang {
      None => None,
      Some(v) => Some(&v),
    }
  }

  #[inline]
  fn lang_arc(&self) -> Option<Arc<str>> {
    self.lang.clone()
  }
}

impl<'a> skorm_core::Object for LiteralRef<'a> {
  type Named = !;
  type Blank = !;
  type Literal = Self;

  #[inline]
  fn get(&self) -> skorm_core::RdfObject<!, !, Self> {
    skorm_core::RdfObject::Literal(self.clone())
  }
}

#[derive(Clone, Copy)]
pub struct SubjectRef<'a> {
  stored: StoredSubject,
  db: &'a Store,
}

impl<'a> SubjectRef<'a> {
  fn new(stored: StoredSubject, db: &'a Store) -> Self {
    Self { stored, db }
  }
}

impl<'a> skorm_core::Subject for SubjectRef<'a> {
  type Named = NamedNodeRef<'a>;
  type Blank = BlankNodeRef<'a>;

  fn get(&self) -> skorm_core::RdfSubject<Self::Named, Self::Blank> {
    match &self.stored {
      StoredSubject::Named(uri) => skorm_core::RdfSubject::Named(NamedNodeRef::new(*uri, self.db)),
      StoredSubject::Blank(n) => skorm_core::RdfSubject::Blank(BlankNodeRef::new(n.0)),
    }
  }
}

impl<'a> skorm_core::Object for SubjectRef<'a> {
  type Named = NamedNodeRef<'a>;
  type Blank = BlankNodeRef<'a>;
  type Literal = !;

  fn get(&self) -> skorm_core::RdfObject<Self::Named, Self::Blank, !> {
    match &self.stored {
      StoredSubject::Named(uri) => skorm_core::RdfObject::Named(NamedNodeRef::new(*uri, self.db)),
      StoredSubject::Blank(n) => skorm_core::RdfObject::Blank(BlankNodeRef::new(n.0)),
    }
  }
}

#[derive(Clone, Copy)]
pub struct PredicateRef<'a> {
  stored: StoredPredicate,
  db: &'a Store,
}

impl<'a> PredicateRef<'a> {
  fn new(stored: StoredPredicate, db: &'a Store) -> Self {
    Self { stored, db }
  }
}

impl<'a> skorm_core::Predicate for PredicateRef<'a> {
  type Named = NamedNodeRef<'a>;

  #[inline]
  fn get(&self) -> Self::Named {
    NamedNodeRef::new(self.stored.0, self.db)
  }
}

impl<'a> skorm_core::Subject for PredicateRef<'a> {
  type Named = NamedNodeRef<'a>;
  type Blank = !;

  #[inline]
  fn get(&self) -> skorm_core::RdfSubject<Self::Named, !> {
    skorm_core::RdfSubject::Named(skorm_core::Predicate::get(self))
  }
}

impl<'a> skorm_core::Object for PredicateRef<'a> {
  type Named = NamedNodeRef<'a>;
  type Blank = !;
  type Literal = !;

  #[inline]
  fn get(&self) -> skorm_core::RdfObject<Self::Named, !, !> {
    skorm_core::RdfObject::Named(skorm_core::Predicate::get(self))
  }
}

#[derive(Clone, Copy)]
pub struct ObjectRef<'a> {
  stored: StoredObject,
  db: &'a Store,
}

impl<'a> ObjectRef<'a> {
  fn new(stored: StoredObject, db: &'a Store) -> Self {
    Self { stored, db }
  }
}

impl<'a> skorm_core::Object for ObjectRef<'a> {
  type Named = NamedNodeRef<'a>;
  type Blank = BlankNodeRef<'a>;
  type Literal = LiteralRef<'a>;

  fn get(&self) -> skorm_core::RdfObject<Self::Named, Self::Blank, Self::Literal> {
    match &self.stored {
      StoredObject::Named(uri) => skorm_core::RdfObject::Named(NamedNodeRef::new(*uri, self.db)),
      StoredObject::Blank(n) => skorm_core::RdfObject::Blank(BlankNodeRef::new(n.0)),
      StoredObject::Literal(n) => skorm_core::RdfObject::Literal(LiteralRef::new(*n, self.db)),
    }
  }
}

#[derive(Clone, Copy)]
pub struct TripleRef<'a> {
  stored: Trip,
  db: &'a Store,
}

impl<'a> skorm_core::Triple for TripleRef<'a> {
  type Subject = SubjectRef<'a>;
  type Predicate = PredicateRef<'a>;
  type Object = ObjectRef<'a>;

  fn subj(&self) -> Self::Subject {
    SubjectRef::new(self.stored.lookup(&self.db.inner).subject, self.db)
  }

  fn pred(&self) -> Self::Predicate {
    PredicateRef::new(self.stored.lookup(&self.db.inner).predicate, self.db)
  }

  fn obj(&self) -> Self::Object {
    ObjectRef::new(self.stored.lookup(&self.db.inner).object, self.db)
  }
}

pub trait AsSubjectRef: sealed::Sealed {
  type Error;

  fn as_subject_ref<'a>(&self, db: &'a Store) -> Result<SubjectRef<'a>, Self::Error>;
}

pub trait AsPredicateRef: sealed::Sealed {
  type Error;

  fn as_predicate_ref<'a>(&self, db: &'a Store) -> Result<PredicateRef<'a>, Self::Error>;
}

pub trait AsObjectRef: sealed::Sealed {
  type Error;

  fn as_object_ref<'a>(&self, db: &'a Store) -> Result<ObjectRef<'a>, Self::Error>;
}

#[evitable(description("Failed to parse '{}' as a valid subject.", value))]
pub struct ParseContext {
  value: Arc<str>,
}

impl sealed::Sealed for Arc<str> {}
impl AsSubjectRef for Arc<str> {
  type Error = ParseError;
  fn as_subject_ref<'a>(&self, db: &'a Store) -> ParseResult<SubjectRef<'a>> {
    match db.inner.named_node(self.clone()) {
      None => Err(
        ParseContext {
          value: self.clone(),
        }
        .into(),
      ),
      Some(uri) => Ok(SubjectRef {
        db,
        stored: uri.into(),
      }),
    }
  }
}
impl AsPredicateRef for Arc<str> {
  type Error = ParseError;
  fn as_predicate_ref<'a>(&self, db: &'a Store) -> ParseResult<PredicateRef<'a>> {
    match db.inner.named_node(self.clone()) {
      None => Err(
        ParseContext {
          value: self.clone(),
        }
        .into(),
      ),
      Some(uri) => Ok(PredicateRef {
        db,
        stored: uri.into(),
      }),
    }
  }
}
impl AsObjectRef for Arc<str> {
  type Error = ParseError;
  fn as_object_ref<'a>(&self, db: &'a Store) -> ParseResult<ObjectRef<'a>> {
    match db.inner.named_node(self.clone()) {
      None => Err(
        ParseContext {
          value: self.clone(),
        }
        .into(),
      ),
      Some(uri) => Ok(ObjectRef {
        db,
        stored: uri.into(),
      }),
    }
  }
}

impl sealed::Sealed for str {}
impl AsSubjectRef for str {
  type Error = <Arc<str> as AsSubjectRef>::Error;

  #[inline]
  fn as_subject_ref<'a>(&self, db: &'a Store) -> ParseResult<SubjectRef<'a>> {
    let value: Arc<str> = self.into();
    value.as_subject_ref(db)
  }
}
impl AsPredicateRef for str {
  type Error = <Arc<str> as AsPredicateRef>::Error;

  #[inline]
  fn as_predicate_ref<'a>(&self, db: &'a Store) -> ParseResult<PredicateRef<'a>> {
    let value: Arc<str> = self.into();
    value.as_predicate_ref(db)
  }
}
impl AsObjectRef for str {
  type Error = <Arc<str> as AsObjectRef>::Error;

  #[inline]
  fn as_object_ref<'a>(&self, db: &'a Store) -> ParseResult<ObjectRef<'a>> {
    let value: Arc<str> = self.into();
    value.as_object_ref(db)
  }
}

impl<'a> sealed::Sealed for NamedNodeRef<'a> {}
impl<'a> AsPredicateRef for NamedNodeRef<'a> {
  type Error = !;

  #[inline]
  fn as_predicate_ref<'b>(&self, db: &'b Store) -> Result<PredicateRef<'b>, !> {
    Ok(PredicateRef {
      db,
      stored: self.stored.into(),
    })
  }
}
impl<'a> AsSubjectRef for NamedNodeRef<'a> {
  type Error = !;

  #[inline]
  fn as_subject_ref<'b>(&self, db: &'b Store) -> Result<SubjectRef<'b>, !> {
    Ok(SubjectRef {
      db,
      stored: self.stored.into(),
    })
  }
}
impl<'a> AsObjectRef for NamedNodeRef<'a> {
  type Error = !;

  #[inline]
  fn as_object_ref<'b>(&self, db: &'b Store) -> Result<ObjectRef<'b>, !> {
    Ok(ObjectRef {
      db,
      stored: self.stored.into(),
    })
  }
}

impl<'a> sealed::Sealed for BlankNodeRef<'a> {}
impl<'a> AsSubjectRef for BlankNodeRef<'a> {
  type Error = !;

  #[inline]
  fn as_subject_ref<'b>(&self, db: &'b Store) -> Result<SubjectRef<'b>, !> {
    Ok(SubjectRef {
      db,
      stored: self.stored.into(),
    })
  }
}
impl<'a> AsObjectRef for BlankNodeRef<'a> {
  type Error = !;

  #[inline]
  fn as_object_ref<'b>(&self, db: &'b Store) -> Result<ObjectRef<'b>, !> {
    Ok(ObjectRef {
      db,
      stored: self.stored.into(),
    })
  }
}

impl<'a> sealed::Sealed for LiteralRef<'a> {}
impl<'a> AsObjectRef for LiteralRef<'a> {
  type Error = !;

  #[inline]
  fn as_object_ref<'b>(&self, db: &'b Store) -> Result<ObjectRef<'b>, !> {
    Ok(ObjectRef {
      db,
      stored: self.stored.into(),
    })
  }
}

impl<'a> sealed::Sealed for PredicateRef<'a> {}
impl<'a> AsSubjectRef for PredicateRef<'a> {
  type Error = !;

  #[inline]
  fn as_subject_ref<'b>(&self, db: &'b Store) -> Result<SubjectRef<'b>, !> {
    Ok(SubjectRef {
      db,
      stored: self.stored.0.into(),
    })
  }
}
impl<'a> AsPredicateRef for PredicateRef<'a> {
  type Error = !;

  #[inline]
  fn as_predicate_ref<'b>(&self, db: &'b Store) -> Result<PredicateRef<'b>, !> {
    Ok(PredicateRef {
      db,
      stored: self.stored,
    })
  }
}
impl<'a> AsObjectRef for PredicateRef<'a> {
  type Error = !;

  #[inline]
  fn as_object_ref<'b>(&self, db: &'b Store) -> Result<ObjectRef<'b>, !> {
    Ok(ObjectRef {
      db,
      stored: self.stored.0.into(),
    })
  }
}

impl<'a> sealed::Sealed for SubjectRef<'a> {}
impl<'a> AsSubjectRef for SubjectRef<'a> {
  type Error = !;

  #[inline]
  fn as_subject_ref<'b>(&self, db: &'b Store) -> Result<SubjectRef<'b>, !> {
    Ok(SubjectRef {
      db,
      stored: self.stored,
    })
  }
}
impl<'a> AsObjectRef for SubjectRef<'a> {
  type Error = !;

  #[inline]
  fn as_object_ref<'b>(&self, db: &'b Store) -> Result<ObjectRef<'b>, !> {
    Ok(ObjectRef {
      db,
      stored: self.stored.into(),
    })
  }
}

impl<'a> sealed::Sealed for ObjectRef<'a> {}
impl<'a> AsObjectRef for ObjectRef<'a> {
  type Error = !;

  #[inline]
  fn as_object_ref<'b>(&self, db: &'b Store) -> Result<ObjectRef<'b>, !> {
    Ok(ObjectRef {
      db,
      stored: self.stored,
    })
  }
}
