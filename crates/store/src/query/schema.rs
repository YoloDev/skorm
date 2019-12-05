use super::*;
use crate::store::schema::*;
use skorm_core::*;

#[derive(Clone)]
pub struct Class<'a> {
  subj: SubjectRef<'a>,
}

impl<'a> Subject for Class<'a> {
  type Named = <SubjectRef<'a> as Subject>::Named;
  type Blank = <SubjectRef<'a> as Subject>::Blank;

  #[inline]
  fn get(&self) -> RdfSubject<Self::Named, Self::Blank> {
    Subject::get(&self.subj)
  }
}

impl<'a> Object for Class<'a> {
  type Named = <SubjectRef<'a> as Object>::Named;
  type Blank = <SubjectRef<'a> as Object>::Blank;
  type Literal = <SubjectRef<'a> as Object>::Literal;

  #[inline]
  fn get(&self) -> RdfObject<Self::Named, Self::Blank, Self::Literal> {
    Object::get(&self.subj)
  }
}

impl<'a> Class<'a> {
  pub fn instances(&self) -> Vec<Inst<'a>> {
    self.subj.db.instances_of(self).unwrap()
  }
}

impl<'a> sealed::Sealed for Class<'a> {}
impl<'a> AsSubjectRef for Class<'a> {
  type Error = <SubjectRef<'a> as AsSubjectRef>::Error;

  #[inline]
  fn as_subject_ref<'b>(&self, db: &'b Store) -> Result<SubjectRef<'b>, Self::Error> {
    self.subj.as_subject_ref(db)
  }
}
impl<'a> AsObjectRef for Class<'a> {
  type Error = <SubjectRef<'a> as AsObjectRef>::Error;

  #[inline]
  fn as_object_ref<'b>(&self, db: &'b Store) -> Result<ObjectRef<'b>, Self::Error> {
    self.subj.as_object_ref(db)
  }
}

#[derive(Clone)]
pub struct Prop<'a> {
  pred: PredicateRef<'a>,
}

impl<'a> Predicate for Prop<'a> {
  type Named = <PredicateRef<'a> as Subject>::Named;

  #[inline]
  fn get(&self) -> Self::Named {
    Predicate::get(&self.pred)
  }
}

impl<'a> Subject for Prop<'a> {
  type Named = <PredicateRef<'a> as Subject>::Named;
  type Blank = <PredicateRef<'a> as Subject>::Blank;

  #[inline]
  fn get(&self) -> RdfSubject<Self::Named, Self::Blank> {
    Subject::get(&self.pred)
  }
}

impl<'a> Object for Prop<'a> {
  type Named = <PredicateRef<'a> as Object>::Named;
  type Blank = <PredicateRef<'a> as Object>::Blank;
  type Literal = <PredicateRef<'a> as Object>::Literal;

  #[inline]
  fn get(&self) -> RdfObject<Self::Named, Self::Blank, Self::Literal> {
    Object::get(&self.pred)
  }
}

impl<'a> sealed::Sealed for Prop<'a> {}
impl<'a> AsPredicateRef for Prop<'a> {
  type Error = <PredicateRef<'a> as AsPredicateRef>::Error;

  #[inline]
  fn as_predicate_ref<'b>(&self, db: &'b Store) -> Result<PredicateRef<'b>, Self::Error> {
    self.pred.as_predicate_ref(db)
  }
}
impl<'a> AsSubjectRef for Prop<'a> {
  type Error = <PredicateRef<'a> as AsSubjectRef>::Error;

  #[inline]
  fn as_subject_ref<'b>(&self, db: &'b Store) -> Result<SubjectRef<'b>, Self::Error> {
    self.pred.as_subject_ref(db)
  }
}
impl<'a> AsObjectRef for Prop<'a> {
  type Error = <PredicateRef<'a> as AsObjectRef>::Error;

  #[inline]
  fn as_object_ref<'b>(&self, db: &'b Store) -> Result<ObjectRef<'b>, Self::Error> {
    self.pred.as_object_ref(db)
  }
}

#[derive(Clone)]
pub struct Inst<'a> {
  subj: SubjectRef<'a>,
}

impl<'a> Subject for Inst<'a> {
  type Named = <SubjectRef<'a> as Subject>::Named;
  type Blank = <SubjectRef<'a> as Subject>::Blank;

  #[inline]
  fn get(&self) -> RdfSubject<Self::Named, Self::Blank> {
    Subject::get(&self.subj)
  }
}

impl<'a> Object for Inst<'a> {
  type Named = <SubjectRef<'a> as Object>::Named;
  type Blank = <SubjectRef<'a> as Object>::Blank;
  type Literal = <SubjectRef<'a> as Object>::Literal;

  #[inline]
  fn get(&self) -> RdfObject<Self::Named, Self::Blank, Self::Literal> {
    Object::get(&self.subj)
  }
}

impl<'a> sealed::Sealed for Inst<'a> {}
impl<'a> AsSubjectRef for Inst<'a> {
  type Error = <SubjectRef<'a> as AsSubjectRef>::Error;

  #[inline]
  fn as_subject_ref<'b>(&self, db: &'b Store) -> Result<SubjectRef<'b>, Self::Error> {
    self.subj.as_subject_ref(db)
  }
}
impl<'a> AsObjectRef for Inst<'a> {
  type Error = <SubjectRef<'a> as AsObjectRef>::Error;

  #[inline]
  fn as_object_ref<'b>(&self, db: &'b Store) -> Result<ObjectRef<'b>, Self::Error> {
    self.subj.as_object_ref(db)
  }
}

pub trait SchemaStore: sealed::Sealed {
  fn classes(&self) -> Vec<Class>;
  fn properties(&self) -> Vec<Prop>;
  fn instances_of<I: AsSubjectRef + ?Sized>(
    &self,
    class: &I,
  ) -> Result<Vec<Inst>, <I as AsSubjectRef>::Error>;
}

impl SchemaStore for Store {
  fn classes(&self) -> Vec<Class> {
    SchemaDatabase::classes(&self.inner)
      .iter()
      .cloned()
      .map(|stored| Class {
        subj: SubjectRef { stored, db: self },
      })
      .collect()
  }

  fn properties(&self) -> Vec<Prop> {
    SchemaDatabase::properties(&self.inner)
      .iter()
      .cloned()
      .map(|stored| Prop {
        pred: PredicateRef { stored, db: self },
      })
      .collect()
  }

  fn instances_of<I: AsSubjectRef + ?Sized>(
    &self,
    class: &I,
  ) -> Result<Vec<Inst>, <I as AsSubjectRef>::Error> {
    let class = class.as_subject_ref(self)?;
    Ok(
      SchemaDatabase::instances_of(&self.inner, class.stored.clone())
        .iter()
        .cloned()
        .map(|stored| Inst {
          subj: SubjectRef { stored, db: self },
        })
        .collect(),
    )
  }
}
