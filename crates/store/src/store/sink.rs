use super::{
  expand, insert_prefix, prefix::ExpandError, Lit, Store, StoredBlankNode, StoredLiteral,
  StoredObject, StoredPredicate, StoredSubject, StoredTriple, StoredUri, TripleDatabase, Uri,
};
use evitable::*;
use skorm_parse::{
  AsRdfObject, AsRdfParserSink, AsRdfPredicate, AsRdfPrefix, AsRdfStatement, AsRdfSubject,
  AsRdfTriple, RdfParserSink, Statement,
};
use std::collections::HashMap;
use std::sync::atomic::Ordering;

#[evitable]
pub enum StoreInsertContext {
  #[evitable(description("Prefix not found."), from = ExpandError)]
  PrefixNotFound,
}

pub struct StoreSink<'a> {
  store: &'a mut Store,
  blanks: HashMap<skorm_parse::BlankNode, StoredBlankNode>,
}

impl<'a> RdfParserSink for StoreSink<'a> {
  type Error = StoreInsertError;

  fn insert(&mut self, statement: &impl AsRdfStatement) -> StoreInsertResult<()> {
    fn insert_named_node(
      db: &mut StoreSink,
      node: skorm_parse::NamedNode,
    ) -> StoreInsertResult<Uri> {
      let stored = match node {
        skorm_parse::NamedNode::Iri(i) => StoredUri(i),
        skorm_parse::NamedNode::Prefixed(p, v) => {
          let i = expand(&mut db.store.inner, p, &v)?;
          StoredUri(i.into())
        }
      };

      Ok(db.store.inner.uri(stored))
    }

    fn insert_blank_node(db: &mut StoreSink, node: skorm_parse::BlankNode) -> StoredBlankNode {
      let blanks = &mut db.blanks;
      let store = &mut db.store;

      blanks
        .entry(node)
        .or_insert_with(|| {
          StoredBlankNode::from(store.inner.next_blank.fetch_add(1, Ordering::Relaxed))
        })
        .clone()
    }

    fn insert_literal(db: &mut StoreSink, node: skorm_parse::Literal) -> StoreInsertResult<Lit> {
      let stored = match node {
        skorm_parse::Literal::Simple(v) => StoredLiteral::Simple(v),
        skorm_parse::Literal::LangTagged(v, l) => StoredLiteral::LangTagged(v, l),
        skorm_parse::Literal::Typed(v, t) => StoredLiteral::Typed(v, insert_named_node(db, t)?),
      };

      Ok(db.store.inner.lit(stored))
    }

    fn insert_subject(
      db: &mut StoreSink,
      subj: skorm_parse::Subject,
    ) -> StoreInsertResult<StoredSubject> {
      Ok(match subj {
        skorm_parse::Subject::Named(n) => StoredSubject::Named(insert_named_node(db, n)?),
        skorm_parse::Subject::Blank(n) => StoredSubject::Blank(insert_blank_node(db, n)),
      })
    }

    fn insert_predicate(
      db: &mut StoreSink,
      pred: skorm_parse::Predicate,
    ) -> StoreInsertResult<StoredPredicate> {
      insert_named_node(db, pred.0).map(Into::into)
    }

    fn insert_object(
      db: &mut StoreSink,
      obj: skorm_parse::Object,
    ) -> StoreInsertResult<StoredObject> {
      Ok(match obj {
        skorm_parse::Object::Named(n) => StoredObject::Named(insert_named_node(db, n)?),
        skorm_parse::Object::Blank(n) => StoredObject::Blank(insert_blank_node(db, n)),
        skorm_parse::Object::Literal(l) => StoredObject::Literal(insert_literal(db, l)?),
      })
    }

    match statement.as_statement() {
      Statement::Prefix(p) => {
        let p = p.as_prefix();
        insert_prefix(&mut self.store.inner, p.prefix.into(), p.uri.into());
        Ok(())
      }

      Statement::Triple(t) => {
        let subject = insert_subject(self, t.subject().as_subject())?;
        let predicate = insert_predicate(self, t.predicate().as_predicate())?;
        let object = insert_object(self, t.object().as_object())?;

        let id = self.store.inner.triple(StoredTriple {
          subject,
          predicate,
          object,
        });
        let mut triples = self.store.inner.triples();

        if let None = triples.insert(id) {
          // value did not already exist.
          self.store.inner.set_triples(triples);
        }

        Ok(())
      }
    }
  }
}

impl<'a> AsRdfParserSink<'a> for Store {
  type Sink = StoreSink<'a>;

  fn as_sink(&'a mut self) -> Self::Sink {
    StoreSink {
      store: self,
      blanks: HashMap::new(),
    }
  }
}
