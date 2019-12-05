mod input;
mod parse;

use super::db::*;
use super::prefix::*;
use parse::{parse_name as pn, Name};
use std::sync::Arc;

#[salsa::query_group(NameLookupDatabaseStorage)]
pub(crate) trait NameLookupDatabase: TripleDatabase + PrefixDatabase {
  fn parse_name(&self, name: Arc<str>) -> Option<Name>;
  fn named_node(&self, name: Arc<str>) -> Option<Uri>;
}

fn parse_name(_db: &impl NameLookupDatabase, name: Arc<str>) -> Option<Name> {
  match pn(&name) {
    Ok(n) => Some(n),
    Err(_) => None,
  }
}

fn named_node(db: &impl NameLookupDatabase, name: Arc<str>) -> Option<Uri> {
  Some(match db.parse_name(name)? {
    Name::Iri(i) => db.uri(StoredUri::from(Arc::from(i.as_ref()))),
    Name::Prefixed(p, v) => {
      let i = match expand(db, Arc::from(p.as_ref()), &v) {
        Ok(i) => Some(i),
        Err(_) => None,
      }?;
      db.uri(StoredUri::from(Arc::from(i.as_ref())))
    }
  })
}
