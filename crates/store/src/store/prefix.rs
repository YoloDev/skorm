use evitable::*;
use im::{HashSet, Vector};
use itertools::Itertools;
use smallvec::SmallVec;
use std::sync::Arc;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct StoredPrefixName(pub(crate) Arc<str>);

impl From<Arc<str>> for StoredPrefixName {
  #[inline]
  fn from(s: Arc<str>) -> Self {
    Self(s)
  }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct StoredPrefixValue(pub(crate) Arc<str>);

impl From<Arc<str>> for StoredPrefixValue {
  #[inline]
  fn from(s: Arc<str>) -> Self {
    Self(s)
  }
}

macro_rules! intern_id {
  ($name:ident, $lookup:ident, $inner:ident) => {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub(crate) struct $name(salsa::InternId);

    impl $name {
      #[inline]
      #[allow(dead_code)]
      pub(crate) fn lookup(self, db: &impl PrefixDatabase) -> $inner {
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

intern_id!(PrefixName, lookup_prefix_name, StoredPrefixName);
intern_id!(PrefixValue, lookup_prefix_value, StoredPrefixValue);

#[salsa::query_group(PrefixDatabaseStorage)]
pub(crate) trait PrefixDatabase {
  #[salsa::interned]
  fn prefix_name(&self, name: StoredPrefixName) -> PrefixName;

  #[salsa::interned]
  fn prefix_value(&self, value: StoredPrefixValue) -> PrefixValue;

  #[salsa::input]
  fn prefix(&self, name: PrefixName) -> PrefixValue;

  #[salsa::input]
  fn prefixes(&self) -> HashSet<PrefixName>;

  fn length_sorted_prefixes(&self) -> Vector<PrefixName>;
}

fn length_sorted_prefixes(db: &impl PrefixDatabase) -> Vector<PrefixName> {
  db.prefixes()
    .iter()
    .cloned()
    .sorted_by_key(|prefix| {
      let prefix_name = prefix.lookup(db);
      std::cmp::Reverse(prefix_name.0.len())
    })
    .collect()
}

pub(crate) fn insert_prefix(db: &mut impl PrefixDatabase, prefix: Arc<str>, value: Arc<str>) {
  let name_id = db.prefix_name(prefix.into());
  let value_id = db.prefix_value(value.into());
  db.set_prefix(name_id, value_id);

  let mut prefixes = db.prefixes();
  if let None = prefixes.insert(name_id) {
    db.set_prefixes(prefixes);
  }
}

#[evitable(description("Prefix '{}' not found.", prefix))]
pub struct ExpandContext {
  prefix: Arc<str>,
}

pub(crate) fn expand(
  db: &impl PrefixDatabase,
  prefix: Arc<str>,
  value: &str,
) -> ExpandResult<String> {
  let name_id = db.prefix_name(prefix.clone().into());
  let prefixes = db.prefixes();
  ensure!(prefixes.contains(&name_id), ExpandContext { prefix });

  let prefix_value_id = db.prefix(name_id);
  let prefix_value = prefix_value_id.lookup(db);
  let mut ret = String::with_capacity(value.len() + prefix_value.0.len());
  ret.push_str(&prefix_value.0);
  ret.push_str(value);
  Ok(ret)
}

pub(crate) fn shrink(db: &impl PrefixDatabase, uri: &str) -> Option<String> {
  for prefix in db.length_sorted_prefixes() {
    let prefix_value_id = db.prefix(prefix);
    let prefix_value = prefix_value_id.lookup(db);
    if uri.starts_with(prefix_value.0.as_ref()) {
      let prefix_name = prefix.lookup(db);
      let mut result = String::with_capacity(prefix_name.0.len() + uri.len() + 1);
      result.push_str(&prefix_name.0);
      result.push(':');
      result.push_str(&uri);
      return Some(result);
    }
  }

  None
}

#[evitable]
pub enum ExpandIriContext {
  #[evitable(description = "Failed to expand", from = ExpandError)]
  Expand,

  #[evitable(description("Failed to parse {}", value))]
  Parse { value: String },
}

impl super::Store {
  pub fn expand_iri(&self, value: &str) -> ExpandIriResult<String> {
    let parts: SmallVec<[&str; 2]> = value.splitn(2, ':').collect();
    ensure!(
      parts.len() == 2,
      ExpandIriContext::Parse {
        value: value.to_owned()
      }
    );

    expand(&self.inner, parts[0].into(), parts[1].into()).map_err(Into::into)
  }

  #[inline]
  pub fn shrink_iri(&self, iri: &str) -> Option<String> {
    shrink(&self.inner, iri)
  }
}
