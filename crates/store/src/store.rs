pub(crate) mod db;
pub(crate) mod named;
pub(crate) mod prefix;
pub(crate) mod schema;
pub(crate) mod sink;

pub(crate) use db::*;
use named::*;
use prefix::*;
use schema::*;
use skorm_core::*;
use std::sync::atomic::AtomicU32;

#[salsa::database(
  db::TripleDatabaseStorage,
  schema::SchemaDatabaseStorage,
  prefix::PrefixDatabaseStorage,
  named::NameLookupDatabaseStorage
)]
pub(crate) struct StoreImpl {
  runtime: salsa::Runtime<Self>,
  next_blank: AtomicU32,
}

impl Default for StoreImpl {
  #[inline]
  fn default() -> Self {
    Self {
      runtime: Default::default(),
      next_blank: AtomicU32::from(1),
    }
  }
}

impl salsa::Database for StoreImpl {
  #[inline]
  fn salsa_runtime(&self) -> &salsa::Runtime<Self> {
    &self.runtime
  }

  #[inline]
  fn salsa_runtime_mut(&mut self) -> &mut salsa::Runtime<Self> {
    &mut self.runtime
  }
}

#[derive(Default)]
pub struct Store {
  pub(crate) inner: StoreImpl,
}

impl Store {
  #[inline]
  pub fn new() -> Self {
    Default::default()
  }
}
