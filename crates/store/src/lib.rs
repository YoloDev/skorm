#![feature(type_alias_impl_trait)]
#![feature(specialization)]

pub mod query;
pub(crate) mod store;

pub use query::schema;
pub use store::Store;

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
