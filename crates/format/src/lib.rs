pub trait PrefixRegistry {
  fn split(&self, iri: &str) -> (String, String);
}
