use crate::parse::InputBuf;
pub use evitable::*;
use nom::error::{VerboseError, VerboseErrorKind};

#[evitable]
pub enum Context {
  #[evitable(description = "IO error", from = std::io::Error)]
  Io,

  #[evitable(description = "URL parse error", from = url::ParseError)]
  Url,

  #[evitable(description = "UTF8 error", from = std::str::Utf8Error)]
  Utf8,

  #[evitable(description = "Parse error")]
  Parse,
}

impl<'a> InputBuf<'a> {
  pub(crate) fn convert_error(&self, e: VerboseError<Self>) -> String {
    let input = self.as_ref();
    use nom::Offset;
    use std::iter::repeat;
    let lines: Vec<_> = input.lines().map(String::from).collect();
    let mut result = String::new();
    for (i, (substring, kind)) in e.errors.iter().enumerate() {
      let mut offset = input.offset(substring.as_ref());
      if lines.is_empty() {
        match kind {
          VerboseErrorKind::Char(c) => {
            result += &format!("{}: expected '{}', got empty input\n\n", i, c);
          }
          VerboseErrorKind::Context(s) => {
            result += &format!("{}: in {}, got empty input\n\n", i, s);
          }
          VerboseErrorKind::Nom(e) => {
            result += &format!("{}: in {:?}, got empty input\n\n", i, e);
          }
        }
      } else {
        let mut line = 0;
        let mut column = 0;
        for (j, l) in lines.iter().enumerate() {
          if offset <= l.len() {
            line = j;
            column = offset;
            break;
          } else {
            offset = offset - l.len() - 1;
          }
        }
        match kind {
          VerboseErrorKind::Char(c) => {
            result += &format!("{}: at line {}:\n", i, line);
            result += &lines[line];
            result += "\n";
            if column > 0 {
              result += &repeat(' ').take(column).collect::<String>();
            }
            result += "^\n";
            result += &format!(
              "expected '{}', found {}\n\n",
              c,
              substring.as_ref().chars().next().unwrap()
            );
            if let Some(actual) = substring.as_ref().chars().next() {
              result += &format!("expected '{}', found {}\n\n", c, actual);
            } else {
              result += &format!("expected '{}', got end of input\n\n", c);
            }
          }
          VerboseErrorKind::Context(s) => {
            result += &format!("{}: at line {}, in {}:\n", i, line, s);
            result += &lines[line];
            result += "\n";
            if column > 0 {
              result += &repeat(' ').take(column).collect::<String>();
            }
            result += "^\n\n";
          }
          VerboseErrorKind::Nom(e) => {
            result += &format!("{}: at line {}, in {:?}:\n", i, line, e);
            result += &lines[line];
            result += "\n";
            if column > 0 {
              result += &repeat(' ').take(column).collect::<String>();
            }
            result += "^\n\n";
          }
        }
      }
    }
    result
  }
}
