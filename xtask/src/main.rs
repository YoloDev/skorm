//! See https://github.com/matklad/cargo-xtask/.
//!
//! This binary defines various auxiliary build commands, which are not
//! expressible with just `cargo`. Notably, it provides `cargo xtask codegen`
//! for code generation and `cargo xtask install` for installation of
//! rust-analyzer server and client.
//!
//! This binary is integrated into the `cargo` command line by using an alias in
//! `.cargo/config`.

use std::error::Error;
use structopt::StructOpt;
use xtask::{install_git_hook, tasks::Args, Mode};

fn main() -> Result<(), Box<dyn Error>> {
  install_git_hook(Mode::Verify)?;

  let args = Args::from_args();
  args.run()?;

  Ok(())
}
