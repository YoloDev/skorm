use crate::tasks::{Common, XResult, XTask};
use crate::{install_git_hook, Mode};
use structopt::StructOpt;

#[derive(StructOpt)]
pub struct SetupTask {}

impl XTask for SetupTask {
  fn run(self, common: Common) -> XResult {
    install_git_hook(Mode::Overwrite)?;

    Ok(())
  }
}
