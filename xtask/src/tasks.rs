mod setup;

use std::error::Error;
use structopt::StructOpt;

pub type XResult = Result<(), Box<dyn Error>>;

#[derive(StructOpt)]
pub enum Task {
  /// Configures git hooks.
  Setup {
    #[structopt(flatten)]
    task: setup::SetupTask,
  },
}

#[derive(StructOpt)]
struct Common {}

trait XTask {
  fn run(self, common: Common) -> XResult;
}

impl XTask for Task {
  fn run(self, common: Common) -> XResult {
    match self {
      Task::Setup { task } => task.run(common),
    }
  }
}

/// Skorm build tasks
#[derive(StructOpt)]
#[structopt(name = "cargo xtask", no_version, bin_name("cargo xtask"))]
pub struct Args {
  #[structopt(flatten)]
  common: Common,

  #[structopt(subcommand)]
  task: Task,
}

impl Args {
  pub fn run(self) -> XResult {
    self.task.run(self.common)
  }
}
