pub mod tasks;

use glob::glob;
use std::{
  env,
  error::Error,
  ffi::OsStr,
  fs,
  path::{Path, PathBuf},
  process::{Command, Output, Stdio},
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mode {
  Overwrite,
  Verify,
}

#[derive(Debug, Clone)]

pub struct Crate {
  name: String,
  dir: PathBuf,
}

pub type Result<T> = std::result::Result<T, Box<dyn Error>>;

const TOOLCHAIN: &str = "nightly";

pub fn project_root() -> PathBuf {
  Path::new(
    &env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned()),
  )
  .ancestors()
  .nth(1)
  .unwrap()
  .to_path_buf()
}

pub fn crates_root() -> PathBuf {
  project_root().join("crates")
}

pub fn crates() -> impl Iterator<Item = Crate> {
  let crates_dir = crates_root();
  glob(crates_dir.join("*/Cargo.toml").to_str().unwrap())
    .unwrap()
    .map(|result| {
      let manifest = result.unwrap();
      let dir = manifest.parent().unwrap();
      let name = dir.file_name().and_then(OsStr::to_str).unwrap();
      Crate {
        name: name.into(),
        dir: dir.into(),
      }
    })
}

pub struct Cmd<'a> {
  pub unix: &'a str,
  pub windows: &'a str,
  pub work_dir: &'a str,
}

impl Cmd<'_> {
  pub fn run(self) -> Result<()> {
    if cfg!(windows) {
      run(self.windows, self.work_dir)
    } else {
      run(self.unix, self.work_dir)
    }
  }
  pub fn run_with_output(self) -> Result<Output> {
    if cfg!(windows) {
      run_with_output(self.windows, self.work_dir)
    } else {
      run_with_output(self.unix, self.work_dir)
    }
  }
}

pub fn run(cmdline: &str, dir: &str) -> Result<()> {
  do_run(cmdline, dir, |c| {
    c.stdout(Stdio::inherit());
  })
  .map(|_| ())
}

pub fn run_with_output(cmdline: &str, dir: &str) -> Result<Output> {
  do_run(cmdline, dir, |_| {})
}

pub fn run_rustfmt(mode: Mode) -> Result<()> {
  match Command::new("rustup")
    .args(&["run", TOOLCHAIN, "--", "cargo", "fmt", "--version"])
    .stderr(Stdio::null())
    .stdout(Stdio::null())
    .status()
  {
    Ok(status) if status.success() => (),
    _ => install_rustfmt()?,
  };

  if mode == Mode::Verify {
    run(
      &format!("rustup run {} -- cargo fmt -- --check", TOOLCHAIN),
      ".",
    )?;
  } else {
    run(&format!("rustup run {} -- cargo fmt --all", TOOLCHAIN), ".")?;
  }
  Ok(())
}

pub fn install_rustfmt() -> Result<()> {
  run(&format!("rustup install {}", TOOLCHAIN), ".")?;
  run(
    &format!("rustup component add rustfmt --toolchain {}", TOOLCHAIN),
    ".",
  )
}

pub fn install_git_hook(mode: Mode) -> Result<()> {
  let result_path = Path::new(if cfg!(windows) {
    "./.git/hooks/pre-commit.exe"
  } else {
    "./.git/hooks/pre-commit"
  });

  if !result_path.exists() || mode == Mode::Overwrite {
    run("cargo build --package xtask --bin pre-commit", ".")?;
    if cfg!(windows) {
      fs::copy("./target/debug/pre-commit.exe", result_path)?;
    } else {
      fs::copy("./target/debug/pre-commit", result_path)?;
    }
  }

  Ok(())
}

pub fn run_clippy() -> Result<()> {
  match Command::new("rustup")
    .args(&["run", TOOLCHAIN, "--", "cargo", "clippy", "--version"])
    .stderr(Stdio::null())
    .stdout(Stdio::null())
    .status()
  {
    Ok(status) if status.success() => (),
    _ => install_clippy()?,
  };

  let allowed_lints = [
    "clippy::collapsible_if",
    "clippy::map_clone", // FIXME: remove when Iterator::copied stabilizes (1.36.0)
    "clippy::needless_pass_by_value",
    "clippy::nonminimal_bool",
    "clippy::redundant_pattern_matching",
  ];

  run(
    &format!(
      "rustup run {} -- cargo clippy --all-features --all-targets -- -A {}",
      TOOLCHAIN,
      allowed_lints.join(" -A ")
    ),
    ".",
  )?;

  Ok(())
}

pub fn install_clippy() -> Result<()> {
  run(&format!("rustup install {}", TOOLCHAIN), ".")?;
  run(
    &format!("rustup component add clippy --toolchain {}", TOOLCHAIN),
    ".",
  )
}

fn do_run<F>(cmdline: &str, dir: &str, mut f: F) -> Result<Output>
where
  F: FnMut(&mut Command),
{
  eprintln!("\nwill run: {}", cmdline);
  let proj_dir = project_root().join(dir);
  let mut args = cmdline.split_whitespace();
  let exec = args.next().unwrap();
  let mut cmd = Command::new(exec);
  f(cmd
    .args(args)
    .current_dir(proj_dir)
    .stderr(Stdio::inherit()));
  let output = cmd.output()?;
  if !output.status.success() {
    return Err(format!("`{}` exited with {}", cmdline, output.status).into());
  }

  Ok(output)
}
