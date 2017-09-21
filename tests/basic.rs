extern crate walkdir;

use walkdir::WalkDir;
use std::process::{Command, Stdio};

#[test]
fn all_dsl() {
    Command::new("cargo")
        .args(&["build", "--examples", "--release"])
        .stdout(Stdio::null())
        .status()
        .unwrap();

    for entry in WalkDir::new("tests") {
        let entry = entry.unwrap();
        if entry.path().extension().map_or(false, |x| x == "lyk") {
            let status = Command::new("target/release/examples/dsl-lower")
                .arg(entry.path())
                .status()
                .unwrap();
            if !status.success() {
                std::process::exit(1);
            }
        }
    }
    let status = Command::new("target/release/examples/dart-lex")
        .arg("tests")
        .status()
        .unwrap();
    if !status.success() {
        std::process::exit(1);
    }
}
