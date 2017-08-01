extern crate walkdir;

use walkdir::WalkDir;
use std::process::Command;

#[test]
fn all_dsl() {
    for entry in WalkDir::new("tests") {
        let entry = entry.unwrap();
        if entry.path().extension().map_or(false, |x| x == "lyk") {
            Command::new("cargo")
                .args(&["run", "--bin", "test-dsl"])
                .arg(entry.path())
                .status()
                .unwrap();
        }
    }
    Command::new("cargo")
        .args(&["run", "--bin", "test-dart-lex", "tests"])
        .status()
        .unwrap();
}
