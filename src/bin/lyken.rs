#[macro_use]
extern crate clap;
extern crate lyken;
extern crate mktemp;

use mktemp::Temp;
use std::path::PathBuf;
use std::process::{self, Stdio};
use std::fs::{self, File};
use std::io::prelude::*;
use lyken::dart::parse::Parser;
use lyken::dart::print::Printer;
use lyken::dsl::lower::Lowerer;

fn main() {
    let matches = clap_app!(lyken =>
        (@subcommand reformat =>
            (@arg FILE: +required)
        )
        (@subcommand run =>
            (@arg FILE: +required)
        )
    ).get_matches();

    if let Some(matches) = matches.subcommand_matches("reformat") {
        let path = PathBuf::from(matches.value_of("FILE").unwrap());
        match Parser::with_file(&path, |mut p| p.dsl_items()) {
            Ok(items) => {
                let result = Printer::new().dsl_items(&items);
                File::create(path)
                    .unwrap()
                    .write_all(result.as_bytes())
                    .unwrap();
            }
            Err(error) => {
                println!("{}", error);
                std::process::exit(1);
            }
        }
    } else if let Some(matches) = matches.subcommand_matches("run") {
        let path = PathBuf::from(matches.value_of("FILE").unwrap());
        let temp_dir = Temp::new_dir().unwrap();
        let temp_dir = temp_dir.as_ref().join(path.file_stem().unwrap());
        fs::create_dir(&temp_dir).unwrap();
        lyken::dart::sdk::with_cmd(|cmd| {
            let status = cmd.arg("flutter create .")
                .current_dir(&temp_dir)
                .stdout(Stdio::null())
                .status()
                .unwrap();
            if !status.success() {
                process::exit(1);
            }
        });
        match Parser::with_file(&path, |mut p| p.dsl_items()) {
            Ok(items) => {
                let code = Lowerer::new().lower_items(&items);
                let result = Printer::new().dart_items(&code);
                File::create(temp_dir.join("lib/main.dart"))
                    .unwrap()
                    .write_all(result.as_bytes())
                    .unwrap();
            }
            Err(error) => {
                println!("{}", error);
                std::process::exit(1);
            }
        }
        lyken::dart::sdk::with_cmd(|cmd| {
            let status = cmd.arg("flutter run")
                .current_dir(&temp_dir)
                .status()
                .unwrap();
            if !status.success() {
                process::exit(1);
            }
        });
    }
}
