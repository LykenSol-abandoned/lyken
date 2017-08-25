extern crate lyken;

use std::env;
use std::path::PathBuf;
use std::fs::File;
use std::io::prelude::*;
use lyken::dart::parse::Parser;
use lyken::dart::print::Printer;
use lyken::dsl::lift::Lifter;

fn main() {
    let path = PathBuf::from(env::args().nth(1).unwrap());
    match Parser::with_file(&path, |mut p| p.dart_items()) {
        Ok(items) => {
            let mut out = File::create(path.with_extension("lyk")).unwrap();
            let code = Lifter::new().lift_items(&items);
            let result = Printer::new().dsl_items(&code);
            for token in result {
                write!(out, "{}", token).unwrap();
            }
        }
        Err(error) => {
            println!("{}", error);
            std::process::exit(1);
        }
    }
}
