extern crate lyken;

use std::env;
use std::path::PathBuf;
use std::fs::File;
use std::io::prelude::*;
use lyken::dart::parse::Parser;
use lyken::dart::print::Printer;
use lyken::dsl::lower::Lowerer;

fn main() {
    let path = PathBuf::from(env::args().nth(1).unwrap());
    match Parser::with_file(&path, |mut p| p.dsl_items()) {
        Ok(items) => {
            let mut out = File::create(path.with_extension("dart")).unwrap();
            let code = Lowerer::new().lower_items(&items);
            let result = Printer::new().dart_items(&code);
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
