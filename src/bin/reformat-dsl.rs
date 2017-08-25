extern crate lyken;

use std::env;
use std::path::PathBuf;
use std::fs::File;
use std::io::prelude::*;
use lyken::dart::parse::Parser;
use lyken::dart::print::Printer;

fn main() {
    let path = PathBuf::from(env::args().nth(1).unwrap());
    match Parser::with_file(&path, |mut p| p.dsl_items()) {
        Ok(items) => {
            let mut out = File::create(path).unwrap();
            let result = Printer::new().dsl_items(&items);
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
