extern crate lyken;

use std::env;
use std::path::PathBuf;
use std::fs::File;
use std::io::prelude::*;
use lyken::dart::parse::Parser;
use lyken::dart::print::Printer;
use lyken::dsl::lower::Lowerer;
use lyken::dsl::resolve;

fn main() { lyken::with_globals(lyken_main) }
fn lyken_main() {
    let path = PathBuf::from(env::args().nth(1).unwrap());
    match Parser::with_file(&path, |mut p| p.dsl_items()) {
        Ok(items) => {
            resolve::resolve(&items, true);
            let code = Lowerer::new().lower_items(&items);
            let result = Printer::new().dart_items(&code);
            File::create(path.with_extension("dart"))
                .unwrap()
                .write_all(result.as_bytes())
                .unwrap();
        }
        Err(error) => {
            println!("{}", error);
            std::process::exit(1);
        }
    }
}
