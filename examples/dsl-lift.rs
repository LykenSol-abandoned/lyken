extern crate lyken;

use std::env;
use std::path::PathBuf;
use std::fs::File;
use std::io::prelude::*;
use lyken::dart::ast::Module;
use lyken::dart::print::Printer;
use lyken::dart::resolve;
use lyken::dsl::lift::Lifter;

fn main() { lyken::with_globals(lyken_main) }
fn lyken_main() {
    let path = PathBuf::from(env::args().nth(1).unwrap());
    let module = Module::load(&path);
    resolve::resolve(module.clone(), true);
    let code = Lifter::new().lift_items(&module.items);
    let result = Printer::new().dsl_items(&code);
    File::create(path.with_extension("lyk"))
        .unwrap()
        .write_all(result.as_bytes())
        .unwrap();
    if module.has_error {
        std::process::exit(1);
    }
}
