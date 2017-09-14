extern crate walkdir;
extern crate lyken;

use walkdir::WalkDir;
use lyken::dart::ast::Module;
use std::env;

fn main() {
    for entry in WalkDir::new(env::args().nth(1).unwrap()) {
        let entry = entry.unwrap();
        if entry.path().extension().map_or(false, |x| x == "dart") {
            Module::load(&entry.path());
        }
    }
}
