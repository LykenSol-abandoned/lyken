extern crate walkdir;
extern crate lyken;

use lyken::dart::ast::{Item, Module};
use walkdir::WalkDir;
use lyken::dart::resolve;
use std::env;

fn main() {
    'outer: for entry in WalkDir::new(env::args().nth(1).unwrap()) {
        let entry = entry.unwrap();
        if entry.path().extension().map_or(false, |x| x == "dart") {
            let module = Module::load(&entry.path());
            for item in &module.items {
                if let Item::PartOf { .. } = **item {
                    continue 'outer;
                }
            }
            resolve::resolve(module)
        }
    }
}
