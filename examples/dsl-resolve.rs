extern crate lyken;
extern crate walkdir;

use lyken::dart::parse::Parser;
use walkdir::WalkDir;
use lyken::dsl::resolve;
use std::env;

fn main() { lyken::with_globals(lyken_main) }
fn lyken_main() {
    for entry in WalkDir::new(env::args().nth(1).unwrap()) {
        let entry = entry.unwrap();
        if entry.path().extension().map_or(false, |x| x == "lyk") {
            match Parser::with_file(entry.path(), |mut p| p.dsl_items()) {
                Ok(items) => resolve::resolve(&items, true),
                Err(error) => {
                    println!("{}", error);
                }
            }
        }
    }
}
