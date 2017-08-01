extern crate walkdir;
extern crate difference;
extern crate lyken;

use walkdir::WalkDir;
use std::fs::File;
use std::io::prelude::*;
use difference::Changeset;
use lyken::dart::lex::Lexer;
use std::env;
use std::fmt::Write;

fn main() {
    let mut src = String::new();
    for entry in WalkDir::new(env::args().nth(1).unwrap()) {
        let entry = entry.unwrap();
        if entry.path().extension().map_or(false, |x| x == "dart") {
            let mut file = File::open(entry.path()).unwrap();
            src.clear();
            file.read_to_string(&mut src).unwrap();
            match Lexer::new(&src).tokenize() {
                Ok(tokens) => {
                    let mut result = String::new();
                    for token in tokens {
                        write!(result, "{}", token).unwrap();
                    }
                    if result != src {
                        let diff = Changeset::new(&src, &result, " ");
                        println!("{} {}", entry.path().display(), diff);
                    }
                }
                Err(error) => {
                    println!("{}:{} {:?}", entry.path().display(), error.line, error.err);
                }
            }
        }
    }
}
