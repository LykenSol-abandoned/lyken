#![feature(rustc_private)]

extern crate walkdir;
extern crate difference;
extern crate lyken;

use walkdir::WalkDir;
use difference::Changeset;
use lyken::dart::lex::Lexer;
use std::env;
use std::fmt::Write;

fn main() {
    for entry in WalkDir::new(env::args().nth(1).unwrap()) {
        let entry = entry.unwrap();
        if entry.path().extension().map_or(false, |x| x == "dart") {
            let file = lyken::codemap().load_file(&entry.path()).unwrap();
            let src = file.src.clone().unwrap();
            match Lexer::new(lyken::mk_sp(file.start_pos, file.end_pos)).tokenize() {
                Ok(tokens) => {
                    let mut result = String::new();
                    for (_, token) in tokens {
                        write!(result, "{}", token).unwrap();
                    }
                    if result != *src {
                        let diff = Changeset::new(&src, &result, " ");
                        println!("{} {}", entry.path().display(), diff);
                    }
                }
                Err(error) => {
                    println!("{:?}: {:?}", error.span, error.err);
                }
            }
        }
    }
}
