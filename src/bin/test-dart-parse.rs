extern crate walkdir;
extern crate lyken;

use walkdir::WalkDir;
use std::fs::File;
use std::io::prelude::*;
use lyken::dart::lex::Lexer;
use lyken::dart::parse::{self, Parser};
use std::env;

fn main() {
    let mut src = String::new();
    for entry in WalkDir::new(env::args().nth(1).unwrap()) {
        let entry = entry.unwrap();
        if entry.path().extension().map_or(false, |x| x == "dart") {
            let mut file = File::open(&entry.path()).unwrap();
            src.clear();
            file.read_to_string(&mut src).unwrap();
            match Lexer::new(&src).tokenize() {
                Ok(tokens) => {
                    match Parser::new(tokens.iter().cloned()).parse_items() {
                        Ok(_) => {}
                        Err(parse::Error(parse::ErrorKind::ExpectedAt {
                                             expected,
                                             line,
                                             col,
                                         },
                                         state)) => {
                            println!(
                                "{}:{}:{}: expected {:?}",
                                entry.path().display(),
                                line,
                                col,
                                expected
                            );
                            if let Some(ref backtrace) = state.backtrace {
                                println!("{:?}", backtrace);
                            }
                        }
                        Err(error) => {
                            println!("{}: {:?}", entry.path().display(), error);
                        }
                    }
                }
                Err(error) => {
                    println!("{}:{} {:?}", entry.path().display(), error.line, error.err);
                }
            }
        }
    }
}
