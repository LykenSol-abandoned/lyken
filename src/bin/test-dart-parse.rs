#![feature(rustc_private)]

extern crate walkdir;
extern crate lyken;

use walkdir::WalkDir;
use lyken::dart::lex::Lexer;
use lyken::dart::parse::{self, Parser};
use std::env;

fn main() {
    for entry in WalkDir::new(env::args().nth(1).unwrap()) {
        let entry = entry.unwrap();
        if entry.path().extension().map_or(false, |x| x == "dart") {
            let file = lyken::codemap().load_file(&entry.path()).unwrap();
            match Lexer::new(lyken::mk_sp(file.start_pos, file.end_pos)).tokenize() {
                Ok(tokens) => match Parser::new(tokens.iter().cloned()).parse_items() {
                    Ok(_) => {}
                    Err(parse::Error(parse::ErrorKind::ExpectedAt { expected, span }, state)) => {
                        println!("{:?}: expected {:?}", span, expected);
                        if let Some(ref backtrace) = state.backtrace {
                            println!("{:?}", backtrace);
                        }
                    }
                    Err(error) => {
                        println!("{}: {:?}", entry.path().display(), error);
                    }
                },
                Err(error) => {
                    println!("{:?}: {:?}", error.span, error.err);
                }
            }
        }
    }
}
