#![feature(rustc_private)]

extern crate lyken;

use std::fs::File;
use std::io::prelude::*;
use lyken::dart::lex::*;
use lyken::dsl::parse::*;
use lyken::dart::codegen::*;
use std::env;
use std::path::PathBuf;

fn main() {
    let path = PathBuf::from(env::args().nth(1).unwrap());
    let file = lyken::codemap().load_file(&path).unwrap();
    match Lexer::new(lyken::mk_sp(file.start_pos, file.end_pos)).tokenize() {
        Ok(tokens) => match Parser::new(tokens.iter().cloned()).parse_items() {
            Ok(items) => {
                let mut out = File::create(path.with_extension("dart")).unwrap();
                let result = Codegen::new().codegen_items(&items);
                for token in result {
                    write!(out, "{}", token).unwrap();
                }
            }
            Err(Error(ErrorKind::ExpectedAt { expected, span }, state)) => {
                println!("{:?}: expected {:?}", span, expected);
                if let Some(ref backtrace) = state.backtrace {
                    println!("{:?}", backtrace);
                }
            }
            Err(error) => {
                println!("{}: {:?}", path.display(), error);
            }
        },
        Err(error) => {
            println!("{:?}: {:?}", error.span, error.err);
        }
    }
}
