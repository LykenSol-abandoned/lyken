#![feature(rustc_private)]

extern crate lyken;

use std::env;
use std::path::PathBuf;
use std::fs::File;
use std::io::prelude::*;
use lyken::dart::lex::Lexer;
use lyken::dart::parse::{Error, ErrorKind, Parser};
use lyken::dart::codegen::Codegen;
use lyken::dart::print::Printer;

fn main() {
    let path = PathBuf::from(env::args().nth(1).unwrap());
    let file = lyken::codemap().load_file(&path).unwrap();
    match Lexer::new(lyken::mk_sp(file.start_pos, file.end_pos)).tokenize() {
        Ok(tokens) => match Parser::new(tokens.iter().cloned()).dsl_items() {
            Ok(items) => {
                let mut out = File::create(path.with_extension("dart")).unwrap();
                let code = Codegen::new().codegen_items(&items);
                let result = Printer::new().dart_items(&code);
                for token in result {
                    write!(out, "{}", token).unwrap();
                }
            }
            Err(Error(ErrorKind::ExpectedAt { expected, span }, state)) => {
                println!("{:?}: expected {:?}", span, expected);
                if let Some(ref backtrace) = state.backtrace {
                    println!("{:?}", backtrace);
                }
                std::process::exit(1);
            }
            Err(error) => {
                println!("{}: {:?}", path.display(), error);
                std::process::exit(1);
            }
        },
        Err(error) => {
            println!("{:?}: {:?}", error.span, error.err);
            std::process::exit(1);
        }
    }
}
