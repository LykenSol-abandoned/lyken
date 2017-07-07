extern crate lyken;

use std::fs::File;
use std::io::prelude::*;
use lyken::dart::lex::*;
use lyken::dart::parse::*;
use lyken::dart::codegen::*;
use std::env;
use std::path::PathBuf;

fn main() {
    let path = PathBuf::from(env::args().nth(1).unwrap());
    let mut file = File::open(&path).unwrap();
    let mut src = String::new();
    file.read_to_string(&mut src).unwrap();
    match Lexer::new(&src).tokenize() {
        Ok(tokens) => {
            let items = Parser::new(tokens.iter()).parse_items();
            let mut out = File::create(path.with_extension("dart")).unwrap();
            Codegen::new(&mut out).codegen_items(&items).unwrap();
        }
        Err(error) => {
            println!("{}:{} {:?}", path.display(), error.line, error.err);
        }
    }
}
