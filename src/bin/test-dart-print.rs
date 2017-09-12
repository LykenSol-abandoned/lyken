extern crate lyken;
extern crate walkdir;

use std::env;
use lyken::dart::lex::{Lexer, Token};
use lyken::dart::parse::Parser;
use lyken::dart::print::Printer;
use walkdir::WalkDir;

fn main() {
    for entry in WalkDir::new(env::args().nth(1).unwrap()) {
        let entry = entry.unwrap();
        if entry.path().extension().map_or(false, |x| x == "dart") {
            let tokens = Lexer::from_file(&entry.path()).unwrap().tokenize().unwrap();
            match Parser::new(entry.path(), &tokens).dart_module() {
                Ok(module) => {
                    let result = Printer::new().dart_items(&module.items);
                    let mut old = tokens.iter().filter(|&&(_, token)| !token.is_whitespace());
                    let mut new = result.iter().filter(|token| !token.is_whitespace());
                    loop {
                        match (old.next(), new.next()) {
                            (
                                Some(&(_, Token::Punctuation(','))),
                                Some(&Token::Punctuation(')')),
                            ) |
                            (
                                Some(&(_, Token::Punctuation(','))),
                                Some(&Token::Punctuation(']')),
                            ) |
                            (
                                Some(&(_, Token::Punctuation(','))),
                                Some(&Token::Punctuation('}')),
                            ) => {
                                old.next();
                            }
                            (Some(&(span, old)), Some(new)) => {
                                if old.to_string() != new.to_string() {
                                    println!("{:?}: `{}` != `{}`", span, old, new);
                                    break;
                                }
                            }
                            (Some(&(span, old)), None) => {
                                println!("{:?}: `{}` missing", span, old);
                                break;
                            }
                            (None, Some(new)) => {
                                println!("{}: `{}` extraneous", entry.path().display(), new);
                                break;
                            }
                            (None, None) => break,
                        }
                    }
                }
                Err(error) => {
                    println!("{}", error);
                    std::process::exit(1);
                }
            }
        }
    }
}
