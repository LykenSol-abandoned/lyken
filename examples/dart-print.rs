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
                    let fm = lyken::codemap().new_filemap(String::new(), result);
                    let result = Lexer::new(lyken::mk_sp(fm.start_pos, fm.end_pos))
                        .tokenize()
                        .unwrap();

                    fn whitespace_not_comment(token: &Token) -> bool {
                        match *token {
                            Token::WhiteSpace(_) => true,
                            _ => false,
                        }
                    }

                    let mut old = tokens
                        .iter()
                        .filter(|&&(_, token)| !whitespace_not_comment(&token));
                    let mut new = result
                        .iter()
                        .filter(|&&(_, token)| !whitespace_not_comment(&token));
                    loop {
                        let (old, new) = match (old.next(), new.next()) {
                            (
                                Some(&(_, Token::Punctuation(','))),
                                new @ Some(&(_, Token::Punctuation(')'))),
                            ) |
                            (
                                Some(&(_, Token::Punctuation(','))),
                                new @ Some(&(_, Token::Punctuation(']'))),
                            ) |
                            (
                                Some(&(_, Token::Punctuation(','))),
                                new @ Some(&(_, Token::Punctuation('}'))),
                            ) => (old.next(), new),
                            x => x,
                        };
                        match (old, new) {
                            (Some(&(span, old)), Some(&(_, new))) => {
                                if old.to_string() != new.to_string() {
                                    println!("{:?}: `{}` != `{}`", span, old, new);
                                    break;
                                }
                            }
                            (Some(&(span, old)), None) => {
                                println!("{:?}: `{}` missing", span, old);
                                break;
                            }
                            (None, Some(&(_, new))) => {
                                println!("{}: `{}` extraneous", entry.path().display(), new);
                                break;
                            }
                            (None, None) => break,
                        }
                    }
                }
                Err(error) => {
                    println!("{}", error);
                }
            }
        }
    }
}
