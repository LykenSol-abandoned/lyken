#![feature(decl_macro)]
#![recursion_limit="128"]

extern crate lyken_gll;

use lyken_gll::grammar::grammar;
use std::env;
use std::fs::File;
use std::path::PathBuf;

fn main() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    let mut dart = grammar!{
        Tokens = {
            {} |
            first:Token rest:Tokens
        };
        Token = {
            ws:WhiteSpace |
            comment:Comment |
            ident:Ident |
            punct:Punct
        };

        WhiteSpacePrefix = { { " " | "\t" | "\n" } { {} | WhiteSpacePrefix } };
        NotWhiteSpace = { !" " !"\t" !"\n" };
        WhiteSpace = { WhiteSpacePrefix NotWhiteSpace };

        IdentStart = { ('a'..='z') | ('A'..='Z') | "_" | "$" };
        IdentCont = { IdentStart | ('0'..='9') };
        NotIdent = { !('a'..='z') !('A'..='Z') !"_" !"$" !('0'..='9') };
        IdentPrefix = { IdentStart | IdentPrefix IdentCont };
        Ident = { IdentPrefix NotIdent };

        Comment = {
            "//" SingleLineCommentBody |
            MultiLineComment
        };
        SingleLineCommentBody = { "\n" | !"\n" (..) SingleLineCommentBody };
        MultiLineCommentBody = { {} | { !"/*" !"*/" (..) | MultiLineComment } MultiLineCommentBody };
        MultiLineComment = { "/*" MultiLineCommentBody "*/" };

        Punct = {
            "." | "," | ":" | ";" | "+" | "*" | "-" | { "/" !"/" !"*" } | "=" | "^" | "%" | "#" |
            "{" | "}" | "[" | "]" | "(" | ")" | "&" | "|" | "!" | "<" | ">" | "@" | "~" | "?"
        };

        StringLiteral = { Raw | Triple | Quote };
        Raw = { "r" };
        Quote = { "'" | "\"" };
        Triple = { "'''" | "\"\"\"" };

    };
    dart.generate(&mut File::create(&out_dir.join("parse.rs")).unwrap());
}
