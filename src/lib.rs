#![feature(rustc_private)]

extern crate unicode_xid;
extern crate syntax;

pub mod dart {
    pub mod codegen;
    pub mod lex;
    pub mod parse;
}

pub mod dsl;