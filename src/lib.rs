#![feature(conservative_impl_trait, rustc_private)]

#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate enum_primitive;
extern crate unicode_width;
extern crate unicode_xid;
extern crate syntax;

pub mod dart {
    pub mod ast;
    pub mod codegen;
    pub mod lex;
    pub mod parse;
}

pub mod dsl {
    pub mod ast;
    pub mod parse;
}
