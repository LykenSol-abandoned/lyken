#![feature(catch_expr, coerce_unsized, conservative_impl_trait, rustc_private, unsize,
          thread_local_state)]

#[macro_use]
extern crate enum_primitive;
#[macro_use]
extern crate error_chain;
extern crate git2;
extern crate syntax;
extern crate unicode_width;
extern crate unicode_xid;
extern crate url;
extern crate xdg;

use syntax::codemap::{self, BytePos, CodeMap, FilePathMapping, NO_EXPANSION, SPAN_DEBUG};
use std::rc::Rc;
use std::fmt;

#[macro_use]
pub mod node;

pub mod dart {
    pub mod ast;
    pub mod fold;
    pub mod lex;
    pub mod parse;
    pub mod print;
    pub mod resolve;
    pub mod sdk;
    pub mod visit;
}

pub mod dsl {
    pub mod ast;
    pub mod fold;
    pub mod lift;
    pub mod lower;
    pub mod parse;
    pub mod print;
    pub mod resolve;
    pub mod visit;
}

pub fn codemap() -> Rc<CodeMap> {
    thread_local!(static CODEMAP: Rc<CodeMap> = {
        SPAN_DEBUG.with(|d| d.set(|span, f| {
            write!(f, "{}", codemap().span_to_string(span))
        }));

        Rc::new(CodeMap::new(FilePathMapping::empty()))
    });
    CODEMAP.with(|c| c.clone())
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Span {
    pub lo: BytePos,
    pub hi: BytePos,
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.to_span())
    }
}
impl Span {
    pub fn to_span(&self) -> codemap::Span {
        codemap::Span::new(self.lo, self.hi, NO_EXPANSION)
    }
}

pub fn mk_sp(lo: BytePos, hi: BytePos) -> Span {
    Span { lo, hi }
}
