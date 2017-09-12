#![feature(catch_expr, coerce_unsized, conservative_impl_trait, rustc_private, unsize)]

#[macro_use]
extern crate enum_primitive;
#[macro_use]
extern crate error_chain;
extern crate syntax;
extern crate unicode_width;
extern crate unicode_xid;
extern crate url;

use syntax::codemap::{BytePos, CodeMap, FilePathMapping, Span, NO_EXPANSION, SPAN_DEBUG};
use std::rc::Rc;

#[macro_use]
pub mod node;

pub mod dart {
    pub mod ast;
    pub mod lex;
    pub mod parse;
    pub mod print;
    pub mod resolve;
    pub mod sdk;
    pub mod visit;
}

pub mod dsl {
    pub mod ast;
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

// FIXME replace Span fields with methods.
#[allow(deprecated)]
pub fn mk_sp(lo: BytePos, hi: BytePos) -> Span {
    Span {
        lo,
        hi,
        ctxt: NO_EXPANSION,
    }
}
