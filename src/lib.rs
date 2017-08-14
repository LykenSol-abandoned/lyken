#![feature(conservative_impl_trait, rustc_private)]

#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate enum_primitive;
extern crate unicode_xid;
extern crate syntax;

use syntax::codemap::{CodeMap, FilePathMapping, Span, BytePos, SPAN_DEBUG, NO_EXPANSION};
use std::rc::Rc;

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

pub fn codemap() -> Rc<CodeMap> {
    thread_local!(static CODEMAP: Rc<CodeMap> = {
        SPAN_DEBUG.with(|d| d.set(|span, f| {
            write!(f, "{}", codemap().span_to_string(span))
        }));

        Rc::new(CodeMap::new(FilePathMapping::empty()))
    });
    CODEMAP.with(|c| c.clone())
}

pub fn mk_sp(lo: BytePos, hi: BytePos) -> Span {
    Span {
        lo,
        hi,
        ctxt: NO_EXPANSION,
    }
}
