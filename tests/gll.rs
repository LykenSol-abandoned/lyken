extern crate lyken;

use lyken::gll::{Candidate, Label, ParseNode, Parser, StackNode};
use std::fmt;
use std::fs::File;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum Gamma0 {
    _0,
    _1,
    _2,
    _3,
    _4,
    _5,
    _6,
    _7,
    _8,
    _9,
}

macro_rules! L {
    ("L₀") => (Gamma0::_0);
    ("A") => (Gamma0::_1);
    ("A ::= ·'a'A'b'") => (Gamma0::_2);
    ("A ::= 'a'A·'b'") => (Gamma0::_3);
    ("A ::= 'a'A'b'·") => (Gamma0::_4);
    ("A ::= ·'a'A'c'") => (Gamma0::_5);
    ("A ::= 'a'A·'c'") => (Gamma0::_6);
    ("A ::= 'a'A'c'·") => (Gamma0::_7);
    ("A ::= ·'a'") => (Gamma0::_8);
    ("A ::= 'a'·") => (Gamma0::_9);
}

impl Default for Gamma0 {
    fn default() -> Gamma0 {
        L!("L₀")
    }
}

impl fmt::Display for Gamma0 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            L!("L₀") => "L₀",
            L!("A") => "A",
            L!("A ::= ·'a'A'b'") => "A ::= ·'a'A'b'",
            L!("A ::= 'a'A·'b'") => "A ::= 'a'A·'b'",
            L!("A ::= 'a'A'b'·") => "A ::= 'a'A'b'·",
            L!("A ::= ·'a'A'c'") => "A ::= ·'a'A'c'",
            L!("A ::= 'a'A·'c'") => "A ::= 'a'A·'c'",
            L!("A ::= 'a'A'c'·") => "A ::= 'a'A'c'·",
            L!("A ::= ·'a'") => "A ::= ·'a'",
            L!("A ::= 'a'·") => "A ::= 'a'·",
        };
        write!(f, "{}", s)
    }
}

impl Label for Gamma0 {
    fn nonterminal_before_dot(&self) -> Option<Gamma0> {
        match *self {
            L!("A ::= 'a'A·'b'") | L!("A ::= 'a'A·'c'") => Some(L!("A")),
            _ => None,
        }
    }
    fn dot_at_end(&self) -> bool {
        match *self {
            L!("A ::= 'a'A'b'·") | L!("A ::= 'a'A'c'·") | L!("A ::= 'a'·") => true,
            _ => false,
        }
    }
    fn parent_nonterminal(&self) -> Gamma0 {
        match *self {
            L!("L₀") => L!("L₀"),
            L!("A") |
            L!("A ::= ·'a'A'b'") |
            L!("A ::= 'a'A·'b'") |
            L!("A ::= 'a'A'b'·") |
            L!("A ::= ·'a'A'c'") |
            L!("A ::= 'a'A·'c'") |
            L!("A ::= 'a'A'c'·") |
            L!("A ::= ·'a'") |
            L!("A ::= 'a'·") => L!("A"),
        }
    }
    fn parse(p: &mut Parser<Gamma0>, input: &[u8]) -> Result<(), ()> {
        let mut c = Candidate {
            l: L!("A"),
            u: StackNode { l: L!("A"), i: 0 },
            i: 0,
            w: ParseNode::DUMMY,
        };
        loop {
            match c.l {
                L!("L₀") => if let Some(next) = p.candidates.remove() {
                    c = next;
                } else if p.sppf.children.contains_key(&ParseNode {
                    l: Some(L!("A")),
                    i: 0,
                    j: input.len(),
                }) {
                    return Ok(());
                } else {
                    return Err(());
                },
                L!("A") => {
                    p.candidates
                        .add(L!("A ::= ·'a'A'b'"), c.u, c.i, ParseNode::DUMMY);
                    p.candidates
                        .add(L!("A ::= ·'a'A'c'"), c.u, c.i, ParseNode::DUMMY);
                    p.candidates
                        .add(L!("A ::= ·'a'"), c.u, c.i, ParseNode::DUMMY);
                    c.l = L!("L₀");
                }
                L!("A ::= ·'a'A'b'") => if input.get(c.i) == Some(&b'a') {
                    c.w = ParseNode::terminal(c.i, c.i + 1);
                    c.i += 1;
                    c.u = p.create(L!("A ::= 'a'A·'b'"), c.u, c.i, c.w);
                    c.l = L!("A");
                } else {
                    c.l = L!("L₀");
                },
                L!("A ::= 'a'A·'b'") => if input.get(c.i) == Some(&b'b') {
                    let c_r = ParseNode::terminal(c.i, c.i + 1);
                    c.i += 1;
                    c.w = p.sppf.add_packed(L!("A ::= 'a'A'b'·"), c.w, c_r);
                    c.l = L!("A ::= 'a'A'b'·");
                } else {
                    c.l = L!("L₀");
                },
                L!("A ::= 'a'A'b'·") => {
                    p.pop(c.u, c.i, c.w);
                    c.l = L!("L₀");
                }
                L!("A ::= ·'a'A'c'") => if input.get(c.i) == Some(&b'a') {
                    c.w = ParseNode::terminal(c.i, c.i + 1);
                    c.i += 1;
                    c.u = p.create(L!("A ::= 'a'A·'c'"), c.u, c.i, c.w);
                    c.l = L!("A");
                } else {
                    c.l = L!("L₀");
                },
                L!("A ::= 'a'A·'c'") => if input.get(c.i) == Some(&b'c') {
                    let c_r = ParseNode::terminal(c.i, c.i + 1);
                    c.i += 1;
                    c.w = p.sppf.add_packed(L!("A ::= 'a'A'c'·"), c.w, c_r);
                    c.l = L!("A ::= 'a'A'c'·")
                } else {
                    c.l = L!("L₀");
                },
                L!("A ::= 'a'A'c'·") => {
                    p.pop(c.u, c.i, c.w);
                    c.l = L!("L₀");
                }
                L!("A ::= ·'a'") => if input.get(c.i) == Some(&b'a') {
                    let c_r = ParseNode::terminal(c.i, c.i + 1);
                    c.i += 1;
                    c.w = p.sppf.add_packed(L!("A ::= 'a'·"), c.w, c_r);
                    c.l = L!("A ::= 'a'·");
                } else {
                    c.l = L!("L₀");
                },
                L!("A ::= 'a'·") => {
                    p.pop(c.u, c.i, c.w);
                    c.l = L!("L₀");
                }
            }
        }
    }
}

#[test]
fn gamma_0() {
    let mut parser = Parser::default();
    let r = Gamma0::parse(&mut parser, b"aac");
    parser
        .gss
        .print(&mut File::create("target/gss.dot").unwrap())
        .unwrap();
    parser
        .sppf
        .print(&mut File::create("target/sppf.dot").unwrap())
        .unwrap();
    r.unwrap()
}
