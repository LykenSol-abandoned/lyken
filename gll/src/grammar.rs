use std::io::{self, Write};
use std::fmt;
use std::iter;
use std::slice;
use ordermap::OrderMap;
use syntax::symbol::Symbol;

pub struct Grammar<A> {
    rules: OrderMap<Symbol, Rule<A>>,
}

impl<A> Grammar<A> {
    pub fn new() -> Self {
        Grammar {
            rules: OrderMap::new(),
        }
    }
    pub fn add_rule(&mut self, name: &str, rule: Rule<A>) {
        self.rules.insert(Symbol::from(name), rule);
    }
}

pub struct Rule<A> {
    label: Label,
    kind: RuleKind<A>,
}

pub enum RuleKind<A> {
    Sequence(Sequence<A>),
    Alternation(Label, Vec<Symbol>),
}

impl<A> Rule<A> {
    pub fn sequence(seq: Sequence<A>) -> Self {
        Rule {
            label: Label::empty(),
            kind: RuleKind::Sequence(seq),
        }
    }
    pub fn alternation(rules: &[&str]) -> Self {
        Rule {
            label: Label::empty(),
            kind: RuleKind::Alternation(Label::empty(), rules.iter().map(|&r| Symbol::from(r)).collect()),
        }
    }
    fn start_label(&self) -> Label {
        match self.kind {
            RuleKind::Sequence(ref seq) => if seq.units.is_empty() {
                self.label
            } else {
                seq.labels_before[0]
            },
            RuleKind::Alternation(l, _) => l,
        }
    }
}

pub struct Sequence<A> {
    units: Vec<Unit<A>>,
    labels_before: Vec<Label>,
}

impl<A> Sequence<A> {
    pub fn new(units: Vec<Unit<A>>) -> Self {
        Sequence {
            units,
            labels_before: vec![],
        }
    }
}

pub enum Unit<A> {
    Atom(A),
    Rule(Symbol),
}

impl<A> Unit<A> {
    pub fn rule(r: &str) -> Self {
        Unit::Rule(Symbol::from(r))
    }
}

pub trait Atom {
    fn to_label_description(&self) -> String;
    fn to_rust_slice(&self) -> String;
}

impl Atom for str {
    fn to_label_description(&self) -> String {
        format!("'{}'", self.escape_default())
    }
    fn to_rust_slice(&self) -> String {
        format!("{:?}", self)
    }
}

impl Atom for char {
    fn to_label_description(&self) -> String {
        self.to_string().to_label_description()
    }
    fn to_rust_slice(&self) -> String {
        self.to_string().to_rust_slice()
    }
}

#[derive(Copy, Clone)]
pub struct Label {
    pub description: Symbol,
}

impl Label {
    fn new(s: &str) -> Label {
        Label {
            description: Symbol::from(s),
        }
    }
    fn empty() -> Label {
        Label::new("")
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, r#"L!("{}")"#, self.description)
    }
}

pub macro grammar {
    (@unit $rule:ident) => {
        Unit::rule(stringify!($rule))
    },
    (@unit $atom:expr) => {
        Unit::Atom($atom)
    },
    ($($rule_name:ident =
        $($arm_name:ident { $($unit:tt)* })|+;
    )*) => ({
        let mut grammar = Grammar::new();
        $(
            grammar.add_rule(stringify!($rule_name),
                Rule::alternation(&[$(stringify!($arm_name)),*]));
            $(grammar.add_rule(stringify!($arm_name),
                Rule::sequence(Sequence::new(vec![$(grammar!(@unit $unit)),*])));)*
        )*
        grammar
    })
}

impl<A: Atom> Grammar<A> {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    pub fn generate(&mut self, out: &mut Write) -> io::Result<()> {
        macro_rules! put {
            ($($x:expr),*) => ({
                $(write!(out, "{}", $x)?;)*
            })
        }
        for (rule_name, rule) in &mut self.rules {
            rule.label = Label::new(&rule_name.as_str());
            match rule.kind {
                RuleKind::Sequence(ref mut seq) => {
                    for i in 0..seq.units.len() {
                        let mut s = format!("{} ::=", rule_name);
                        for (j, unit) in seq.units.iter().enumerate() {
                            if i == j {
                                s.push('·');
                            } else {
                                s.push(' ');
                            }
                            match *unit {
                                Unit::Atom(ref a) => {
                                    s.push_str(&a.to_label_description());
                                }
                                Unit::Rule(r) => {
                                    s.push_str(&r.as_str());
                                }
                            }
                        }
                        seq.labels_before.push(Label::new(&s));
                    }
                }
                RuleKind::Alternation(ref mut start_label, _) => {
                    *start_label = Label::new(&format!("{} ::=·", rule_name));
                }
            }
        }
        let labels: Vec<_> = self.rules.values().flat_map(|rule| {
            let labels = match rule.kind {
                RuleKind::Sequence(ref seq) => &seq.labels_before[..],
                RuleKind::Alternation(ref l, _) => slice::from_ref(l),
            };
            iter::once(rule.label).chain(labels.iter().cloned())
        })
        .collect();

        put!("extern crate lyken_gll;

use self::lyken_gll::{Call, Label, ParseNode};
use std::fmt;
use std::ops::Range;

pub type Parser<'a> = lyken_gll::Parser<_L, &'a str>;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum _L {");
        for i in 0..labels.len() {
            put!(
                "
    _", i, ",");
        }
        put!("
}

macro_rules! L {");
        for (i, l) in labels.iter().enumerate() {
            put!("
    (\"", l.description, "\") => (_L::_", i, ");");
        }
        put!("
}

impl fmt::Display for _L {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {");

        for l in labels.iter() {
            put!("
            ", l, " => \"", l.description, "\",");
        }
        put!("
        };
        write!(f, \"{}\", s)
    }
}

impl Label for _L {}");
        for (name, rule) in &self.rules {
            put!("

pub struct ", name, " {
    pub span: Range<usize>,
}

impl ", name, " {
    pub fn parse(p: &mut Parser) -> Result<Self, ()> {
        let call = Call {
            l: ", rule.start_label(), ",
            i: 0
        };
        p.candidates.add(", rule.start_label(), ", call, 0, ParseNode::DUMMY);
        parse(p);
        if let Some(results) = p.sppf.results.get(&call) {
            if let Some(r) = results.iter().rev().next() {
                return Ok(Self {
                    span: r.i..r.j
                });
            }
        }
        Err(())
    }
}");
        }
        put!("
fn parse(p: &mut Parser) {
    while let Some(mut c) = p.candidates.remove() {
        match c.l {");
        for rule in self.rules.values() {
            match rule.kind {
                RuleKind::Alternation(start_label, ref rules) => {
                    put!("
            ", start_label, " => {");
                    for r in rules {
                        put!("
                c.l = ", self.rules[r].start_label(), ";
                c.w = ParseNode::DUMMY;
                p.call(c, ", rule.label, ");")
                    }
                    put!("
            }
            ", rule.label, " => {
                p.ret(c.u, c.i, c.w);
            }");
                }
                RuleKind::Sequence(ref seq) => {
                    for (i, unit) in seq.units.iter().enumerate() {
                        let next_label = if i == seq.units.len() - 1 {
                            rule.label
                        } else {
                            seq.labels_before[i + 1]
                        };
                        match *unit {
                            Unit::Rule(r) => put!("
            ", seq.labels_before[i], " => {
                c.l = ", self.rules[&r].start_label(), ";
                p.call(c, ", next_label, ");
            }"),
                            Unit::Atom(ref a) => {
                                let a = a.to_rust_slice();
                                put!("
            ", seq.labels_before[i], " => if p.input[c.i..].starts_with(", a, ") {
                let j = c.i + ", a, ".len();
                c.w = p.sppf.add_children(", next_label, ", c.w, ParseNode::terminal(c.i, j));
                p.candidates.add(", next_label, ", c.u, j, c.w);
            },")
                            }
                        }
                    }

                    put!("
            ", rule.label, " => {");
                    if seq.units.is_empty() {
                        put!("
                c.w = p.sppf.add_children(", rule.label, ", ParseNode::DUMMY, ParseNode::terminal(c.i, c.i));");
                    }
                    put!("
                p.ret(c.u, c.i, c.w);
            }");
                }
            }
        }

        put!("
        }
    }
}
");
        Ok(())
    }
}
