use syntax::symbol::Symbol;
use dart::lex::Token;

pub enum Item {
    ComponentDef(Symbol, Vec<FieldDef>, Instance),
    Verbatim(Language, Vec<Token>),
}

pub struct Instance {
    pub name: Symbol,
    pub fields: Vec<Field>,
}

pub struct FieldDef {
    pub name: Symbol,
    pub ty: Option<Type>,
    pub default: Option<Expr>,
}

pub struct Field {
    pub name: Symbol,
    pub value: Expr,
}

pub enum Type {
    Verbatim(Language, Vec<Token>),
}

pub enum Expr {
    Instance(Instance),
    Array(Vec<Expr>),
    Verbatim(Language, Vec<Token>),
}

pub enum Language {
    Dart,
}
