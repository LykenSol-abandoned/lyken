use syntax::symbol::Symbol;

pub enum Item {
    ComponentDef(Symbol, Vec<FieldDef>, Instance),
    Verbatim(Language, String),
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
    Verbatim(Language, String),
}

pub enum Expr {
    Instance(Instance),
    Array(Vec<Expr>),
    Verbatim(Language, String),
}

pub enum Language {
    Dart
}
