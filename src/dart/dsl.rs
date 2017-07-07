use syntax::symbol::Symbol;

pub enum Item {
    ComponentDef(Symbol, Vec<FieldDef>, Instance),
    VerbatimDart(String),
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

pub struct Type {
    pub dart: String,
}

pub enum Expr {
    Instance(Instance),
    VerbatimDart(String),
}
