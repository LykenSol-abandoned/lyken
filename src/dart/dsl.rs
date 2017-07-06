pub enum Item {
    ComponentDef(String, Vec<ComponentPart>),
    VerbatimDart(String),
}

pub enum ComponentPart {
    Instance(Instance),
    Field(String, Option<Type>, Option<Expr>),
}

pub struct Instance {
    pub name: String,
    pub fields: Vec<Field>,
}

pub struct Field {
    pub name: String,
    pub value: Expr,
}

pub struct Type {
    pub dart: String,
}

pub enum Expr {
    Instance(Instance),
    VerbatimDart(String),
}
