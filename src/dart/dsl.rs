pub enum Item {
    ComponentDef(String, Vec<ComponentPart>),
    VerbatimDart(String),
}

pub enum ComponentPart {
    Instance(String, Vec<Field>),
    Field(String, Option<Type>, Option<Expr>),
}

pub struct Field {
    pub name: String,
    pub value: Expr,
}

pub struct Type {
    pub dart: String,
}

pub struct Expr {
    pub dart: String,
}
