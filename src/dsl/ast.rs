use syntax::symbol::Symbol;
use dart;
use node::Node;

pub enum Item {
    ComponentDef {
        name: Symbol,
        fields: Vec<FieldDef>,
        dart_members: Vec<Node<dart::ast::ClassMember>>,
        body: Option<Expr>,
    },
    Dart(Node<dart::ast::Item>),
}

pub struct FieldDef {
    pub mutable: bool,
    pub name: Symbol,
    pub ty: Option<Type>,
    pub default: Option<Expr>,
}

pub struct Field {
    pub name: Symbol,
    pub value: Expr,
}

pub enum Type {
    Dart(Node<dart::ast::Type>),
}

pub enum Expr {
    Instance {
        name: Symbol,
        unnamed: Vec<Expr>,
        fields: Vec<Field>,
    },
    Array(Vec<Expr>),
    Dart(Node<dart::ast::Expr>),
}
