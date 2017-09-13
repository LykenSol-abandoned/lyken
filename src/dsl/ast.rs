use syntax::symbol::Symbol;
use dart;
use node::Node;

#[derive(Debug)]
pub enum Item {
    ComponentDef {
        name: Symbol,
        fields: Vec<Node<FieldDef>>,
        dart_members: Vec<Node<dart::ast::ClassMember>>,
        body: Option<Node<Expr>>,
    },
    Dart(Node<dart::ast::Item>),
}

#[derive(Debug)]
pub struct FieldDef {
    pub mutable: bool,
    pub name: Symbol,
    pub ty: Option<Node<Type>>,
    pub default: Option<Node<Expr>>,
}

#[derive(Debug)]
pub enum Config {
    Field { name: Symbol, value: Node<Expr> },
    EventHandler {
        name: Symbol,
        block: Node<dart::ast::Statement>,
    },
}

#[derive(Debug)]
pub enum Type {
    Dart(Node<dart::ast::Type>),
}

#[derive(Debug)]
pub enum Expr {
    Instance {
        path: Node<dart::ast::Qualified>,
        unnamed: Vec<Node<Expr>>,
        config: Vec<Config>,
    },
    Array(Vec<Node<Expr>>),
    Dart(Node<dart::ast::Expr>),
}
