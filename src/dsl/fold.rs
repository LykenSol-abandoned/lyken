use dart;
use dart::fold::Fold as DartFold;
use dsl::ast::{Config, Expr, FieldDef, Item, Type};
use node::Node;

pub trait Folder: dart::fold::Folder {
    fn dsl_items(&mut self, items: &Vec<Node<Item>>) -> Vec<Node<Item>> {
        items.super_fold(self)
    }
    fn dsl_item(&mut self, item: Node<Item>) -> Node<Item> {
        item.super_fold(self)
    }
    fn dsl_field_def(&mut self, field_def: Node<FieldDef>) -> Node<FieldDef> {
        field_def.super_fold(self)
    }
    fn dsl_config(&mut self, field: &Config) -> Config {
        field.super_fold(self)
    }
    fn dsl_type(&mut self, ty: Node<Type>) -> Node<Type> {
        ty.super_fold(self)
    }
    fn dsl_expr(&mut self, expr: Node<Expr>) -> Node<Expr> {
        expr.super_fold(self)
    }
}

pub trait Fold {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self;
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self;
}

impl Fold for Vec<Node<Item>> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dsl_items(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        self.iter().map(|item| item.fold(folder)).collect()
    }
}

impl Fold for Node<Item> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dsl_item(self.clone())
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        match **self {
            Item::ComponentDef {
                name,
                ref fields,
                ref dart_members,
                ref body,
            } => Node::new(Item::ComponentDef {
                name,
                fields: fields.iter().map(|field| field.fold(folder)).collect(),
                dart_members: dart_members
                    .iter()
                    .map(|dart_member| dart_member.fold(folder))
                    .collect(),
                body: body.as_ref().map(|body| body.fold(folder)),
            }),
            Item::Dart(ref item) => Node::new(Item::Dart(item.fold(folder))),
        }
    }
}

impl Fold for Node<FieldDef> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dsl_field_def(self.clone())
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        let FieldDef {
            mutable,
            name,
            ref ty,
            ref default,
        } = **self;
        Node::new(FieldDef {
            mutable,
            name,
            ty: ty.as_ref().map(|ty| ty.fold(folder)),
            default: default.as_ref().map(|expr| expr.fold(folder)),
        })
    }
}

impl Fold for Config {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dsl_config(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        match *self {
            Config::Field { name, ref value } => Config::Field {
                name,
                value: value.fold(folder),
            },
            Config::EventHandler { name, ref block } => Config::EventHandler {
                name,
                block: block.fold(folder),
            },
        }
    }
}

impl Fold for Node<Type> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dsl_type(self.clone())
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        match **self {
            Type::Dart(ref ty) => Node::new(Type::Dart(ty.fold(folder))),
        }
    }
}

impl Fold for Node<Expr> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dsl_expr(self.clone())
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        match **self {
            Expr::Instance {
                ref path,
                ref unnamed,
                ref config,
            } => Node::new(Expr::Instance {
                path: path.fold(folder),
                unnamed: unnamed.iter().map(|arg| arg.fold(folder)).collect(),
                config: config.iter().map(|config| config.fold(folder)).collect(),
            }),
            Expr::Array(ref expressions) => Node::new(Expr::Array(
                expressions.iter().map(|expr| expr.fold(folder)).collect(),
            )),
            Expr::Dart(ref expr) => Node::new(Expr::Dart(expr.fold(folder))),
        }
    }
}
