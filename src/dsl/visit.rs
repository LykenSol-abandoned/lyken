use dart;
use dart::visit::Visit as DartVisit;
use dsl::ast::{Config, Expr, FieldDef, Item, Type};
use node::Node;

pub trait Visitor: dart::visit::Visitor {
    fn dsl_items(&mut self, items: &[Node<Item>]) {
        items.super_visit(self)
    }
    fn dsl_item(&mut self, item: Node<Item>) {
        item.super_visit(self)
    }
    fn dsl_field_def(&mut self, field_def: Node<FieldDef>) {
        field_def.super_visit(self)
    }
    fn dsl_config(&mut self, config: &Config) {
        config.super_visit(self)
    }
    fn dsl_type(&mut self, ty: Node<Type>) {
        ty.super_visit(self)
    }
    fn dsl_expr(&mut self, expr: Node<Expr>) {
        expr.super_visit(self)
    }
}

pub trait Visit {
    fn visit<V: Visitor>(&self, visitor: &mut V);
    fn super_visit<V: Visitor>(&self, visitor: &mut V);
}

impl Visit for [Node<Item>] {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dsl_items(self);
    }
    fn super_visit<V: Visitor>(&self, visitor: &mut V) {
        for item in self {
            item.visit(visitor);
        }
    }
}

impl Visit for Node<Item> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dsl_item(self.clone());
    }
    fn super_visit<V: Visitor>(&self, visitor: &mut V) {
        match **self {
            Item::ComponentDef {
                ref fields,
                ref dart_members,
                ref body,
                ..
            } => {
                for field in fields {
                    field.visit(visitor);
                }
                for dart_member in dart_members {
                    dart_member.visit(visitor);
                }
                if let Some(ref body) = *body {
                    body.visit(visitor);
                }
            }
            Item::Dart(ref item) => {
                item.visit(visitor);
            }
        }
    }
}

impl Visit for Node<FieldDef> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dsl_field_def(self.clone());
    }
    fn super_visit<V: Visitor>(&self, visitor: &mut V) {
        if let Some(ref ty) = self.ty {
            ty.visit(visitor);
        }
        if let Some(ref expr) = self.default {
            expr.visit(visitor);
        }
    }
}

impl Visit for Config {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dsl_config(self);
    }
    fn super_visit<V: Visitor>(&self, visitor: &mut V) {
        match *self {
            Config::Field { ref value, .. } => {
                value.visit(visitor);
            }
            Config::EventHandler { ref block, .. } => {
                block.visit(visitor);
            }
        }
    }
}

impl Visit for Node<Type> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dsl_type(self.clone());
    }
    fn super_visit<V: Visitor>(&self, visitor: &mut V) {
        match **self {
            Type::Dart(ref ty) => {
                ty.visit(visitor);
            }
        }
    }
}

impl Visit for Node<Expr> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dsl_expr(self.clone());
    }
    fn super_visit<V: Visitor>(&self, visitor: &mut V) {
        match **self {
            Expr::Instance {
                ref path,
                ref unnamed,
                ref config,
                ..
            } => {
                path.visit(visitor);
                for arg in unnamed {
                    arg.visit(visitor);
                }
                for conf in config {
                    conf.visit(visitor);
                }
            }
            Expr::Array(ref expressions) => for expr in expressions {
                expr.visit(visitor);
            },
            Expr::Dart(ref expr) => {
                expr.visit(visitor);
            }
        }
    }
}
