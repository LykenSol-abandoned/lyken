use dart;
use dart::visit::Visit as DartVisit;
use dsl::ast::{Expr, Field, FieldDef, Item, Type};
use node::Node;

pub trait Visitor: dart::visit::Visitor {
    fn dsl_items(&mut self, items: &[Node<Item>]) {
        items.walk(self)
    }
    fn dsl_item(&mut self, item: Node<Item>) {
        item.walk(self)
    }
    fn dsl_field_def(&mut self, field_def: Node<FieldDef>) {
        field_def.walk(self)
    }
    fn dsl_field(&mut self, field: &Field) {
        field.walk(self)
    }
    fn dsl_type(&mut self, ty: Node<Type>) {
        ty.walk(self)
    }
    fn dsl_expr(&mut self, expr: Node<Expr>) {
        expr.walk(self)
    }
}

pub trait Visit {
    fn visit<V: Visitor>(&self, visitor: &mut V);
    fn walk<V: Visitor>(&self, visitor: &mut V);
}

impl Visit for [Node<Item>] {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dsl_items(self);
    }
    fn walk<V: Visitor>(&self, visitor: &mut V) {
        for item in self {
            item.visit(visitor);
        }
    }
}

impl Visit for Node<Item> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dsl_item(self.clone());
    }
    fn walk<V: Visitor>(&self, visitor: &mut V) {
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
    fn walk<V: Visitor>(&self, visitor: &mut V) {
        if let Some(ref ty) = self.ty {
            ty.visit(visitor);
        }
        if let Some(ref expr) = self.default {
            expr.visit(visitor);
        }
    }
}

impl Visit for Field {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dsl_field(self);
    }
    fn walk<V: Visitor>(&self, visitor: &mut V) {
        self.value.visit(visitor);
    }
}

impl Visit for Node<Type> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dsl_type(self.clone());
    }
    fn walk<V: Visitor>(&self, visitor: &mut V) {
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
    fn walk<V: Visitor>(&self, visitor: &mut V) {
        match **self {
            Expr::Instance {
                ref path,
                ref unnamed,
                ref fields,
                ..
            } => {
                path.visit(visitor);
                for arg in unnamed {
                    arg.visit(visitor);
                }
                for field in fields {
                    field.visit(visitor);
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
