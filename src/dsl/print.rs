use dsl::ast::*;
use dart::print::{BoxKind, Printer};
use node::Node;

impl Printer {
    pub fn dsl_items(mut self, items: &[Node<Item>]) -> String {
        for item in items {
            self.dsl_item(item);
        }
        self.pretty_print()
    }

    fn dsl_item(&mut self, item: &Item) {
        self.enter(BoxKind::Block);
        match *item {
            Item::ComponentDef {
                name,
                ref fields,
                ref dart_members,
                ref body,
            } => {
                self.print_str("def ");
                self.print_ident(name);
                self.print_str(" {");
                self.enter(BoxKind::Block);
                for field in fields {
                    self.enter(BoxKind::Block);
                    self.dsl_field_def(field);
                    self.enter(BoxKind::CommaDelim);
                    self.exit();
                    self.exit();
                }
                for dart_member in dart_members {
                    self.dart_class_member(dart_member, name);
                }
                if let Some(ref body) = *body {
                    self.print_str("..");
                    self.dsl_expr(body);
                }
                self.exit();
                self.print_str("}");
            }
            Item::Dart(ref item) => self.dart_item(item),
        }
        self.exit();
    }

    fn dsl_field_def(&mut self, field: &FieldDef) {
        if field.mutable {
            self.print_str("mut ");
        }
        self.print_ident(field.name);
        if let Some(ref ty) = field.ty {
            self.print_str(": ");
            self.dsl_ty(ty);
        }
        if let Some(ref expr) = field.default {
            self.print_str(" = ");
            self.dsl_expr(expr);
        }
    }

    fn dsl_config(&mut self, config: &Config) {
        match *config {
            Config::Field { name, ref value } => {
                self.print_ident(name);
                self.print_str(": ");
                self.dsl_expr(value);
            }
            Config::EventHandler { name, ref block } => {
                self.print_str("on ");
                self.print_ident(name);
                self.print_str(" ");
                self.dart_statement(block);
            }
        }
    }

    fn dsl_ty(&mut self, ty: &Type) {
        match *ty {
            Type::Dart(ref ty) => self.dart_type(ty),
        }
    }

    fn dsl_expr(&mut self, expr: &Expr) {
        match *expr {
            Expr::Instance {
                ref path,
                ref unnamed,
                ref config,
            } => {
                self.dart_qualified(path);
                if !unnamed.is_empty() {
                    self.print_str("(");
                    self.enter(BoxKind::Inline);
                    for (i, unnamed_item) in unnamed.iter().enumerate() {
                        self.dsl_expr(unnamed_item);
                        if i < unnamed.len() - 1 {
                            self.enter(BoxKind::CommaDelim);
                            self.exit();
                        }
                    }
                    self.exit();
                    self.print_str(")");
                }
                if !config.is_empty() {
                    self.print_str(" {");
                    self.enter(BoxKind::Block);
                    for (i, c) in config.iter().enumerate() {
                        self.dsl_config(c);
                        if i < config.len() - 1 {
                            self.enter(BoxKind::CommaDelim);
                            self.exit();
                        }
                    }
                    self.exit();
                    self.print_str("}");
                }
            }
            Expr::Array(ref args) => {
                self.print_str("[");
                self.enter(BoxKind::Inline);
                for (i, arg) in args.iter().enumerate() {
                    self.dsl_expr(arg);
                    if i < args.len() - 1 {
                        self.enter(BoxKind::CommaDelim);
                        self.exit();
                    }
                }
                self.exit();
                self.print_str("]");
            }
            Expr::Dart(ref expr) => {
                self.dart_expr(expr);
            }
        }
    }
}
