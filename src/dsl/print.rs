use dsl::ast::*;
use dart::print::Printer;
use dart::lex::Token;
use node::Node;

impl Printer {
    pub fn dsl_items(mut self, items: &[Node<Item>]) -> Vec<Token> {
        for item in items {
            self.dsl_item(item);
            self.new_line();
        }
        self.pretty_print()
    }

    fn dsl_item(&mut self, item: &Item) {
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
                self.enter();
                for field in fields {
                    self.dsl_field_def(field);
                    self.print_str(",");
                    self.new_line();
                }
                for dart_member in dart_members {
                    self.dart_class_member(dart_member, name);
                    self.new_line();
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
                    self.enter();
                    for (i, unnamed_item) in unnamed.iter().enumerate() {
                        self.dsl_expr(unnamed_item);
                        if i < unnamed.len() - 1 {
                            self.print_str(", ");
                        }
                    }
                    self.exit();
                    self.print_str(")");
                }
                if !config.is_empty() {
                    self.print_str(" {");
                    self.enter();
                    for (i, c) in config.iter().enumerate() {
                        self.dsl_config(c);
                        if i < config.len() - 1 {
                            self.print_str(", ");
                        }
                    }
                    self.exit();
                    self.print_str("}");
                }
            }
            Expr::Array(ref args) => {
                self.print_str("[");
                self.enter();
                for (i, arg) in args.iter().enumerate() {
                    self.dsl_expr(arg);
                    if i < args.len() - 1 {
                        self.print_str(", ");
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
