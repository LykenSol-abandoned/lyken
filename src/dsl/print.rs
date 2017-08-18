use dsl::ast::*;
use dart::print::Printer;
use dart::lex::Token;

impl Printer {
    pub fn dsl_items(mut self, items: &[Item]) -> Vec<Token> {
        for item in items {
            self.dsl_item(item);
            self.new_line();
        }
        self.pretty_print()
    }

    fn dsl_item(&mut self, item: &Item) {
        match *item {
            Item::ComponentDef(name, ref fields, ref instance) => {
                self.print_str("def ");
                self.print_ident(name);
                self.print_str(" {");
                self.enter();
                for (i, field) in fields.iter().enumerate() {
                    self.dsl_field_def(field);
                    if i < fields.len() - 1 {
                        self.print_str(", ");
                    }
                }
                self.dsl_instance(instance);
                self.exit();
                self.print_str("}");
            }
            Item::Dart(ref items) => for item in items {
                self.dart_item(item);
            },
        }
    }

    fn dsl_instance(&mut self, instance: &Instance) {
        self.print_ident(instance.name);
        self.print_str(" {");
        self.enter();
        for (i, field) in instance.fields.iter().enumerate() {
            self.dsl_field(field);
            if i < instance.fields.len() - 1 {
                self.print_str(", ");
            }
        }
        self.exit();
        self.print_str("}");
    }

    fn dsl_field_def(&mut self, field: &FieldDef) {
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

    fn dsl_field(&mut self, field: &Field) {
        self.print_ident(field.name);
        self.print_str(": ");
        self.dsl_expr(&field.value);
    }

    fn dsl_ty(&mut self, ty: &Type) {
        match *ty {
            Type::Dart(ref ty) => self.dart_type(ty),
        }
    }

    fn dsl_expr(&mut self, expr: &Expr) {
        match *expr {
            Expr::Instance(ref instance) => {
                self.dsl_instance(instance);
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
