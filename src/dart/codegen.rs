use dsl::*;
use dart::lex::{Lexer, Token};
use syntax::symbol::Symbol;

pub struct Codegen {
    tokens: Vec<Token>,
}

impl Codegen {
    pub fn new() -> Self {
        Codegen { tokens: vec![] }

    }

    fn write_str(&mut self, string: &str) {
        self.tokens.extend(Lexer::new(string).tokenize().unwrap());
    }

    fn write_ident(&mut self, ident: &Symbol) {
        self.tokens.push(Token::Identifier(*ident));
    }

    pub fn codegen_items(mut self, items: &[Item]) -> Vec<Token> {
        for item in items {
            self.codegen_item(item);
        }
        self.tokens
    }

    fn codegen_item(&mut self, item: &Item) {
        match *item {
            Item::ComponentDef(ref name, ref fields, ref instance) => {
                self.write_str("class ");
                self.write_ident(name);
                self.write_str(" extends StatelessWidget {\n");
                if !fields.is_empty() {
                    self.write_str("const ");
                    self.write_ident(name);
                    self.write_str("({ Key key");
                    for field in fields {
                        self.write_str(", this.");
                        self.write_ident(&field.name);
                        if let Some(ref value) = field.default {
                            self.write_str(" = ");
                            self.codegen_expr(value);
                        }
                    }
                    self.write_str(" }) : super(key: key);\n");
                    self.codegen_field_defs(fields);
                }
                self.write_str("@override\n");
                self.write_str("Widget build(BuildContext context) {\n");
                self.write_str("return ");
                self.codegen_instance(instance);
                self.write_str(";\n");
                self.write_str("}\n");
                self.write_str("}\n");
            }
            Item::Verbatim(Language::Dart, ref dart) => {
                self.tokens.extend(dart);
                self.write_str("\n");
            }
        }
    }

    fn codegen_field_defs(&mut self, fields: &[FieldDef]) {
        for field in fields {
            self.codegen_field_def(field);
        }
    }

    fn codegen_field_def(&mut self, field: &FieldDef) {
        self.write_str("final ");
        if let Some(ref ty) = field.ty {
            self.codegen_type(ty);
            self.write_str(" ");
        }
        self.write_ident(&field.name);
        self.write_str(";\n");
    }

    fn codegen_field(&mut self, field: &Field) {
        self.write_ident(&field.name);
        self.write_str(": ");
        self.codegen_expr(&field.value);
        self.write_str(",\n");
    }

    fn codegen_expr(&mut self, expr: &Expr) {
        match *expr {
            Expr::Instance(ref instance) => self.codegen_instance(&instance),
            Expr::Array(ref exprs) => {
                self.write_str("[\n");
                for expr in exprs {
                    self.codegen_expr(expr);
                    self.write_str(",\n");
                }
                self.write_str("]");
            }
            Expr::Verbatim(Language::Dart, ref dart) => {
                self.tokens.extend(dart);
            }
        }
    }

    fn codegen_type(&mut self, ty: &Type) {
        match *ty {
            Type::Verbatim(Language::Dart, ref dart) => {
                self.tokens.extend(dart);
            }
        }
    }

    fn codegen_instance(&mut self, instance: &Instance) {
        self.write_str("new ");
        self.write_ident(&instance.name);
        self.write_str("(\n");
        for field in &instance.fields {
            self.codegen_field(field);
        }
        self.write_str(")");
    }
}
