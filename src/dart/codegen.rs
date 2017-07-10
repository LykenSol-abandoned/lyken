use std::io::prelude::*;
use std::io;
use dsl::*;

pub struct Codegen<'a> {
    out: &'a mut Write,
}

impl<'a> Codegen<'a> {
    pub fn new(out: &'a mut Write) -> Self {
        Codegen { out }

    }

    pub fn codegen_items(&mut self, items: &[Item]) -> io::Result<()> {
        for item in items {
            self.codegen_item(item)?;
        }
        Ok(())
    }

    fn codegen_item(&mut self, item: &Item) -> io::Result<()> {
        match *item {
            Item::ComponentDef(ref name, ref fields, ref instance) => {
                writeln!(self.out, "class {} extends StatelessWidget {{", name)?;
                if !fields.is_empty() {
                    write!(self.out, "const {}({{ Key key", name)?;
                    for field in fields {
                        write!(self.out, ", this.{}", field.name)?;
                        if let Some(ref value) = field.default {
                            write!(self.out, " = ")?;
                            self.codegen_expr(value)?;
                        }
                    }
                    writeln!(self.out, " }}) : super(key: key);")?;
                    self.codegen_field_defs(fields)?;
                }
                writeln!(self.out, "@override")?;
                writeln!(self.out, "Widget build(BuildContext context) {{")?;
                self.codegen_instance(instance)?;
                writeln!(self.out, "}}")?;
                writeln!(self.out, "}}")
            }
            Item::Verbatim(Language::Dart, ref dart) => writeln!(self.out, "{}", dart),
        }
    }

    fn codegen_field_defs(&mut self, fields: &[FieldDef]) -> io::Result<()> {
        for field in fields {
            self.codegen_field_def(field)?;
        }
        Ok(())
    }

    fn codegen_field_def(&mut self, field: &FieldDef) -> io::Result<()> {
        write!(self.out, "final")?;
        if let Some(ref ty) = field.ty {
            write!(self.out, " ")?;
            self.codegen_type(ty)?;
        }
        writeln!(self.out, " {};", field.name)
    }

    fn codegen_field(&mut self, field: &Field) -> io::Result<()> {
        write!(self.out, "{}: ", field.name)?;
        self.codegen_expr(&field.value)?;
        writeln!(self.out, ",")
    }

    fn codegen_expr(&mut self, expr: &Expr) -> io::Result<()> {
        match *expr {
            Expr::Instance(ref instance) => self.codegen_instance(&instance),
            Expr::Array(ref exprs) => {
                writeln!(self.out, "[")?;
                for expr in exprs {
                    self.codegen_expr(expr)?;
                    writeln!(self.out, ",")?;
                }
                write!(self.out, "]")
            }
            Expr::Verbatim(Language::Dart, ref dart) => write!(self.out, "{}", dart),
        }
    }

    fn codegen_type(&mut self, ty: &Type) -> io::Result<()> {
        match *ty {
            Type::Verbatim(Language::Dart, ref dart) => write!(self.out, "{}", dart),
        }
    }

    fn codegen_instance(&mut self, instance: &Instance) -> io::Result<()> {
        writeln!(self.out, "new {}(", instance.name)?;
        for field in &instance.fields {
            self.codegen_field(field)?;
        }
        write!(self.out, ")")
    }
}
