use std::io::prelude::*;
use std::io;
use dart::dsl::*;

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
            Item::ComponentDef(ref name, ref parts) => {
                writeln!(self.out, "class {} extends StatelessWidget {{", name)?;
                writeln!(self.out, "@override")?;
                writeln!(self.out, "Widget build(BuildContext context) {{")?;
                self.codegen_part(&parts[0])?;
                writeln!(self.out, "}}")?;
                writeln!(self.out, "}}")?;
            }
            Item::VerbatimDart(ref dart) => {
                writeln!(self.out, "{}", dart)?;
            }
        }
        Ok(())
    }

    fn codegen_part(&mut self, part: &ComponentPart) -> io::Result<()> {
        match *part {
            ComponentPart::Instance(ref instance) => {
                write!(self.out, "return ")?;
                self.codegen_instance(&instance)?;
                writeln!(self.out, ";")?;
            }
            ComponentPart::Field(..) => {
                unimplemented!();
            }
        }
        Ok(())
    }

    fn codegen_field(&mut self, field: &Field) -> io::Result<()> {
        write!(self.out, "{}: ", field.name)?;
        self.codegen_expr(&field.value)?;
        writeln!(self.out, ",")?;
        Ok(())
    }

    fn codegen_expr(&mut self, expr: &Expr) -> io::Result<()> {
        match *expr {
            Expr::Instance(ref instance) => {
                self.codegen_instance(&instance)?;
            }
            Expr::VerbatimDart(ref dart) => {
                write!(self.out, "{}", dart)?;
            }
        }
        Ok(())
    }

    fn codegen_instance(&mut self, instance: &Instance) -> io::Result<()> {
        writeln!(self.out, "new {}( ", instance.name)?;
        for field in &instance.fields {
            self.codegen_field(field)?;
        }
        write!(self.out, ")")?;
        Ok(())
    }
}
