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
            ComponentPart::Instance(ref iname, ref args) => {
                writeln!(self.out, "return new {}(", iname)?;
                for arg in args {
                    self.codegen_field(arg)?;
                }
                writeln!(self.out, ");")?;
            }
            ComponentPart::Field(..) => {}
        }
        Ok(())
    }

    fn codegen_field(&mut self, field: &Field) -> io::Result<()> {
        writeln!(self.out, "{}: {},", field.name, field.value.dart)?;
        Ok(())
    }
}
