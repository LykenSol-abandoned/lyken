use std::io::prelude::*;
use std::io;
use dart::dsl::*;

pub fn codegen_flutter(items: &[Item], out: &mut Write) -> io::Result<()> {
    for item in items {
        match *item {
            Item::ComponentDef(ref name, ref parts) => {
                writeln!(out, "class {} extends StatelessWidget {{", name)?;
                writeln!(out, "@override")?;
                writeln!(out, "Widget build(BuildContext context) {{")?;
                match parts[0] {
                    ComponentPart::Instance(ref iname, ref args) => {
                        writeln!(out, "return new {}(", iname)?;
                        for arg in args {
                            writeln!(out, "{}: {},", arg.name, arg.value.dart)?;
                        }
                        writeln!(out, ");")?;
                    }
                    ComponentPart::Field(..) => {}
                }
                writeln!(out, "}}")?;
                writeln!(out, "}}")?;
            }
            Item::VerbatimDart(ref dart) => {
                write!(out, "{}", dart)?;
            }
        }
    }
    Ok(())
}
