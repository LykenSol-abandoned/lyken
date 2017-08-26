use dsl::ast;
use dart::ast::*;
use node::Node;

pub struct Lifter {}

impl Lifter {
    pub fn new() -> Self {
        Lifter {}
    }

    pub fn lift_items(&mut self, items: &[Node<Item>]) -> Vec<ast::Item> {
        items
            .iter()
            .map(|item| self.lift_item(item.clone()))
            .collect()
    }

    fn lift_item(&mut self, item: Node<Item>) -> ast::Item {
        match *item.clone() {
            Item::Class {
                ref metadata,
                abstract_: false,
                name,
                ref generics,
                superclass: Some(ref superclass),
                ref mixins,
                ref interfaces,
                ref members,
            } => {
                if !metadata.is_empty() {
                    return ast::Item::Dart(item);
                }
                if !generics.is_empty() {
                    return ast::Item::Dart(item);
                }
                if !mixins.is_empty() {
                    return ast::Item::Dart(item);
                }
                if !interfaces.is_empty() {
                    return ast::Item::Dart(item);
                }
                match **superclass {
                    Type::Path(ref name, ref types) if types.is_empty() => {
                        if name.prefix.is_some() {
                            return ast::Item::Dart(item);
                        }
                        if name.name != "StatelessWidget" {
                            return ast::Item::Dart(item);
                        }
                    }
                    _ => return ast::Item::Dart(item),
                }
                let mut fields = vec![];
                let mut dart_members = vec![];
                let mut build_return = None;
                for member in members {
                    match *member.clone() {
                        ClassMember::Fields {
                            ref metadata,
                            static_: false,
                            ref var_type,
                            ref initializers,
                        } => {
                            if !metadata.is_empty() {
                                return ast::Item::Dart(item);
                            }
                            if var_type.fcv != FinalConstVar::Final {
                                return ast::Item::Dart(item);
                            }
                            if initializers.len() > 1 {
                                return ast::Item::Dart(item);
                            }
                            let ty;
                            match *var_type.ty {
                                Type::Infer => {
                                    ty = None;
                                }
                                _ => {
                                    ty = Some(ast::Type::Dart(var_type.ty.clone()));
                                }
                            }
                            let mut default = None;
                            if let Some(ref init) = initializers[0].init {
                                default = Some(self.lift_expr(init.clone()));
                            }
                            fields.push(ast::FieldDef {
                                mutable: false,
                                name: initializers[0].name,
                                ty,
                                default,
                            });
                        }
                        _ => {}
                    }
                }
                for member in members {
                    match *member.clone() {
                        ClassMember::Method(ref meta, ref qualif, ref function) => {
                            match function.name {
                                FnName::Regular(name) if name == "build" => {}
                                _ => {
                                    dart_members.push(member.clone());
                                    continue;
                                }
                            }
                            if meta.len() != 1 {
                                return ast::Item::Dart(item);
                            }
                            if meta[0].suffix.is_some() {
                                return ast::Item::Dart(item);
                            }
                            if meta[0].arguments.is_some() {
                                return ast::Item::Dart(item);
                            }
                            if meta[0].qualified.prefix.is_some() {
                                return ast::Item::Dart(item);
                            }
                            if !qualif.is_empty() {
                                return ast::Item::Dart(item);
                            }
                            if !function.generics.is_empty() {
                                return ast::Item::Dart(item);
                            }
                            if let Some(ref body) = function.body {
                                match *body {
                                    FnBody::Block(ref stm) => match **stm {
                                        Statement::Block(ref stm) => {
                                            if stm.len() != 1 {
                                                return ast::Item::Dart(item);
                                            }
                                            match *stm[0] {
                                                Statement::Return(Some(ref expr)) => {
                                                    build_return =
                                                        Some(self.lift_expr(expr.clone()));
                                                }
                                                _ => return ast::Item::Dart(item),
                                            }
                                        }
                                        _ => return ast::Item::Dart(item),
                                    },
                                    _ => return ast::Item::Dart(item),
                                }
                            }
                        }
                        ClassMember::Constructor {
                            ref metadata,
                            ref method_qualifiers,
                            name: None,
                            ref sig,
                            ref initializers,
                            function_body: None,
                        } => {
                            if !metadata.is_empty() {
                                return ast::Item::Dart(item);
                            }
                            if method_qualifiers.len() > 1 || method_qualifiers.is_empty() {
                                return ast::Item::Dart(item);
                            }
                            match *sig.return_type {
                                Type::Infer => {}
                                _ => return ast::Item::Dart(item),
                            }
                            if sig.optional_kind != OptionalArgKind::Named {
                                return ast::Item::Dart(item);
                            }
                            if sig.async || sig.generator {
                                return ast::Item::Dart(item);
                            }
                            if initializers.len() != 1 {
                                return ast::Item::Dart(item);
                            }
                            match initializers[0] {
                                ConstructorInitializer::Super(ident, ref args) => {
                                    if ident.is_some() {
                                        return ast::Item::Dart(item);
                                    }
                                    if !args.unnamed.is_empty() {
                                        return ast::Item::Dart(item);
                                    }
                                    if args.named.len() != 1 {
                                        return ast::Item::Dart(item);
                                    }
                                    if args.named[0].name != "key" {
                                        return ast::Item::Dart(item);
                                    }
                                    match *args.named[0].expr {
                                        Expr::Identifier(ident) => if ident != "key" {
                                            return ast::Item::Dart(item);
                                        },
                                        _ => return ast::Item::Dart(item),
                                    }
                                }
                                _ => {
                                    return ast::Item::Dart(item);
                                }
                            }
                            if !sig.required.is_empty() {
                                return ast::Item::Dart(item);
                            }
                            if sig.optional.len() != fields.len() + 1 {
                                return ast::Item::Dart(item);
                            }
                            if !sig.optional[0].metadata.is_empty() {
                                return ast::Item::Dart(item);
                            }
                            if sig.optional[0].covariant {
                                return ast::Item::Dart(item);
                            }
                            if sig.optional[0].field {
                                return ast::Item::Dart(item);
                            }
                            if sig.optional[0].var.name != "key" {
                                return ast::Item::Dart(item);
                            }
                            match *sig.optional[0].ty.ty {
                                Type::Path(ref name, ref types) => {
                                    if name.name != "Key" || !name.prefix.is_none() {
                                        return ast::Item::Dart(item);
                                    }
                                    if !types.is_empty() {
                                        return ast::Item::Dart(item);
                                    }
                                }
                                _ => return ast::Item::Dart(item),
                            }

                            for arg in &sig.optional[1..] {
                                if !arg.metadata.is_empty() || arg.covariant || !arg.field {
                                    return ast::Item::Dart(item);
                                }
                                if arg.ty.fcv != FinalConstVar::Var {
                                    return ast::Item::Dart(item);
                                }
                                match *arg.ty.ty.clone() {
                                    Type::Infer => {}
                                    _ => return ast::Item::Dart(item),
                                }
                                let mut default = None;
                                if let Some(expr) = arg.var.init.clone() {
                                    default = Some(self.lift_expr(expr));
                                }
                                for field in &mut fields {
                                    if field.name == arg.var.name {
                                        field.default = default;
                                        break;
                                    }
                                }
                            }
                        }
                        ClassMember::Fields { .. } => {}
                        _ => dart_members.push(member.clone()),
                    }
                }
                return ast::Item::ComponentDef {
                    name,
                    fields,
                    dart_members,
                    body: build_return,
                };
            }
            _ => ast::Item::Dart(item),
        }
    }
    fn lift_expr(&mut self, expr: Node<Expr>) -> ast::Expr {
        match *expr.clone() {
            Expr::List {
                const_: _,
                element_ty: _,
                ref elements,
            } => {
                let mut exprs = vec![];
                for element in elements {
                    exprs.push(self.lift_expr(element.clone()));
                }
                ast::Expr::Array(exprs)
            }
            Expr::New {
                const_: _,
                ref ty,
                ctor: None,
                ref args,
            } => match *ty.clone() {
                Type::Path(ref name, ref path) => {
                    if name.prefix.is_some() {
                        return ast::Expr::Dart(expr);
                    }
                    if !path.is_empty() {
                        return ast::Expr::Dart(expr);
                    } else {
                        let mut fields = vec![];
                        let mut unnamed = vec![];

                        for arg in &args.unnamed {
                            unnamed.push(self.lift_expr(arg.clone()));
                        }

                        for arg in &args.named {
                            fields.push(self.lift_field(arg));
                        }
                        return ast::Expr::Instance {
                            name: name.name,
                            unnamed,
                            fields,
                        };
                    }
                }
                _ => return ast::Expr::Dart(expr),
            },
            _ => ast::Expr::Dart(expr),
        }
    }

    fn lift_field(&mut self, field: &NamedArg) -> ast::Field {
        ast::Field {
            name: field.name,
            value: self.lift_expr(field.expr.clone()),
        }
    }
}
