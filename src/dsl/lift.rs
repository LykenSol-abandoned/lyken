use dsl::ast;
use dart::ast::*;
use node::Node;

pub struct Lifter {}

#[derive(Copy, Clone)]
enum Strategy {
    Plain,
    StatelessWidget,
}

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
        macro_rules! require {
            ($($c:expr),+) => {
                if !($($c)&&+) {
                    return ast::Item::Dart(item);
                }
            };
        }
        match *item.clone() {
            Item::Class {
                ref metadata,
                abstract_: false,
                name,
                ref generics,
                ref superclass,
                ref mixins,
                ref interfaces,
                ref members,
            } => {
                require![
                    metadata.is_empty(),
                    generics.is_empty(),
                    mixins.is_empty(),
                    interfaces.is_empty()
                ];
                let strategy = match *superclass {
                    Some(ref superclass) => {
                        require![
                            superclass.params.is_empty(),
                            superclass.prefix.is_none(),
                            superclass.name == "StatelessWidget"
                        ];
                        Strategy::StatelessWidget
                    }
                    None => Strategy::Plain,
                };
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
                            require![
                                metadata.is_empty(),
                                var_type.fcv == FinalConstVar::Final,
                                initializers.len() >= 1
                            ];
                            let ty = match *var_type.ty {
                                Type::Infer => None,
                                _ => Some(ast::Type::Dart(var_type.ty.clone())),
                            };
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
                            require![
                                meta.len() == 1,
                                meta[0].qualified.prefix.is_none(),
                                meta[0].qualified.name == "override",
                                meta[0].arguments.is_none(),
                                qualif.is_empty(),
                                function.generics.is_empty(),
                                match function.body {
                                    Some(FnBody::Block(ref stm)) => match **stm {
                                        Statement::Block(ref stm) => {
                                            stm.len() == 1 && match *stm[0] {
                                                Statement::Return(Some(ref expr)) => {
                                                    build_return =
                                                        Some(self.lift_expr(expr.clone()));
                                                    true
                                                }
                                                _ => false,
                                            }
                                        }
                                        _ => false,
                                    },
                                    _ => false,
                                }
                            ];
                        }
                        ClassMember::Constructor {
                            ref metadata,
                            ref method_qualifiers,
                            name: None,
                            ref sig,
                            ref initializers,
                            function_body: None,
                        } => {
                            require![
                                metadata.is_empty(),
                                method_qualifiers.len() == 1,
                                method_qualifiers[0] == MethodQualifiers::Const,
                                match *sig.return_type {
                                    Type::Infer => true,
                                    _ => false,
                                },
                                sig.optional_kind == OptionalArgKind::Named,
                                !sig.async,
                                !sig.generator,
                                match strategy {
                                    Strategy::StatelessWidget => {
                                        initializers.len() == 1 && match initializers[0] {
                                            ConstructorInitializer::Super(ident, ref args) => {
                                                ident.is_none() && args.unnamed.is_empty() &&
                                                    args.named.len() == 1 &&
                                                    args.named[0].name == "key" &&
                                                    match *args.named[0].expr {
                                                        Expr::Identifier(ident) => ident == "key",
                                                        _ => false,
                                                    }
                                            }
                                            _ => false,
                                        }
                                    }
                                    Strategy::Plain => initializers.is_empty(),
                                },
                                sig.required.is_empty(),
                                match strategy {
                                    Strategy::StatelessWidget => {
                                        sig.optional.len() == fields.len() + 1 &&
                                            sig.optional[0].metadata.is_empty() &&
                                            !sig.optional[0].covariant &&
                                            !sig.optional[0].field &&
                                            sig.optional[0].var.name == "key" &&
                                            match *sig.optional[0].ty.ty {
                                                Type::Path(ref qualified) => {
                                                    qualified.prefix.is_none() &&
                                                        qualified.name == "Key" &&
                                                        qualified.params.is_empty()
                                                }
                                                _ => false,
                                            }
                                    }
                                    Strategy::Plain => sig.optional.len() == fields.len(),
                                }
                            ];

                            for (i, arg) in sig.optional.iter().enumerate() {
                                match strategy {
                                    Strategy::StatelessWidget if i == 0 => {
                                        continue;
                                    }
                                    _ => {}
                                }
                                require![
                                    arg.metadata.is_empty(),
                                    !arg.covariant,
                                    arg.field,
                                    arg.ty.fcv == FinalConstVar::Var,
                                    match *arg.ty.ty {
                                        Type::Infer => true,
                                        _ => false,
                                    }
                                ];
                                let mut default = None;
                                if let Some(ref expr) = arg.var.init {
                                    default = Some(self.lift_expr(expr.clone()));
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
                ast::Item::ComponentDef {
                    name,
                    fields,
                    dart_members,
                    body: build_return,
                }
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
                ref path,
                ref args,
            } => {
                if path.prefix.is_some() {
                    return ast::Expr::Dart(expr);
                }
                if !path.params.is_empty() {
                    return ast::Expr::Dart(expr);
                }
                let mut fields = vec![];
                let mut unnamed = vec![];

                for arg in &args.unnamed {
                    unnamed.push(self.lift_expr(arg.clone()));
                }

                for arg in &args.named {
                    fields.push(self.lift_field(arg));
                }
                ast::Expr::Instance {
                    name: path.name,
                    unnamed,
                    fields,
                }
            }
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
