use dsl::ast::*;
use dart::ast;
use syntax::symbol::Symbol;
use node::Node;

pub struct Lowerer {}

#[derive(Copy, Clone)]
enum Strategy {
    Plain,
    StatelessWidget,
    StatefulWidget,
}

impl Lowerer {
    pub fn new() -> Self {
        Lowerer {}
    }

    pub fn lower_items(&mut self, items: &[Node<Item>]) -> Vec<Node<ast::Item>> {
        items
            .iter()
            .flat_map(|item| self.lower_item(item))
            .collect()
    }

    fn lower_item(&mut self, item: &Item) -> Vec<Node<ast::Item>> {
        match *item {
            Item::ComponentDef {
                name,
                ref fields,
                ref dart_members,
                ref body,
            } => {
                let strategy = if body.is_none() {
                    Strategy::Plain
                } else if fields.iter().any(|f| f.mutable) {
                    Strategy::StatefulWidget
                } else {
                    Strategy::StatelessWidget
                };

                let mut items = vec![];
                let mut class_members = vec![];
                if let Strategy::StatefulWidget = strategy {
                    let state_name = format!("_{}State", name.as_str().trim_left_matches('_'));

                    class_members.extend(
                        fields
                            .iter()
                            .filter(|f| f.mutable)
                            .map(|field| self.lower_field_def(field)),
                    );
                    let mut class_members = vec![];
                    class_members.extend(
                        self.lower_constructor(strategy, fields.iter().filter(|f| !f.mutable)),
                    );

                    class_members.extend(
                        fields
                            .iter()
                            .filter(|f| !f.mutable)
                            .map(|field| self.lower_field_def(field)),
                    );

                    class_members.push(Node::new(ast::ClassMember::Method(
                        vec![ast::MetadataItem::simple("override")],
                        vec![],
                        Node::new(ast::Function {
                            name: ast::FnName::regular("createState"),
                            generics: vec![],
                            sig: ast::FnSig {
                                return_type: ast::Type::simple_path(&state_name[..]),
                                required: vec![],
                                optional: vec![],
                                optional_kind: ast::OptionalArgKind::Named,
                                async: false,
                                generator: false,
                            },
                            body: Some(ast::FnBody::Arrow(Node::new(ast::Expr::New {
                                const_: false,
                                path: ast::Qualified::one(&state_name[..], vec![]),
                                args: ast::Args {
                                    unnamed: vec![],
                                    named: vec![],
                                },
                            }))),
                        }),
                    )));

                    items.push(Node::new(ast::Item::Class {
                        metadata: vec![],
                        abstract_: false,
                        name,
                        generics: vec![],
                        superclass: Some(ast::Qualified::one("StatefulWidget", vec![])),
                        mixins: vec![],
                        interfaces: vec![],
                        members: class_members,
                    }));
                } else {
                    class_members.extend(self.lower_constructor(strategy, fields));
                    class_members.extend(fields.iter().map(|field| self.lower_field_def(field)));
                }

                class_members.extend(dart_members.iter().cloned());

                if let Some(ref body) = *body {
                    class_members.push(Node::new(ast::ClassMember::Method(
                        vec![ast::MetadataItem::simple("override")],
                        vec![],
                        Node::new(ast::Function {
                            name: ast::FnName::regular("build"),
                            generics: vec![],
                            sig: ast::FnSig {
                                return_type: ast::Type::simple_path("Widget"),
                                required: vec![
                                    ast::ArgDef::simple(
                                        ast::Type::simple_path("BuildContext"),
                                        "context",
                                    ),
                                ],
                                optional: vec![],
                                optional_kind: ast::OptionalArgKind::Named,
                                async: false,
                                generator: false,
                            },
                            body: Some(ast::FnBody::Block(Node::new(ast::Statement::Block(vec![
                                Node::new(ast::Statement::Return(Some(self.lower_expr(body)))),
                            ])))),
                        }),
                    )));
                }

                let superclass = match strategy {
                    Strategy::StatelessWidget => {
                        Some(ast::Qualified::one("StatelessWidget", vec![]))
                    }
                    Strategy::StatefulWidget => Some(ast::Qualified::one(
                        "State",
                        vec![ast::Type::simple_path(name)],
                    )),
                    Strategy::Plain => None,
                };

                items.push(Node::new(ast::Item::Class {
                    metadata: vec![],
                    abstract_: false,
                    name: match strategy {
                        Strategy::StatefulWidget => Symbol::intern(
                            &format!("_{}State", name.as_str().trim_left_matches('_')),
                        ),
                        Strategy::Plain | Strategy::StatelessWidget => name,
                    },
                    generics: vec![],
                    superclass,
                    mixins: vec![],
                    interfaces: vec![],
                    members: class_members,
                }));
                items
            }
            Item::Dart(ref item) => vec![item.clone()],
        }
    }

    fn lower_constructor<'a, I: IntoIterator<Item = &'a Node<FieldDef>>>(
        &mut self,
        strategy: Strategy,
        fields: I,
    ) -> Option<Node<ast::ClassMember>> {
        let mut args = vec![];

        match strategy {
            Strategy::Plain => {}
            Strategy::StatefulWidget | Strategy::StatelessWidget => {
                args.push(ast::ArgDef::simple(ast::Type::simple_path("Key"), "key"));
            }
        }

        let mut has_fields = false;
        for field in fields {
            if field.name.as_str().starts_with('_') {
                continue;
            }
            has_fields = true;
            args.push(ast::ArgDef {
                metadata: vec![],
                covariant: false,
                ty: ast::VarType {
                    fcv: ast::FinalConstVar::Var,
                    ty: Node::new(ast::Type::Infer),
                },
                field: true,
                var: Node::new(ast::VarDef {
                    name: field.name,
                    init: field
                        .default
                        .as_ref()
                        .map(|default| self.lower_expr(default)),
                }),
            });
        }

        if !has_fields {
            return None;
        }

        let sig = ast::FnSig {
            return_type: Node::new(ast::Type::Infer),
            required: vec![],
            optional: args,
            optional_kind: ast::OptionalArgKind::Named,
            async: false,
            generator: false,
        };
        Some(Node::new(ast::ClassMember::Constructor {
            metadata: vec![],
            method_qualifiers: vec![ast::MethodQualifiers::Const],
            name: None,
            sig,
            initializers: match strategy {
                Strategy::Plain => vec![],
                Strategy::StatefulWidget | Strategy::StatelessWidget => vec![
                    ast::ConstructorInitializer::Super(
                        None,
                        ast::Args {
                            unnamed: vec![],
                            named: vec![
                                ast::NamedArg {
                                    name: Symbol::intern("key"),
                                    expr: Node::new(ast::Expr::Identifier(Symbol::intern("key"))),
                                },
                            ],
                        },
                    ),
                ],
            },
            function_body: None,
        }))
    }

    fn lower_field_def(&mut self, field: &FieldDef) -> Node<ast::ClassMember> {
        let mut var_ty = Node::new(ast::Type::Infer);
        let mut var_expr = None;
        if let Some(ref ty) = field.ty {
            var_ty = self.lower_type(ty);
        }
        if let Some(ref expr) = field.default {
            var_expr = Some(self.lower_expr(expr));
        }
        Node::new(ast::ClassMember::Fields {
            metadata: vec![],
            static_: false,
            var_type: ast::VarType {
                fcv: if field.mutable {
                    ast::FinalConstVar::Var
                } else {
                    ast::FinalConstVar::Final
                },
                ty: var_ty,
            },
            initializers: vec![
                Node::new(ast::VarDef {
                    name: field.name,
                    init: var_expr,
                }),
            ],
        })
    }

    fn lower_field(&mut self, field: &Field) -> ast::NamedArg {
        ast::NamedArg {
            name: field.name,
            expr: self.lower_expr(&field.value),
        }
    }

    fn lower_expr(&mut self, expr: &Expr) -> Node<ast::Expr> {
        match *expr {
            Expr::Instance {
                ref path,
                ref unnamed,
                ref fields,
            } => {
                let unnamed = unnamed.iter().map(|expr| self.lower_expr(expr)).collect();
                let named = fields.iter().map(|field| self.lower_field(field)).collect();
                Node::new(ast::Expr::New {
                    const_: false,
                    path: path.clone(),
                    args: ast::Args { unnamed, named },
                })
            }
            Expr::Array(ref exprs) => {
                let elements = exprs.iter().map(|expr| self.lower_expr(expr)).collect();
                Node::new(ast::Expr::List {
                    const_: false,
                    element_ty: None,
                    elements,
                })
            }
            Expr::Dart(ref dart) => dart.clone(),
        }
    }

    fn lower_type(&mut self, ty: &Type) -> Node<ast::Type> {
        match *ty {
            Type::Dart(ref dart) => dart.clone(),
        }
    }
}
