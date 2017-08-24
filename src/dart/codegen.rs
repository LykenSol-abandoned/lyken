use dsl::ast::*;
use dart::ast;
use syntax::symbol::Symbol;
use node::Node;

pub struct Codegen {}

impl Codegen {
    pub fn new() -> Self {
        Codegen {}
    }

    pub fn codegen_items(&mut self, items: &[Item]) -> Vec<Node<ast::Item>> {
        items
            .iter()
            .flat_map(|item| self.codegen_item(item))
            .collect()
    }

    fn codegen_item(&mut self, item: &Item) -> Vec<Node<ast::Item>> {
        match *item {
            Item::ComponentDef {
                mut name,
                ref fields,
                ref dart_members,
                ref body,
            } => {
                let mut items = vec![];
                let mut superclass = Some(ast::Type::simple_path("StatelessWidget"));
                let mut class_members = vec![];
                if fields.iter().any(|f| f.mutable) {
                    superclass = Some(ast::Type::simple_path("StatefulWidget"));

                    let state_name = format!("_{}State", name.as_str().trim_left_matches('_'));

                    let mut class_members = vec![];
                    class_members.extend(self.codegen_constructor(
                        fields.iter().filter(|f| !f.mutable),
                    ));

                    class_members.extend(
                        fields
                            .iter()
                            .filter(|f| !f.mutable)
                            .map(|field| self.codegen_field_def(field)),
                    );

                    let body = Some(ast::FnBody::Arrow(Node::new(ast::Expr::New {
                        const_: false,
                        ty: ast::Type::simple_path(&state_name),
                        ctor: None,
                        args: ast::Args {
                            unnamed: vec![],
                            named: vec![],
                        },
                    })));
                    class_members.push(Node::new(ast::ClassMember::Method(
                        vec![ast::MetadataItem::simple("override")],
                        vec![],
                        Node::new(ast::Function {
                            name: ast::FnName::Regular(Symbol::intern("createState")),
                            generics: vec![],
                            sig: ast::FnSig {
                                return_type: ast::Type::simple_path(&state_name),
                                required: vec![],
                                optional: vec![],
                                optional_kind: ast::OptionalArgKind::Named,
                                async: false,
                                generator: false,
                            },
                            body,
                        }),
                    )));

                    items.push(Node::new(ast::Item::Class {
                        metadata: vec![],
                        abstract_: false,
                        name,
                        generics: vec![],
                        superclass,
                        mixins: vec![],
                        interfaces: vec![],
                        members: class_members,
                    }));

                    superclass = Some(Node::new(ast::Type::Path(
                        ast::Qualified::simple("State"),
                        vec![
                            Node::new(ast::Type::Path(
                                ast::Qualified { prefix: None, name },
                                vec![],
                            )),
                        ],
                    )));
                    name = Symbol::intern(&state_name);
                } else {
                    class_members.extend(self.codegen_constructor(fields));
                }

                class_members.extend(
                    fields
                        .iter()
                        .filter(|f| f.mutable)
                        .map(|field| self.codegen_field_def(field)),
                );

                class_members.extend(dart_members.iter().cloned());

                let body = Some(ast::FnBody::Block(Node::new(ast::Statement::Block(vec![
                    Node::new(ast::Statement::Return(Some(self.codegen_instance(body)))),
                ]))));
                class_members.push(Node::new(ast::ClassMember::Method(
                    vec![ast::MetadataItem::simple("override")],
                    vec![],
                    Node::new(ast::Function {
                        name: ast::FnName::Regular(Symbol::intern("build")),
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
                        body,
                    }),
                )));
                items.push(Node::new(ast::Item::Class {
                    metadata: vec![],
                    abstract_: false,
                    name,
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

    fn codegen_constructor<'a, I: IntoIterator<Item = &'a FieldDef>>(
        &mut self,
        fields: I,
    ) -> Option<Node<ast::ClassMember>> {
        let mut args = vec![ast::ArgDef::simple(ast::Type::simple_path("Key"), "key")];

        for field in fields {
            if field.name.as_str().starts_with('_') {
                continue;
            }
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
                        .map(|default| self.codegen_expr(default)),
                }),
            });
        }

        if args.len() == 1 {
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
            initializers: vec![
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
            function_body: None,
        }))
    }

    fn codegen_field_def(&mut self, field: &FieldDef) -> Node<ast::ClassMember> {
        let mut var_ty = Node::new(ast::Type::Infer);
        let mut var_expr = None;
        if let Some(ref ty) = field.ty {
            var_ty = self.codegen_type(ty);
        }
        if let Some(ref expr) = field.default {
            var_expr = Some(self.codegen_expr(expr));
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

    fn codegen_field(&mut self, field: &Field) -> ast::NamedArg {
        ast::NamedArg {
            name: field.name,
            expr: self.codegen_expr(&field.value),
        }
    }

    fn codegen_expr(&mut self, expr: &Expr) -> Node<ast::Expr> {
        match *expr {
            Expr::Instance(ref instance) => self.codegen_instance(&instance),
            Expr::Array(ref exprs) => {
                let elements = exprs.iter().map(|expr| self.codegen_expr(expr)).collect();
                Node::new(ast::Expr::List {
                    const_: false,
                    element_ty: None,
                    elements,
                })
            }
            Expr::Dart(ref dart) => dart.clone(),
        }

    }

    fn codegen_type(&mut self, ty: &Type) -> Node<ast::Type> {
        match *ty {
            Type::Dart(ref dart) => dart.clone(),
        }
    }

    fn codegen_instance(&mut self, instance: &Instance) -> Node<ast::Expr> {
        let unnamed = instance
            .unnamed
            .iter()
            .map(|expr| self.codegen_expr(expr))
            .collect();
        let named = instance
            .fields
            .iter()
            .map(|field| self.codegen_field(field))
            .collect();
        Node::new(ast::Expr::New {
            const_: false,
            ty: Node::new(ast::Type::Path(
                ast::Qualified {
                    prefix: None,
                    name: instance.name,
                },
                vec![],
            )),
            ctor: None,
            args: ast::Args { unnamed, named },
        })
    }
}
