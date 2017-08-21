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
        items.iter().map(|item| self.codegen_item(item)).collect()
    }

    fn codegen_item(&mut self, item: &Item) -> Node<ast::Item> {
        match *item {
            Item::ComponentDef(name, ref fields, ref instance) => {
                let params;
                let body;
                let superclass;
                let mut class_members = vec![];
                superclass = Some(ast::Type::simple_path("StatelessWidget"));
                if !fields.is_empty() {
                    let mut normal = vec![];
                    let mut default = vec![];
                    normal.push(ast::ArgDef::simple(ast::Type::simple_path("Key"), "key"));

                    for field in fields {
                        let mut var_ty = Node::new(ast::Type::Infer);
                        if let Some(ref ty) = field.ty {
                            var_ty = self.codegen_type(ty);
                        }
                        let arg = ast::ArgDef {
                            metadata: vec![],
                            covariant: false,
                            ty: ast::VarType {
                                fcv: ast::FinalConstVar::Var,
                                ty: var_ty,
                            },
                            field: true,
                            name: field.name,
                        };
                        if let Some(ref value) = field.default {
                            default.push(ast::OptionalArgDef {
                                arg,
                                default: Some(self.codegen_expr(value)),
                            });
                        } else {
                            normal.push(arg);
                        }
                    }
                    params = ast::FnSig {
                        return_type: Node::new(ast::Type::Infer),
                        required: normal,
                        optional: default,
                        optional_kind: ast::OptionalArgKind::Named,
                        async: false,
                        generator: false,
                    };
                    class_members.push(ast::ClassMember::Constructor {
                        metadata: vec![],
                        method_qualifiers: vec![ast::MethodQualifiers::Const],
                        name: Some(name),
                        sig: params,
                        initializers: vec![
                            ast::ConstructorInitializer::Super(
                                None,
                                ast::Args {
                                    unnamed: vec![],
                                    named: vec![
                                        ast::NamedArg {
                                            name: Symbol::intern("key"),
                                            expr: Node::new(
                                                ast::Expr::Identifier(Symbol::intern("key")),
                                            ),
                                        },
                                    ],
                                },
                            ),
                        ],
                        function_body: None,
                    });
                }

                class_members.extend(self.codegen_field_defs(fields));
                body = Some(ast::FnBody::Block(Node::new(ast::Statement::Block(vec![
                    Node::new(ast::Statement::Return(
                        Some(self.codegen_instance(instance)),
                    )),
                ]))));
                class_members.push(ast::ClassMember::Method(
                    vec![ast::MetadataItem::simple("override")],
                    vec![],
                    ast::Function {
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
                    },
                ));
                Node::new(ast::Item::Class {
                    metadata: vec![],
                    abstract_: false,
                    name,
                    generics: vec![],
                    superclass,
                    mixins: vec![],
                    interfaces: vec![],
                    members: class_members,
                })
            }
            Item::Dart(ref item) => item.clone(),
        }
    }

    fn codegen_field_defs(&mut self, fields: &[FieldDef]) -> Vec<ast::ClassMember> {
        fields
            .iter()
            .map(|field| self.codegen_field_def(field))
            .collect()
    }

    fn codegen_field_def(&mut self, field: &FieldDef) -> ast::ClassMember {
        let mut var_ty = Node::new(ast::Type::Infer);
        let mut var_expr = None;
        if let Some(ref ty) = field.ty {
            var_ty = self.codegen_type(ty);
        }
        if let Some(ref expr) = field.default {
            var_expr = Some(self.codegen_expr(expr));
        }
        ast::ClassMember::Fields {
            metadata: vec![],
            static_: false,
            var_type: ast::VarType {
                fcv: ast::FinalConstVar::Final,
                ty: var_ty,
            },
            initializers: vec![
                ast::NameAndInitializer {
                    name: field.name,
                    init: var_expr,
                },
            ],
        }
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
        let args = instance
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
            args: ast::Args {
                unnamed: vec![],
                named: args,
            },
        })
    }
}
