use dsl::ast;
use dart::ast::*;
use dart::sdk;
use dart::resolve::Res;
use node::Node;
use std::collections::HashMap;
use std::mem;
use std::path::Path;
use syntax::symbol::Symbol;

pub struct Lifter {
    stateless_widget_class: Res,
    stateful_widget_class: Res,
    state_class: Res,
    classes: HashMap<Node<Item>, Class>,
}

struct Class {
    name: Symbol,
    kind: ClassKind,
    fields: Vec<ast::FieldDef>,
    dart_members: Vec<Node<ClassMember>>,
    build_return: Option<ast::Expr>,
}

#[derive(Clone)]
enum ClassKind {
    Plain,
    Remove,
    StatelessWidget,
    StatefulWidget,
    State { widget_class: Node<Item> },
}

impl Lifter {
    pub fn new() -> Self {
        let mut stateless_widget_class = None;
        let mut stateful_widget_class = None;
        let mut state_class = None;
        let module =
            sdk::resolve_import(Path::new(""), "package:flutter/src/widgets/framework.dart");
        for item in &module.items {
            if let Item::Class { name, .. } = **item {
                if name == "StatelessWidget" {
                    stateless_widget_class = Some(Res::Class(item.clone()));
                }
                if name == "StatefulWidget" {
                    stateful_widget_class = Some(Res::Class(item.clone()));
                }
                if name == "State" {
                    state_class = Some(Res::Class(item.clone()));
                }
            }
        }
        Lifter {
            stateless_widget_class: stateless_widget_class.unwrap(),
            stateful_widget_class: stateful_widget_class.unwrap(),
            state_class: state_class.unwrap(),
            classes: HashMap::new(),
        }
    }

    pub fn lift_items(&mut self, items: &[Node<Item>]) -> Vec<ast::Item> {
        for item in items {
            self.collect_item(item.clone());
        }
        let mut state_classes = vec![];
        for (item, class) in &self.classes {
            if let ClassKind::State { ref widget_class } = class.kind {
                if let Some(widget_class) = self.classes.get(widget_class) {
                    if let ClassKind::StatefulWidget = widget_class.kind {
                        state_classes.push(item.clone());
                    }
                }
            }
        }

        for item in state_classes {
            let (kind, fields, dart_members, build_return);
            {
                let class = self.classes.get_mut(&item).unwrap();
                kind = mem::replace(&mut class.kind, ClassKind::Remove);
                fields = mem::replace(&mut class.fields, vec![]);
                dart_members = mem::replace(&mut class.dart_members, vec![]);
                build_return = class.build_return.take();
            }
            if let ClassKind::State { ref widget_class } = kind {
                let widget_class = self.classes.get_mut(widget_class).unwrap();
                widget_class.fields.extend(fields);
                widget_class.dart_members.extend(dart_members);
                widget_class.build_return = build_return;
            }
        }

        let mut replacements: HashMap<_, _> = self.classes
            .drain()
            .filter_map(
                |(
                    item,
                    Class {
                        name,
                        kind,
                        fields,
                        dart_members,
                        build_return,
                    },
                )| {
                    if let ClassKind::Remove = kind {
                        return Some((item, vec![]));
                    }
                    Some((
                        item,
                        vec![
                            ast::Item::ComponentDef {
                                name,
                                fields,
                                dart_members,
                                body: match (build_return, kind) {
                                    (None, ClassKind::Plain) => None,
                                    (Some(build_return), ClassKind::StatelessWidget) |
                                    (Some(build_return), ClassKind::StatefulWidget) => {
                                        Some(build_return)
                                    }

                                    _ => return None,
                                },
                            },
                        ],
                    ))
                },
            )
            .collect();

        items
            .iter()
            .flat_map(|item| {
                if let Some(replacement) = replacements.remove(item) {
                    replacement
                } else {
                    vec![ast::Item::Dart(item.clone())]
                }
            })
            .collect()
    }

    fn collect_item(&mut self, item: Node<Item>) {
        macro_rules! require {
            ($($c:expr),+) => {
                if !($($c)&&+) {
                    return;
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
                let kind = match *superclass {
                    Some(ref superclass) => {
                        let class_kind = superclass.res().get().unwrap();
                        if class_kind == self.stateless_widget_class {
                            ClassKind::StatelessWidget
                        } else if class_kind == self.stateful_widget_class {
                            ClassKind::StatefulWidget
                        } else if class_kind == self.state_class {
                            let qualified = match *superclass.params[0] {
                                Type::Path(ref qualified) => qualified,
                                _ => return,
                            };
                            ClassKind::State {
                                widget_class: match qualified.res().get().unwrap() {
                                    Res::Class(item) => item,
                                    _ => return,
                                },
                            }
                        } else {
                            return;
                        }
                    }
                    None => ClassKind::Plain,
                };
                let mut class = Class {
                    name,
                    kind,
                    fields: vec![],
                    dart_members: vec![],
                    build_return: None,
                };
                let mut pub_fields = 0;
                let mut failed = false;
                let mut dart_members = members.clone();
                dart_members.retain(|member| {
                    macro_rules! require {
                        ($($c:expr),+) => {
                            if !($($c)&&+) {
                                failed = true;
                                return false;
                            }
                        };
                    }
                    if failed {
                        return false;
                    }
                    match *member.clone() {
                        ClassMember::Fields {
                            ref metadata,
                            static_: false,
                            ref var_type,
                            ref initializers,
                        } => {
                            require![
                                metadata.is_empty(),
                                var_type.fcv != FinalConstVar::Const,
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
                            if !initializers[0].name.as_str().starts_with('_') {
                                pub_fields += 1;
                            }
                            class.fields.push(ast::FieldDef {
                                mutable: var_type.fcv == FinalConstVar::Var,
                                name: initializers[0].name,
                                ty,
                                default,
                            });
                            false
                        }
                        _ => true,
                    }
                });

                if failed {
                    return;
                }

                dart_members.retain(|member| {
                    macro_rules! require {
                        ($($c:expr),+) => {
                            if !($($c)&&+) {
                                failed = true;
                                return false;
                            }
                        };
                    }
                    if failed {
                        return false;
                    }
                    match *member.clone() {
                        ClassMember::Method(ref meta, ref qualif, ref function) => {
                            match function.name {
                                FnName::Regular(name) if name == "build" => {}
                                _ => {
                                    return true;
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
                                                    class.build_return =
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
                            true
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
                                match class.kind {
                                    ClassKind::StatelessWidget | ClassKind::StatefulWidget => {
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
                                    ClassKind::Plain => initializers.is_empty(),
                                    ClassKind::State { .. } | ClassKind::Remove => false,
                                },
                                sig.required.is_empty(),
                                match class.kind {
                                    ClassKind::StatelessWidget => {
                                        sig.optional.len() == pub_fields + 1 &&
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
                                    ClassKind::Plain => sig.optional.len() == pub_fields,
                                    ClassKind::StatefulWidget |
                                    ClassKind::State { .. } |
                                    ClassKind::Remove => false,
                                }
                            ];

                            for (i, arg) in sig.optional.iter().enumerate() {
                                match class.kind {
                                    ClassKind::StatelessWidget if i == 0 => {
                                        continue;
                                    }
                                    _ => {}
                                }
                                require![
                                    arg.metadata.is_empty(),
                                    !arg.covariant,
                                    arg.field,
                                    !arg.var.name.as_str().starts_with('_'),
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
                                for field in &mut class.fields {
                                    if field.name == arg.var.name {
                                        field.default = default;
                                        break;
                                    }
                                }
                            }
                            false
                        }
                        _ => true,
                    }
                });
                if failed {
                    return;
                }
                class.dart_members = dart_members;
                self.classes.insert(item, class);
            }
            _ => {}
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
