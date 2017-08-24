use dart::ast::{ClassMember, Expr, FnName, ForLoop, Function, Item, Qualified, Statement, Type,
                VarDef};
use dart::visit::{self, Visit, Visitor};
use node::Node;
use std::collections::HashMap;
use syntax::symbol::Symbol;

#[derive(Clone, Debug)]
pub enum Res {
    This,
    Super,
    Var(Node<VarDef>),
    Function(Node<Function>),
    Class(Node<Item>),
    Enum(Node<Item>),
    TypeAlias(Node<Item>),
}

node_field!(res: Res);

pub fn resolve(items: &[Node<Item>]) {
    let mut resolver = Resolver {
        collector: Collector {
            map: HashMap::new(),
        },
    };
    for item in items {
        match **item {
            Item::Class { name, .. } | Item::MixinClass { name, .. } => {
                resolver
                    .collector
                    .map
                    .insert(name, Res::Class(item.clone()));
            }
            Item::Enum { name, .. } => {
                resolver.collector.map.insert(name, Res::Enum(item.clone()));
            }
            Item::TypeAlias { name, .. } => {
                resolver
                    .collector
                    .map
                    .insert(name, Res::TypeAlias(item.clone()));
            }
            Item::Function(ref function) => {
                function.visit(&mut resolver.collector);
            }
            Item::Vars(_, ref vars) => for var in vars {
                var.visit(&mut resolver.collector);
            },
            _ => {}
        }
    }
    for item in items {
        item.visit(&mut resolver);
    }
}

struct Collector {
    map: HashMap<Symbol, Res>,
}

impl Visitor for Collector {
    fn visit_function(&mut self, function: Node<Function>) {
        if let FnName::Regular(name) = function.name {
            self.map.insert(name, Res::Function(function.clone()));
        }
    }
    fn visit_var_def(&mut self, var: Node<VarDef>) {
        self.map.insert(var.name, Res::Var(var.clone()));
    }
}

struct Resolver {
    collector: Collector,
}

impl Resolver {
    fn in_lexical_scope<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        let old_map = self.collector.map.clone();
        let result = f(self);
        self.collector.map = old_map;
        result
    }
}

impl Visitor for Resolver {
    fn visit_item(&mut self, item: Node<Item>) {
        if let Item::Class { ref members, .. } = *item {
            self.in_lexical_scope(|this| {
                for member in members {
                    match **member {
                        ClassMember::Fields {
                            ref initializers, ..
                        } => for field in initializers {
                            field.visit(&mut this.collector);
                        },
                        ClassMember::Method(_, _, ref function) => {
                            function.visit(&mut this.collector);
                        }
                        _ => {}
                    }
                }
                this.collector.map.insert(Symbol::intern("this"), Res::This);
                this.collector
                    .map
                    .insert(Symbol::intern("super"), Res::Super);
                visit::walk_item(this, item.clone());
            });
        } else {
            visit::walk_item(self, item.clone());
        }
    }
    fn visit_function(&mut self, function: Node<Function>) {
        function.visit(&mut self.collector);
        self.in_lexical_scope(|this| visit::walk_function(this, function));
    }
    fn visit_statement(&mut self, statement: Node<Statement>) {
        if let Statement::For(_, ForLoop::InVar(..), _) = *statement {
            self.in_lexical_scope(|this| visit::walk_statement(this, statement));
        } else {
            visit::walk_statement(self, statement);
        }
    }
    fn visit_block(&mut self, statements: &[Node<Statement>]) {
        self.in_lexical_scope(|this| visit::walk_block(this, statements));
    }
    fn visit_var_def(&mut self, var: Node<VarDef>) {
        var.visit(&mut self.collector);
        visit::walk_var_def(self, var)
    }
    fn visit_expr(&mut self, expr: Node<Expr>) {
        if let Expr::Identifier(name) = *expr {
            if let Some(res) = self.collector.map.get(&name) {
                expr.res().set(res.clone());
            }
        }
        visit::walk_expr(self, expr)
    }
    fn visit_type(&mut self, ty: Node<Type>) {
        if let Type::Path(Qualified { prefix: None, name }, _) = *ty {
            if let Some(res) = self.collector.map.get(&name) {
                ty.res().set(res.clone());
            }
        }
        visit::walk_type(self, ty)
    }
}
