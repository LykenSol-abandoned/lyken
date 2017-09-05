use dart::ast::{ClassMember, Expr, FnName, ForLoop, Function, Import, ImportFilter, Item, Module,
                Qualified, Statement, TypeParameter, VarDef};
use dart::visit::{self, Visit, Visitor};
use dart::sdk;
use node::Node;
use std::collections::HashMap;
use syntax::symbol::Symbol;

#[derive(Clone, Debug)]
pub enum Res {
    Void,
    Dynamic,
    Null,
    False,
    True,
    This,
    Super,
    Var(Node<VarDef>),
    Function(Node<Function>),
    Getter(Node<Function>),
    Setter(Node<Function>),
    GetterSetter(Node<Function>, Node<Function>),
    Class(Node<Item>),
    Constructor(Node<ClassMember>),
    Enum(Node<Item>),
    TypeAlias(Node<Item>),
    TypeParameter(Node<TypeParameter>),
}

impl Res {
    pub fn lookup_member(&self, name: Symbol) -> Option<Res> {
        if let Res::Class(ref item) = *self {
            if let Item::Class {
                ref superclass,
                ref members,
                ..
            } = **item
            {
                let mut collector = Collector {
                    map: HashMap::new(),
                    root_module: None,
                };
                collector.collect_class_members(superclass.clone(), members);
                return collector.map.get(&name).cloned();
            }
        }
        None
    }
}

node_field!(res: Res);

pub fn resolve(module: Node<Module>) {
    let mut resolver = Resolver {
        collector: Collector {
            map: HashMap::new(),
            root_module: Some(module.clone()),
        },
    };
    let mut core_prelude = true;
    for item in &module.items {
        match **item {
            Item::LibraryName { ref path, .. } => {
                if *path == [Symbol::intern("dart"), Symbol::intern("core")] {
                    core_prelude = false;
                }
            }
            Item::Import(_, ref import) => {
                resolver.collector.add_import_item(import);
            }
            _ => {}
        }
    }
    if core_prelude {
        resolver.collector.add_import("dart:core", &[]);
    }
    resolver
        .collector
        .map
        .insert(Symbol::intern("void"), Res::Void);
    resolver
        .collector
        .map
        .insert(Symbol::intern("dynamic"), Res::Dynamic);
    resolver
        .collector
        .map
        .insert(Symbol::intern("null"), Res::Null);
    resolver
        .collector
        .map
        .insert(Symbol::intern("false"), Res::False);
    resolver
        .collector
        .map
        .insert(Symbol::intern("true"), Res::True);
    module.visit(&mut resolver.collector);
    module.visit(&mut resolver);
}

struct Collector {
    map: HashMap<Symbol, Res>,
    root_module: Option<Node<Module>>,
}

impl Collector {
    fn add_import_item(&mut self, import: &Import) {
        if import.deferred {
            return;
        }
        if import.as_ident != None {
            return;
        }
        self.add_import(&import.uri.get_simple_string(), &import.filters);
    }
    fn add_import(&mut self, uri: &str, _filters: &[ImportFilter]) {
        let module = sdk::resolve_import(self.root_module.clone().unwrap(), uri);
        let mut import_collector = Collector {
            map: HashMap::new(),
            root_module: Some(module.clone())
        };
        module.visit(&mut import_collector);
        self.map.extend(import_collector.map);
    }

    fn lookup_qualified(&mut self, qualified: &Qualified) -> Option<Res> {
        if let Some(ref prefix) = qualified.prefix {
            self.lookup_qualified(prefix)
                .and_then(|prefix| prefix.lookup_member(qualified.name))
        } else {
            self.map.get(&qualified.name).cloned()
        }
    }

    fn collect_class_members(
        &mut self,
        superclass: Option<Node<Qualified>>,
        members: &[Node<ClassMember>],
    ) {
        if let Some(superclass) = superclass {
            if let Some(Res::Class(item)) = self.lookup_qualified(&superclass) {
                if let Item::Class {
                    ref superclass,
                    ref members,
                    ..
                } = *item
                {
                    self.collect_class_members(superclass.clone(), members);
                } else {
                    println!("superclass is not Class: {:#?}", item);
                }
            } else {
                println!("unknown superclass {:?}", superclass);
            }
        }
        for member in members {
            member.visit(self);
        }
    }
}

impl Visitor for Collector {
    fn visit_item(&mut self, item: Node<Item>) {
        match *item {
            Item::Class { name, .. } | Item::MixinClass { name, .. } => {
                self.map.insert(name, Res::Class(item.clone()));
            }
            Item::Enum { name, .. } => {
                self.map.insert(name, Res::Enum(item.clone()));
            }
            Item::TypeAlias { name, .. } => {
                self.map.insert(name, Res::TypeAlias(item.clone()));
            }
            Item::Function(ref function) => {
                function.visit(self);
            }
            Item::Vars(_, ref vars) => for var in vars {
                var.visit(self);
            },
            Item::Part { ref module, .. } => {
                module.visit(self);
            }
            Item::Export(_, ref uri, ref filters) => {
                self.add_import(&uri.get_simple_string(), filters);
            }
            _ => {}
        }
    }
    fn visit_class_member(&mut self, class_member: Node<ClassMember>) {
        match *class_member {
            ClassMember::Fields {
                ref initializers, ..
            } => for field in initializers {
                field.visit(self);
            },
            ClassMember::Method(_, _, ref function) => {
                function.visit(self);
            }
            ClassMember::Constructor {
                name: Some(name), ..
            } => {
                self.map
                    .insert(name, Res::Constructor(class_member.clone()));
            }
            _ => {}
        }
    }
    fn visit_function(&mut self, function: Node<Function>) {
        match function.name {
            FnName::Regular(name) => {
                self.map.insert(name, Res::Function(function.clone()));
            }
            FnName::Getter(name) => {
                self.map.insert(name, Res::Getter(function.clone()));
            }
            FnName::Setter(name) => {
                self.map.insert(name, Res::Setter(function.clone()));
            }
            _ => {}
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
        if let Item::Class {
            ref members,
            ref superclass,
            ..
        } = *item
        {
            self.in_lexical_scope(|this| {
                this.collector
                    .collect_class_members(superclass.clone(), members);
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
            } else {
                println!("unknown value {}", name);
            }
        }
        visit::walk_expr(self, expr)
    }
    fn visit_qualified(&mut self, qualified: Node<Qualified>) {
        if let Some(res) = self.collector.lookup_qualified(&qualified) {
            qualified.res().set(res);
        } else {
            println!("unknown path {:?}", qualified);
        }
        visit::walk_qualified(self, qualified)
    }
    fn visit_generics(&mut self, generics: &[Node<TypeParameter>]) {
        for generic in generics {
            self.collector
                .map
                .insert(generic.name, Res::TypeParameter(generic.clone()));
        }
        visit::walk_generics(self, generics)
    }
}
