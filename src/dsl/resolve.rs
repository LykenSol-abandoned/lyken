use dart;
use dart::resolve::{Collector, Resolver, TopLevelResolver};
use dart::visit::Visit as DartVisit;
use dsl::ast::{FieldDef, Item};
use dsl::visit::{Visit, Visitor};
use node::Node;
use syntax::symbol::Symbol;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Res {
    Component(Node<Item>),
    Field(Node<FieldDef>),
}

pub fn resolve(items: &[Node<Item>], fully_resolve: bool) {
    let collector = &mut Collector::new();

    collector.import(None, "dart:core", &[], None);

    items.super_visit(collector);
    if collector.has_error {
        return;
    }

    items.super_visit(&mut TopLevelResolver { collector });
    if fully_resolve {
        items.super_visit(&mut Resolver { collector });
    }
}

impl Collector {
    pub fn record_dsl<S: Into<Symbol>>(&mut self, name: S, res: Res) {
        self.record(name, dart::resolve::Res::Dsl(res));
    }
}

impl Visitor for Collector {
    fn dsl_item(&mut self, item: Node<Item>) {
        match *item {
            Item::ComponentDef { name, .. } => {
                self.record_dsl(name, Res::Component(item.clone()));
            }
            Item::Dart(ref item) => {
                item.visit(self);
            }
        }
    }
    fn dsl_field_def(&mut self, field_def: Node<FieldDef>) {
        self.record_dsl(field_def.name, Res::Field(field_def.clone()));
    }
}

impl<'a> Visitor for TopLevelResolver<'a> {}

impl<'a> Visitor for Resolver<'a> {
    fn dsl_item(&mut self, item: Node<Item>) {
        self.in_lexical_scope(|this| {
            if let Item::ComponentDef { .. } = *item {
                item.super_visit(this.collector);
            }
            item.super_visit(this);
        });
    }
}
