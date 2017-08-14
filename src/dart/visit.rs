use dart::ast::{Args, ClassMember, ConstructorInitializer, Expr, FnBody, FnSig, ForLoop, Function,
                Item, Metadata, Statement, StringLiteral, Suffix, Type, TypeParameter};
use node::Node;

pub trait Visitor: Sized {
    fn visit_item(&mut self, item: &Item) {
        walk_item(self, item)
    }
    fn visit_class_member(&mut self, class_member: &ClassMember) {
        walk_class_member(self, class_member)
    }
    fn visit_constructor_initializer(&mut self, initializer: &ConstructorInitializer) {
        walk_constructor_initializer(self, initializer)
    }
    fn visit_metadata(&mut self, metadata: &Metadata) {
        walk_metadata(self, metadata)
    }
    fn visit_generics(&mut self, generics: &[TypeParameter]) {
        walk_generics(self, generics)
    }
    fn visit_type(&mut self, ty: Node<Type>) {
        walk_type(self, ty)
    }
    fn visit_function(&mut self, function: &Function) {
        walk_function(self, function)
    }
    fn visit_fn_sig(&mut self, sig: &FnSig) {
        walk_fn_sig(self, sig)
    }
    fn visit_fn_body(&mut self, fn_body: &FnBody) {
        walk_fn_body(self, fn_body)
    }
    fn visit_statement(&mut self, statement: Node<Statement>) {
        walk_statement(self, statement)
    }
    fn visit_expr(&mut self, expr: Node<Expr>) {
        walk_expr(self, expr)
    }
    fn visit_args(&mut self, args: &Args) {
        walk_args(self, args)
    }
    fn visit_suffix(&mut self, suffix: &Suffix) {
        walk_suffix(self, suffix)
    }
    fn visit_string_literal(&mut self, string_literal: &StringLiteral) {
        walk_string_literal(self, string_literal)
    }
}

pub fn walk_item<V: Visitor>(visitor: &mut V, item: &Item) {
    match *item {
        Item::LibraryName { ref metadata, .. } |
        Item::PartOf { ref metadata, .. } |
        Item::Enum { ref metadata, .. } => {
            visitor.visit_metadata(metadata);
        }
        Item::Import(ref metadata, ref import) => {
            visitor.visit_metadata(metadata);
            visitor.visit_string_literal(&import.uri);
        }
        Item::Export(ref metadata, ref string_literal, _) => {
            visitor.visit_metadata(metadata);
            visitor.visit_string_literal(string_literal);
        }
        Item::Part {
            ref metadata,
            ref uri,
        } => {
            visitor.visit_metadata(metadata);
            visitor.visit_string_literal(uri);
        }
        Item::Class {
            ref metadata,
            ref generics,
            ref superclass,
            ref mixins,
            ref interfaces,
            ref members,
            ..
        } => {
            visitor.visit_metadata(metadata);
            visitor.visit_generics(generics);
            if let Some(ref superclass) = *superclass {
                visitor.visit_type(superclass.clone());
            }
            for mixin in mixins {
                visitor.visit_type(mixin.clone());
            }
            for interface in interfaces {
                visitor.visit_type(interface.clone());
            }
            for member in members {
                visitor.visit_class_member(member);
            }
        }
        Item::MixinClass {
            ref metadata,
            ref generics,
            ref mixins,
            ref interfaces,
            ..
        } => {
            visitor.visit_metadata(metadata);
            visitor.visit_generics(generics);
            for mixin in mixins {
                visitor.visit_type(mixin.clone());
            }
            for interface in interfaces {
                visitor.visit_type(interface.clone());
            }
        }
        Item::TypeAlias {
            ref metadata,
            ref generics,
            ref ty,
            ..
        } => {
            visitor.visit_metadata(metadata);
            visitor.visit_generics(generics);
            visitor.visit_type(ty.clone());
        }
        Item::Function(ref function) => {
            visitor.visit_function(&function);
        }
        Item::Global(ref var_type, ref vars) => {
            visitor.visit_type(var_type.ty.clone());
            for _var in vars {
                if let Some(ref init) = _var.init {
                    visitor.visit_expr(init.clone());
                }
            }
        }
    }
}

pub fn walk_class_member<V: Visitor>(visitor: &mut V, class_member: &ClassMember) {
    match *class_member {
        ClassMember::Redirect {
            ref metadata,
            ref sig,
            ref ty,
            ..
        } => {
            visitor.visit_metadata(metadata);
            visitor.visit_fn_sig(sig);
            visitor.visit_type(ty.clone());
        }
        ClassMember::Constructor {
            ref metadata,
            ref sig,
            ref initializers,
            ref function_body,
            ..
        } => {
            visitor.visit_metadata(metadata);
            visitor.visit_fn_sig(sig);
            for initializer in initializers {
                visitor.visit_constructor_initializer(initializer);
            }
            if let Some(ref function_body) = *function_body {
                visitor.visit_fn_body(function_body);
            }
        }
        ClassMember::Method(ref metadata, _, ref function) => {
            visitor.visit_metadata(metadata);
            visitor.visit_function(function);
        }
        ClassMember::Fields {
            ref metadata,
            ref var_type,
            ref initializers,
            ..
        } => {
            visitor.visit_metadata(metadata);
            visitor.visit_type(var_type.ty.clone());
            for initializer in initializers {
                if let Some(ref init) = initializer.init {
                    visitor.visit_expr(init.clone());
                }
            }
        }
    }
}

pub fn walk_constructor_initializer<V: Visitor>(
    visitor: &mut V,
    initializer: &ConstructorInitializer,
) {
    match *initializer {
        ConstructorInitializer::Super(_, ref args) |
        ConstructorInitializer::This(_, ref args) |
        ConstructorInitializer::Assert(ref args) => {
            visitor.visit_args(args);
        }
        ConstructorInitializer::Field(_, _, ref expr) => {
            visitor.visit_expr(expr.clone());
        }
    }
}

pub fn walk_metadata<V: Visitor>(visitor: &mut V, metadata: &Metadata) {
    for metadata_item in metadata {
        if let Some(ref arguments) = metadata_item.arguments {
            visitor.visit_args(arguments);
        }
    }
}

pub fn walk_generics<V: Visitor>(visitor: &mut V, generics: &[TypeParameter]) {
    for generic in generics {
        if let Some(ref extension) = generic.extends {
            visitor.visit_type(extension.clone());
        }
    }
}

pub fn walk_type<V: Visitor>(visitor: &mut V, ty: Node<Type>) {
    match *ty {
        Type::Path(_, ref params) => for ty in params {
            visitor.visit_type(ty.clone());
        },
        Type::FunctionOld(ref sig) | Type::Function(ref sig) => {
            visitor.visit_fn_sig(sig);
        }
        Type::Infer => {}
    }
}

pub fn walk_function<V: Visitor>(visitor: &mut V, function: &Function) {
    visitor.visit_generics(&function.generics);
    visitor.visit_fn_sig(&function.sig);
    if let Some(ref body) = function.body {
        visitor.visit_fn_body(body);
    }
}

pub fn walk_fn_sig<V: Visitor>(visitor: &mut V, sig: &FnSig) {
    visitor.visit_type(sig.return_type.clone());
    for arg in &sig.required {
        visitor.visit_type(arg.ty.ty.clone());
    }
    for arg in &sig.optional {
        visitor.visit_type(arg.arg.ty.ty.clone());
        if let Some(ref default) = arg.default {
            visitor.visit_expr(default.clone());
        }
    }
}

pub fn walk_fn_body<V: Visitor>(visitor: &mut V, fn_body: &FnBody) {
    match *fn_body {
        FnBody::Arrow(ref expr) => {
            visitor.visit_expr(expr.clone());
        }
        FnBody::Block(ref statement) => {
            visitor.visit_statement(statement.clone());
        }
        FnBody::Native(ref string_literal) => if let Some(ref string_literal) = *string_literal {
            visitor.visit_string_literal(string_literal);
        },
    }
}

pub fn walk_statement<V: Visitor>(visitor: &mut V, statement: Node<Statement>) {
    match *statement {
        Statement::Block(ref statements) => for statement in statements {
            visitor.visit_statement(statement.clone());
        },
        Statement::Var(ref var_type, ref vars) => {
            visitor.visit_type(var_type.ty.clone());
            for _var in vars {
                if let Some(ref init) = _var.init {
                    visitor.visit_expr(init.clone());
                }
            }
        }
        Statement::Function(ref function) => {
            visitor.visit_function(function.clone());
        }
        Statement::For(_, ref for_loop, ref statement) => {
            match *for_loop {
                ForLoop::CLike(ref statement, ref expr, ref expressions) => {
                    visitor.visit_statement(statement.clone());
                    if let Some(ref expr) = *expr {
                        visitor.visit_expr(expr.clone());
                    }
                    for expression in expressions {
                        visitor.visit_expr(expression.clone());
                    }
                }
                ForLoop::In(ref var_type, _, ref expr) => {
                    if let Some(ref var_type) = *var_type {
                        visitor.visit_type(var_type.ty.clone());
                    }
                    visitor.visit_expr(expr.clone());
                }
            }
            visitor.visit_statement(statement.clone());
        }
        Statement::While(ref expr, ref statement) => {
            visitor.visit_expr(expr.clone());
            visitor.visit_statement(statement.clone());
        }
        Statement::DoWhile(ref statement, ref expr) => {
            visitor.visit_statement(statement.clone());
            visitor.visit_expr(expr.clone());
        }
        Statement::Switch(ref expr, ref cases) => {
            visitor.visit_expr(expr.clone());
            for case in cases {
                if let Some(ref value) = case.value {
                    visitor.visit_expr(value.clone());
                }
                for statement in &case.statements {
                    visitor.visit_statement(statement.clone());
                }
            }
        }
        Statement::If(ref expr, ref statement, ref optional_statement) => {
            visitor.visit_expr(expr.clone());
            visitor.visit_statement(statement.clone());
            if let Some(ref optional_statement) = *optional_statement {
                visitor.visit_statement(optional_statement.clone());
            }
        }
        Statement::Rethrow => {}
        Statement::Try(ref statement, ref try_parts) => {
            visitor.visit_statement(statement.clone());
            for try_part in try_parts {
                if let Some(ref on) = try_part.on {
                    visitor.visit_type(on.clone());
                }
                visitor.visit_statement(try_part.block.clone());
            }
        }
        Statement::Break(_) => {}
        Statement::Continue(_) => {}
        Statement::Return(ref expr) | Statement::Expression(ref expr) => {
            if let Some(ref expr) = *expr {
                visitor.visit_expr(expr.clone());
            }
        }
        Statement::Yield(ref expr) | Statement::YieldEach(ref expr) => {
            visitor.visit_expr(expr.clone());
        }
        Statement::Assert(ref args) => {
            visitor.visit_args(args);
        }
        Statement::Labelled(_, ref statement) => {
            visitor.visit_statement(statement.clone());
        }
    }
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: Node<Expr>) {
    match *expr {
        Expr::Unary(_, ref expr) => visitor.visit_expr(expr.clone()),
        Expr::Binary(_, ref a, ref b) => {
            visitor.visit_expr(a.clone());
            visitor.visit_expr(b.clone());
        }
        Expr::Conditional(ref a, ref b, ref c) => {
            visitor.visit_expr(a.clone());
            visitor.visit_expr(b.clone());
            visitor.visit_expr(c.clone());
        }
        Expr::Is(ref expr, ref ty) | Expr::IsNot(ref expr, ref ty) | Expr::As(ref expr, ref ty) => {
            visitor.visit_expr(expr.clone());
            visitor.visit_type(ty.clone());
        }
        Expr::Suffix(ref expr, ref suffix) => {
            visitor.visit_expr(expr.clone());
            visitor.visit_suffix(suffix);
        }
        Expr::Identifier(_) => {}
        Expr::New {
            ref ty, ref args, ..
        } => {
            visitor.visit_type(ty.clone());
            visitor.visit_args(args.clone());
        }
        Expr::List {
            ref element_ty,
            ref elements,
            ..
        } => {
            if let Some(ref element_type) = *element_ty {
                visitor.visit_type(element_type.clone());
            }
            for expr in elements {
                visitor.visit_expr(expr.clone());
            }
        }
        Expr::Map {
            ref kv_ty, ref kv, ..
        } => {
            if let Some((ref k, ref v)) = *kv_ty {
                visitor.visit_type(k.clone());
                visitor.visit_type(v.clone());
            }
            for &(ref k, ref v) in kv {
                visitor.visit_expr(k.clone());
                visitor.visit_expr(v.clone());
            }
        }
        Expr::Number(_) => {}
        Expr::String(ref string_literals) => for sl in string_literals {
            visitor.visit_string_literal(sl);
        },
        Expr::Symbol(_) => {}
        Expr::Paren(ref expr) => {
            visitor.visit_expr(expr.clone());
        }
        Expr::Throw(ref expr) => {
            visitor.visit_expr(expr.clone());
        }
        Expr::Cascade(ref expr, ref cascade) => {
            visitor.visit_expr(expr.clone());
            for suffix in &cascade.suffixes {
                visitor.visit_suffix(suffix);
            }
            if let Some((_, ref expr)) = cascade.assign {
                visitor.visit_expr(expr.clone());
            }
        }
        Expr::Closure(ref fn_sig, ref fn_body) => {
            visitor.visit_fn_sig(fn_sig.clone());
            visitor.visit_fn_body(fn_body.clone());
        }
    }
}

pub fn walk_args<V: Visitor>(visitor: &mut V, args: &Args) {
    for arg in &args.unnamed {
        visitor.visit_expr(arg.clone());
    }
    for arg in &args.named {
        visitor.visit_expr(arg.expr.clone());
    }
}

pub fn walk_suffix<V: Visitor>(visitor: &mut V, suffix: &Suffix) {
    match *suffix {
        Suffix::Index(ref expr) => {
            visitor.visit_expr(expr.clone());
        }
        Suffix::Call(ref types, ref args) => {
            for ty in types {
                visitor.visit_type(ty.clone());
            }
            visitor.visit_args(args);
        }
        Suffix::Field(_) => {}
        Suffix::FieldIfNotNull(_) => {}
    }
}

pub fn walk_string_literal<V: Visitor>(visitor: &mut V, string_literal: &StringLiteral) {
    for &(ref expr, _) in &string_literal.interpolated {
        visitor.visit_expr(expr.clone());
    }
}
