use dart::ast::{Args, ClassMember, ConstructorInitializer, Expr, FnBody, FnSig, ForLoop, Function,
                Item, Metadata, Module, Qualified, Statement, StringLiteral, Suffix, Type,
                TypeParameter, VarDef};
use node::Node;

pub trait Visitor: Sized {
    fn visit_module(&mut self, module: Node<Module>) {
        walk_module(self, module)
    }
    fn visit_item(&mut self, item: Node<Item>) {
        walk_item(self, item)
    }
    fn visit_class_member(&mut self, class_member: Node<ClassMember>) {
        walk_class_member(self, class_member)
    }
    fn visit_constructor_initializer(&mut self, initializer: &ConstructorInitializer) {
        walk_constructor_initializer(self, initializer)
    }
    fn visit_metadata(&mut self, metadata: &Metadata) {
        walk_metadata(self, metadata)
    }
    fn visit_qualified(&mut self, qualified: Node<Qualified>) {
        walk_qualified(self, qualified)
    }
    fn visit_generics(&mut self, generics: &[Node<TypeParameter>]) {
        walk_generics(self, generics)
    }
    fn visit_type(&mut self, ty: Node<Type>) {
        walk_type(self, ty)
    }
    fn visit_function(&mut self, function: Node<Function>) {
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
    fn visit_block(&mut self, statements: &[Node<Statement>]) {
        walk_block(self, statements)
    }
    fn visit_var_def(&mut self, var: Node<VarDef>) {
        walk_var_def(self, var)
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

pub trait Visit {
    fn visit<V: Visitor>(&self, visitor: &mut V);
}

impl Visit for Node<Module> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_module(self.clone());
    }
}

impl Visit for Node<Item> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_item(self.clone());
    }
}

impl Visit for Node<ClassMember> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_class_member(self.clone());
    }
}

impl Visit for ConstructorInitializer {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_constructor_initializer(self);
    }
}

impl Visit for Metadata {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_metadata(self);
    }
}

impl Visit for Node<Qualified> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_qualified(self.clone());
    }
}

impl Visit for [Node<TypeParameter>] {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_generics(self);
    }
}

impl Visit for Node<Type> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_type(self.clone());
    }
}

impl Visit for Node<Function> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_function(self.clone());
    }
}

impl Visit for FnSig {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_fn_sig(self);
    }
}

impl Visit for FnBody {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_fn_body(self);
    }
}

impl Visit for Node<Statement> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_statement(self.clone());
    }
}

impl Visit for [Node<Statement>] {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_block(self);
    }
}

impl Visit for Node<VarDef> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_var_def(self.clone());
    }
}

impl Visit for Node<Expr> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_expr(self.clone());
    }
}

impl Visit for Args {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_args(self);
    }
}

impl Visit for Suffix {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_suffix(self);
    }
}

impl Visit for StringLiteral {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_string_literal(self);
    }
}

pub fn walk_module<V: Visitor>(visitor: &mut V, module: Node<Module>) {
    for item in &module.items {
        item.visit(visitor);
    }
}

pub fn walk_item<V: Visitor>(visitor: &mut V, item: Node<Item>) {
    match *item {
        Item::LibraryName { ref metadata, .. } |
        Item::PartOf { ref metadata, .. } |
        Item::Enum { ref metadata, .. } => {
            metadata.visit(visitor);
        }
        Item::Import(ref metadata, ref import) => {
            metadata.visit(visitor);
            import.uri.visit(visitor);
        }
        Item::Export(ref metadata, ref string_literal, _) => {
            metadata.visit(visitor);
            string_literal.visit(visitor);
        }
        Item::Part {
            ref metadata,
            ref uri,
            ref module,
        } => {
            metadata.visit(visitor);
            uri.visit(visitor);
            module.visit(visitor);
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
            metadata.visit(visitor);
            generics.visit(visitor);
            if let Some(ref superclass) = *superclass {
                superclass.visit(visitor);
            }
            for mixin in mixins {
                mixin.visit(visitor);
            }
            for interface in interfaces {
                interface.visit(visitor);
            }
            for member in members {
                member.visit(visitor);
            }
        }
        Item::MixinClass {
            ref metadata,
            ref generics,
            ref mixins,
            ref interfaces,
            ..
        } => {
            metadata.visit(visitor);
            generics.visit(visitor);
            for mixin in mixins {
                mixin.visit(visitor);
            }
            for interface in interfaces {
                interface.visit(visitor);
            }
        }
        Item::TypeAlias {
            ref metadata,
            ref generics,
            ref ty,
            ..
        } => {
            metadata.visit(visitor);
            generics.visit(visitor);
            ty.visit(visitor);
        }
        Item::Function(ref function) => {
            function.visit(visitor);
        }
        Item::Vars(ref var_type, ref vars) => {
            var_type.ty.visit(visitor);
            for var in vars {
                var.visit(visitor);
            }
        }
    }
}

pub fn walk_class_member<V: Visitor>(visitor: &mut V, class_member: Node<ClassMember>) {
    match *class_member {
        ClassMember::Redirect {
            ref metadata,
            ref sig,
            ref ty,
            ..
        } => {
            metadata.visit(visitor);
            sig.visit(visitor);
            ty.visit(visitor);
        }
        ClassMember::Constructor {
            ref metadata,
            ref sig,
            ref initializers,
            ref function_body,
            ..
        } => {
            metadata.visit(visitor);
            sig.visit(visitor);
            for initializer in initializers {
                initializer.visit(visitor);
            }
            if let Some(ref function_body) = *function_body {
                function_body.visit(visitor);
            }
        }
        ClassMember::Method(ref metadata, _, ref function) => {
            metadata.visit(visitor);
            function.visit(visitor);
        }
        ClassMember::Fields {
            ref metadata,
            ref var_type,
            ref initializers,
            ..
        } => {
            metadata.visit(visitor);
            var_type.ty.visit(visitor);
            for field in initializers {
                field.visit(visitor);
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
            args.visit(visitor);
        }
        ConstructorInitializer::Field(_, _, ref expr) => {
            expr.visit(visitor);
        }
    }
}

pub fn walk_metadata<V: Visitor>(visitor: &mut V, metadata: &Metadata) {
    for metadata_item in metadata {
        metadata_item.qualified.visit(visitor);
        if let Some(ref arguments) = metadata_item.arguments {
            arguments.visit(visitor);
        }
    }
}

pub fn walk_qualified<V: Visitor>(visitor: &mut V, qualified: Node<Qualified>) {
    if let Some(ref prefix) = qualified.prefix {
        prefix.visit(visitor);
    }
    for ty in &qualified.params {
        ty.visit(visitor);
    }
}

pub fn walk_generics<V: Visitor>(visitor: &mut V, generics: &[Node<TypeParameter>]) {
    for generic in generics {
        if let Some(ref extension) = generic.extends {
            extension.visit(visitor);
        }
    }
}

pub fn walk_type<V: Visitor>(visitor: &mut V, ty: Node<Type>) {
    match *ty {
        Type::Path(ref qualified) => {
            qualified.visit(visitor);
        }
        Type::FunctionOld(ref sig) | Type::Function(ref sig) => {
            sig.visit(visitor);
        }
        Type::Infer => {}
    }
}

pub fn walk_function<V: Visitor>(visitor: &mut V, function: Node<Function>) {
    function.generics.visit(visitor);
    function.sig.visit(visitor);
    if let Some(ref body) = function.body {
        body.visit(visitor);
    }
}

pub fn walk_fn_sig<V: Visitor>(visitor: &mut V, sig: &FnSig) {
    sig.return_type.visit(visitor);
    for arg in &sig.required {
        arg.ty.ty.visit(visitor);
        arg.var.visit(visitor);
    }
    for arg in &sig.optional {
        arg.ty.ty.visit(visitor);
        arg.var.visit(visitor);
    }
}

pub fn walk_fn_body<V: Visitor>(visitor: &mut V, fn_body: &FnBody) {
    match *fn_body {
        FnBody::Arrow(ref expr) => {
            expr.visit(visitor);
        }
        FnBody::Block(ref statement) => {
            statement.visit(visitor);
        }
        FnBody::Native(ref string_literal) => if let Some(ref string_literal) = *string_literal {
            string_literal.visit(visitor);
        },
    }
}

pub fn walk_statement<V: Visitor>(visitor: &mut V, statement: Node<Statement>) {
    match *statement {
        Statement::Block(ref statements) => {
            statements.visit(visitor);
        }
        Statement::Vars(ref var_type, ref vars) => {
            var_type.ty.visit(visitor);
            for var in vars {
                var.visit(visitor);
            }
        }
        Statement::Function(ref function) => {
            function.visit(visitor);
        }
        Statement::For(_, ref for_loop, ref statement) => {
            match *for_loop {
                ForLoop::CLike(ref statement, ref expr, ref expressions) => {
                    statement.visit(visitor);
                    if let Some(ref expr) = *expr {
                        expr.visit(visitor);
                    }
                    for expression in expressions {
                        expression.visit(visitor);
                    }
                }
                ForLoop::In(_, ref expr) => {
                    expr.visit(visitor);
                }
                ForLoop::InVar(ref var_type, ref var, ref expr) => {
                    var_type.ty.visit(visitor);
                    var.visit(visitor);
                    expr.visit(visitor);
                }
            }
            statement.visit(visitor);
        }
        Statement::While(ref expr, ref statement) => {
            expr.visit(visitor);
            statement.visit(visitor);
        }
        Statement::DoWhile(ref statement, ref expr) => {
            statement.visit(visitor);
            expr.visit(visitor);
        }
        Statement::Switch(ref expr, ref cases) => {
            expr.visit(visitor);
            for case in cases {
                if let Some(ref value) = case.value {
                    value.visit(visitor);
                }
                for statement in &case.statements {
                    statement.visit(visitor);
                }
            }
        }
        Statement::If(ref expr, ref statement, ref optional_statement) => {
            expr.visit(visitor);
            statement.visit(visitor);
            if let Some(ref optional_statement) = *optional_statement {
                optional_statement.visit(visitor);
            }
        }
        Statement::Rethrow => {}
        Statement::Try(ref statement, ref try_parts) => {
            statement.visit(visitor);
            for try_part in try_parts {
                if let Some(ref on) = try_part.on {
                    on.visit(visitor);
                }
                try_part.block.visit(visitor);
            }
        }
        Statement::Break(_) => {}
        Statement::Continue(_) => {}
        Statement::Return(ref expr) | Statement::Expression(ref expr) => {
            if let Some(ref expr) = *expr {
                expr.visit(visitor);
            }
        }
        Statement::Yield(ref expr) | Statement::YieldEach(ref expr) => {
            expr.visit(visitor);
        }
        Statement::Assert(ref args) => {
            args.visit(visitor);
        }
        Statement::Labelled(_, ref statement) => {
            statement.visit(visitor);
        }
    }
}

pub fn walk_block<V: Visitor>(visitor: &mut V, statements: &[Node<Statement>]) {
    for statement in statements {
        statement.visit(visitor);
    }
}

pub fn walk_var_def<V: Visitor>(visitor: &mut V, var: Node<VarDef>) {
    if let Some(ref init) = var.init {
        init.visit(visitor);
    }
}
pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: Node<Expr>) {
    match *expr {
        Expr::Unary(_, ref expr) => expr.visit(visitor),
        Expr::Binary(_, ref a, ref b) => {
            a.visit(visitor);
            b.visit(visitor);
        }
        Expr::Conditional(ref a, ref b, ref c) => {
            a.visit(visitor);
            b.visit(visitor);
            c.visit(visitor);
        }
        Expr::Is(ref expr, ref ty) | Expr::IsNot(ref expr, ref ty) | Expr::As(ref expr, ref ty) => {
            expr.visit(visitor);
            ty.visit(visitor);
        }
        Expr::Suffix(ref expr, ref suffix) => {
            expr.visit(visitor);
            suffix.visit(visitor);
        }
        Expr::Identifier(_) => {}
        Expr::New {
            ref path, ref args, ..
        } => {
            path.visit(visitor);
            args.visit(visitor);
        }
        Expr::List {
            ref element_ty,
            ref elements,
            ..
        } => {
            if let Some(ref element_type) = *element_ty {
                element_type.visit(visitor);
            }
            for expr in elements {
                expr.visit(visitor);
            }
        }
        Expr::Map {
            ref kv_ty, ref kv, ..
        } => {
            if let Some((ref k, ref v)) = *kv_ty {
                k.visit(visitor);
                v.visit(visitor);
            }
            for &(ref k, ref v) in kv {
                k.visit(visitor);
                v.visit(visitor);
            }
        }
        Expr::Number(_) => {}
        Expr::String(ref string_literals) => for sl in string_literals {
            sl.visit(visitor);
        },
        Expr::Symbol(_) => {}
        Expr::Paren(ref expr) => {
            expr.visit(visitor);
        }
        Expr::Throw(ref expr) => {
            expr.visit(visitor);
        }
        Expr::Cascade(ref expr, ref cascade) => {
            expr.visit(visitor);
            for suffix in &cascade.suffixes {
                suffix.visit(visitor);
            }
            if let Some((_, ref expr)) = cascade.assign {
                expr.visit(visitor);
            }
        }
        Expr::Closure(ref fn_sig, ref fn_body) => {
            fn_sig.visit(visitor);
            fn_body.visit(visitor);
        }
    }
}

pub fn walk_args<V: Visitor>(visitor: &mut V, args: &Args) {
    for arg in &args.unnamed {
        arg.visit(visitor);
    }
    for arg in &args.named {
        arg.expr.visit(visitor);
    }
}

pub fn walk_suffix<V: Visitor>(visitor: &mut V, suffix: &Suffix) {
    match *suffix {
        Suffix::Index(ref expr) => {
            expr.visit(visitor);
        }
        Suffix::Call(ref types, ref args) => {
            for ty in types {
                ty.visit(visitor);
            }
            args.visit(visitor);
        }
        Suffix::Field(_) => {}
        Suffix::FieldIfNotNull(_) => {}
    }
}

pub fn walk_string_literal<V: Visitor>(visitor: &mut V, string_literal: &StringLiteral) {
    for &(ref expr, _) in &string_literal.interpolated {
        expr.visit(visitor);
    }
}
