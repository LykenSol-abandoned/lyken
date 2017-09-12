use dart::ast::{Args, ClassMember, ConstructorInitializer, Expr, FnBody, FnSig, ForLoop, Function,
                Item, Metadata, Module, Qualified, Statement, StringLiteral, Suffix, TryPart,
                Type, TypeParameter, VarDef};
use node::Node;

pub trait Visitor: Sized {
    fn visit_node<T: VisitNode>(&mut self, node: Node<T>) {
        VisitNode::visit(node, self)
    }
    fn dart_module(&mut self, module: Node<Module>) {
        module.walk(self)
    }
    fn dart_item(&mut self, item: Node<Item>) {
        item.walk(self)
    }
    fn dart_class_member(&mut self, class_member: Node<ClassMember>) {
        class_member.walk(self)
    }
    fn dart_constructor_initializer(&mut self, initializer: &ConstructorInitializer) {
        initializer.walk(self)
    }
    fn dart_metadata(&mut self, metadata: &Metadata) {
        metadata.walk(self)
    }
    fn dart_qualified(&mut self, qualified: Node<Qualified>) {
        qualified.walk(self)
    }
    fn dart_generics(&mut self, generics: &[Node<TypeParameter>]) {
        generics.walk(self)
    }
    fn dart_type(&mut self, ty: Node<Type>) {
        ty.walk(self)
    }
    fn dart_function(&mut self, function: Node<Function>) {
        function.walk(self)
    }
    fn dart_fn_sig(&mut self, sig: &FnSig) {
        sig.walk(self)
    }
    fn dart_fn_body(&mut self, fn_body: &FnBody) {
        fn_body.walk(self)
    }
    fn dart_try_part(&mut self, try_part: &TryPart) {
        try_part.walk(self)
    }
    fn dart_statement(&mut self, statement: Node<Statement>) {
        statement.walk(self)
    }
    fn dart_block(&mut self, statements: &[Node<Statement>]) {
        statements.walk(self)
    }
    fn dart_var_def(&mut self, var: Node<VarDef>) {
        var.walk(self)
    }
    fn dart_expr(&mut self, expr: Node<Expr>) {
        expr.walk(self)
    }
    fn dart_args(&mut self, args: &Args) {
        args.walk(self)
    }
    fn dart_suffix(&mut self, suffix: &Suffix) {
        suffix.walk(self)
    }
    fn dart_string_literal(&mut self, string_literal: &StringLiteral) {
        string_literal.walk(self)
    }
}

pub trait Visit {
    fn visit<V: Visitor>(&self, visitor: &mut V);
    fn walk<V: Visitor>(&self, visitor: &mut V);
}

pub trait VisitNode: 'static {
    fn visit<V: Visitor>(node: Node<Self>, visitor: &mut V);
    fn walk<V: Visitor>(node: Node<Self>, visitor: &mut V);
}

impl<T: VisitNode> Visit for Node<T> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_node(self.clone());
    }
    fn walk<V: Visitor>(&self, visitor: &mut V) {
        VisitNode::walk(self.clone(), visitor)
    }
}

impl VisitNode for Module {
    fn visit<V: Visitor>(module: Node<Self>, visitor: &mut V) {
        visitor.dart_module(module);
    }

    fn walk<V: Visitor>(module: Node<Self>, visitor: &mut V) {
        for item in &module.items {
            item.visit(visitor);
        }
    }
}

impl VisitNode for Item {
    fn visit<V: Visitor>(item: Node<Self>, visitor: &mut V) {
        visitor.dart_item(item);
    }
    fn walk<V: Visitor>(item: Node<Self>, visitor: &mut V) {
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
}

impl VisitNode for ClassMember {
    fn visit<V: Visitor>(class_member: Node<Self>, visitor: &mut V) {
        visitor.dart_class_member(class_member);
    }
    fn walk<V: Visitor>(class_member: Node<Self>, visitor: &mut V) {
        match *class_member {
            ClassMember::Redirect {
                ref metadata,
                ref sig,
                ref path,
                ..
            } => {
                metadata.visit(visitor);
                sig.visit(visitor);
                path.visit(visitor);
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
}

impl Visit for ConstructorInitializer {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dart_constructor_initializer(self);
    }
    fn walk<V: Visitor>(&self, visitor: &mut V) {
        match *self {
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
}

impl Visit for Metadata {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dart_metadata(self);
    }
    fn walk<V: Visitor>(&self, visitor: &mut V) {
        for metadata_item in self {
            metadata_item.qualified.visit(visitor);
            if let Some(ref arguments) = metadata_item.arguments {
                arguments.visit(visitor);
            }
        }
    }
}

impl VisitNode for Qualified {
    fn visit<V: Visitor>(qualified: Node<Self>, visitor: &mut V) {
        visitor.dart_qualified(qualified);
    }
    fn walk<V: Visitor>(qualified: Node<Self>, visitor: &mut V) {
        if let Some(ref prefix) = qualified.prefix {
            prefix.visit(visitor);
        }
        for ty in &qualified.params {
            ty.visit(visitor);
        }
    }
}

impl Visit for [Node<TypeParameter>] {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dart_generics(self);
    }
    fn walk<V: Visitor>(&self, visitor: &mut V) {
        for generic in self {
            if let Some(ref extension) = generic.extends {
                extension.visit(visitor);
            }
        }
    }
}

impl VisitNode for Type {
    fn visit<V: Visitor>(ty: Node<Self>, visitor: &mut V) {
        visitor.dart_type(ty);
    }
    fn walk<V: Visitor>(ty: Node<Self>, visitor: &mut V) {
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
}

impl VisitNode for Function {
    fn visit<V: Visitor>(function: Node<Self>, visitor: &mut V) {
        visitor.dart_function(function);
    }
    fn walk<V: Visitor>(function: Node<Self>, visitor: &mut V) {
        function.generics.visit(visitor);
        function.sig.visit(visitor);
        if let Some(ref body) = function.body {
            body.visit(visitor);
        }
    }
}

impl Visit for FnSig {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dart_fn_sig(self);
    }
    fn walk<V: Visitor>(&self, visitor: &mut V) {
        self.return_type.visit(visitor);
        for arg in &self.required {
            arg.ty.ty.visit(visitor);
            arg.var.visit(visitor);
        }
        for arg in &self.optional {
            arg.ty.ty.visit(visitor);
            arg.var.visit(visitor);
        }
    }
}

impl Visit for FnBody {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dart_fn_body(self);
    }
    fn walk<V: Visitor>(&self, visitor: &mut V) {
        match *self {
            FnBody::Arrow(ref expr) => {
                expr.visit(visitor);
            }
            FnBody::Block(ref statement) => {
                statement.visit(visitor);
            }
            FnBody::Native(ref string_literal) => {
                if let Some(ref string_literal) = *string_literal {
                    string_literal.visit(visitor);
                }
            }
        }
    }
}

impl Visit for TryPart {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dart_try_part(self);
    }
    fn walk<V: Visitor>(&self, visitor: &mut V) {
        if let Some(ref on) = self.on {
            on.visit(visitor);
        }
        if let Some(ref catch) = self.catch {
            catch.exception.visit(visitor);
            if let Some(ref trace) = catch.trace {
                trace.visit(visitor);
            }
        }
        self.block.visit(visitor);
    }
}

impl VisitNode for Statement {
    fn visit<V: Visitor>(statement: Node<Self>, visitor: &mut V) {
        visitor.dart_statement(statement);
    }
    fn walk<V: Visitor>(statement: Node<Self>, visitor: &mut V) {
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
                    try_part.visit(visitor);
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
}

impl Visit for [Node<Statement>] {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dart_block(self);
    }
    fn walk<V: Visitor>(&self, visitor: &mut V) {
        for statement in self {
            statement.visit(visitor);
        }
    }
}

impl VisitNode for VarDef {
    fn visit<V: Visitor>(var_def: Node<Self>, visitor: &mut V) {
        visitor.dart_var_def(var_def);
    }
    fn walk<V: Visitor>(var_def: Node<Self>, visitor: &mut V) {
        if let Some(ref init) = var_def.init {
            init.visit(visitor);
        }
    }
}

impl VisitNode for Expr {
    fn visit<V: Visitor>(expr: Node<Self>, visitor: &mut V) {
        visitor.dart_expr(expr);
    }
    fn walk<V: Visitor>(expr: Node<Self>, visitor: &mut V) {
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
            Expr::Is(ref expr, ref ty) |
            Expr::IsNot(ref expr, ref ty) |
            Expr::As(ref expr, ref ty) => {
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
}

impl Visit for Args {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dart_args(self);
    }
    fn walk<V: Visitor>(&self, visitor: &mut V) {
        for arg in &self.unnamed {
            arg.visit(visitor);
        }
        for arg in &self.named {
            arg.expr.visit(visitor);
        }
    }
}

impl Visit for Suffix {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dart_suffix(self);
    }
    fn walk<V: Visitor>(&self, visitor: &mut V) {
        match *self {
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
}

impl Visit for StringLiteral {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.dart_string_literal(self);
    }
    fn walk<V: Visitor>(&self, visitor: &mut V) {
        for &(ref expr, _) in &self.interpolated {
            expr.visit(visitor);
        }
    }
}
