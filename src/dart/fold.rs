use dart::ast::{ArgDef, Args, Cascade, CatchPart, ClassMember, ConstructorInitializer, Expr,
                FnBody, FnSig, ForLoop, Function, Item, Meta, MetaItem, Module, NamedArg,
                Qualified, Statement, StringLiteral, Suffix, SwitchCase, TryPart, Type,
                TypeParameter, VarDef, VarType};
use node::Node;

pub trait Folder: Sized {
    fn dart_module(&mut self, module: Node<Module>) -> Node<Module> {
        module.super_fold(self)
    }
    fn dart_item(&mut self, item: Node<Item>) -> Node<Item> {
        item.super_fold(self)
    }
    fn dart_class_member(&mut self, class_member: Node<ClassMember>) -> Node<ClassMember> {
        class_member.super_fold(self)
    }
    fn dart_constructor_initializer(
        &mut self,
        initializer: &ConstructorInitializer,
    ) -> ConstructorInitializer {
        initializer.super_fold(self)
    }
    fn dart_meta_item(&mut self, meta_item: &MetaItem) -> MetaItem {
        meta_item.super_fold(self)
    }
    fn dart_meta(&mut self, meta: &Meta) -> Meta {
        meta.super_fold(self)
    }
    fn dart_qualified(&mut self, qualified: Node<Qualified>) -> Node<Qualified> {
        qualified.super_fold(self)
    }
    fn dart_type_parameter(&mut self, type_parameter: Node<TypeParameter>) -> Node<TypeParameter> {
        type_parameter.super_fold(self)
    }
    fn dart_generics(&mut self, generics: &Vec<Node<TypeParameter>>) -> Vec<Node<TypeParameter>> {
        generics.super_fold(self)
    }
    fn dart_type(&mut self, ty: Node<Type>) -> Node<Type> {
        ty.super_fold(self)
    }
    fn dart_function(&mut self, function: Node<Function>) -> Node<Function> {
        function.super_fold(self)
    }
    fn dart_arg_def(&mut self, arg_def: &ArgDef) -> ArgDef {
        arg_def.super_fold(self)
    }
    fn dart_fn_sig(&mut self, sig: &FnSig) -> FnSig {
        sig.super_fold(self)
    }
    fn dart_fn_body(&mut self, fn_body: &FnBody) -> FnBody {
        fn_body.super_fold(self)
    }
    fn dart_try_part(&mut self, try_part: &TryPart) -> TryPart {
        try_part.super_fold(self)
    }
    fn dart_for_loop(&mut self, for_loop: &ForLoop) -> ForLoop {
        for_loop.super_fold(self)
    }
    fn dart_statement(&mut self, statement: Node<Statement>) -> Node<Statement> {
        statement.super_fold(self)
    }
    fn dart_block(&mut self, statements: &Vec<Node<Statement>>) -> Vec<Node<Statement>> {
        statements.super_fold(self)
    }
    fn dart_var_def(&mut self, var: Node<VarDef>) -> Node<VarDef> {
        var.super_fold(self)
    }
    fn dart_expr(&mut self, expr: Node<Expr>) -> Node<Expr> {
        expr.super_fold(self)
    }
    fn dart_args(&mut self, args: &Args) -> Args {
        args.super_fold(self)
    }
    fn dart_suffix(&mut self, suffix: &Suffix) -> Suffix {
        suffix.super_fold(self)
    }
    fn dart_cascade(&mut self, cascade: &Cascade) -> Cascade {
        cascade.super_fold(self)
    }
    fn dart_string_literal(&mut self, string_literal: &StringLiteral) -> StringLiteral {
        string_literal.super_fold(self)
    }
    fn dart_var_ty(&mut self, var_ty: &VarType) -> VarType {
        var_ty.super_fold(self)
    }
    fn dart_switch_case(&mut self, switch_case: &SwitchCase) -> SwitchCase {
        switch_case.super_fold(self)
    }
    fn dart_named_arg(&mut self, named_arg: &NamedArg) -> NamedArg {
        named_arg.super_fold(self)
    }
}

pub trait Fold {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self;
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self;
}

impl Fold for Node<Module> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_module(self.clone())
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        let Module {
            ref path,
            ref items,
            has_error,
        } = **self;
        Node::new(Module {
            path: path.clone(),
            items: items.iter().map(|item| item.fold(folder)).collect(),
            has_error,
        })
    }
}

impl Fold for Node<Item> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_item(self.clone())
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        match **self {
            Item::LibraryName { ref meta, ref path } => Node::new(Item::LibraryName {
                meta: meta.fold(folder),
                path: path.clone(),
            }),
            Item::Import(ref meta, ref import) => {
                Node::new(Item::Import(meta.fold(folder), import.clone()))
            }
            Item::Export(ref meta, ref str_lit, ref import_filters) => Node::new(Item::Export(
                meta.fold(folder),
                str_lit.clone(),
                import_filters.clone(),
            )),
            Item::Part {
                ref meta,
                ref uri,
                ref module,
            } => Node::new(Item::Part {
                meta: meta.fold(folder),
                uri: uri.clone(),
                module: module.fold(folder),
            }),
            Item::PartOf { ref meta, ref path } => Node::new(Item::PartOf {
                meta: meta.fold(folder),
                path: path.clone(),
            }),
            Item::Class {
                ref meta,
                abstract_,
                name,
                ref generics,
                ref superclass,
                ref mixins,
                ref interfaces,
                ref members,
            } => Node::new(Item::Class {
                meta: meta.fold(folder),
                abstract_,
                name,
                generics: generics
                    .iter()
                    .map(|generic| generic.fold(folder))
                    .collect(),
                superclass: superclass
                    .as_ref()
                    .map(|superclass| superclass.fold(folder)),
                mixins: mixins.iter().map(|mixin| mixin.fold(folder)).collect(),
                interfaces: interfaces
                    .iter()
                    .map(|interface| interface.fold(folder))
                    .collect(),
                members: members.iter().map(|member| member.fold(folder)).collect(),
            }),
            Item::MixinClass {
                ref meta,
                abstract_,
                name,
                ref generics,
                ref mixins,
                ref interfaces,
            } => Node::new(Item::MixinClass {
                meta: meta.fold(folder),
                abstract_,
                name,
                generics: generics
                    .iter()
                    .map(|generic| generic.fold(folder))
                    .collect(),
                mixins: mixins.iter().map(|mixin| mixin.fold(folder)).collect(),
                interfaces: interfaces
                    .iter()
                    .map(|interface| interface.fold(folder))
                    .collect(),
            }),
            Item::Enum {
                ref meta,
                name,
                ref values,
            } => Node::new(Item::Enum {
                meta: meta.fold(folder),
                name,
                values: values
                    .iter()
                    .map(|&(ref meta, val)| (meta.fold(folder), val))
                    .collect(),
            }),
            Item::TypeAlias {
                ref meta,
                name,
                ref generics,
                ref ty,
            } => Node::new(Item::TypeAlias {
                meta: meta.fold(folder),
                name,
                generics: generics
                    .iter()
                    .map(|generic| generic.fold(folder))
                    .collect(),
                ty: ty.fold(folder),
            }),
            Item::Function {
                ref meta,
                external,
                ref function,
            } => Node::new(Item::Function {
                meta: meta.fold(folder),
                external,
                function: function.fold(folder),
            }),
            Item::Vars(ref meta, ref var_ty, ref defs) => Node::new(Item::Vars(
                meta.fold(folder),
                var_ty.fold(folder),
                defs.iter()
                    .map(|interface| interface.fold(folder))
                    .collect(),
            )),
        }
    }
}

impl Fold for Node<ClassMember> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_class_member(self.clone())
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        match **self {
            ClassMember::Redirect {
                ref meta,
                ref method_qualifiers,
                name,
                ref sig,
                ref path,
            } => Node::new(ClassMember::Redirect {
                meta: meta.fold(folder),
                method_qualifiers: method_qualifiers.clone(),
                name,
                sig: sig.fold(folder),
                path: path.fold(folder),
            }),
            ClassMember::Constructor {
                ref meta,
                ref method_qualifiers,
                name,
                ref sig,
                ref initializers,
                ref function_body,
            } => Node::new(ClassMember::Constructor {
                meta: meta.fold(folder),
                method_qualifiers: method_qualifiers.clone(),
                name,
                sig: sig.fold(folder),
                initializers: initializers
                    .iter()
                    .map(|initializer| initializer.fold(folder))
                    .collect(),
                function_body: function_body
                    .as_ref()
                    .map(|function_body| function_body.fold(folder)),
            }),
            ClassMember::Method(ref meta, ref method_qualifiers, ref function) => {
                Node::new(ClassMember::Method(
                    meta.fold(folder),
                    method_qualifiers.clone(),
                    function.fold(folder),
                ))
            }
            ClassMember::Fields {
                ref meta,
                static_,
                ref var_type,
                ref initializers,
            } => Node::new(ClassMember::Fields {
                meta: meta.fold(folder),
                static_,
                var_type: var_type.fold(folder),
                initializers: initializers
                    .iter()
                    .map(|initializer| initializer.fold(folder))
                    .collect(),
            }),
        }
    }
}

impl Fold for ConstructorInitializer {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_constructor_initializer(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        match *self {
            ConstructorInitializer::Super(name, ref args) => {
                ConstructorInitializer::Super(name, args.fold(folder))
            }
            ConstructorInitializer::This(name, ref args) => {
                ConstructorInitializer::This(name, args.fold(folder))
            }
            ConstructorInitializer::Assert(ref args) => {
                ConstructorInitializer::Assert(args.fold(folder))
            }
            ConstructorInitializer::Field(this, name, ref expr) => {
                ConstructorInitializer::Field(this, name, expr.fold(folder))
            }
        }
    }
}

impl Fold for MetaItem {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_meta_item(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        match *self {
            MetaItem::Comments(ref coments) => MetaItem::Comments(coments.clone()),
            MetaItem::Attribute {
                ref qualified,
                ref arguments,
            } => MetaItem::Attribute {
                qualified: qualified.fold(folder),
                arguments: arguments.as_ref().map(|arguments| arguments.fold(folder)),
            },
        }
    }
}

impl Fold for Meta {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_meta(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        self.iter()
            .map(|meta_item| meta_item.fold(folder))
            .collect()
    }
}

impl Fold for Node<Qualified> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_qualified(self.clone())
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        let Qualified {
            ref prefix,
            name,
            ref params,
        } = **self;
        Node::new(Qualified {
            prefix: prefix.as_ref().map(|prefix| prefix.fold(folder)),
            name,
            params: params
                .iter()
                .map(|interface| interface.fold(folder))
                .collect(),
        })
    }
}

impl Fold for Node<TypeParameter> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_type_parameter(self.clone())
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        let TypeParameter {
            ref meta,
            name,
            ref extends,
        } = **self;
        Node::new(TypeParameter {
            meta: meta.fold(folder),
            name,
            extends: extends.as_ref().map(|extends| extends.fold(folder)),
        })
    }
}

impl Fold for Vec<Node<TypeParameter>> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_generics(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        self.iter().map(|param| param.fold(folder)).collect()
    }
}

impl Fold for Node<Type> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_type(self.clone())
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        match **self {
            Type::Path(ref qualified) => Node::new(Type::Path(qualified.fold(folder))),
            Type::FunctionOld(ref sig) => Node::new(Type::FunctionOld(sig.fold(folder))),
            Type::Function(ref sig) => Node::new(Type::Function(sig.fold(folder))),
            Type::Infer => Node::new(Type::Infer),
        }
    }
}

impl Fold for Node<Function> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_function(self.clone())
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        let Function {
            name,
            ref generics,
            ref sig,
            ref body,
        } = **self;
        Node::new(Function {
            name,
            generics: generics
                .iter()
                .map(|generic| generic.fold(folder))
                .collect(),
            sig: sig.fold(folder),
            body: body.as_ref().map(|body| body.fold(folder)),
        })
    }
}

impl Fold for ArgDef {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_arg_def(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        let ArgDef {
            ref meta,
            covariant,
            ref ty,
            field,
            default_uses_eq,
            ref var,
        } = *self;
        ArgDef {
            meta: meta.fold(folder),
            covariant,
            ty: ty.fold(folder),
            field,
            default_uses_eq,
            var: var.fold(folder),
        }
    }
}

impl Fold for FnSig {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_fn_sig(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        let FnSig {
            ref return_type,
            ref required,
            ref optional,
            optional_kind,
            async,
            generator,
        } = *self;
        FnSig {
            return_type: return_type.fold(folder),
            required: required.iter().map(|req| req.fold(folder)).collect(),
            optional: optional.iter().map(|opt| opt.fold(folder)).collect(),
            optional_kind,
            async,
            generator,
        }
    }
}

impl Fold for FnBody {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_fn_body(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        match *self {
            FnBody::Arrow(ref expr) => FnBody::Arrow(expr.fold(folder)),
            FnBody::Block(ref stmt) => FnBody::Block(stmt.fold(folder)),
            FnBody::Native(ref str_lit) => {
                FnBody::Native(str_lit.as_ref().map(|str_lit| str_lit.fold(folder)))
            }
        }
    }
}

impl Fold for TryPart {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_try_part(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        let TryPart {
            ref on,
            ref catch,
            ref block,
        } = *self;
        TryPart {
            on: on.as_ref().map(|on| on.fold(folder)),
            catch: catch.as_ref().map(
                |&CatchPart {
                     ref exception,
                     ref trace,
                 }| {
                    CatchPart {
                        exception: exception.fold(folder),
                        trace: trace.as_ref().map(|trace| trace.fold(folder)),
                    }
                },
            ),
            block: block.fold(folder),
        }
    }
}

impl Fold for ForLoop {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_for_loop(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        match *self {
            ForLoop::CLike(ref init, ref cond, ref increment) => ForLoop::CLike(
                init.fold(folder),
                cond.as_ref().map(|cond| cond.fold(folder)),
                increment.iter().map(|inc| inc.fold(folder)).collect(),
            ),
            ForLoop::In(item, ref collection) => ForLoop::In(item, collection.fold(folder)),
            ForLoop::InVar(ref var_ty, ref var_def, ref expr) => {
                ForLoop::InVar(var_ty.fold(folder), var_def.fold(folder), expr.fold(folder))
            }
        }
    }
}

impl Fold for Node<Statement> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_statement(self.clone())
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        match **self {
            Statement::Comments(ref comments, ref statement) => Node::new(Statement::Comments(
                comments.clone(),
                statement.as_ref().map(|statement| statement.fold(folder)),
            )),
            Statement::Block(ref block) => Node::new(Statement::Block(
                block
                    .iter()
                    .map(|statement| statement.fold(folder))
                    .collect(),
            )),
            Statement::Vars(ref var_type, ref var_defs) => Node::new(Statement::Vars(
                var_type.fold(folder),
                var_defs
                    .iter()
                    .map(|var_def| var_def.fold(folder))
                    .collect(),
            )),
            Statement::Function(ref function) => {
                Node::new(Statement::Function(function.fold(folder)))
            }
            Statement::For(await, ref for_loop, ref statement) => Node::new(Statement::For(
                await,
                for_loop.fold(folder),
                statement.fold(folder),
            )),
            Statement::While(ref expr, ref statement) => {
                Node::new(Statement::While(expr.fold(folder), statement.fold(folder)))
            }
            Statement::DoWhile(ref statement, ref expr) => Node::new(Statement::DoWhile(
                statement.fold(folder),
                expr.fold(folder),
            )),
            Statement::Switch(ref expr, ref cases) => Node::new(Statement::Switch(
                expr.fold(folder),
                cases.iter().map(|case| case.fold(folder)).collect(),
            )),
            Statement::If(ref expr, ref statement, ref else_stm) => Node::new(Statement::If(
                expr.fold(folder),
                statement.fold(folder),
                else_stm.as_ref().map(|else_stm| else_stm.fold(folder)),
            )),
            Statement::Rethrow => Node::new(Statement::Rethrow),
            Statement::Try(ref statement, ref try_parts) => Node::new(Statement::Try(
                statement.fold(folder),
                try_parts
                    .iter()
                    .map(|try_part| try_part.fold(folder))
                    .collect(),
            )),
            Statement::Break(name) => Node::new(Statement::Break(name)),
            Statement::Continue(name) => Node::new(Statement::Continue(name)),
            Statement::Return(ref expr) => Node::new(Statement::Return(
                expr.as_ref().map(|expr| expr.fold(folder)),
            )),
            Statement::Yield(ref expr) => Node::new(Statement::Yield(expr.fold(folder))),
            Statement::YieldEach(ref expr) => Node::new(Statement::YieldEach(expr.fold(folder))),
            Statement::Expression(ref expr) => Node::new(Statement::Expression(
                expr.as_ref().map(|expr| expr.fold(folder)),
            )),
            Statement::Assert(ref args) => Node::new(Statement::Assert(args.fold(folder))),
            Statement::Labelled(name, ref statement) => {
                Node::new(Statement::Labelled(name, statement.fold(folder)))
            }
        }
    }
}

impl Fold for Vec<Node<Statement>> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_block(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        self.iter()
            .map(|statement| statement.fold(folder))
            .collect()
    }
}

impl Fold for Node<VarDef> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_var_def(self.clone())
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        let VarDef { name, ref init } = **self;
        Node::new(VarDef {
            name,
            init: init.as_ref().map(|init| init.fold(folder)),
        })
    }
}

impl Fold for Node<Expr> {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_expr(self.clone())
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        match **self {
            Expr::Comments(ref comments, ref expr) => {
                Node::new(Expr::Comments(comments.clone(), expr.fold(folder)))
            }
            Expr::Unary(un_op, ref expr) => Node::new(Expr::Unary(un_op, expr.fold(folder))),
            Expr::Binary(bin_op, ref expr_l, ref expr_r) => Node::new(Expr::Binary(
                bin_op,
                expr_l.fold(folder),
                expr_r.fold(folder),
            )),
            Expr::Conditional(ref expr1, ref expr2, ref expr3) => Node::new(Expr::Conditional(
                expr1.fold(folder),
                expr2.fold(folder),
                expr3.fold(folder),
            )),
            Expr::Is(ref expr, ref ty) => Node::new(Expr::Is(expr.fold(folder), ty.fold(folder))),
            Expr::IsNot(ref expr, ref ty) => {
                Node::new(Expr::IsNot(expr.fold(folder), ty.fold(folder)))
            }
            Expr::As(ref expr, ref ty) => Node::new(Expr::As(expr.fold(folder), ty.fold(folder))),
            Expr::Suffix(ref expr, ref suffix) => {
                Node::new(Expr::Suffix(expr.fold(folder), suffix.fold(folder)))
            }
            Expr::Identifier(name) => Node::new(Expr::Identifier(name)),
            Expr::Closure(ref sig, ref body) => {
                Node::new(Expr::Closure(sig.fold(folder), body.fold(folder)))
            }
            Expr::New {
                const_,
                ref path,
                ref args,
            } => Node::new(Expr::New {
                const_,
                path: path.fold(folder),
                args: args.fold(folder),
            }),
            Expr::List {
                const_,
                ref element_ty,
                ref elements,
            } => Node::new(Expr::List {
                const_,
                element_ty: element_ty.as_ref().map(|ty| ty.fold(folder)),
                elements: elements
                    .iter()
                    .map(|element| element.fold(folder))
                    .collect(),
            }),
            Expr::Map {
                const_,
                ref kv_ty,
                ref kv,
            } => Node::new(Expr::Map {
                const_,
                kv_ty: kv_ty
                    .as_ref()
                    .map(|&(ref k, ref v)| (k.fold(folder), v.fold(folder))),
                kv: kv.iter()
                    .map(|&(ref k, ref v)| (k.fold(folder), v.fold(folder)))
                    .collect(),
            }),
            Expr::Number(name) => Node::new(Expr::Number(name)),
            Expr::String(ref parts) => Node::new(Expr::String(
                parts.iter().map(|part| part.fold(folder)).collect(),
            )),
            Expr::Symbol(ref content) => Node::new(Expr::Symbol(content.clone())),
            Expr::Paren(ref expr) => Node::new(Expr::Paren(expr.fold(folder))),
            Expr::Throw(ref expr) => Node::new(Expr::Throw(expr.fold(folder))),
            Expr::Cascade(ref expr, ref cascade) => {
                Node::new(Expr::Cascade(expr.fold(folder), cascade.fold(folder)))
            }
        }
    }
}

impl Fold for Args {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_args(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        let Args {
            ref unnamed,
            ref named,
        } = *self;
        Args {
            unnamed: unnamed.iter().map(|unnamed| unnamed.fold(folder)).collect(),
            named: named.iter().map(|arg| arg.fold(folder)).collect(),
        }
    }
}

impl Fold for Suffix {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_suffix(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        match *self {
            Suffix::Index(ref expr) => Suffix::Index(expr.fold(folder)),
            Suffix::Field(name) => Suffix::Field(name),
            Suffix::FieldIfNotNull(name) => Suffix::FieldIfNotNull(name),
            Suffix::Call(ref types, ref args) => Suffix::Call(
                types.iter().map(|ty| ty.fold(folder)).collect(),
                args.fold(folder),
            ),
        }
    }
}

impl Fold for Cascade {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_cascade(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        let Cascade {
            ref suffixes,
            ref assign,
        } = *self;
        Cascade {
            suffixes: suffixes.iter().map(|suffix| suffix.fold(folder)).collect(),
            assign: assign
                .as_ref()
                .map(|&(val, ref expr)| (val, expr.fold(folder))),
        }
    }
}

impl Fold for StringLiteral {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_string_literal(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        let StringLiteral {
            raw,
            triple,
            quote,
            prefix,
            ref interpolated,
        } = *self;
        StringLiteral {
            raw,
            triple,
            quote,
            prefix,
            interpolated: interpolated
                .iter()
                .map(|&(ref expr, span)| (expr.fold(folder), span))
                .collect(),
        }
    }
}

impl Fold for VarType {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_var_ty(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        let VarType { fcv, ref ty } = *self;
        VarType {
            fcv,
            ty: ty.fold(folder),
        }
    }
}

impl Fold for SwitchCase {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_switch_case(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        let SwitchCase {
            ref labels,
            ref value,
            ref statements,
        } = *self;
        SwitchCase {
            labels: labels.clone(),
            value: value.as_ref().map(|val| val.fold(folder)),
            statements: statements
                .iter()
                .map(|statement| statement.fold(folder))
                .collect(),
        }
    }
}

impl Fold for NamedArg {
    fn fold<F: Folder>(&self, folder: &mut F) -> Self {
        folder.dart_named_arg(self)
    }
    fn super_fold<F: Folder>(&self, folder: &mut F) -> Self {
        let NamedArg {
            ref comments,
            name,
            ref expr,
        } = *self;
        NamedArg {
            comments: comments.clone(),
            name,
            expr: expr.fold(folder),
        }
    }
}
