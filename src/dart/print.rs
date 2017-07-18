use dart::ast::*;
use dart::lex::{Lexer, Token};
use syntax::symbol::Symbol;
use syntax::codemap::Span;
use unicode_width::UnicodeWidthChar;
use node::Node;

fn str_to_span(string: &str) -> Span {
    let fm = ::codemap().new_filemap(String::new(), string.to_string());
    ::mk_sp(fm.start_pos, fm.end_pos)
}

fn span_to_str(span: Span) -> String {
    ::codemap().span_to_snippet(span).unwrap()
}

pub struct Printer {
    tokens: Vec<Token>,
    open_boxes: Vec<LayoutBox>,
}

struct LayoutBox {
    children: Vec<LayoutBox>,
    enter: usize,
    exit: usize,
}

impl Printer {
    pub fn new() -> Self {
        Printer {
            tokens: vec![],
            open_boxes: vec![],
        }
    }

    fn enter(&mut self) {
        self.open_boxes.push(LayoutBox {
            children: vec![],
            enter: self.tokens.len(),
            exit: 0,
        });
    }

    fn exit(&mut self) {
        let mut node = self.open_boxes.pop().unwrap();
        node.exit = self.tokens.len();
        self.open_boxes.last_mut().unwrap().children.push(node);
    }

    fn new_line(&mut self) {
        self.print_str("\n");
    }

    fn print_str(&mut self, string: &str) {
        self.tokens.extend(
            Lexer::new(str_to_span(string))
                .tokenize()
                .unwrap()
                .into_iter()
                .map(|(_, t)| t),
        );
    }

    fn print_ident(&mut self, ident: Symbol) {
        self.tokens.push(Token::Identifier(ident));
    }

    fn print_string_lit(&mut self, literal: &StringLiteral) {
        self.tokens.push(Token::StringLiteral {
            contents: literal.prefix,
            raw: literal.raw,
            triple: literal.triple,
            quote: literal.quote,
            interpolation_before: false,
            interpolation_after: !literal.interpolated.is_empty(),
        });
        for (i, &(ref expr, contents)) in literal.interpolated.iter().enumerate() {
            self.print_expr(expr);
            self.tokens.push(Token::StringLiteral {
                contents,
                raw: literal.raw,
                triple: literal.triple,
                quote: literal.quote,
                interpolation_before: true,
                interpolation_after: i != literal.interpolated.len() - 1,
            });
        }
    }

    pub fn print_items(mut self, items: &[Node<Item>]) -> Vec<Token> {
        self.enter();
        for item in items {
            self.print_item(item);
            self.new_line();
        }
        self.pretty_print()
    }

    fn print_item(&mut self, item: &Item) {
        match *item {
            Item::LibraryName {
                ref metadata,
                ref path,
            } => {
                self.print_metadata(metadata);
                self.print_str("library ");
                for (i, ident) in path.iter().enumerate() {
                    self.print_ident(*ident);
                    if i < path.len() - 1 {
                        self.print_str(".");
                    }
                }
                self.print_str(";");
            }
            Item::Import(ref metadata, ref spec) => {
                self.print_metadata(metadata);
                self.print_str("import ");
                self.print_string_lit(&spec.uri);
                if spec.deferred == true {
                    self.print_str(" deferred ");
                }
                if let Some(ident) = spec.as_ident {
                    self.print_str("as ");
                    self.print_ident(ident);
                }
                for filter in &spec.filters {
                    self.print_combinator(filter);
                }
                self.print_str(";");
            }
            Item::Export(ref metadata, ref uri, ref combinators) => {
                self.print_metadata(metadata);
                self.print_str(" export ");
                self.print_string_lit(uri);
                self.print_str(" ");
                for comb in combinators {
                    self.print_combinator(comb);
                }
                self.print_str(";");
            }
            Item::Part {
                ref metadata,
                ref uri,
            } => {
                self.print_metadata(metadata);
                self.print_str("part ");
                self.print_string_lit(uri);
                self.print_str(";");
            }
            Item::PartOf {
                ref metadata,
                ref path,
            } => {
                self.print_metadata(metadata);
                self.print_str("part of ");
                self.print_ident(path[0]);
                for it in &path[1..] {
                    self.print_str(".");
                    self.print_ident(*it);
                }
                self.print_str(";");
            }
            Item::Class {
                ref metadata,
                abstract_,
                name,
                ref generics,
                ref superclass,
                ref mixins,
                ref interfaces,
                ref members,
            } => {
                self.print_metadata(metadata);
                if abstract_ {
                    self.print_str("abstract ");
                }
                self.print_str("class ");
                self.print_ident(name);
                if !generics.is_empty() {
                    self.print_str("< ");
                    for (i, param) in generics.iter().enumerate() {
                        self.print_type_parameter(param);
                        if i < generics.len() - 1 {
                            self.print_str(", ");
                        }
                    }
                    self.print_str(" >");
                }
                self.print_str(" ");
                if let Some(ref super_class) = *superclass {
                    self.print_str("extends ");
                    self.print_type(super_class);
                    self.print_str(" ");
                    if !mixins.is_empty() {
                        self.print_str("with ");
                        for (i, mix) in mixins.iter().enumerate() {
                            self.print_type(mix);
                            if i < mixins.len() - 1 {
                                self.print_str(", ");
                            }
                        }
                    }
                }
                if !interfaces.is_empty() {
                    self.print_str("implements ");
                    for (i, interface) in interfaces.iter().enumerate() {
                        self.print_type(interface);
                        if i < interfaces.len() - 1 {
                            self.print_str(", ");
                        }
                    }
                }
                self.print_str("{");
                self.enter();
                for member in members {
                    self.print_class_member(member, name);
                    self.new_line();
                }
                self.exit();
                self.print_str("}");

            }
            Item::MixinClass {
                ref metadata,
                abstract_,
                name,
                ref generics,
                ref mixins,
                ref interfaces,
            } => {
                self.print_metadata(metadata);
                if abstract_ {
                    self.print_str("abstract ");
                }
                self.print_str("class ");
                self.print_ident(name);
                if !generics.is_empty() {
                    self.print_str("<");
                    for (i, param) in generics.iter().enumerate() {
                        self.print_type_parameter(param);
                        if i < generics.len() - 1 {
                            self.print_str(", ");
                        }
                    }
                    self.print_str(">");
                }
                self.print_str(" = ");
                self.print_type(&mixins[0]);
                self.print_str(" with ");
                for (i, ty) in mixins[1..].iter().enumerate() {
                    self.print_type(ty);
                    if i < mixins.len() - 1 {
                        self.print_str(", ");
                    }
                }
                if !interfaces.is_empty() {
                    self.print_str("implements ");
                    for (i, interface) in interfaces.iter().enumerate() {
                        self.print_type(interface);
                        if i < interfaces.len() - 1 {
                            self.print_str(", ");
                        }
                    }
                }
                self.print_str(";");
            }
            Item::Enum {
                ref metadata,
                name,
                ref values,
            } => {
                self.print_metadata(metadata);
                self.print_str("enum ");
                self.print_ident(name);
                self.print_str(" {");
                self.enter();
                for (i, value) in values.iter().enumerate() {
                    self.print_ident(*value);
                    if i < values.len() - 1 {
                        self.print_str(",");
                    }
                }
                self.exit();
                self.print_str("}");
            }
            Item::TypeAlias {
                ref metadata,
                name,
                ref generics,
                ref ty,
            } => {
                self.print_metadata(metadata);
                self.print_str("typedef ");
                self.print_typed_name(ty, "", name, generics);
                self.print_str(";");
            }
            Item::Function(ref func) => {
                self.print_function(func);
            }
            Item::Global(ref ty, ref name_and_init) => {
                self.print_var_decl(ty, name_and_init);
            }
        }
    }

    fn print_statement(&mut self, statement: &Statement) {
        match *statement {
            Statement::Block(ref statements) => {
                self.print_str("{");
                self.enter();
                for stm in statements {
                    self.print_statement(stm);
                    self.new_line();
                }
                self.exit();
                self.print_str("}");
            }
            Statement::Var(ref ty, ref name_and_init) => {
                self.print_var_decl(ty, name_and_init);
            }
            Statement::Function(ref func) => {
                self.print_function(func);
            }
            Statement::For(await, ref for_parts, ref stmt) => {
                if await {
                    self.print_str("await ");
                }
                self.print_str("for(");
                match *for_parts {
                    ForLoop::CLike(ref stm, ref exp, ref body) => {
                        self.print_statement(stm);
                        self.print_str(" ");
                        if let Some(ref expr) = *exp {
                            self.print_expr(expr);
                            self.print_str("; ");
                        }
                        for (i, expr) in body.iter().enumerate() {
                            self.print_expr(expr);
                            if i < body.len() - 1 {
                                self.print_str(", ");
                            }
                        }
                    }
                    ForLoop::In(ref ty, name, ref expr) => {
                        if let Some(ref ty) = *ty {
                            self.print_var_decl(ty, &[NameAndInitializer { name, init: None }]);
                        } else {
                            self.print_ident(name);
                        }
                        self.print_str(" in ");
                        self.print_expr(expr);
                    }
                }
                self.print_str(") ");
                self.print_statement(stmt);

            }
            Statement::While(ref expr, ref stm) => {
                self.print_str("while(");
                self.print_expr(expr);
                self.print_str(") ");
                self.print_statement(stm);
            }
            Statement::DoWhile(ref stm, ref expr) => {
                self.print_str("do ");
                self.print_statement(stm);
                self.print_str("while(");
                self.print_expr(expr);
                self.print_str(") ");
            }
            Statement::Switch(ref expr, ref cases) => {
                self.print_str("switch(");
                self.print_expr(expr);
                self.print_str(") {");
                self.enter();
                for case in cases {
                    for &label in &case.labels {
                        self.print_ident(label);
                        self.print_str(" ");
                    }
                    if let Some(ref expr) = case.value {
                        self.print_str("case");
                        self.print_expr(expr);
                    } else {
                        self.print_str(" default");
                    }
                    self.print_str(": ");
                    for stm in &case.statements {
                        self.enter();
                        self.print_statement(stm);
                        self.exit();
                    }
                }
                self.exit();
                self.print_str("}");
            }
            Statement::If(ref expr, ref stm, ref else_stm) => {
                self.print_str("if(");
                self.print_expr(expr);
                self.print_str(") ");
                self.print_statement(stm);
                if let Some(ref else_stm) = *else_stm {
                    self.print_str("else ");
                    self.print_statement(else_stm);
                }
            }
            Statement::Rethrow => {
                self.print_str("retrow");
            }
            Statement::Try(ref stm, ref parts) => {
                self.print_str("try ");
                self.print_statement(stm);
                for part in parts {
                    if let Some(ref ty) = part.on {
                        self.print_str("on ");
                        self.print_type(ty);

                    }
                    if let Some(ref catch) = part.catch {
                        self.print_str("catch ");
                        self.print_ident(catch.exception);
                        if let Some(trace) = catch.trace {
                            self.print_ident(trace);
                        }
                    }
                    if part.catch.is_none() & part.on.is_none() {
                        self.print_statement(&part.block);
                    }
                }
            }
            Statement::Break(ref expr) => {
                self.print_str("break");
                if let Some(ref ident) = *expr {
                    self.print_str(" ");
                    self.print_ident(*ident);
                }
                self.print_str(";");
            }
            Statement::Continue(ref expr) => {
                self.print_str("continue");
                if let Some(ref ident) = *expr {
                    self.print_str(" ");
                    self.print_ident(*ident);
                }
                self.print_str(";");
            }
            Statement::Return(ref expr) => {
                self.print_str("return");
                if let Some(ref ident) = *expr {
                    self.print_str(" ");
                    self.print_expr(ident);
                }
                self.print_str(";");
            }
            Statement::Yield(ref expr) => {
                self.print_str("yield ");
                self.print_expr(expr);
                self.print_str(";");
            }
            Statement::YieldEach(ref expr) => {
                self.print_str("yield* ");
                self.print_expr(expr);
                self.print_str(";");
            }

            Statement::Expression(ref expr) => if let Some(ref exp) = *expr {
                self.print_expr(exp);
                self.print_str(";");
            },
            Statement::Assert(ref args) => {
                self.print_str("assert");
                self.print_arguments(args);
                self.print_str(";");
            }
            Statement::Labelled(ref name, ref stm) => {
                self.print_ident(*name);
                self.print_str(": ");
                self.print_statement(stm);
            }
        }
    }

    fn print_expr(&mut self, expr: &Expr) {
        match *expr {
            Expr::Unary(un_op, ref operand) => match un_op {
                UnOp::PostDec | UnOp::PostInc => {
                    self.print_expr(operand);
                    self.print_unary_op(un_op);
                }
                _ => {
                    self.print_unary_op(un_op);
                    self.print_expr(operand);
                }
            },
            Expr::Binary(operator, ref operand1, ref operand2) => {
                self.print_expr(operand1);
                self.print_str(" ");
                self.print_binary_op(operator);
                self.print_str(" ");
                self.print_expr(operand2);
            }
            Expr::Conditional(ref exp, ref res1, ref res2) => {
                self.print_expr(exp);
                self.print_str(" ? ");
                self.print_expr(res1);
                self.print_str(" : ");
                self.print_expr(res2);
            }
            Expr::Is(ref operand, ref tp) => {
                self.print_expr(operand);
                self.print_str(" is ");
                self.print_type(tp);
            }
            Expr::IsNot(ref operand, ref tp) => {
                self.print_expr(operand);
                self.print_str(" is! ");
                self.print_type(tp);
            }
            Expr::As(ref operand, ref tp) => {
                self.print_expr(operand);
                self.print_str(" as ");
                self.print_type(tp);
            }
            Expr::Suffix(ref expr, ref suffix) => {
                self.print_expr(expr);
                self.print_suffix(suffix);
            }
            Expr::Identifier(ref obj) => {
                self.print_ident(*obj);
            }
            Expr::Closure(ref args, ref body) => {
                self.print_fn_args(args);
                self.print_function_body(body);
            }
            Expr::New {
                const_,
                ref ty,
                ctor,
                ref args,
            } => {
                if const_ == true {
                    self.print_str("const ");
                } else {
                    self.print_str("new ");
                }
                self.print_type(ty);
                if let Some(ctor) = ctor {
                    self.print_str(".");
                    self.print_ident(ctor);
                }
                self.print_arguments(args);
            }
            Expr::List {
                const_,
                ref element_ty,
                ref elements,
            } => {
                if const_ {
                    self.print_str("const ");
                }
                if let Some(ref ty) = *element_ty {
                    self.print_str("<");
                    self.print_type(ty);
                    self.print_str(">");
                }
                if !elements.is_empty() {
                    self.print_str("[");
                    self.enter();
                    if elements.len() > 1 {
                        for (i, elem) in elements.iter().enumerate() {
                            self.print_expr(elem);
                            if i < elements.len() - 1 {
                                self.print_str(",");
                            }
                        }
                    } else {
                        self.print_expr(&elements[0]);
                    }
                    self.exit();
                    self.print_str("]");
                }
            }
            Expr::Map {
                const_,
                ref kv_ty,
                ref kv,
            } => {
                if const_ {
                    self.print_str("const ");
                }
                if let Some((ref ty1, ref ty2)) = *kv_ty {
                    self.print_str("<");
                    self.print_type(ty1);
                    self.print_str(", ");
                    self.print_type(ty2);
                    self.print_str(">");
                }
                self.print_str("{");
                self.enter();
                for (i, &(ref k, ref v)) in kv.iter().enumerate() {
                    self.print_expr(k);
                    self.print_str(" : ");
                    self.print_expr(v);
                    if i < kv.len() - 1 {
                        self.print_str(", ");
                    }
                }
                self.exit();
                self.print_str("}");
            }
            Expr::Number(n) => {
                self.print_str(&n.as_str());
            }
            Expr::String(ref literals) => for lit in literals {
                self.print_string_lit(lit);
            },
            Expr::Paren(ref expr) => {
                self.print_str("(");
                self.print_expr(expr);
                self.print_str(")");
            }
            Expr::Symbol(ref literal) => {
                self.print_str("#");
                match *literal {
                    SymbolLiteral::Op(ref op) => {
                        self.print_operator(*op);
                    }
                    SymbolLiteral::Path(ref items) => for (i, item) in items.iter().enumerate() {
                        self.print_ident(*item);
                        if i < items.len() - 1 {
                            self.print_str(".");
                        }
                    },
                }
            }
            Expr::Throw(ref expr) => {
                self.print_str("throw ");
                self.print_expr(expr);
            }
            Expr::Cascade(ref expr, ref cascade) => {
                self.print_expr(expr);
                self.print_cascade(cascade)
            }
        }
    }

    fn print_function(&mut self, func: &Function) {
        self.print_type_spaced(&func.sig.return_type);
        self.print_function_name(func.name);
        self.print_fn_args(&func.sig);
        if let Some(ref body) = func.body {
            self.print_function_body(body);
        }
    }

    fn print_cascade(&mut self, cascade: &Cascade) {
        for suffix in &cascade.suffixes {
            match *suffix {
                Suffix::Field(..) => {
                    self.print_str(".");
                }
                _ => {
                    self.print_str("..");
                }
            }
            self.print_suffix(&suffix);
            if let Some((op, ref expr)) = cascade.assign {
                self.print_str(" ");
                if let Some(op) = op {
                    self.print_binary_op(BinOp::Value(op));
                    self.print_str(" ");
                } else {
                    self.print_str("= ");
                }
                self.print_expr(expr);
            }
        }
    }

    fn print_suffix(&mut self, suffix: &Suffix) {
        match *suffix {
            Suffix::Index(ref op) => {
                self.print_str("[");
                self.print_expr(op);
                self.print_str("]");
            }
            Suffix::Field(obj) => {
                self.print_str(".");
                self.print_ident(obj);
            }
            Suffix::FieldIfNotNull(obj) => {
                self.print_str("?.");
                self.print_ident(obj);
            }
            Suffix::Call(ref types, ref args) => {
                if !types.is_empty() {
                    self.print_str("<");
                    for (i, ty) in types.iter().enumerate() {
                        self.print_type(ty);
                        if i < types.len() - 1 {
                            self.print_str(", ");
                        }
                    }
                    self.print_str(">");
                }
                self.print_arguments(args);
            }
        }
    }

    fn print_arguments(&mut self, args: &Args) {
        self.print_str("(");
        self.enter();
        for (i, arg) in args.unnamed.iter().enumerate() {
            self.print_expr(arg);
            if i < args.unnamed.len() - 1 {
                self.print_str(", ");
            }
        }
        if args.unnamed.len() != 0 && args.named.len() != 0 {
            self.print_str(", ");
        }
        for (i, arg) in args.named.iter().enumerate() {
            self.print_named_argurment(arg);
            if i < args.named.len() - 1 {
                self.print_str(", ");
            }
        }
        self.exit();
        self.print_str(")");
    }

    fn print_binary_op(&mut self, operator: BinOp) {
        self.print_str(operator.as_str());
    }

    fn print_type(&mut self, ty: &Type) {
        match *ty {
            Type::Path(ref qualified, ref types) => {
                self.print_qualified(qualified);
                if !types.is_empty() {
                    self.print_str("<");
                    for (i, ty) in types.iter().enumerate() {
                        self.print_type(ty);
                        if i < types.len() - 1 {
                            self.print_str(", ");
                        }
                    }
                    self.print_str(">");
                }
            }
            Type::Function(ref signature) => {
                self.print_type(&signature.return_type);
                self.print_str(" Function");
                self.print_fn_args(&signature);
            }
            Type::FunctionOld(_) | Type::Infer => {
                unreachable!();
            }
        }
    }

    fn print_type_spaced(&mut self, ty: &Type) {
        match *ty {
            Type::Path(..) | Type::Function(..) => {
                self.print_type(ty);
                self.print_str(" ");
            }
            Type::FunctionOld(_) => {
                unreachable!();
            }
            Type::Infer => {}
        }
    }

    fn print_typed_name(
        &mut self,
        ty: &Type,
        prefix: &str,
        name: Symbol,
        params: &[TypeParameter],
    ) {
        match *ty {
            Type::Path(..) | Type::Infer | Type::Function(..) => {
                self.print_type_spaced(ty);
                self.print_str(prefix);
                self.print_ident(name);
                if !params.is_empty() {
                    self.print_str("<");
                    for (i, param) in params.iter().enumerate() {
                        self.print_type_parameter(param);
                        if i < params.len() - 1 {
                            self.print_str(", ");
                        }
                    }
                    self.print_str(">");
                }
            }
            Type::FunctionOld(ref signature) => {
                self.print_typed_name(&signature.return_type, prefix, name, params);
                self.print_fn_args(&signature);
            }
        }
    }

    fn print_qualified(&mut self, obj: &Qualified) {
        if let Some(pref) = obj.prefix {
            self.print_ident(pref);
            self.print_str(".");
        }
        self.print_ident(obj.name);
    }

    fn print_named_argurment(&mut self, arg: &NamedArg) {
        self.print_ident(arg.name);
        self.print_str(": ");
        self.print_expr(&arg.expr);
    }

    fn print_function_body(&mut self, body: &FnBody) {
        match *body {
            FnBody::Arrow(ref expr) => {
                self.print_str(" => ");
                self.print_expr(expr);
                self.print_str(";");
            }
            FnBody::Block(ref stm) => {
                self.print_str(" ");
                self.print_statement(stm);
            }
            FnBody::Native(ref lit) => {
                self.print_str("native");
                if let Some(ref lit) = *lit {
                    self.print_string_lit(lit);
                }
            }
        }
    }

    fn print_fn_args(&mut self, args: &FnSig) {
        self.print_str("(");
        for (i, it) in args.required.iter().enumerate() {
            self.print_normal_formal_parameter(it);
            if i < args.required.len() - 1 {
                self.print_str(", ");
            }
        }
        if !args.optional.is_empty() {
            match args.optional_kind {
                OptionalArgKind::Positional => {
                    self.print_str("[");
                    for (i, it) in args.optional.iter().enumerate() {
                        self.print_normal_formal_parameter(&it.arg);
                        if let Some(ref expr) = it.default {
                            self.print_str(" = ");
                            self.print_expr(expr);
                        }
                        if i < args.optional.len() - 1 {
                            self.print_str(", ");
                        }
                    }
                    self.print_str("]");
                }
                OptionalArgKind::Named => {
                    self.print_str("{");
                    for (i, it) in args.optional.iter().enumerate() {
                        self.print_normal_formal_parameter(&it.arg);
                        if let Some(ref expr) = it.default {
                            self.print_str(" : ");
                            self.print_expr(expr);
                        }
                        if i < args.optional.len() - 1 {
                            self.print_str(", ");
                        }
                    }
                    self.print_str("}");
                }
            }
        }
        self.print_str(")");

        if args.async {
            if !args.generator {
                self.print_str(" async ");
            } else {
                self.print_str(" async* ");
            }
        } else {
            if args.generator {
                self.print_str(" sync* ");
            }
        }
    }

    fn print_normal_formal_parameter(&mut self, param: &ArgDef) {
        self.print_metadata(&param.metadata);
        match param.ty.fcv {
            FinalConstVar::Final => self.print_str("final "),
            FinalConstVar::Const => self.print_str("const "),
            FinalConstVar::Var => {}
        }
        if param.field {
            self.print_typed_name(&param.ty.ty, "this.", param.name, &[]);
        } else {
            self.print_typed_name(&param.ty.ty, "", param.name, &[]);
        }
    }

    fn print_var_decl(&mut self, var_ty: &VarType, vars: &[NameAndInitializer]) {
        match var_ty.fcv {
            FinalConstVar::Final => self.print_str("final "),
            FinalConstVar::Const => self.print_str("const "),
            FinalConstVar::Var => if let Type::Infer = *var_ty.ty {
                self.print_str("var ");
            },
        }
        self.print_type_spaced(&var_ty.ty);
        for (i, var) in vars.iter().enumerate() {
            self.print_name_and_initializer(var);
            if i < vars.len() - 1 {
                self.print_str(", ");
            }
        }
        self.print_str(";");
    }

    fn print_name_and_initializer(&mut self, ident: &NameAndInitializer) {
        self.print_ident(ident.name);
        if let Some(ref x) = ident.init {
            self.print_str(" = ");
            self.print_expr(x);
        }
    }

    fn print_metadata(&mut self, metadata: &Metadata) {
        for meta in metadata {
            self.print_metadata_item(meta);
            self.new_line();
        }
    }

    fn print_metadata_item(&mut self, item: &MetadataItem) {
        self.print_str("@");
        self.print_qualified(&item.qualified);
        if let Some(suffix) = item.suffix {
            self.print_str(".");
            self.print_ident(suffix);
        }

        if let Some(ref args) = item.arguments {
            self.print_arguments(args);
        }
    }

    fn print_combinator(&mut self, comb: &ImportFilter) {
        if comb.hide == true {
            self.print_str("hide ");
        }
        for (i, ident) in comb.names.iter().enumerate() {
            self.print_ident(*ident);
            if i < comb.names.len() - 1 {
                self.print_str(", ");
            }
        }
    }

    fn print_type_parameter(&mut self, param: &TypeParameter) {
        self.print_metadata(&param.metadata);
        self.print_str(" ");
        self.print_ident(param.name);
        if let Some(ref ty) = param.extends {
            self.print_str(" extends ");
            self.print_type(ty);
        }
    }

    fn print_class_member(&mut self, member: &ClassMember, class_name: Symbol) {
        match *member {
            ClassMember::Redirect {
                ref metadata,
                ref method_qualifiers,
                name,
                ref sig,
                ref ty,
            } => {

                self.print_metadata(metadata);
                for qualifier in method_qualifiers {
                    self.print_method_qualifier(qualifier);
                    self.print_str(" ");
                }
                self.print_ident(class_name);
                if let Some(name) = name {
                    self.print_str(".");
                    self.print_ident(name);
                }
                self.print_str(" ");
                self.print_fn_args(sig);
                self.print_str(" = ");
                self.print_type(ty);
                if let Some(name) = name {
                    self.print_str(".");
                    self.print_ident(name);
                }
                self.print_str(";");
            }
            ClassMember::Constructor {
                ref metadata,
                ref method_qualifiers,
                name,
                ref sig,
                ref initializers,
                ref function_body,
            } => {
                self.print_metadata(metadata);
                for qualifier in method_qualifiers {
                    self.print_method_qualifier(qualifier);
                    self.print_str(" ");
                }
                self.print_ident(class_name);
                if let Some(name) = name {
                    self.print_str(".");
                    self.print_ident(name);
                }
                self.print_str(" ");
                self.print_fn_args(sig);
                self.print_str(" ");
                if initializers.len() > 0 {
                    self.print_str(": ");
                    for (i, init) in initializers.iter().enumerate() {
                        self.print_initializer(init);
                        if i < initializers.len() - 1 {
                            self.print_str(", ");
                        }
                    }
                }
                self.print_str(" ");
                if let Some(ref body) = *function_body {
                    self.print_function_body(body);
                }
            }
            ClassMember::Method(ref metadata, ref qualifiers, ref func) => {
                self.print_metadata(metadata);
                for qualifier in qualifiers {
                    self.print_method_qualifier(qualifier);
                    self.print_str(" ");
                }
                self.print_function(func);
            }
            ClassMember::Fields {
                ref metadata,
                static_,
                ref var_type,
                ref initializers,
            } => {
                self.print_metadata(metadata);
                if static_ {
                    self.print_str("static ");
                }
                self.print_var_decl(var_type, initializers);
            }
        }
    }

    fn print_function_name(&mut self, func: FnName) {
        match func {
            FnName::Regular(name) => {
                self.print_ident(name);
            }
            FnName::Getter(name) => {
                self.print_str("get ");
                self.print_ident(name);
            }
            FnName::Setter(name) => {
                self.print_str("set ");
                self.print_ident(name);
            }
            FnName::Operator(op) => {
                self.print_str("operator ");
                self.print_operator(op);
            }
        }
    }

    fn print_initializer(&mut self, init: &ConstructorInitializer) {
        match *init {
            ConstructorInitializer::Super(name, ref arguments) => {
                self.print_str("super");
                if let Some(name) = name {
                    self.print_str(".");
                    self.print_ident(name);
                }
                self.print_arguments(arguments);
                self.print_str(";");
            }
            ConstructorInitializer::This(name, ref arguments) => {
                self.print_str("this");
                if let Some(name) = name {
                    self.print_str(".");
                    self.print_ident(name);
                }
                self.print_str(" ");
                self.print_arguments(arguments);
            }
            ConstructorInitializer::Assert(ref arguments) => {
                self.print_str("assert");
                self.print_arguments(arguments);
            }
            ConstructorInitializer::Field(this, name, ref expr) => {
                if this {
                    self.print_str("this.");
                }
                self.print_ident(name);
                self.print_str(" = ");
                self.print_expr(expr);
            }
        }
    }

    fn print_operator(&mut self, op: OverloadedOp) {
        match op {
            OverloadedOp::BitNot => {
                self.print_str("~");
            }
            OverloadedOp::Index => {
                self.print_str("[]");
            }
            OverloadedOp::IndexAssign => {
                self.print_str("[] =");
            }
            OverloadedOp::Bool(op) => {
                self.print_binary_op(BinOp::Bool(op));
            }
            OverloadedOp::Value(op) => {
                self.print_binary_op(BinOp::Value(op));
            }
        }
    }

    fn print_unary_op(&mut self, operator: UnOp) {
        match operator {
            UnOp::Neg => self.print_str("-"),
            UnOp::Not => self.print_str("!"),
            UnOp::BitNot => self.print_str("~"),
            UnOp::Await => self.print_str("await "),
            UnOp::PreInc | UnOp::PostInc => self.print_str("++"),
            UnOp::PreDec | UnOp::PostDec => self.print_str("--"),
        }
    }

    fn print_method_qualifier(&mut self, qualifier: &MethodQualifiers) {
        match *qualifier {
            MethodQualifiers::External => {
                self.print_str("external");
            }
            MethodQualifiers::Static => {
                self.print_str("static");
            }
            MethodQualifiers::Const => {
                self.print_str("const");
            }
            MethodQualifiers::Final => {
                self.print_str("final");
            }
            MethodQualifiers::Factory => {
                self.print_str("factory");
            }
        }
    }
}

const LINE_WIDTH: usize = 80;
const INDENT: &str = "  ";

impl Printer {
    fn token_size(&self, token: Token) -> usize {
        let mut size: usize = 0;
        match token {
            Token::WhiteSpace(s) => for c in span_to_str(s).chars() {
                size += UnicodeWidthChar::width(c).unwrap_or(1);
            },
            Token::IntegerLiteral(s) | Token::Identifier(s) => for c in s.as_str().chars() {
                size += UnicodeWidthChar::width(c).unwrap_or(1);
            },
            Token::Punctuation(c) => size += UnicodeWidthChar::width(c).unwrap_or(1),
            Token::StringLiteral {
                contents,
                raw,
                triple,
                quote: _,
                interpolation_before,
                interpolation_after,
            } => {
                if raw {
                    size += 1;
                }
                if interpolation_before {
                    size += 1;
                } else {
                    size += 1 + 2 * triple as usize;
                }
                for c in span_to_str(contents).chars() {
                    size += UnicodeWidthChar::width(c).unwrap_or(1);
                }
                if interpolation_after {
                    size += 2;
                } else {
                    size += 1 + 2 * triple as usize;
                }
            }
        }
        size
    }

    fn line_size(&self, tokens: &[Token], line_size: &mut usize) -> Option<usize> {
        for (i, it) in tokens.iter().enumerate() {
            match *it {
                Token::WhiteSpace(s) => {
                    let s = span_to_str(s);
                    if s == "\n" {
                        return Some(i);
                    }
                    for c in s.chars() {
                        *line_size += UnicodeWidthChar::width(c).unwrap_or(1);
                    }
                }
                Token::IntegerLiteral(s) | Token::Identifier(s) => for c in s.as_str().chars() {
                    *line_size += UnicodeWidthChar::width(c).unwrap_or(1);
                },
                Token::Punctuation(c) => *line_size += UnicodeWidthChar::width(c).unwrap_or(1),
                Token::StringLiteral {
                    contents,
                    raw,
                    triple,
                    quote: _,
                    interpolation_before,
                    interpolation_after,
                } => {
                    if raw {
                        *line_size += 1;
                    }
                    if interpolation_before {
                        *line_size += 1;
                    } else {
                        *line_size += 1 + 2 * triple as usize;
                    }
                    for c in span_to_str(contents).chars() {
                        *line_size += UnicodeWidthChar::width(c).unwrap_or(1);
                    }
                    if interpolation_after {
                        *line_size += 2;
                    } else {
                        *line_size += 1 + 2 * triple as usize;
                    }
                }
            }
        }
        None
    }

    fn search_breaker(&self, line: &[Token], size: usize, pos: &mut usize) -> Option<char> {
        let mut i = line.len() - 1;
        *pos = line.len() - 1;
        let mut new_size = size;
        while i > 0 && new_size > LINE_WIDTH {
            new_size -= self.token_size(line[i]);
            *pos -= 1;
            i -= 1;
        }
        while i > 0 && new_size > 0 {
            match line[i] {
                Token::Punctuation(c) => match c {
                    '(' | '[' | '{' | ',' | '+' | '-' | ')' | ']' | '}' | '.' => return Some(c),
                    _ => {}
                },
                _ => {}
            }
            i -= 1;
            *pos -= 1;
            new_size -= self.token_size(line[i]);
        }
        None
    }

    fn break_line(&self, line: &[Token], depth: usize) -> Vec<Vec<Token>> {
        fn put_spaces(depth: usize) -> Token {
            let mut string = String::new();
            for _ in 0..depth {
                string.push_str(INDENT);
            }
            Token::WhiteSpace(str_to_span(&string))
        }

        let mut lines = vec![];
        if line.is_empty() {
            return lines;
        }
        let mut size = 0;
        if let Some(pos) = self.line_size(line, &mut size) {
            if size + depth * INDENT.len() < LINE_WIDTH {
                let mut vec = vec![];
                vec.push(put_spaces(depth));
                vec.extend(&line[0..pos]);
                lines.push(vec);
                lines.extend(self.break_line(&line[pos + 1..], depth));
            } else {
                let mut breaker_pos = 0;
                if let Some(c) = self.search_breaker(
                    &line[0..pos],
                    size + depth * INDENT.len(),
                    &mut breaker_pos,
                ) {
                    let mut vec = vec![];
                    if c == ',' {
                        vec.push(put_spaces(depth));
                    } else {
                        vec.push(put_spaces(depth + 1));
                    }
                    vec.extend(&line[0..breaker_pos + 1]);
                    lines.push(vec);
                    lines.extend(self.break_line(&line[breaker_pos + 1..], depth + 1));
                } else {
                    if line.len() > 0 {
                        let mut vec = vec![];
                        vec.push(put_spaces(depth));
                        vec.extend(line.to_vec());
                        lines.push(vec);
                    }
                }
            }
        } else {
            if size + depth * INDENT.len() < LINE_WIDTH {
                if line.len() > 0 {
                    let mut vec = vec![put_spaces(depth)];
                    vec.extend(line);
                    lines.push(vec);
                }
            } else {
                let mut breaker_pos = 0;
                if let Some(c) =
                    self.search_breaker(&line, size + depth * INDENT.len(), &mut breaker_pos)
                {
                    if breaker_pos < line.len() {
                        let mut vec = vec![];
                        if c == ',' {
                            vec.push(put_spaces(depth));
                        } else {
                            vec.push(put_spaces(depth + 1));
                        }
                        vec.extend(&line[0..breaker_pos + 1]);
                        lines.push(vec);
                        lines.extend(self.break_line(&line[breaker_pos + 1..], depth + 1));
                    }
                } else {
                    let mut vec = vec![];
                    vec.push(put_spaces(depth));
                    vec.extend(line.to_vec());
                    lines.push(vec);
                }
            }
        }
        lines
    }

    fn print_layout_box(&self, node: &LayoutBox, depth: usize) -> Vec<Vec<Token>> {
        let mut lines = vec![];
        let mut cur = node.enter;
        for child in &node.children {
            lines.extend(self.break_line(&self.tokens[cur..child.enter], depth));
            cur = child.exit;
            lines.extend(self.print_layout_box(child, depth + 1));
        }
        lines.extend(self.break_line(&self.tokens[cur..node.exit], depth));
        lines
    }

    fn pretty_print(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        let mut root = self.open_boxes.pop().unwrap();
        root.exit = self.tokens.len();
        for it in self.print_layout_box(&root, 0) {
            tokens.extend(it);
            tokens.push(Token::WhiteSpace(str_to_span("\n")));
        }
        tokens
    }
}
