#![allow(unused_doc_comment)]

use dart::ast::*;
use dart::lex::Token;
use syntax::symbol::Symbol;
use syntax::codemap::Span;

#[derive(Clone)]
pub struct Parser<I> {
    tokens: I,
    cur: Option<Token>,
    cur_span: Span,
    skip_blocks: bool,
}

error_chain! {
    types {
        Error, ErrorKind, ParseResultExt, ParseResult;
    }

    errors {
        ExpectedAt {
            expected: Expected,
            span: Span,
        }
    }
}

#[derive(Debug)]
pub enum Expected {
    Punctuation(char),
    Punctuation2(char, char),
    Keyword(&'static str),
    Expr,
    Ident,
    BinOp(BinOp),
    OverloadedOp,
    NumberLiteral,
    StringLiteral,
}

macro_rules! expected {
    ($p:expr, $kind:ident $($rest:tt)*) => {
        bail!(ErrorKind::ExpectedAt {
            expected: Expected::$kind $($rest)*,
            span: $p.cur_span
        })
    }
}

impl<I: Clone + Iterator<Item = (Span, Token)>> Parser<I> {
    pub fn new(tokens: I) -> Self {
        let mut parser = Parser {
            tokens,
            cur: None,
            cur_span: Span::default(),
            skip_blocks: false,
        };
        parser.bump();
        parser
    }

    pub fn skip_blocks(mut self) -> Self {
        self.skip_blocks = true;
        self
    }

    fn out_of_tokens(&self) -> bool {
        self.cur.is_none()
    }

    fn is_keyword(&self, s: &str) -> bool {
        if let Some(token) = self.cur {
            token.as_ident().map_or(false, |x| x == s)
        } else {
            false
        }
    }

    fn bump_raw(&mut self) {
        match self.tokens.next() {
            Some((span, token)) => {
                self.cur_span = span;
                self.cur = Some(token);
            }
            None => {
                self.cur = None;
            }
        }
    }

    fn bump(&mut self) {
        loop {
            self.bump_raw();
            match self.cur {
                Some(Token::WhiteSpace(_)) => {}
                _ => return,
            }
        }
    }

    fn expect_punctuation(&mut self, c: char) -> ParseResult<()> {
        if self.cur != Some(Token::Punctuation(c)) {
            expected!(self, Punctuation(c));
        }
        self.bump();
        Ok(())
    }

    fn eat_punctuation(&mut self, c: char) -> bool {
        self.try(|p| p.expect_punctuation(c)).is_some()
    }

    fn is_punctuation(&self, c: char) -> bool {
        self.probe(|p| p.eat_punctuation(c))
    }

    fn expect_punctuation2(&mut self, c1: char, c2: char) -> ParseResult<()> {
        if !self.is_punctuation(c1) {
            expected!(self, Punctuation2(c1, c2));
        }
        self.bump_raw();
        if !self.is_punctuation(c2) {
            expected!(self, Punctuation2(c1, c2));
        }
        self.bump();
        Ok(())
    }

    fn eat_punctuation2(&mut self, c1: char, c2: char) -> bool {
        self.try(|p| p.expect_punctuation2(c1, c2)).is_some()
    }

    fn is_punctuation2(&self, c1: char, c2: char) -> bool {
        self.probe(|p| p.eat_punctuation2(c1, c2))
    }

    fn expect_bin_op(&mut self, op: BinOp) -> ParseResult<()> {
        for op2 in BinOp::values() {
            if op2 != op && op2.as_str().starts_with(op.as_str()) {
                if self.is_bin_op(op2) {
                    expected!(self, BinOp(op));
                }
            }
        }
        for c in op.as_str().chars() {
            if !self.is_punctuation(c) {
                expected!(self, BinOp(op));
            }
            self.bump_raw();
        }
        if let Some(Token::WhiteSpace(_)) = self.cur {
            self.bump();
        }
        Ok(())
    }

    fn eat_bin_op(&mut self, op: BinOp) -> bool {
        self.try(|p| p.expect_bin_op(op)).is_some()
    }

    fn is_bin_op(&self, op: BinOp) -> bool {
        self.probe(|p| p.eat_bin_op(op))
    }

    fn expect_keyword(&mut self, s: &'static str) -> ParseResult<()> {
        if !self.is_keyword(s) {
            expected!(self, Keyword(s));
        }
        self.bump();
        Ok(())
    }

    fn eat_keyword(&mut self, s: &'static str) -> bool {
        self.expect_keyword(s).is_ok()
    }

    fn probe<F: FnOnce(&mut Self) -> R, R>(&self, f: F) -> R {
        f(&mut self.clone())
    }

    fn try<F: FnOnce(&mut Self) -> ParseResult<T>, T>(&mut self, f: F) -> Option<T> {
        let mut parser = self.clone();
        let result = f(&mut parser);
        if result.is_ok() {
            *self = parser;
        }
        result.ok()
    }

    fn parse_ident(&mut self) -> ParseResult<Symbol> {
        let ident = if let Some(Token::Identifier(ident)) = self.cur {
            ident
        } else {
            expected!(self, Ident);
        };
        match &ident.as_str()[..] {
            "assert" | "break" | "case" | "catch" | "class" | "const" | "continue" |
            "default" | "do" | "else" | "enum" | "extends" | "ﬁnal" | "ﬁnally" | "for" |
            "if" | "in" | "is" | "new" | "rethrow" | "return" | "switch" | "throw" | "try" |
            "var" | "while" | "with" => {
                expected!(self, Ident);
            }
            _ => {}
        }
        self.bump();
        Ok(ident)
    }

    fn parse_unreserved_ident(&mut self) -> ParseResult<Symbol> {
        let ident = self.try(|p| {
            let ident = p.parse_ident()?;
            match &ident.as_str()[..] {
                "abstract" | "as" | "deferred" | "export" | "external" | "factory" | "get" |
                "implements" | "import" | "library" | "operator" | "part" | "set" | "static" |
                "typedef" => expected!(p, Ident),
                _ => {}
            }
            Ok(ident)
        });
        if let Some(ident) = ident {
            Ok(ident)
        } else {
            expected!(self, Ident)
        }
    }

    fn parse_one_or_more<F: FnMut(&mut Self) -> ParseResult<T>, T>(
        &mut self,
        delim: char,
        mut f: F,
    ) -> ParseResult<Vec<T>> {
        let mut list = vec![];
        loop {
            list.push(f(self)?);
            if !self.eat_punctuation(delim) {
                break;
            }
        }
        Ok(list)
    }

    fn parse_qualified(&mut self) -> ParseResult<Qualified> {
        let name = self.parse_ident()?;
        if self.eat_punctuation('.') {
            Ok(Qualified {
                prefix: Some(name),
                name: self.parse_ident()?,
            })
        } else {
            Ok(Qualified { prefix: None, name })
        }
    }

    pub fn parse_type(&mut self) -> ParseResult<Box<Type>> {
        let name = self.parse_unreserved_ident()?;
        let qualified = if let Some(second) = self.try(|p| {
            p.expect_punctuation('.')?;
            p.parse_unreserved_ident()
        })
        {
            Qualified {
                prefix: Some(name),
                name: second,
            }
        } else {
            Qualified {
                prefix: None,
                name: name,
            }
        };
        let generics = if self.is_punctuation('<') {
            self.parse_type_params()?
        } else {
            vec![]
        };
        let mut ty = Box::new(Type::Path(qualified, generics));
        if self.eat_keyword("Function") {
            let sig = self.parse_fn_args(ty)?;
            ty = Box::new(Type::Function(sig));
        }
        Ok(ty)
    }

    fn parse_type_params(&mut self) -> ParseResult<Vec<Box<Type>>> {
        self.expect_punctuation('<')?;
        let type_args = self.parse_one_or_more(',', |p| p.parse_type())?;
        self.expect_punctuation('>')?;
        Ok(type_args)
    }

    fn parse_arguments(&mut self) -> ParseResult<Args> {
        let mut unnamed_arguments = vec![];

        self.expect_punctuation('(')?;
        loop {
            if self.is_punctuation(')') {
                break;
            }
            if self.probe(|p| p.parse_ident().is_ok() && p.is_punctuation(':')) {
                break;
            }
            unnamed_arguments.push(self.parse_expr()?);
            if !self.eat_punctuation(',') {
                break;
            }
        }
        let mut named_arguments = vec![];
        loop {
            if self.is_punctuation(')') {
                break;
            }
            let name = self.parse_ident()?;
            self.expect_punctuation(':')?;
            let expr = self.parse_expr()?;
            named_arguments.push(NamedArg { name, expr });
            if !self.eat_punctuation(',') {
                break;
            }
        }
        self.expect_punctuation(')')?;
        Ok(Args {
            unnamed: unnamed_arguments,
            named: named_arguments,
        })
    }

    fn parse_metadata(&mut self) -> ParseResult<Metadata> {
        let mut meta = vec![];
        let mut meta_item;
        while self.eat_punctuation('@') {
            meta_item = MetadataItem {
                qualified: self.parse_qualified()?,
                suffix: None,
                arguments: None,
            };
            if self.eat_punctuation('.') {
                meta_item.suffix = Some(self.parse_ident()?);
            }
            if self.is_punctuation('(') {
                meta_item.arguments = Some(self.parse_arguments()?);
            }

            meta.push(meta_item);
        }
        Ok(meta)
    }

    pub fn parse_expr(&mut self) -> ParseResult<Box<Expr>> {
        if self.eat_keyword("throw") {
            return Ok(Box::new(Expr::Throw(self.parse_expr()?)));
        }
        let mut expr = self.parse_conditional_expr()?;
        while let Some(cascade) = self.try(|p| p.parse_casacade()) {
            expr = Box::new(Expr::Cascade(expr, cascade));
        }
        if let Some(op) = self.try(|p| p.parse_assign_op()) {
            expr = Box::new(Expr::Binary(BinOp::Assign(op), expr, self.parse_expr()?));
        }
        Ok(expr)
    }

    pub fn parse_expr_no_cascade(&mut self) -> ParseResult<Box<Expr>> {
        if self.eat_keyword("throw") {
            return Ok(Box::new(Expr::Throw(self.parse_expr_no_cascade()?)));
        }
        let mut expr = self.parse_conditional_expr()?;
        if let Some(op) = self.try(|p| p.parse_assign_op()) {
            expr = Box::new(Expr::Binary(
                BinOp::Assign(op),
                expr,
                self.parse_expr_no_cascade()?,
            ));
        }
        Ok(expr)
    }

    fn parse_conditional_expr(&mut self) -> ParseResult<Box<Expr>> {
        let expr = self.parse_is_null_expression()?;
        if !self.is_punctuation2('?', '?') && self.eat_punctuation('?') {
            let expr2 = self.parse_expr_no_cascade()?;
            self.expect_punctuation(':')?;
            Ok(Box::new(Expr::Conditional(
                expr,
                expr2,
                self.parse_expr_no_cascade()?,
            )))
        } else {
            Ok(expr)
        }
    }

    fn parse_is_null_expression(&mut self) -> ParseResult<Box<Expr>> {
        let mut expr = self.parse_or_expr()?;
        while self.eat_bin_op(BinOp::Value(ValueBinOp::IfNull)) {
            expr = Box::new(Expr::Binary(
                BinOp::Value(ValueBinOp::IfNull),
                expr,
                self.parse_or_expr()?,
            ));
        }
        Ok(expr)
    }

    fn parse_or_expr(&mut self) -> ParseResult<Box<Expr>> {
        let mut expr = self.parse_and_expr()?;
        while self.eat_bin_op(BinOp::Bool(BoolBinOp::Or)) {
            expr = Box::new(Expr::Binary(
                BinOp::Bool(BoolBinOp::Or),
                expr,
                self.parse_and_expr()?,
            ));
        }
        Ok(expr)
    }

    fn parse_and_expr(&mut self) -> ParseResult<Box<Expr>> {
        let mut expr = self.parse_equality_expr()?;
        while self.eat_bin_op(BinOp::Bool(BoolBinOp::And)) {
            expr = Box::new(Expr::Binary(
                BinOp::Bool(BoolBinOp::And),
                expr,
                self.parse_equality_expr()?,
            ));
        }
        Ok(expr)
    }

    fn parse_equality_expr(&mut self) -> ParseResult<Box<Expr>> {
        let expr = self.parse_relational_expr()?;
        if self.eat_bin_op(BinOp::Bool(BoolBinOp::Eq)) {
            Ok(Box::new(Expr::Binary(
                BinOp::Bool(BoolBinOp::Eq),
                expr,
                self.parse_relational_expr()?,
            )))
        } else if self.eat_bin_op(BinOp::Bool(BoolBinOp::Ne)) {
            Ok(Box::new(Expr::Binary(
                BinOp::Bool(BoolBinOp::Ne),
                expr,
                self.parse_relational_expr()?,
            )))
        } else {
            Ok(expr)
        }
    }

    fn parse_relational_expr(&mut self) -> ParseResult<Box<Expr>> {
        let expr = self.parse_bit_or_expr()?;
        if self.eat_keyword("is") {
            if self.eat_punctuation('!') {
                Ok(Box::new(Expr::IsNot(expr, self.parse_type()?)))
            } else {
                Ok(Box::new(Expr::Is(expr, self.parse_type()?)))
            }
        } else if self.eat_keyword("as") {
            Ok(Box::new(Expr::As(expr, self.parse_type()?)))
        } else if self.eat_bin_op(BinOp::Bool(BoolBinOp::Ge)) {
            Ok(Box::new(Expr::Binary(
                BinOp::Bool(BoolBinOp::Ge),
                expr,
                self.parse_bit_or_expr()?,
            )))
        } else if self.eat_bin_op(BinOp::Bool(BoolBinOp::Gt)) {
            Ok(Box::new(Expr::Binary(
                BinOp::Bool(BoolBinOp::Gt),
                expr,
                self.parse_bit_or_expr()?,
            )))
        } else if self.eat_bin_op(BinOp::Bool(BoolBinOp::Le)) {
            Ok(Box::new(Expr::Binary(
                BinOp::Bool(BoolBinOp::Le),
                expr,
                self.parse_bit_or_expr()?,
            )))
        } else if self.eat_bin_op(BinOp::Bool(BoolBinOp::Lt)) {
            Ok(Box::new(Expr::Binary(
                BinOp::Bool(BoolBinOp::Lt),
                expr,
                self.parse_bit_or_expr()?,
            )))
        } else {
            Ok(expr)
        }
    }

    fn parse_bit_or_expr(&mut self) -> ParseResult<Box<Expr>> {
        let mut expr = self.parse_bit_xor_expr()?;
        while self.eat_bin_op(BinOp::Value(ValueBinOp::BitOr)) {
            expr = Box::new(Expr::Binary(
                BinOp::Value(ValueBinOp::BitOr),
                expr,
                self.parse_bit_xor_expr()?,
            ));
        }
        Ok(expr)
    }

    fn parse_bit_xor_expr(&mut self) -> ParseResult<Box<Expr>> {
        let mut expr = self.parse_bit_and_expr()?;
        while self.eat_bin_op(BinOp::Value(ValueBinOp::BitXor)) {
            expr = Box::new(Expr::Binary(
                BinOp::Value(ValueBinOp::BitXor),
                expr,
                self.parse_bit_and_expr()?,
            ));
        }
        Ok(expr)
    }

    fn parse_bit_and_expr(&mut self) -> ParseResult<Box<Expr>> {
        let mut expr = self.parse_shift_expr()?;
        while self.eat_bin_op(BinOp::Value(ValueBinOp::BitAnd)) {
            expr = Box::new(Expr::Binary(
                BinOp::Value(ValueBinOp::BitAnd),
                expr,
                self.parse_shift_expr()?,
            ));
        }
        Ok(expr)
    }

    fn parse_shift_expr(&mut self) -> ParseResult<Box<Expr>> {
        let mut expr = self.parse_add_expr()?;
        loop {
            if self.eat_bin_op(BinOp::Value(ValueBinOp::Lsh)) {
                expr = Box::new(Expr::Binary(
                    BinOp::Value(ValueBinOp::Lsh),
                    expr,
                    self.parse_add_expr()?,
                ));
            } else if self.eat_bin_op(BinOp::Value(ValueBinOp::Rsh)) {
                expr = Box::new(Expr::Binary(
                    BinOp::Value(ValueBinOp::Rsh),
                    expr,
                    self.parse_add_expr()?,
                ));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_add_expr(&mut self) -> ParseResult<Box<Expr>> {
        let mut expr = self.parse_mult_expr()?;
        loop {
            if self.eat_bin_op(BinOp::Value(ValueBinOp::Add)) {
                expr = Box::new(Expr::Binary(
                    BinOp::Value(ValueBinOp::Add),
                    expr,
                    self.parse_mult_expr()?,
                ));
            } else if self.eat_bin_op(BinOp::Value(ValueBinOp::Sub)) {
                expr = Box::new(Expr::Binary(
                    BinOp::Value(ValueBinOp::Sub),
                    expr,
                    self.parse_mult_expr()?,
                ));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_mult_expr(&mut self) -> ParseResult<Box<Expr>> {
        let mut expr = self.parse_unary_expr()?;
        loop {
            if self.eat_bin_op(BinOp::Value(ValueBinOp::Mul)) {
                expr = Box::new(Expr::Binary(
                    BinOp::Value(ValueBinOp::Mul),
                    expr,
                    self.parse_unary_expr()?,
                ));
            } else if self.eat_bin_op(BinOp::Value(ValueBinOp::Div)) {
                expr = Box::new(Expr::Binary(
                    BinOp::Value(ValueBinOp::Div),
                    expr,
                    self.parse_unary_expr()?,
                ));
            } else if self.eat_bin_op(BinOp::Value(ValueBinOp::Mod)) {
                expr = Box::new(Expr::Binary(
                    BinOp::Value(ValueBinOp::Mod),
                    expr,
                    self.parse_unary_expr()?,
                ));
            } else if self.eat_bin_op(BinOp::Value(ValueBinOp::TruncDiv)) {
                expr = Box::new(Expr::Binary(
                    BinOp::Value(ValueBinOp::TruncDiv),
                    expr,
                    self.parse_unary_expr()?,
                ));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_unary_expr(&mut self) -> ParseResult<Box<Expr>> {
        if self.eat_punctuation('-') {
            Ok(Box::new(Expr::Unary(UnOp::Neg, self.parse_unary_expr()?)))
        } else if self.eat_punctuation('!') {
            Ok(Box::new(Expr::Unary(UnOp::Not, self.parse_unary_expr()?)))
        } else if self.eat_punctuation('~') {
            Ok(Box::new(
                Expr::Unary(UnOp::BitNot, self.parse_unary_expr()?),
            ))
        } else if self.eat_keyword("await") {
            Ok(Box::new(Expr::Unary(UnOp::Await, self.parse_unary_expr()?)))
        } else if self.eat_punctuation2('+', '+') {
            Ok(Box::new(Expr::Unary(UnOp::PreInc, self.parse_expr()?)))
        } else if self.eat_punctuation2('-', '-') {
            Ok(Box::new(Expr::Unary(UnOp::PreDec, self.parse_expr()?)))
        } else {
            Ok(self.parse_postfix_expr()?)
        }
    }

    fn parse_postfix_expr(&mut self) -> ParseResult<Box<Expr>> {
        let mut expr = self.parse_primary_expr()?;
        while let Some(suffix) = self.parse_suffix()? {
            expr = Box::new(Expr::Suffix(expr, suffix));
        }
        if self.eat_punctuation2('+', '+') {
            Ok(Box::new(Expr::Unary(UnOp::PostInc, expr)))
        } else if self.eat_punctuation2('-', '-') {
            Ok(Box::new(Expr::Unary(UnOp::PostDec, expr)))
        } else {
            Ok(expr)
        }
    }

    fn parse_casacade(&mut self) -> ParseResult<Cascade> {
        if self.probe(|p| p.eat_punctuation2('.', '.') && p.parse_ident().is_ok()) {
            self.bump();
        } else {
            self.expect_punctuation2('.', '.')?;
        }
        let mut suffixes = vec![];
        while let Some(suffix) = self.parse_suffix()? {
            suffixes.push(suffix);
        }
        let assign = self.try(|p| Ok((p.parse_assign_op()?, p.parse_expr_no_cascade()?)));
        Ok(Cascade { suffixes, assign })
    }

    fn parse_assign_op(&mut self) -> ParseResult<Option<ValueBinOp>> {
        for op in ValueBinOp::values() {
            if self.eat_bin_op(BinOp::Assign(Some(op))) {
                return Ok(Some(op));
            }
        }
        self.expect_bin_op(BinOp::Assign(None))?;
        Ok(None)
    }

    fn parse_suffix(&mut self) -> ParseResult<Option<Suffix>> {
        if self.eat_punctuation('[') {
            let expr = self.parse_expr()?;
            self.expect_punctuation(']')?;
            Ok(Some(Suffix::Index(expr)))
        } else if !self.is_punctuation2('.', '.') && self.eat_punctuation('.') {
            Ok(Some(Suffix::Field(self.parse_ident()?)))
        } else if self.eat_punctuation2('?', '.') {
            Ok(Some(Suffix::FieldIfNotNull(self.parse_ident()?)))
        } else if self.is_punctuation('(') {
            Ok(Some(Suffix::Call(vec![], self.parse_arguments()?)))
        } else if let Some(generics) = self.try(|p| p.parse_type_params()) {
            Ok(Some(Suffix::Call(generics, self.parse_arguments()?)))
        } else {
            Ok(None)
        }
    }

    fn parse_primary_expr(&mut self) -> ParseResult<Box<Expr>> {
        if self.is_punctuation('(') {
            let args = self.try(|p| {
                let args = p.parse_fn_args(Box::new(Type::Infer))?;
                if p.is_punctuation('{') || p.is_punctuation2('=', '>') {
                    Ok(args)
                } else {
                    expected!(p, Punctuation2('=', '>'));
                }
            });
            if let Some(args) = args {
                return Ok(Box::new(Expr::Closure(args, self.parse_fn_body(false)?)));
            }
            self.expect_punctuation('(')?;
            let expr = self.parse_expr()?;
            self.expect_punctuation(')')?;
            return Ok(Box::new(Expr::Paren(expr)));
        }
        let const_ = self.eat_keyword("const");
        let mut generics = if self.is_punctuation('<') {
            self.parse_type_params()?
        } else {
            vec![]
        };
        if self.eat_punctuation('[') {
            let mut elements = vec![];
            loop {
                if self.is_punctuation(']') {
                    break;
                }
                elements.push(self.parse_expr()?);
                if !self.eat_punctuation(',') {
                    break;
                }
            }
            self.expect_punctuation(']')?;
            let element_ty = if generics.is_empty() {
                None
            } else {
                assert_eq!(generics.len(), 1);
                generics.pop()
            };
            return Ok(Box::new(Expr::List {
                const_,
                element_ty,
                elements,
            }));
        }
        if self.eat_punctuation('{') {
            let mut kv = vec![];
            loop {
                if self.is_punctuation('}') {
                    break;
                }
                let k = self.parse_expr()?;
                self.expect_punctuation(':')?;
                kv.push((k, self.parse_expr()?));
                if !self.eat_punctuation(',') {
                    break;
                }
            }
            self.expect_punctuation('}')?;
            let kv_ty = if generics.is_empty() {
                None
            } else {
                assert_eq!(generics.len(), 2);
                let v_ty = generics.pop().unwrap();
                let k_ty = generics.pop().unwrap();
                Some((k_ty, v_ty))
            };
            return Ok(Box::new(Expr::Map { const_, kv_ty, kv }));
        }
        if const_ || self.eat_keyword("new") {
            let ty = self.parse_type()?;
            let ctor = if self.eat_punctuation('.') {
                let name = self.parse_ident()?;
                Some(name)
            } else {
                None
            };
            let args = self.parse_arguments()?;
            return Ok(Box::new(Expr::New {
                const_,
                ty,
                ctor,
                args,
            }));
        }
        if let Some(number) = self.try(|p| p.parse_number_literal()) {
            return Ok(number);
        }
        if let Some(str_lit) = self.try(|p| p.parse_string_literal()) {
            let mut strings = vec![str_lit];
            while let Some(str_lit) = self.try(|p| p.parse_string_literal()) {
                strings.push(str_lit);
            }
            return Ok(Box::new(Expr::String(strings)));
        }
        if self.eat_punctuation('#') {
            return Ok(Box::new(Expr::Identifier(self.parse_ident()?)));
        }
        if let Ok(ident) = self.parse_ident() {
            return Ok(Box::new(Expr::Identifier(ident)));
        }
        expected!(self, Expr);
    }

    fn parse_number_literal(&mut self) -> ParseResult<Box<Expr>> {
        let mut number = String::new();

        if let Some(Token::IntegerLiteral(int_part)) = self.cur {
            self.bump_raw();
            number += &int_part.as_str();
        }

        if self.cur == Some(Token::Punctuation('.')) {
            self.bump_raw();
            number.push('.');
            if let Some(Token::IntegerLiteral(frac_part)) = self.cur {
                self.bump_raw();
                number += &frac_part.as_str();
            }
        }

        if number.is_empty() {
            expected!(self, NumberLiteral);
        }

        if let Some(Token::WhiteSpace(_)) = self.cur {
            self.bump();
        }
        Ok(Box::new(Expr::Number(Symbol::intern(&number))))
    }

    fn parse_fn_args(&mut self, return_type: Box<Type>) -> ParseResult<FnSig> {
        let mut sig = FnSig {
            return_type,
            required: vec![],
            optional: vec![],
            optional_kind: OptionalArgKind::default(),
            async: false,
            generator: false,
        };
        self.expect_punctuation('(')?;
        loop {
            if self.is_punctuation(')') {
                break;
            }
            if self.eat_punctuation('[') {
                loop {
                    if self.is_punctuation(']') {
                        break;
                    }
                    let arg = self.parse_arg_def()?;
                    let mut default = None;
                    if self.eat_punctuation('=') {
                        default = Some(self.parse_expr()?);
                    }
                    sig.optional.push(OptionalArgDef { arg, default });
                    if !self.eat_punctuation(',') {
                        break;
                    }
                }
                self.expect_punctuation(']')?;
                break;
            }
            if self.eat_punctuation('{') {
                sig.optional_kind = OptionalArgKind::Named;
                loop {
                    if self.is_punctuation('}') {
                        break;
                    }
                    let arg = self.parse_arg_def()?;
                    let mut default = None;
                    if self.eat_punctuation(':') || self.eat_punctuation('=') {
                        default = Some(self.parse_expr()?);
                    }
                    sig.optional.push(OptionalArgDef { arg, default });
                    if !self.eat_punctuation(',') {
                        break;
                    }
                }
                self.expect_punctuation('}')?;
                break;
            }
            sig.required.push(self.parse_arg_def()?);
            if !self.eat_punctuation(',') {
                break;
            }
        }
        self.expect_punctuation(')')?;
        if self.eat_keyword("async") {
            sig.async = true;
            sig.generator = self.eat_punctuation('*');
        } else if self.eat_keyword("sync") {
            self.expect_punctuation('*')?;
            sig.generator = true;
        }
        Ok(sig)
    }

    fn parse_arg_def(&mut self) -> ParseResult<ArgDef> {
        let metadata = self.parse_metadata()?;
        let covariant = self.eat_keyword("covariant");
        let mut ty = self.parse_var_type(false)?;
        let mut field = false;
        if self.eat_keyword("this") {
            self.expect_punctuation('.')?;
            field = true;
        }
        let name = self.parse_ident()?;
        if self.is_punctuation('(') {
            ty.ty = Box::new(Type::FunctionOld(self.parse_fn_args(ty.ty)?));
        }
        Ok(ArgDef {
            metadata,
            covariant,
            ty,
            field,
            name,
        })
    }

    fn parse_var_type(&mut self, requires_var: bool) -> ParseResult<VarType> {
        let fcv = if self.eat_keyword("const") {
            FinalConstVar::Const
        } else if self.eat_keyword("final") {
            FinalConstVar::Final
        } else {
            FinalConstVar::Var
        };
        let ty = self.try(|p| {
            let ty = p.parse_type()?;

            if p.probe(|p| p.parse_ident().is_ok()) {
                Ok(ty)
            } else {
                expected!(p, Ident);
            }
        }).ok_or(())
            .or_else(|_| -> ParseResult<_> {
                if let FinalConstVar::Var = fcv {
                    if requires_var {
                        self.expect_keyword("var")?;
                    } else {
                        self.eat_keyword("var");
                    }
                }
                Ok(Box::new(Type::Infer))
            })?;

        Ok(VarType { fcv, ty })
    }

    fn parse_block(&mut self) -> ParseResult<Box<Statement>> {
        self.expect_punctuation('{')?;
        let mut statements = vec![];
        if self.skip_blocks {
            let mut depth = 0;
            while !self.out_of_tokens() {
                if self.is_punctuation('{') {
                    depth += 1;
                }
                if self.is_punctuation('}') {
                    if depth == 0 {
                        break;
                    }
                    depth -= 1;
                }
                self.bump();
            }
        } else {
            loop {
                if self.is_punctuation('}') {
                    break;
                }
                statements.push(self.parse_statement()?);
            }
        }
        self.expect_punctuation('}')?;
        Ok(Box::new(Statement::Block(statements)))
    }

    fn parse_catch_part(&mut self) -> ParseResult<CatchPart> {
        self.expect_punctuation('(')?;
        let exception = self.parse_ident()?;
        let trace = if self.eat_punctuation(',') {
            Some(self.parse_ident()?)
        } else {
            None
        };
        self.expect_punctuation(')')?;
        Ok(CatchPart { exception, trace })
    }

    fn parse_statement(&mut self) -> ParseResult<Box<Statement>> {
        if let Some((label, _)) = self.try(|p| Ok((p.parse_ident()?, p.expect_punctuation(':')?))) {
            return Ok(Box::new(
                Statement::Labelled(label, self.parse_statement()?),
            ));
        }
        if self.is_punctuation('{') {
            return Ok(self.parse_block()?);
        }
        let await = self.eat_keyword("await");
        if self.eat_keyword("for") {
            self.expect_punctuation('(')?;
            let for_loop = self.try(|p| {
                let var_type = p.try(|p| p.parse_var_type(true));
                let name = p.parse_ident()?;
                p.expect_keyword("in")?;
                Ok(ForLoop::In(var_type, name, p.parse_expr()?))
            }).ok_or(())
                .or_else(|_| -> ParseResult<_> {
                    let statement = self.parse_statement()?;
                    let cond = if self.is_punctuation(';') {
                        None
                    } else {
                        Some(self.parse_expr()?)
                    };
                    self.expect_punctuation(';')?;
                    let exprs = if self.is_punctuation(')') {
                        vec![]
                    } else {
                        self.parse_one_or_more(',', |p| p.parse_expr())?
                    };
                    Ok(ForLoop::CLike(statement, cond, exprs))
                })?;
            self.expect_punctuation(')')?;
            return Ok(Box::new(
                Statement::For(await, for_loop, self.parse_statement()?),
            ));
        }
        if self.eat_keyword("while") {
            self.expect_punctuation('(')?;
            let expr = self.parse_expr()?;
            self.expect_punctuation(')')?;
            return Ok(Box::new(Statement::While(expr, self.parse_statement()?)));
        }
        if self.eat_keyword("do") {
            let statement = self.parse_statement()?;
            self.expect_keyword("while")?;
            self.expect_punctuation('(')?;
            let expr = self.parse_expr()?;
            self.expect_punctuation(')')?;
            self.expect_punctuation(';')?;
            return Ok(Box::new(Statement::DoWhile(statement, expr)));
        }
        if self.eat_keyword("switch") {
            self.expect_punctuation('(')?;
            let expr = self.parse_expr()?;
            self.expect_punctuation(')')?;
            self.expect_punctuation('{')?;
            let mut sc = vec![];
            loop {
                if self.is_punctuation('}') {
                    break;
                }
                let mut labels = vec![];
                while !self.is_keyword("case") && !self.is_keyword("default") {
                    labels.push(self.parse_ident()?);
                    self.expect_punctuation(':')?;
                }
                let value = if self.eat_keyword("case") {
                    Some(self.parse_expr()?)
                } else {
                    self.expect_keyword("default")?;
                    None
                };
                self.expect_punctuation(':')?;

                let mut statements = vec![];
                loop {
                    if self.is_punctuation('}') {
                        break;
                    }
                    let done = self.probe(|p| {
                        while p.parse_ident().is_ok() && p.eat_punctuation(':') {}
                        p.is_keyword("case") || p.is_keyword("default")
                    });
                    if done {
                        break;
                    }
                    statements.push(self.parse_statement()?);
                }

                sc.push(SwitchCase {
                    labels,
                    value,
                    statements,
                });
            }
            self.expect_punctuation('}')?;
            return Ok(Box::new(Statement::Switch(expr, sc)));
        }
        if self.eat_keyword("if") {
            self.expect_punctuation('(')?;
            let expr = self.parse_expr()?;
            self.expect_punctuation(')')?;
            let statement = self.parse_statement()?;
            let else_statement = if self.eat_keyword("else") {
                Some(self.parse_statement()?)
            } else {
                None
            };
            return Ok(Box::new(Statement::If(expr, statement, else_statement)));
        }
        if self.eat_keyword("rethrow") {
            return Ok(Box::new(Statement::Rethrow));
        }
        if self.eat_keyword("try") {
            let block = self.parse_block()?;
            let mut parts = vec![];
            loop {
                if self.eat_keyword("on") {
                    let on = Some(self.parse_type()?);
                    let catch = if self.eat_keyword("catch") {
                        Some(self.parse_catch_part()?)
                    } else {
                        None
                    };
                    parts.push(TryPart {
                        on,
                        catch,
                        block: self.parse_block()?,
                    });
                } else if self.eat_keyword("catch") {
                    let catch = Some(self.parse_catch_part()?);
                    parts.push(TryPart {
                        on: None,
                        catch,
                        block: self.parse_block()?,
                    });
                } else if self.eat_keyword("finally") {
                    parts.push(TryPart {
                        on: None,
                        catch: None,
                        block: self.parse_block()?,
                    });
                    break;
                } else {
                    break;
                }
            }
            return Ok(Box::new(Statement::Try(block, parts)));
        }
        if self.eat_keyword("break") {
            if self.is_punctuation(';') {
                self.expect_punctuation(';')?;
                return Ok(Box::new(Statement::Break(None)));
            } else {
                let ident = self.parse_ident()?;
                self.expect_punctuation(';')?;
                return Ok(Box::new(Statement::Break(Some(ident))));
            }
        }
        if self.eat_keyword("continue") {
            if self.is_punctuation(';') {
                self.expect_punctuation(';')?;
                return Ok(Box::new(Statement::Continue(None)));
            } else {
                let ident = self.parse_ident()?;
                self.expect_punctuation(';')?;
                return Ok(Box::new(Statement::Continue(Some(ident))));
            }
        }
        if self.eat_keyword("return") {
            if self.eat_punctuation(';') {
                return Ok(Box::new(Statement::Return(None)));
            }
            let expr = self.parse_expr()?;
            self.expect_punctuation(';')?;
            return Ok(Box::new(Statement::Return(Some(expr))));
        }
        if self.eat_keyword("yield") {
            if self.eat_punctuation('*') {
                let expr = self.parse_expr()?;
                self.expect_punctuation(';')?;
                return Ok(Box::new(Statement::YieldEach(expr)));
            } else {
                let expr = self.parse_expr()?;
                self.expect_punctuation(';')?;
                return Ok(Box::new(Statement::Yield(expr)));
            }
        }
        if self.eat_keyword("assert") {
            let args = self.parse_arguments()?;
            self.expect_punctuation(';')?;
            return Ok(Box::new(Statement::Assert(args)));
        }
        let var_stmt = self.try(|p| {
            let var_type = p.parse_var_type(true)?;
            let names_and_initializers =
                p.parse_one_or_more(',', |p| p.parse_name_and_initializer())?;
            p.expect_punctuation(';')?;
            Ok(Box::new(Statement::Var(var_type, names_and_initializers)))
        });
        if let Some(var_stmt) = var_stmt {
            return Ok(var_stmt);
        }
        if let Some(function) = self.try(|p| p.parse_function(true)) {
            return Ok(Box::new(Statement::Function(function)));
        }
        if self.eat_punctuation(';') {
            return Ok(Box::new(Statement::Expression(None)));
        }
        let expr = self.parse_expr()?;
        self.expect_punctuation(';')?;
        Ok(Box::new(Statement::Expression(Some(expr))))
    }

    fn parse_fn_body(&mut self, requires_semi: bool) -> ParseResult<FnBody> {
        if self.eat_keyword("native") {
            let native_thing = if !self.is_punctuation(';') {
                self.parse_string_literal().ok()
            } else {
                None
            };
            self.expect_punctuation(';')?;
            return Ok(FnBody::Native(native_thing));
        }
        if self.eat_punctuation2('=', '>') {
            let expr = self.parse_expr()?;
            if requires_semi {
                self.expect_punctuation(';')?;
            }
            Ok(FnBody::Arrow(expr))
        } else {
            Ok(FnBody::Block(self.parse_block()?))
        }
    }

    fn parse_type_param_def(&mut self) -> ParseResult<TypeParameter> {
        let metadata = self.parse_metadata()?;
        let name = self.parse_ident()?;
        let extends = if self.eat_keyword("extends") {
            Some(self.parse_type()?)
        } else {
            None
        };
        Ok(TypeParameter {
            metadata,
            name,
            extends,
        })
    }

    fn parse_name_and_initializer(&mut self) -> ParseResult<NameAndInitializer> {
        let name = self.parse_ident()?;
        let init = if self.eat_punctuation('=') {
            Some(self.parse_expr()?)
        } else {
            None
        };
        Ok(NameAndInitializer { name, init })
    }

    fn parse_constructor_initializer(&mut self) -> ParseResult<ConstructorInitializer> {
        if self.eat_keyword("assert") {
            let args = self.parse_arguments()?;
            return Ok(ConstructorInitializer::Assert(args));
        }
        if self.eat_keyword("super") {
            let name = if self.eat_punctuation('.') {
                Some(self.parse_ident()?)
            } else {
                None
            };
            let args = self.parse_arguments()?;
            return Ok(ConstructorInitializer::Super(name, args));
        }
        let (this, field) = if self.eat_keyword("this") {
            let name = if self.eat_punctuation('.') {
                Some(self.parse_ident()?)
            } else {
                None
            };
            if name.is_some() && self.is_punctuation('=') {
                (true, name.unwrap())
            } else {
                let args = self.parse_arguments()?;
                return Ok(ConstructorInitializer::This(name, args));
            }
        } else {
            (false, self.parse_ident()?)
        };

        self.expect_punctuation('=')?;
        let mut expr = self.parse_conditional_expr()?;
        while let Some(cascade) = self.try(|p| p.parse_casacade()) {
            expr = Box::new(Expr::Cascade(expr, cascade));
        }
        return Ok(ConstructorInitializer::Field(this, field, expr));
    }

    fn parse_overloaded_op(&mut self) -> ParseResult<OverloadedOp> {
        if self.eat_punctuation2('[', ']') {
            if self.eat_punctuation('=') {
                return Ok(OverloadedOp::IndexAssign);
            } else {
                return Ok(OverloadedOp::Index);
            }
        }

        if let Some(Token::Punctuation(c)) = self.cur {
            let bin_op = match c {
                '*' => OverloadedOp::Value(ValueBinOp::Mul),
                '/' => OverloadedOp::Value(ValueBinOp::Div),
                '%' => OverloadedOp::Value(ValueBinOp::Mod),
                '~' => {
                    if self.is_punctuation2('~', '/') {
                        self.bump();
                        OverloadedOp::Value(ValueBinOp::TruncDiv)
                    } else {
                        OverloadedOp::BitNot
                    }
                }
                '+' => OverloadedOp::Value(ValueBinOp::Add),
                '-' => OverloadedOp::Value(ValueBinOp::Sub),
                '>' => {
                    if self.is_punctuation2('>', '>') {
                        self.bump();
                        OverloadedOp::Value(ValueBinOp::Rsh)
                    } else if self.is_punctuation2('>', '=') {
                        self.bump();
                        OverloadedOp::Bool(BoolBinOp::Ge)
                    } else {
                        OverloadedOp::Bool(BoolBinOp::Gt)
                    }
                }
                '<' => {
                    if self.is_punctuation2('<', '<') {
                        self.bump();
                        OverloadedOp::Value(ValueBinOp::Lsh)
                    } else if self.is_punctuation2('<', '=') {
                        self.bump();
                        OverloadedOp::Bool(BoolBinOp::Le)
                    } else {
                        OverloadedOp::Bool(BoolBinOp::Lt)
                    }
                }
                '=' => {
                    self.expect_punctuation2('=', '=')?;
                    return Ok(OverloadedOp::Bool(BoolBinOp::Eq));
                }
                '&' => OverloadedOp::Value(ValueBinOp::BitAnd),
                '^' => OverloadedOp::Value(ValueBinOp::BitXor),
                '|' => OverloadedOp::Value(ValueBinOp::BitOr),
                _ => expected!(self, OverloadedOp),
            };
            self.bump();
            Ok(bin_op)
        } else {
            expected!(self, OverloadedOp);
        }
    }

    fn parse_fn_name(&mut self) -> ParseResult<FnName> {
        if let Some((_, name)) = self.try(|p| Ok((p.expect_keyword("get")?, p.parse_ident()?))) {
            return Ok(FnName::Getter(name));
        }
        if let Some((_, name)) = self.try(|p| Ok((p.expect_keyword("set")?, p.parse_ident()?))) {
            return Ok(FnName::Setter(name));
        }
        if self.eat_keyword("operator") {
            return Ok(FnName::Operator(self.parse_overloaded_op()?));
        }
        Ok(FnName::Regular(self.parse_ident()?))
    }

    fn parse_function(&mut self, requires_body: bool) -> ParseResult<Function> {
        let (return_type, name) = self.try(|p| Ok((p.parse_type()?, p.parse_fn_name()?)))
            .ok_or(())
            .or_else(|_| -> ParseResult<_> {
                Ok((Box::new(Type::Infer), self.parse_fn_name()?))
            })?;
        let mut generics = vec![];
        if self.eat_punctuation('<') {
            generics = self.parse_one_or_more(',', |p| p.parse_type_param_def())?;
            self.expect_punctuation('>')?;
        }
        let sig = {
            if let FnName::Getter(_) = name {
                let mut sig = FnSig::default();
                if self.eat_keyword("async") {
                    sig.async = true;
                    sig.generator = self.eat_punctuation('*');
                } else if self.eat_keyword("sync") {
                    self.expect_punctuation('*')?;
                    sig.generator = true;
                }
                sig
            } else {
                self.parse_fn_args(return_type)?
            }
        };
        Ok(Function {
            name,
            generics,
            sig,
            body: if !requires_body && self.eat_punctuation(';') {
                None
            } else {
                Some(self.parse_fn_body(true)?)
            },
        })
    }

    fn parse_class_member(&mut self, class_name: Symbol) -> ParseResult<ClassMember> {
        let metadata = self.parse_metadata()?;
        let mut method_qualifiers = vec![];
        if self.eat_keyword("external") {
            method_qualifiers.push(MethodQualifiers::External);
        }
        if self.eat_keyword("static") {
            method_qualifiers.push(MethodQualifiers::Static);
        }
        if self.eat_keyword("final") {
            method_qualifiers.push(MethodQualifiers::Final);
        }
        if self.eat_keyword("const") {
            method_qualifiers.push(MethodQualifiers::Const);
        }
        if self.eat_keyword("factory") {
            method_qualifiers.push(MethodQualifiers::Factory);
        }
        let is_constructor = self.probe(|p| {
            p.parse_ident().ok() == Some(class_name) &&
                if p.eat_punctuation('.') {
                    p.parse_ident().is_ok()
                } else {
                    true
                } && p.is_punctuation('(')
        });
        if is_constructor {
            self.bump();
            let name = if self.eat_punctuation('.') {
                Some(self.parse_ident()?)
            } else {
                None
            };
            let sig = self.parse_fn_args(Box::new(Type::Infer))?;
            if !self.is_punctuation2('=', '>') && self.eat_punctuation('=') {
                let ty = self.parse_type()?;
                let name = if self.eat_punctuation('.') {
                    Some(self.parse_ident()?)
                } else {
                    None
                };
                self.expect_punctuation(';')?;
                return Ok(ClassMember::Redirect {
                    metadata,
                    method_qualifiers,
                    name,
                    sig,
                    ty,
                });
            }
            let initializers = if self.eat_punctuation(':') {
                self.parse_one_or_more(
                    ',',
                    |p| p.parse_constructor_initializer(),
                )?
            } else {
                vec![]
            };
            let function_body = if self.eat_punctuation(';') {
                None
            } else {
                Some(self.parse_fn_body(true)?)
            };
            return Ok(ClassMember::Constructor {
                metadata,
                method_qualifiers,
                name,
                sig,
                initializers,
                function_body,
            });
        }

        let mut static_ = false;
        let mut fcv = FinalConstVar::Var;
        for &mq in &method_qualifiers {
            match mq {
                MethodQualifiers::Static => static_ = true,
                MethodQualifiers::Final => fcv = FinalConstVar::Final,
                MethodQualifiers::Const => fcv = FinalConstVar::Const,
                _ => break,
            }
        }

        let fields = self.try(|p| {
            let ty = p.try(|p| {
                let ty = p.parse_type()?;
                if p.probe(|p| p.parse_ident().is_ok()) {
                    Ok(ty)
                } else {
                    expected!(p, Ident);
                }
            }).ok_or(())
                .or_else(|_| -> ParseResult<_> {
                    if let FinalConstVar::Var = fcv {
                        p.expect_keyword("var")?;
                    }
                    Ok(Box::new(Type::Infer))
                })?;
            let initializers = p.parse_one_or_more(',', |p| p.parse_name_and_initializer())?;
            p.expect_punctuation(';')?;
            Ok((VarType { fcv, ty }, initializers))
        });

        if let Some((var_type, initializers)) = fields {
            return Ok(ClassMember::Fields {
                metadata,
                static_,
                var_type,
                initializers,
            });
        }
        let function = self.parse_function(false)?;
        Ok(ClassMember::Method(metadata, method_qualifiers, function))
    }

    pub fn parse_item(&mut self) -> ParseResult<Item> {
        let metadata = self.parse_metadata()?;

        if self.eat_keyword("library") {
            let path = self.parse_one_or_more('.', |p| p.parse_ident())?;
            self.expect_punctuation(';')?;
            return Ok(Item::LibraryName { metadata, path });
        }

        if self.eat_keyword("import") {
            let uri = self.parse_string_literal()?;
            let deferred = if self.eat_keyword("deferred") {
                true
            } else {
                false
            };
            let as_ident = if self.eat_keyword("as") {
                Some(self.parse_ident()?)
            } else {
                None
            };
            let filters = self.parse_import_filters()?;
            self.expect_punctuation(';')?;
            return Ok(Item::Import(
                metadata,
                Import {
                    uri,
                    deferred,
                    as_ident,
                    filters,
                },
            ));
        }

        if !self.probe(|p| {
            p.eat_keyword("export");
            p.is_punctuation('(')
        }) && self.eat_keyword("export")
        {
            let uri = self.parse_string_literal()?;
            let import_filters = self.parse_import_filters()?;
            self.expect_punctuation(';')?;
            return Ok(Item::Export(metadata, uri, import_filters));
        }

        if self.eat_keyword("part") {
            if self.eat_keyword("of") {
                let path = self.parse_one_or_more('.', |p| p.parse_ident())?;
                self.expect_punctuation(';')?;
                return Ok(Item::PartOf { metadata, path });
            }
            let uri = self.parse_string_literal()?;
            self.expect_punctuation(';')?;
            return Ok(Item::Part { metadata, uri });
        }

        let _external = self.eat_keyword("external");
        let abstract_ = self.eat_keyword("abstract");
        if self.eat_keyword("class") {
            let class_name = self.parse_ident()?;
            let mut generics = vec![];
            if self.eat_punctuation('<') {
                generics = self.parse_one_or_more(',', |p| p.parse_type_param_def())?;
                self.expect_punctuation('>')?;
            }
            let (superclass, mixins) = if self.eat_keyword("extends") {
                let superclass = Some(self.parse_type()?);
                let mixins = if self.eat_keyword("with") {
                    self.parse_one_or_more(',', |p| p.parse_type())?
                } else {
                    vec![]
                };
                (superclass, mixins)
            } else {
                (None, vec![])
            };
            let interfaces = if self.eat_keyword("implements") {
                self.parse_one_or_more(',', |p| p.parse_type())?
            } else {
                vec![]
            };
            if self.eat_punctuation('{') {
                let mut members = vec![];
                loop {
                    if self.eat_punctuation('}') {
                        break;
                    }
                    members.push(self.parse_class_member(class_name)?);
                }
                return Ok(Item::Class {
                    metadata,
                    abstract_,
                    name: class_name,
                    generics,
                    superclass,
                    mixins,
                    interfaces,
                    members,
                });
            } else {
                self.expect_punctuation('=')?;
                let mut mixins = vec![self.parse_type()?];
                self.expect_keyword("with")?;
                mixins.extend(self.parse_one_or_more(',', |p| p.parse_type())?);
                let interfaces = if self.eat_keyword("implements") {
                    self.parse_one_or_more(',', |p| p.parse_type())?
                } else {
                    vec![]
                };
                self.expect_punctuation(';')?;
                return Ok(Item::MixinClass {
                    metadata,
                    abstract_,
                    name: class_name,
                    generics,
                    mixins,
                    interfaces,
                });
            }
        }

        if self.eat_keyword("enum") {
            let enum_name = self.parse_ident()?;
            self.expect_punctuation('{')?;
            let mut values = vec![];
            loop {
                values.push(self.parse_ident()?);
                if self.is_punctuation(',') {
                    self.expect_punctuation(',')?;
                }
                if self.eat_punctuation('}') {
                    break;
                }
            }
            return Ok(Item::Enum {
                metadata,
                name: enum_name,
                values,
            });
        }

        if self.eat_keyword("typedef") {
            let (return_type, name) = self.try(|p| Ok((p.parse_type()?, p.parse_ident()?)))
                .ok_or(())
                .or_else(|_| -> ParseResult<_> {
                    Ok((Box::new(Type::Infer), self.parse_ident()?))
                })?;
            let mut generics = vec![];
            if self.eat_punctuation('<') {
                generics = self.parse_one_or_more(',', |p| p.parse_type_param_def())?;
                self.expect_punctuation('>')?;
            }
            let sig = self.parse_fn_args(return_type)?;
            let ty = Box::new(Type::FunctionOld(sig));
            self.expect_punctuation(';')?;
            return Ok(Item::TypeAlias {
                metadata,
                name,
                generics,
                ty,
            });
        }

        self.try(|p| {
            let var_type = p.parse_var_type(true)?;
            let names_and_initializers =
                p.parse_one_or_more(',', |p| p.parse_name_and_initializer())?;
            p.expect_punctuation(';')?;
            Ok(Item::Global(var_type, names_and_initializers))
        }).ok_or(())
            .or_else(|_| Ok(Item::Function(self.parse_function(false)?)))
    }

    fn parse_import_filters(&mut self) -> ParseResult<Vec<ImportFilter>> {
        let mut import_filters = vec![];
        loop {
            let hide = if self.eat_keyword("show") {
                false
            } else if self.eat_keyword("hide") {
                true
            } else {
                break;
            };
            let mut names = vec![];
            loop {
                names.push(self.parse_ident()?);
                if !self.eat_punctuation(',') {
                    break;
                }
            }
            import_filters.push(ImportFilter { hide, names });
        }
        Ok(import_filters)
    }

    fn parse_string_literal(&mut self) -> ParseResult<StringLiteral> {
        if let Some(Token::StringLiteral {
                        contents,
                        raw,
                        triple,
                        quote,
                        interpolation_before: false,
                        interpolation_after,
                    }) = self.cur
        {
            self.bump();
            let mut lit = StringLiteral {
                raw,
                triple,
                quote,
                prefix: contents,
                interpolated: vec![],
            };
            if interpolation_after {
                loop {
                    let expr = self.parse_expr()?;
                    match self.cur {
                        Some(Token::StringLiteral {
                                 contents,
                                 raw,
                                 triple,
                                 quote,
                                 interpolation_before: true,
                                 interpolation_after,
                             }) if (raw, triple, quote) == (lit.raw, lit.triple, lit.quote) => {
                            self.bump();
                            lit.interpolated.push((expr, contents));
                            if !interpolation_after {
                                break;
                            }
                        }
                        _ => expected!(self, StringLiteral),
                    }
                }
            }
            Ok(lit)
        } else {
            expected!(self, StringLiteral);
        }
    }

    pub fn parse_items(&mut self) -> ParseResult<Vec<Item>> {
        let mut items = vec![];
        while !self.out_of_tokens() {
            items.push(self.parse_item()?);
        }
        Ok(items)
    }
}
