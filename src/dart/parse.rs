#![allow(unused_doc_comment)]

use dart::ast::*;
use dart::lex::{self, Lexer, Token};
use syntax::symbol::Symbol;
use syntax::codemap::Span;
use node::Node;
use std::{iter, slice};

type Tokens<'a> = iter::Cloned<slice::Iter<'a, (Span, Token)>>;
use std::path::{Path, PathBuf};

#[derive(Clone)]
pub struct Parser<'a> {
    tokens: Tokens<'a>,
    path: PathBuf,
    cur: Option<Token>,
    cur_span: Span,
    skip_blocks: bool,
}

error_chain! {
    types {
        Error, ErrorKind, ParseResultExt, ParseResult;
    }

    links {
        Lex(lex::Error, lex::ErrorKind);
    }

    foreign_links {
        Io(::std::io::Error);
    }

    errors {
        ExpectedAt {
            expected: Expected,
            span: Span,
        } {
            display("{:?}: expected {:?}", span, expected)
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

impl<'a> Parser<'a> {
    pub fn new(path: &Path, tokens: &'a [(Span, Token)]) -> Self {
        let mut parser = Parser {
            path: path.to_path_buf(),
            tokens: tokens.iter().cloned(),
            cur: None,
            cur_span: Span::default(),
            skip_blocks: false,
        };
        parser.bump();
        parser
    }

    pub fn with_file<F: FnOnce(Parser) -> ParseResult<R>, R>(path: &Path, f: F) -> ParseResult<R> {
        let tokens = Lexer::from_file(path)?.tokenize()?;
        f(Parser::new(path, &tokens))
    }

    pub fn skip_blocks(mut self) -> Self {
        self.skip_blocks = true;
        self
    }

    pub fn out_of_tokens(&self) -> bool {
        self.cur.is_none()
    }

    pub fn is_keyword(&self, s: &str) -> bool {
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

    pub fn bump(&mut self) {
        loop {
            self.bump_raw();
            match self.cur {
                Some(Token::WhiteSpace(_)) => {}
                _ => return,
            }
        }
    }

    pub fn expect_punctuation(&mut self, c: char) -> ParseResult<()> {
        if self.cur != Some(Token::Punctuation(c)) {
            expected!(self, Punctuation(c));
        }
        self.bump();
        Ok(())
    }

    pub fn eat_punctuation(&mut self, c: char) -> bool {
        self.try(|p| p.expect_punctuation(c)).is_some()
    }

    pub fn is_punctuation(&self, c: char) -> bool {
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

    pub fn eat_punctuation2(&mut self, c1: char, c2: char) -> bool {
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

    pub fn expect_keyword(&mut self, s: &'static str) -> ParseResult<()> {
        if !self.is_keyword(s) {
            expected!(self, Keyword(s));
        }
        self.bump();
        Ok(())
    }

    pub fn eat_keyword(&mut self, s: &'static str) -> bool {
        self.expect_keyword(s).is_ok()
    }

    pub fn probe<F: FnOnce(&mut Self) -> R, R>(&self, f: F) -> R {
        f(&mut self.clone())
    }

    pub fn try<F: FnOnce(&mut Self) -> ParseResult<T>, T>(&mut self, f: F) -> Option<T> {
        let mut parser = self.clone();
        let result = f(&mut parser);
        if result.is_ok() {
            *self = parser;
        }
        result.ok()
    }

    pub fn parse_ident(&mut self) -> ParseResult<Symbol> {
        let ident = if let Some(Token::Identifier(ident)) = self.cur {
            ident
        } else {
            expected!(self, Ident);
        };
        match &ident.as_str()[..] {
            "assert" |
            "break" |
            "case" |
            "catch" |
            "class" |
            "const" |
            "continue" |
            "default" |
            "do" |
            "else" |
            "enum" |
            "extends" |
            "ﬁnal" |
            "ﬁnally" |
            "for" |
            "if" |
            "in" |
            "is" |
            "new" |
            "rethrow" |
            "return" |
            "switch" |
            "throw" |
            "try" |
            "var" |
            "while" |
            "with" => {
                expected!(self, Ident);
            }
            _ => {}
        }
        self.bump();
        Ok(ident)
    }

    pub fn parse_one_or_more<F: FnMut(&mut Self) -> ParseResult<T>, T>(
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

    fn dart_qualified(&mut self) -> ParseResult<Node<Qualified>> {
        let mut prefix = None;
        loop {
            let name = self.parse_ident()?;
            let params = if self.is_punctuation('<') {
                self.dart_type_params()?
            } else {
                vec![]
            };
            let qualified = Node::new(Qualified {
                prefix,
                name,
                params,
            });

            if !self.eat_punctuation('.') {
                return Ok(qualified);
            }
            prefix = Some(qualified);
        }
    }

    pub fn dart_type(&mut self) -> ParseResult<Node<Type>> {
        let mut ty = Node::new(Type::Path(self.dart_qualified()?));
        if self.eat_keyword("Function") {
            let sig = self.dart_fn_args(ty)?;
            ty = Node::new(Type::Function(sig));
        }
        Ok(ty)
    }

    fn dart_type_params(&mut self) -> ParseResult<Vec<Node<Type>>> {
        self.expect_punctuation('<')?;
        let type_args = self.parse_one_or_more(',', |p| p.dart_type())?;
        self.expect_punctuation('>')?;
        Ok(type_args)
    }

    fn dart_arguments(&mut self) -> ParseResult<Args> {
        let mut unnamed_arguments = vec![];

        self.expect_punctuation('(')?;
        loop {
            if self.is_punctuation(')') {
                break;
            }
            if self.probe(|p| p.parse_ident().is_ok() && p.is_punctuation(':')) {
                break;
            }
            unnamed_arguments.push(self.dart_expr()?);
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
            let expr = self.dart_expr()?;
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

    fn dart_metadata(&mut self) -> ParseResult<Metadata> {
        let mut meta = vec![];
        while self.eat_punctuation('@') {
            let mut meta_item = MetadataItem {
                qualified: self.dart_qualified()?,
                arguments: None,
            };
            if self.is_punctuation('(') {
                meta_item.arguments = Some(self.dart_arguments()?);
            }

            meta.push(meta_item);
        }
        Ok(meta)
    }

    pub fn dart_expr(&mut self) -> ParseResult<Node<Expr>> {
        if self.eat_keyword("throw") {
            return Ok(Node::new(Expr::Throw(self.dart_expr()?)));
        }
        let mut expr = self.dart_conditional_expr()?;
        while let Some(cascade) = self.try(|p| p.dart_casacade()) {
            expr = Node::new(Expr::Cascade(expr, cascade));
        }
        if let Some(op) = self.try(|p| p.dart_assign_op()) {
            expr = Node::new(Expr::Binary(BinOp::Assign(op), expr, self.dart_expr()?));
        }
        Ok(expr)
    }

    pub fn dart_expr_no_cascade(&mut self) -> ParseResult<Node<Expr>> {
        if self.eat_keyword("throw") {
            return Ok(Node::new(Expr::Throw(self.dart_expr_no_cascade()?)));
        }
        let mut expr = self.dart_conditional_expr()?;
        if let Some(op) = self.try(|p| p.dart_assign_op()) {
            expr = Node::new(Expr::Binary(
                BinOp::Assign(op),
                expr,
                self.dart_expr_no_cascade()?,
            ));
        }
        Ok(expr)
    }

    fn dart_conditional_expr(&mut self) -> ParseResult<Node<Expr>> {
        let expr = self.dart_is_null_expression()?;
        if !self.is_punctuation2('?', '?') && self.eat_punctuation('?') {
            let expr2 = self.dart_expr_no_cascade()?;
            self.expect_punctuation(':')?;
            Ok(Node::new(
                Expr::Conditional(expr, expr2, self.dart_expr_no_cascade()?),
            ))
        } else {
            Ok(expr)
        }
    }

    fn dart_is_null_expression(&mut self) -> ParseResult<Node<Expr>> {
        let mut expr = self.dart_or_expr()?;
        while self.eat_bin_op(BinOp::Value(ValueBinOp::IfNull)) {
            expr = Node::new(Expr::Binary(
                BinOp::Value(ValueBinOp::IfNull),
                expr,
                self.dart_or_expr()?,
            ));
        }
        Ok(expr)
    }

    fn dart_or_expr(&mut self) -> ParseResult<Node<Expr>> {
        let mut expr = self.dart_and_expr()?;
        while self.eat_bin_op(BinOp::Bool(BoolBinOp::Or)) {
            expr = Node::new(Expr::Binary(
                BinOp::Bool(BoolBinOp::Or),
                expr,
                self.dart_and_expr()?,
            ));
        }
        Ok(expr)
    }

    fn dart_and_expr(&mut self) -> ParseResult<Node<Expr>> {
        let mut expr = self.dart_equality_expr()?;
        while self.eat_bin_op(BinOp::Bool(BoolBinOp::And)) {
            expr = Node::new(Expr::Binary(
                BinOp::Bool(BoolBinOp::And),
                expr,
                self.dart_equality_expr()?,
            ));
        }
        Ok(expr)
    }

    fn dart_equality_expr(&mut self) -> ParseResult<Node<Expr>> {
        let expr = self.dart_relational_expr()?;
        if self.eat_bin_op(BinOp::Bool(BoolBinOp::Eq)) {
            Ok(Node::new(Expr::Binary(
                BinOp::Bool(BoolBinOp::Eq),
                expr,
                self.dart_relational_expr()?,
            )))
        } else if self.eat_bin_op(BinOp::Bool(BoolBinOp::Ne)) {
            Ok(Node::new(Expr::Binary(
                BinOp::Bool(BoolBinOp::Ne),
                expr,
                self.dart_relational_expr()?,
            )))
        } else {
            Ok(expr)
        }
    }

    fn dart_relational_expr(&mut self) -> ParseResult<Node<Expr>> {
        let expr = self.dart_bit_or_expr()?;
        if self.eat_keyword("is") {
            if self.eat_punctuation('!') {
                Ok(Node::new(Expr::IsNot(expr, self.dart_type()?)))
            } else {
                Ok(Node::new(Expr::Is(expr, self.dart_type()?)))
            }
        } else if self.eat_keyword("as") {
            Ok(Node::new(Expr::As(expr, self.dart_type()?)))
        } else if self.eat_bin_op(BinOp::Bool(BoolBinOp::Ge)) {
            Ok(Node::new(Expr::Binary(
                BinOp::Bool(BoolBinOp::Ge),
                expr,
                self.dart_bit_or_expr()?,
            )))
        } else if self.eat_bin_op(BinOp::Bool(BoolBinOp::Gt)) {
            Ok(Node::new(Expr::Binary(
                BinOp::Bool(BoolBinOp::Gt),
                expr,
                self.dart_bit_or_expr()?,
            )))
        } else if self.eat_bin_op(BinOp::Bool(BoolBinOp::Le)) {
            Ok(Node::new(Expr::Binary(
                BinOp::Bool(BoolBinOp::Le),
                expr,
                self.dart_bit_or_expr()?,
            )))
        } else if self.eat_bin_op(BinOp::Bool(BoolBinOp::Lt)) {
            Ok(Node::new(Expr::Binary(
                BinOp::Bool(BoolBinOp::Lt),
                expr,
                self.dart_bit_or_expr()?,
            )))
        } else {
            Ok(expr)
        }
    }

    fn dart_bit_or_expr(&mut self) -> ParseResult<Node<Expr>> {
        let mut expr = self.dart_bit_xor_expr()?;
        while self.eat_bin_op(BinOp::Value(ValueBinOp::BitOr)) {
            expr = Node::new(Expr::Binary(
                BinOp::Value(ValueBinOp::BitOr),
                expr,
                self.dart_bit_xor_expr()?,
            ));
        }
        Ok(expr)
    }

    fn dart_bit_xor_expr(&mut self) -> ParseResult<Node<Expr>> {
        let mut expr = self.dart_bit_and_expr()?;
        while self.eat_bin_op(BinOp::Value(ValueBinOp::BitXor)) {
            expr = Node::new(Expr::Binary(
                BinOp::Value(ValueBinOp::BitXor),
                expr,
                self.dart_bit_and_expr()?,
            ));
        }
        Ok(expr)
    }

    fn dart_bit_and_expr(&mut self) -> ParseResult<Node<Expr>> {
        let mut expr = self.dart_shift_expr()?;
        while self.eat_bin_op(BinOp::Value(ValueBinOp::BitAnd)) {
            expr = Node::new(Expr::Binary(
                BinOp::Value(ValueBinOp::BitAnd),
                expr,
                self.dart_shift_expr()?,
            ));
        }
        Ok(expr)
    }

    fn dart_shift_expr(&mut self) -> ParseResult<Node<Expr>> {
        let mut expr = self.dart_add_expr()?;
        loop {
            if self.eat_bin_op(BinOp::Value(ValueBinOp::Lsh)) {
                expr = Node::new(Expr::Binary(
                    BinOp::Value(ValueBinOp::Lsh),
                    expr,
                    self.dart_add_expr()?,
                ));
            } else if self.eat_bin_op(BinOp::Value(ValueBinOp::Rsh)) {
                expr = Node::new(Expr::Binary(
                    BinOp::Value(ValueBinOp::Rsh),
                    expr,
                    self.dart_add_expr()?,
                ));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn dart_add_expr(&mut self) -> ParseResult<Node<Expr>> {
        let mut expr = self.dart_mult_expr()?;
        loop {
            if self.eat_bin_op(BinOp::Value(ValueBinOp::Add)) {
                expr = Node::new(Expr::Binary(
                    BinOp::Value(ValueBinOp::Add),
                    expr,
                    self.dart_mult_expr()?,
                ));
            } else if self.eat_bin_op(BinOp::Value(ValueBinOp::Sub)) {
                expr = Node::new(Expr::Binary(
                    BinOp::Value(ValueBinOp::Sub),
                    expr,
                    self.dart_mult_expr()?,
                ));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn dart_mult_expr(&mut self) -> ParseResult<Node<Expr>> {
        let mut expr = self.dart_unary_expr()?;
        loop {
            if self.eat_bin_op(BinOp::Value(ValueBinOp::Mul)) {
                expr = Node::new(Expr::Binary(
                    BinOp::Value(ValueBinOp::Mul),
                    expr,
                    self.dart_unary_expr()?,
                ));
            } else if self.eat_bin_op(BinOp::Value(ValueBinOp::Div)) {
                expr = Node::new(Expr::Binary(
                    BinOp::Value(ValueBinOp::Div),
                    expr,
                    self.dart_unary_expr()?,
                ));
            } else if self.eat_bin_op(BinOp::Value(ValueBinOp::Mod)) {
                expr = Node::new(Expr::Binary(
                    BinOp::Value(ValueBinOp::Mod),
                    expr,
                    self.dart_unary_expr()?,
                ));
            } else if self.eat_bin_op(BinOp::Value(ValueBinOp::TruncDiv)) {
                expr = Node::new(Expr::Binary(
                    BinOp::Value(ValueBinOp::TruncDiv),
                    expr,
                    self.dart_unary_expr()?,
                ));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn dart_unary_expr(&mut self) -> ParseResult<Node<Expr>> {
        if self.eat_punctuation('-') {
            Ok(Node::new(Expr::Unary(UnOp::Neg, self.dart_unary_expr()?)))
        } else if self.eat_punctuation('!') {
            Ok(Node::new(Expr::Unary(UnOp::Not, self.dart_unary_expr()?)))
        } else if self.eat_punctuation('~') {
            Ok(Node::new(
                Expr::Unary(UnOp::BitNot, self.dart_unary_expr()?),
            ))
        } else if self.eat_keyword("await") {
            Ok(Node::new(Expr::Unary(UnOp::Await, self.dart_unary_expr()?)))
        } else if self.eat_punctuation2('+', '+') {
            Ok(Node::new(Expr::Unary(UnOp::PreInc, self.dart_expr()?)))
        } else if self.eat_punctuation2('-', '-') {
            Ok(Node::new(Expr::Unary(UnOp::PreDec, self.dart_expr()?)))
        } else {
            Ok(self.dart_postfix_expr()?)
        }
    }

    fn dart_postfix_expr(&mut self) -> ParseResult<Node<Expr>> {
        let mut expr = self.dart_primary_expr()?;
        while let Some(suffix) = self.dart_suffix()? {
            expr = Node::new(Expr::Suffix(expr, suffix));
        }
        if self.eat_punctuation2('+', '+') {
            Ok(Node::new(Expr::Unary(UnOp::PostInc, expr)))
        } else if self.eat_punctuation2('-', '-') {
            Ok(Node::new(Expr::Unary(UnOp::PostDec, expr)))
        } else {
            Ok(expr)
        }
    }

    fn dart_casacade(&mut self) -> ParseResult<Cascade> {
        if self.probe(|p| p.eat_punctuation2('.', '.') && p.parse_ident().is_ok()) {
            self.bump();
        } else {
            self.expect_punctuation2('.', '.')?;
        }
        let mut suffixes = vec![];
        while let Some(suffix) = self.dart_suffix()? {
            suffixes.push(suffix);
        }
        let assign = self.try(|p| Ok((p.dart_assign_op()?, p.dart_expr_no_cascade()?)));
        Ok(Cascade { suffixes, assign })
    }

    fn dart_assign_op(&mut self) -> ParseResult<Option<ValueBinOp>> {
        for op in ValueBinOp::values() {
            if self.eat_bin_op(BinOp::Assign(Some(op))) {
                return Ok(Some(op));
            }
        }
        self.expect_bin_op(BinOp::Assign(None))?;
        Ok(None)
    }

    fn dart_suffix(&mut self) -> ParseResult<Option<Suffix>> {
        if self.eat_punctuation('[') {
            let expr = self.dart_expr()?;
            self.expect_punctuation(']')?;
            Ok(Some(Suffix::Index(expr)))
        } else if !self.is_punctuation2('.', '.') && self.eat_punctuation('.') {
            Ok(Some(Suffix::Field(self.parse_ident()?)))
        } else if self.eat_punctuation2('?', '.') {
            Ok(Some(Suffix::FieldIfNotNull(self.parse_ident()?)))
        } else if self.is_punctuation('(') {
            Ok(Some(Suffix::Call(vec![], self.dart_arguments()?)))
        } else if let Some(generics) = self.try(|p| p.dart_type_params()) {
            Ok(Some(Suffix::Call(generics, self.dart_arguments()?)))
        } else {
            Ok(None)
        }
    }

    fn dart_primary_expr(&mut self) -> ParseResult<Node<Expr>> {
        if self.is_punctuation('(') {
            let args = self.try(|p| {
                let args = p.dart_fn_args(Node::new(Type::Infer))?;
                if p.is_punctuation('{') || p.is_punctuation2('=', '>') {
                    Ok(args)
                } else {
                    expected!(p, Punctuation2('=', '>'));
                }
            });
            if let Some(args) = args {
                return Ok(Node::new(Expr::Closure(args, self.dart_fn_body(false)?)));
            }
            self.expect_punctuation('(')?;
            let expr = self.dart_expr()?;
            self.expect_punctuation(')')?;
            return Ok(Node::new(Expr::Paren(expr)));
        }
        let const_ = self.eat_keyword("const");
        let mut generics = if self.is_punctuation('<') {
            self.dart_type_params()?
        } else {
            vec![]
        };
        if self.eat_punctuation('[') {
            let mut elements = vec![];
            loop {
                if self.is_punctuation(']') {
                    break;
                }
                elements.push(self.dart_expr()?);
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
            return Ok(Node::new(Expr::List {
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
                let k = self.dart_expr()?;
                self.expect_punctuation(':')?;
                kv.push((k, self.dart_expr()?));
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
            return Ok(Node::new(Expr::Map { const_, kv_ty, kv }));
        }
        if const_ || self.eat_keyword("new") {
            return Ok(Node::new(Expr::New {
                const_,
                path: self.dart_qualified()?,
                args: self.dart_arguments()?,
            }));
        }
        if let Some(number) = self.try(|p| p.dart_number_literal()) {
            return Ok(number);
        }
        if let Some(str_lit) = self.try(|p| p.dart_string_literal()) {
            let mut strings = vec![str_lit];
            while let Some(str_lit) = self.try(|p| p.dart_string_literal()) {
                strings.push(str_lit);
            }
            return Ok(Node::new(Expr::String(strings)));
        }
        if self.eat_punctuation('#') {
            return Ok(Node::new(
                Expr::Symbol(SymbolLiteral::Path(vec![self.parse_ident()?])),
            ));
        }
        if let Ok(ident) = self.parse_ident() {
            return Ok(Node::new(Expr::Identifier(ident)));
        }
        expected!(self, Expr);
    }

    fn dart_number_literal(&mut self) -> ParseResult<Node<Expr>> {
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
        Ok(Node::new(Expr::Number(Symbol::intern(&number))))
    }

    fn dart_fn_args(&mut self, return_type: Node<Type>) -> ParseResult<FnSig> {
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
                    sig.optional.push(self.dart_arg_def(&['='])?);
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
                    sig.optional.push(self.dart_arg_def(&[':', '='])?);
                    if !self.eat_punctuation(',') {
                        break;
                    }
                }
                self.expect_punctuation('}')?;
                break;
            }
            sig.required.push(self.dart_arg_def(&[])?);
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

    fn dart_arg_def(&mut self, default_separators: &[char]) -> ParseResult<ArgDef> {
        let metadata = self.dart_metadata()?;
        let covariant = self.eat_keyword("covariant");
        let mut ty = self.dart_var_type(false)?;
        let mut field = false;
        if self.eat_keyword("this") {
            self.expect_punctuation('.')?;
            field = true;
        }
        let name = self.parse_ident()?;
        if self.is_punctuation('(') {
            ty.ty = Node::new(Type::FunctionOld(self.dart_fn_args(ty.ty)?));
        }
        let mut init = None;
        let mut default_uses_eq = false;
        for &separator in default_separators {
            if self.eat_punctuation(separator) {
                init = Some(self.dart_expr()?);
                if separator == '=' {
                    default_uses_eq = true;
                }
                break;
            }
        }
        Ok(ArgDef {
            metadata,
            covariant,
            ty,
            field,
            default_uses_eq,
            var: Node::new(VarDef { name, init }),
        })
    }

    fn dart_var_type(&mut self, requires_var: bool) -> ParseResult<VarType> {
        let mut fcv = if self.eat_keyword("const") {
            Some(FinalConstVar::Const)
        } else if self.eat_keyword("final") {
            Some(FinalConstVar::Final)
        } else {
            None
        };
        let ty = self.try(|p| {
            let ty = p.dart_type()?;

            if p.probe(|p| p.parse_ident().is_ok()) {
                Ok(ty)
            } else {
                expected!(p, Ident);
            }
        }).ok_or(())
            .or_else(|_| -> ParseResult<_> {
                if fcv.is_none() {
                    if requires_var {
                        self.expect_keyword("var")?;
                        fcv = Some(FinalConstVar::Var);
                    } else {
                        if self.eat_keyword("var") {
                            fcv = Some(FinalConstVar::Var);
                        }
                    }
                }
                Ok(Node::new(Type::Infer))
            })?;

        Ok(VarType { fcv, ty })
    }

    pub fn dart_block(&mut self) -> ParseResult<Node<Statement>> {
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
                statements.push(self.dart_statement()?);
            }
        }
        self.expect_punctuation('}')?;
        Ok(Node::new(Statement::Block(statements)))
    }

    fn dart_catch_part(&mut self) -> ParseResult<CatchPart> {
        self.expect_punctuation('(')?;
        let exception = Node::new(VarDef {
            name: self.parse_ident()?,
            init: None,
        });
        let trace = if self.eat_punctuation(',') {
            Some(Node::new(VarDef {
                name: self.parse_ident()?,
                init: None,
            }))
        } else {
            None
        };
        self.expect_punctuation(')')?;
        Ok(CatchPart { exception, trace })
    }

    fn dart_statement(&mut self) -> ParseResult<Node<Statement>> {
        if let Some((label, _)) = self.try(|p| Ok((p.parse_ident()?, p.expect_punctuation(':')?))) {
            return Ok(Node::new(
                Statement::Labelled(label, self.dart_statement()?),
            ));
        }
        if self.is_punctuation('{') {
            return Ok(self.dart_block()?);
        }
        if let Some((await, _)) =
            self.try(|p| Ok((p.eat_keyword("await"), p.expect_keyword("for")?)))
        {
            self.expect_punctuation('(')?;
            let for_loop = self.try(|p| {
                let var_type = p.try(|p| p.dart_var_type(true));
                let name = p.parse_ident()?;
                p.expect_keyword("in")?;
                let expr = p.dart_expr()?;
                if let Some(var_type) = var_type {
                    Ok(ForLoop::InVar(
                        var_type,
                        Node::new(VarDef { name, init: None }),
                        expr,
                    ))
                } else {
                    Ok(ForLoop::In(name, expr))
                }
            }).ok_or(())
                .or_else(|_| -> ParseResult<_> {
                    let statement = self.dart_statement()?;
                    let cond = if self.is_punctuation(';') {
                        None
                    } else {
                        Some(self.dart_expr()?)
                    };
                    self.expect_punctuation(';')?;
                    let exprs = if self.is_punctuation(')') {
                        vec![]
                    } else {
                        self.parse_one_or_more(',', |p| p.dart_expr())?
                    };
                    Ok(ForLoop::CLike(statement, cond, exprs))
                })?;
            self.expect_punctuation(')')?;
            return Ok(Node::new(
                Statement::For(await, for_loop, self.dart_statement()?),
            ));
        }
        if self.eat_keyword("while") {
            self.expect_punctuation('(')?;
            let expr = self.dart_expr()?;
            self.expect_punctuation(')')?;
            return Ok(Node::new(Statement::While(expr, self.dart_statement()?)));
        }
        if self.eat_keyword("do") {
            let statement = self.dart_statement()?;
            self.expect_keyword("while")?;
            self.expect_punctuation('(')?;
            let expr = self.dart_expr()?;
            self.expect_punctuation(')')?;
            self.expect_punctuation(';')?;
            return Ok(Node::new(Statement::DoWhile(statement, expr)));
        }
        if self.eat_keyword("switch") {
            self.expect_punctuation('(')?;
            let expr = self.dart_expr()?;
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
                    Some(self.dart_expr()?)
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
                    statements.push(self.dart_statement()?);
                }

                sc.push(SwitchCase {
                    labels,
                    value,
                    statements,
                });
            }
            self.expect_punctuation('}')?;
            return Ok(Node::new(Statement::Switch(expr, sc)));
        }
        if self.eat_keyword("if") {
            self.expect_punctuation('(')?;
            let expr = self.dart_expr()?;
            self.expect_punctuation(')')?;
            let statement = self.dart_statement()?;
            let else_statement = if self.eat_keyword("else") {
                Some(self.dart_statement()?)
            } else {
                None
            };
            return Ok(Node::new(Statement::If(expr, statement, else_statement)));
        }
        if self.eat_keyword("rethrow") {
            return Ok(Node::new(Statement::Rethrow));
        }
        if self.eat_keyword("try") {
            let block = self.dart_block()?;
            let mut parts = vec![];
            loop {
                if self.eat_keyword("on") {
                    let on = Some(self.dart_type()?);
                    let catch = if self.eat_keyword("catch") {
                        Some(self.dart_catch_part()?)
                    } else {
                        None
                    };
                    parts.push(TryPart {
                        on,
                        catch,
                        block: self.dart_block()?,
                    });
                } else if self.eat_keyword("catch") {
                    let catch = Some(self.dart_catch_part()?);
                    parts.push(TryPart {
                        on: None,
                        catch,
                        block: self.dart_block()?,
                    });
                } else if self.eat_keyword("finally") {
                    parts.push(TryPart {
                        on: None,
                        catch: None,
                        block: self.dart_block()?,
                    });
                    break;
                } else {
                    break;
                }
            }
            return Ok(Node::new(Statement::Try(block, parts)));
        }
        if self.eat_keyword("break") {
            if self.is_punctuation(';') {
                self.expect_punctuation(';')?;
                return Ok(Node::new(Statement::Break(None)));
            } else {
                let ident = self.parse_ident()?;
                self.expect_punctuation(';')?;
                return Ok(Node::new(Statement::Break(Some(ident))));
            }
        }
        if self.eat_keyword("continue") {
            if self.is_punctuation(';') {
                self.expect_punctuation(';')?;
                return Ok(Node::new(Statement::Continue(None)));
            } else {
                let ident = self.parse_ident()?;
                self.expect_punctuation(';')?;
                return Ok(Node::new(Statement::Continue(Some(ident))));
            }
        }
        if self.eat_keyword("return") {
            if self.eat_punctuation(';') {
                return Ok(Node::new(Statement::Return(None)));
            }
            let expr = self.dart_expr()?;
            self.expect_punctuation(';')?;
            return Ok(Node::new(Statement::Return(Some(expr))));
        }
        if self.eat_keyword("yield") {
            if self.eat_punctuation('*') {
                let expr = self.dart_expr()?;
                self.expect_punctuation(';')?;
                return Ok(Node::new(Statement::YieldEach(expr)));
            } else {
                let expr = self.dart_expr()?;
                self.expect_punctuation(';')?;
                return Ok(Node::new(Statement::Yield(expr)));
            }
        }
        if self.eat_keyword("assert") {
            let args = self.dart_arguments()?;
            self.expect_punctuation(';')?;
            return Ok(Node::new(Statement::Assert(args)));
        }
        let var_stmt = self.try(|p| {
            let var_type = p.dart_var_type(true)?;
            let vars = p.parse_one_or_more(',', |p| p.dart_name_and_initializer())?;
            p.expect_punctuation(';')?;
            Ok(Node::new(Statement::Vars(var_type, vars)))
        });
        if let Some(var_stmt) = var_stmt {
            return Ok(var_stmt);
        }
        if let Some(function) = self.try(|p| p.dart_function(true)) {
            return Ok(Node::new(Statement::Function(function)));
        }
        if self.eat_punctuation(';') {
            return Ok(Node::new(Statement::Expression(None)));
        }
        let expr = self.dart_expr()?;
        self.expect_punctuation(';')?;
        Ok(Node::new(Statement::Expression(Some(expr))))
    }

    fn dart_fn_body(&mut self, requires_semi: bool) -> ParseResult<FnBody> {
        if self.eat_keyword("native") {
            let native_thing = if !self.is_punctuation(';') {
                self.dart_string_literal().ok()
            } else {
                None
            };
            self.expect_punctuation(';')?;
            return Ok(FnBody::Native(native_thing));
        }
        if self.eat_punctuation2('=', '>') {
            let expr = self.dart_expr()?;
            if requires_semi {
                self.expect_punctuation(';')?;
            }
            Ok(FnBody::Arrow(expr))
        } else {
            Ok(FnBody::Block(self.dart_block()?))
        }
    }

    fn dart_type_param_def(&mut self) -> ParseResult<Node<TypeParameter>> {
        let metadata = self.dart_metadata()?;
        let name = self.parse_ident()?;
        let extends = if self.eat_keyword("extends") {
            Some(self.dart_qualified()?)
        } else {
            None
        };
        Ok(Node::new(TypeParameter {
            metadata,
            name,
            extends,
        }))
    }

    fn dart_name_and_initializer(&mut self) -> ParseResult<Node<VarDef>> {
        let name = self.parse_ident()?;
        let init = if self.eat_punctuation('=') {
            Some(self.dart_expr()?)
        } else {
            None
        };
        Ok(Node::new(VarDef { name, init }))
    }

    fn dart_constructor_initializer(&mut self) -> ParseResult<ConstructorInitializer> {
        if self.eat_keyword("assert") {
            let args = self.dart_arguments()?;
            return Ok(ConstructorInitializer::Assert(args));
        }
        if self.eat_keyword("super") {
            let name = if self.eat_punctuation('.') {
                Some(self.parse_ident()?)
            } else {
                None
            };
            let args = self.dart_arguments()?;
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
                let args = self.dart_arguments()?;
                return Ok(ConstructorInitializer::This(name, args));
            }
        } else {
            (false, self.parse_ident()?)
        };

        self.expect_punctuation('=')?;
        let mut expr = self.dart_conditional_expr()?;
        while let Some(cascade) = self.try(|p| p.dart_casacade()) {
            expr = Node::new(Expr::Cascade(expr, cascade));
        }
        return Ok(ConstructorInitializer::Field(this, field, expr));
    }

    fn dart_overloaded_op(&mut self) -> ParseResult<OverloadedOp> {
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
                '~' => if self.is_punctuation2('~', '/') {
                    self.bump();
                    OverloadedOp::Value(ValueBinOp::TruncDiv)
                } else {
                    OverloadedOp::BitNot
                },
                '+' => OverloadedOp::Value(ValueBinOp::Add),
                '-' => OverloadedOp::Value(ValueBinOp::Sub),
                '>' => if self.is_punctuation2('>', '>') {
                    self.bump();
                    OverloadedOp::Value(ValueBinOp::Rsh)
                } else if self.is_punctuation2('>', '=') {
                    self.bump();
                    OverloadedOp::Bool(BoolBinOp::Ge)
                } else {
                    OverloadedOp::Bool(BoolBinOp::Gt)
                },
                '<' => if self.is_punctuation2('<', '<') {
                    self.bump();
                    OverloadedOp::Value(ValueBinOp::Lsh)
                } else if self.is_punctuation2('<', '=') {
                    self.bump();
                    OverloadedOp::Bool(BoolBinOp::Le)
                } else {
                    OverloadedOp::Bool(BoolBinOp::Lt)
                },
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

    fn dart_fn_name(&mut self) -> ParseResult<FnName> {
        if let Some((_, name)) = self.try(|p| Ok((p.expect_keyword("get")?, p.parse_ident()?))) {
            return Ok(FnName::Getter(name));
        }
        if let Some((_, name)) = self.try(|p| Ok((p.expect_keyword("set")?, p.parse_ident()?))) {
            return Ok(FnName::Setter(name));
        }
        if self.eat_keyword("operator") {
            return Ok(FnName::Operator(self.dart_overloaded_op()?));
        }
        Ok(FnName::Regular(self.parse_ident()?))
    }

    fn dart_function(&mut self, requires_body: bool) -> ParseResult<Node<Function>> {
        let return_type_and_name = if self.is_keyword("get") || self.is_keyword("set") {
            None
        } else {
            self.try(|p| Ok((p.dart_type()?, p.dart_fn_name()?)))
        };
        let (return_type, name) = return_type_and_name.ok_or(()).or_else(
            |_| -> ParseResult<_> { Ok((Node::new(Type::Infer), self.dart_fn_name()?)) },
        )?;
        let mut generics = vec![];
        if self.eat_punctuation('<') {
            generics = self.parse_one_or_more(',', |p| p.dart_type_param_def())?;
            self.expect_punctuation('>')?;
        }
        let sig = {
            if let FnName::Getter(_) = name {
                let mut sig = FnSig::default();
                sig.return_type = return_type;
                if self.eat_keyword("async") {
                    sig.async = true;
                    sig.generator = self.eat_punctuation('*');
                } else if self.eat_keyword("sync") {
                    self.expect_punctuation('*')?;
                    sig.generator = true;
                }
                sig
            } else {
                self.dart_fn_args(return_type)?
            }
        };
        Ok(Node::new(Function {
            name,
            generics,
            sig,
            body: if !requires_body && self.eat_punctuation(';') {
                None
            } else {
                Some(self.dart_fn_body(true)?)
            },
        }))
    }

    pub fn dart_class_member(&mut self, class_name: Symbol) -> ParseResult<Node<ClassMember>> {
        let metadata = self.dart_metadata()?;
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
            p.parse_ident().ok() == Some(class_name) && if p.eat_punctuation('.') {
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
            let sig = self.dart_fn_args(Node::new(Type::Infer))?;
            if !self.is_punctuation2('=', '>') && self.eat_punctuation('=') {
                let path = self.dart_qualified()?;
                self.expect_punctuation(';')?;
                return Ok(Node::new(ClassMember::Redirect {
                    metadata,
                    method_qualifiers,
                    name,
                    sig,
                    path,
                }));
            }
            let initializers = if self.eat_punctuation(':') {
                self.parse_one_or_more(',', |p| p.dart_constructor_initializer())?
            } else {
                vec![]
            };
            let function_body = if self.eat_punctuation(';') {
                None
            } else {
                Some(self.dart_fn_body(true)?)
            };
            return Ok(Node::new(ClassMember::Constructor {
                metadata,
                method_qualifiers,
                name,
                sig,
                initializers,
                function_body,
            }));
        }

        let mut static_ = false;
        let mut fcv = None;
        for &mq in &method_qualifiers {
            match mq {
                MethodQualifiers::Static => static_ = true,
                MethodQualifiers::Final => fcv = Some(FinalConstVar::Final),
                MethodQualifiers::Const => fcv = Some(FinalConstVar::Const),
                _ => break,
            }
        }

        let fields = self.try(|p| {
            let ty = p.try(|p| {
                let ty = p.dart_type()?;
                if p.probe(|p| p.parse_ident().is_ok()) {
                    Ok(ty)
                } else {
                    expected!(p, Ident);
                }
            }).ok_or(())
                .or_else(|_| -> ParseResult<_> {
                    if fcv.is_none() {
                        p.expect_keyword("var")?;
                        fcv = Some(FinalConstVar::Var);
                    }
                    Ok(Node::new(Type::Infer))
                })?;
            let initializers = p.parse_one_or_more(',', |p| p.dart_name_and_initializer())?;
            p.expect_punctuation(';')?;
            Ok((VarType { fcv, ty }, initializers))
        });

        if let Some((var_type, initializers)) = fields {
            return Ok(Node::new(ClassMember::Fields {
                metadata,
                static_,
                var_type,
                initializers,
            }));
        }
        let function = self.dart_function(false)?;
        Ok(Node::new(
            ClassMember::Method(metadata, method_qualifiers, function),
        ))
    }

    pub fn dart_item(&mut self) -> ParseResult<Node<Item>> {
        let metadata = self.dart_metadata()?;

        if self.eat_keyword("library") {
            let path = self.parse_one_or_more('.', |p| p.parse_ident())?;
            self.expect_punctuation(';')?;
            return Ok(Node::new(Item::LibraryName { metadata, path }));
        }

        if self.eat_keyword("import") {
            let uri = self.dart_string_literal()?;
            let deferred = if self.eat_keyword("deferred") {
                true
            } else {
                false
            };
            let alias = if self.eat_keyword("as") {
                Some(self.parse_ident()?)
            } else {
                None
            };
            let filters = self.dart_import_filters()?;
            self.expect_punctuation(';')?;
            return Ok(Node::new(Item::Import(
                metadata,
                Import {
                    uri,
                    deferred,
                    alias,
                    filters,
                },
            )));
        }

        if !self.probe(|p| {
            p.eat_keyword("export");
            p.is_punctuation('(')
        }) && self.eat_keyword("export")
        {
            let uri = self.dart_string_literal()?;
            let import_filters = self.dart_import_filters()?;
            self.expect_punctuation(';')?;
            return Ok(Node::new(Item::Export(metadata, uri, import_filters)));
        }

        if self.eat_keyword("part") {
            if self.eat_keyword("of") {
                let path = self.parse_one_or_more('.', |p| p.parse_ident())?;
                self.expect_punctuation(';')?;
                return Ok(Node::new(Item::PartOf { metadata, path }));
            }
            let uri = self.dart_string_literal()?;
            self.expect_punctuation(';')?;
            let mut path = self.path.parent().unwrap().to_path_buf();
            path.extend(uri.get_simple_string().split('/'));
            return Ok(Node::new(Item::Part {
                metadata,
                uri,
                module: Module::load(&path),
            }));
        }

        let abstract_ = self.eat_keyword("abstract");
        if self.eat_keyword("class") {
            let class_name = self.parse_ident()?;
            let mut generics = vec![];
            if self.eat_punctuation('<') {
                generics = self.parse_one_or_more(',', |p| p.dart_type_param_def())?;
                self.expect_punctuation('>')?;
            }
            let (superclass, mixins) = if self.eat_keyword("extends") {
                let superclass = Some(self.dart_qualified()?);
                let mixins = if self.eat_keyword("with") {
                    self.parse_one_or_more(',', |p| p.dart_qualified())?
                } else {
                    vec![]
                };
                (superclass, mixins)
            } else {
                (None, vec![])
            };
            let interfaces = if self.eat_keyword("implements") {
                self.parse_one_or_more(',', |p| p.dart_qualified())?
            } else {
                vec![]
            };
            if self.eat_punctuation('{') {
                let mut members = vec![];
                loop {
                    if self.eat_punctuation('}') {
                        break;
                    }
                    members.push(self.dart_class_member(class_name)?);
                }
                return Ok(Node::new(Item::Class {
                    metadata,
                    abstract_,
                    name: class_name,
                    generics,
                    superclass,
                    mixins,
                    interfaces,
                    members,
                }));
            } else {
                self.expect_punctuation('=')?;
                let mut mixins = vec![self.dart_qualified()?];
                self.expect_keyword("with")?;
                mixins.extend(self.parse_one_or_more(',', |p| p.dart_qualified())?);
                let interfaces = if self.eat_keyword("implements") {
                    self.parse_one_or_more(',', |p| p.dart_qualified())?
                } else {
                    vec![]
                };
                self.expect_punctuation(';')?;
                return Ok(Node::new(Item::MixinClass {
                    metadata,
                    abstract_,
                    name: class_name,
                    generics,
                    mixins,
                    interfaces,
                }));
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
            return Ok(Node::new(Item::Enum {
                metadata,
                name: enum_name,
                values,
            }));
        }

        if self.eat_keyword("typedef") {
            let (return_type, name) = self.try(|p| Ok((p.dart_type()?, p.parse_ident()?)))
                .ok_or(())
                .or_else(
                    |_| -> ParseResult<_> { Ok((Node::new(Type::Infer), self.parse_ident()?)) },
                )?;
            let mut generics = vec![];
            if self.eat_punctuation('<') {
                generics = self.parse_one_or_more(',', |p| p.dart_type_param_def())?;
                self.expect_punctuation('>')?;
            }
            let sig = self.dart_fn_args(return_type)?;
            let ty = Node::new(Type::FunctionOld(sig));
            self.expect_punctuation(';')?;
            return Ok(Node::new(Item::TypeAlias {
                metadata,
                name,
                generics,
                ty,
            }));
        }

        if let Some((var_type, vars)) = self.try(|p| {
            let var_type = p.dart_var_type(true)?;
            let vars = p.parse_one_or_more(',', |p| p.dart_name_and_initializer())?;
            p.expect_punctuation(';')?;
            Ok((var_type, vars))
        }) {
            return Ok(Node::new(Item::Vars(metadata, var_type, vars)));
        }

        let external = self.eat_keyword("external");
        Ok(Node::new(Item::Function {
            metadata,
            external,
            function: self.dart_function(false)?,
        }))
    }

    fn dart_import_filters(&mut self) -> ParseResult<Vec<ImportFilter>> {
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

    fn dart_string_literal(&mut self) -> ParseResult<StringLiteral> {
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
                    let expr = self.dart_expr()?;
                    match self.cur {
                        Some(Token::StringLiteral {
                            contents,
                            raw,
                            triple,
                            quote,
                            interpolation_before: true,
                            interpolation_after,
                        }) if (raw, triple, quote) == (lit.raw, lit.triple, lit.quote) =>
                        {
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

    pub fn dart_module(mut self) -> ParseResult<Node<Module>> {
        let mut items = vec![];
        while !self.out_of_tokens() {
            items.push(self.dart_item()?);
        }
        let mut has_error = false;
        for item in &items {
            if let Item::Part { ref module, .. } = **item {
                has_error |= module.has_error;
            }
        }
        Ok(Node::new(Module {
            path: self.path,
            items,
            has_error,
        }))
    }
}
