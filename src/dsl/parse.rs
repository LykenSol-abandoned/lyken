#![allow(unused_doc_comment)]

use dsl::ast::*;
use dart::lex::Token;
use syntax::symbol::Symbol;
use syntax::codemap::Span;

#[derive(Clone)]
pub struct Parser<I> {
    tokens: I,
    cur: Option<Token>,
    cur_span: Span,
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
    Keyword(&'static str),
    Ident,
    Item,
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
    pub fn new(mut tokens: I) -> Self {
        Parser {
            cur: tokens.next().map(|(_, t)| t),
            tokens: tokens,
            cur_span: Span::default(),
        }
    }

    fn out_of_tokens(&self) -> bool {
        self.cur.is_none()
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

    fn expect_keyword(&mut self, s: &'static str) -> ParseResult<()> {
        if !self.is_keyword(s) {
            expected!(self, Keyword(s));
        }
        self.bump();
        Ok(())
    }

    fn is_keyword(&self, s: &str) -> bool {
        if let Some(token) = self.cur {
            token.as_ident().map_or(false, |x| x == s)
        } else {
            false
        }
    }

    fn eat_keyword(&mut self, s: &'static str) -> bool {
        self.expect_keyword(s).is_ok()
    }

    fn next_token(&mut self) -> Option<Token> {
        if let Some(token) = self.tokens.next().map(|(_, t)| t) {
            Some(token)
        } else {
            None
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
        self.bump();
        Ok(ident)
    }

    fn parse_dart(&mut self) -> ParseResult<Vec<Token>> {
        let mut depth = 0;
        let mut code = vec![];
        while let Some(token) = self.cur {
            if let Token::Punctuation(c) = token {
                match c {
                    ',' if depth == 0 => break,
                    '{' | '[' | '(' => depth += 1,
                    '}' | ']' | ')' => {
                        if depth == 0 {
                            break;
                        }
                        depth -= 1;
                    }
                    _ => {}
                }
            }
            code.push(token);
            self.cur = self.next_token();
        }
        Ok(code)
    }

    fn parse_type(&mut self) -> ParseResult<Type> {
        Ok(Type::Dart(self.parse_dart()?))
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        if let Some(Token::Identifier(_)) = self.cur {
            if self.probe(|p| {
                p.bump();
                p.expect_punctuation('{')
            }).is_ok()
            {
                return Ok(Expr::Instance(self.parse_instance()?));
            }
        }
        if self.eat_punctuation('[') {
            let mut exprs = vec![];
            while !self.is_punctuation(']') {
                exprs.push(self.parse_expr()?);
                if !self.eat_punctuation(',') {
                    break;
                }
            }
            self.expect_punctuation(']')?;
            return Ok(Expr::Array(exprs));
        }
        Ok(Expr::Dart(self.parse_dart()?))
    }

    fn parse_field(&mut self) -> ParseResult<Field> {
        let name = self.parse_ident()?;
        self.expect_punctuation(':')?;
        Ok(Field {
            name: name,
            value: self.parse_expr()?,
        })
    }

    fn parse_fields(&mut self) -> ParseResult<Vec<Field>> {
        let mut fields = vec![];
        while !self.out_of_tokens() {
            if self.is_punctuation('}') {
                break;
            }
            fields.push(self.parse_field()?);
            if !self.eat_punctuation(',') {
                break;
            }
        }
        Ok(fields)
    }

    fn parse_field_def(&mut self) -> ParseResult<FieldDef> {
        let name = self.parse_ident()?;
        let mut fd = FieldDef {
            name: name,
            ty: None,
            default: None,
        };
        if self.eat_punctuation(':') {
            fd.ty = Some(self.parse_type()?);
        }
        if self.eat_punctuation('=') {
            fd.default = Some(self.parse_expr()?);
        }
        Ok(fd)
    }

    fn parse_field_defs(&mut self) -> ParseResult<Vec<FieldDef>> {
        let mut fields = vec![];
        while !self.out_of_tokens() {
            if self.is_punctuation('}') {
                break;
            } else {
                fields.push(self.parse_field_def()?);
            }
            self.expect_punctuation(',')?;
        }
        Ok(fields)
    }

    fn parse_instance(&mut self) -> ParseResult<Instance> {
        let name = self.parse_ident()?;
        self.expect_punctuation('{')?;
        let fields = self.parse_fields()?;
        self.expect_punctuation('}')?;
        Ok(Instance { name, fields })
    }


    fn parse_item(&mut self) -> ParseResult<Item> {
        if self.eat_keyword("def") {
            let name = self.parse_ident()?;
            self.expect_punctuation('{')?;
            let fields = self.parse_field_defs()?;
            let instance = self.parse_instance()?;
            self.expect_punctuation('}')?;
            Ok(Item::ComponentDef(name, fields, instance))
        } else if self.eat_keyword("dart") {
            self.expect_punctuation('{')?;
            let dart = self.parse_dart()?;
            self.expect_punctuation('}')?;
            Ok(Item::Dart(dart))
        } else {
            expected!(self, Item);
        }
    }

    pub fn parse_items(&mut self) -> ParseResult<Vec<Item>> {
        let mut items = vec![];
        while !self.out_of_tokens() {
            items.push(self.parse_item()?);
        }
        if !items.is_empty() {
            Ok(items)
        } else {
            expected!(self, Item)
        }
    }
}
