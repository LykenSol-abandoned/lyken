#![allow(unused_doc_comment)]

use dsl::ast::*;
use dart::lex::Token;
use syntax::codemap::Span;
use dart::parse::{ParseResult, Parser};

impl<I: Clone + Iterator<Item = (Span, Token)>> Parser<I> {
    fn dsl_type(&mut self) -> ParseResult<Type> {
        Ok(Type::Dart(self.dart_type()?))
    }

    fn dsl_expr(&mut self) -> ParseResult<Expr> {
        if self.probe(|p| p.parse_ident().is_ok() && p.eat_punctuation('{')) {
            return Ok(Expr::Instance(self.dsl_instance()?));
        }
        if self.eat_punctuation('[') {
            let mut exprs = vec![];
            while !self.is_punctuation(']') {
                exprs.push(self.dsl_expr()?);
                if !self.eat_punctuation(',') {
                    break;
                }
            }
            self.expect_punctuation(']')?;
            return Ok(Expr::Array(exprs));
        }
        Ok(Expr::Dart(self.dart_expr()?))
    }

    fn dsl_field(&mut self) -> ParseResult<Field> {
        let name = self.parse_ident()?;
        self.expect_punctuation(':')?;
        Ok(Field {
            name: name,
            value: self.dsl_expr()?,
        })
    }

    fn dsl_fields(&mut self) -> ParseResult<Vec<Field>> {
        let mut fields = vec![];
        while !self.out_of_tokens() {
            if self.is_punctuation('}') {
                break;
            }
            fields.push(self.dsl_field()?);
            if !self.eat_punctuation(',') {
                break;
            }
        }
        Ok(fields)
    }

    fn dsl_field_def(&mut self) -> ParseResult<FieldDef> {
        let name = self.parse_ident()?;
        let mut fd = FieldDef {
            name: name,
            ty: None,
            default: None,
        };
        if self.eat_punctuation(':') {
            fd.ty = Some(self.dsl_type()?);
            if self.eat_punctuation('=') {
                fd.default = Some(self.dsl_expr()?);
            }
        } else {
            self.expect_punctuation('=')?;
            fd.default = Some(self.dsl_expr()?);
        }
        Ok(fd)
    }

    fn dsl_field_defs(&mut self) -> ParseResult<Vec<FieldDef>> {
        let mut fields = vec![];
        while let Some(field) = self.try(|p| p.dsl_field_def()) {
            fields.push(field);
            self.expect_punctuation(',')?;
        }
        Ok(fields)
    }

    fn dsl_instance(&mut self) -> ParseResult<Instance> {
        let name = self.parse_ident()?;
        self.expect_punctuation('{')?;
        let fields = self.dsl_fields()?;
        self.expect_punctuation('}')?;
        Ok(Instance { name, fields })
    }


    fn dsl_item(&mut self) -> ParseResult<Item> {
        if self.eat_keyword("def") {
            let name = self.parse_ident()?;
            self.expect_punctuation('{')?;
            let fields = self.dsl_field_defs()?;
            let instance = self.dsl_instance()?;
            self.expect_punctuation('}')?;
            return Ok(Item::ComponentDef(name, fields, instance));
        }
        Ok(Item::Dart(self.dart_item()?))
    }

    pub fn dsl_items(&mut self) -> ParseResult<Vec<Item>> {
        let mut items = vec![];
        while !self.out_of_tokens() {
            items.push(self.dsl_item()?);
        }
        Ok(items)
    }
}
