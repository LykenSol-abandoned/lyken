#![allow(unused_doc_comment)]

use dart;
use dsl::ast::*;
use dart::parse::{ParseResult, Parser};
use node::Node;

impl<'a> Parser<'a> {
    fn dsl_type(&mut self) -> ParseResult<Node<Type>> {
        Ok(Node::new(Type::Dart(self.dart_type()?)))
    }

    fn dsl_expr(&mut self) -> ParseResult<Node<Expr>> {
        if self.probe(|p| {
            p.parse_ident().is_ok() && (p.eat_punctuation('(') || p.eat_punctuation('{'))
        }) {
            let name = self.parse_ident()?;
            let unnamed = if self.eat_punctuation('(') {
                let unnamed = self.parse_one_or_more(',', |p| p.dsl_expr())?;
                self.expect_punctuation(')')?;
                unnamed
            } else {
                vec![]
            };
            let has_config = if !unnamed.is_empty() {
                self.eat_punctuation('{')
            } else {
                self.expect_punctuation('{')?;
                true
            };
            let mut config = vec![];
            if has_config {
                while !self.out_of_tokens() {
                    if self.is_punctuation('}') {
                        break;
                    }
                    config.push(self.dsl_config()?);
                    if !self.eat_punctuation(',') {
                        break;
                    }
                }
                self.expect_punctuation('}')?;
            }

            return Ok(Node::new(Expr::Instance {
                path: dart::ast::Qualified::one(name, vec![]),
                unnamed,
                config,
            }));
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
            return Ok(Node::new(Expr::Array(exprs)));
        }
        Ok(Node::new(Expr::Dart(self.dart_expr()?)))
    }

    fn dsl_config(&mut self) -> ParseResult<Config> {
        if self.eat_keyword("on") {
            Ok(Config::EventHandler {
                name: self.parse_ident()?,
                block: self.dart_block()?,
            })
        } else {
            let name = self.parse_ident()?;
            self.expect_punctuation(':')?;
            Ok(Config::Field {
                name,
                value: self.dsl_expr()?,
            })
        }
    }

    fn dsl_field_def(&mut self) -> ParseResult<Node<FieldDef>> {
        let mutable = self.eat_keyword("mut");
        let name = self.parse_ident()?;
        let mut fd = FieldDef {
            mutable,
            name,
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
        Ok(Node::new(fd))
    }

    fn dsl_field_defs(&mut self) -> ParseResult<Vec<Node<FieldDef>>> {
        let mut fields = vec![];
        while let Some(field) = self.try(|p| p.dsl_field_def()) {
            fields.push(field);
            self.expect_punctuation(',')?;
        }
        Ok(fields)
    }

    fn dsl_item(&mut self) -> ParseResult<Node<Item>> {
        if self.eat_keyword("def") {
            let name = self.parse_ident()?;
            self.expect_punctuation('{')?;
            let fields = self.dsl_field_defs()?;
            let mut dart_members = vec![];
            while let Some(dart_member) = self.try(|p| p.dart_class_member(name)) {
                dart_members.push(dart_member);
            }
            let body = if self.eat_punctuation2('.', '.') {
                Some(self.dsl_expr()?)
            } else {
                None
            };

            self.expect_punctuation('}')?;
            return Ok(Node::new(Item::ComponentDef {
                name,
                fields,
                dart_members,
                body,
            }));
        }
        Ok(Node::new(Item::Dart(self.dart_item()?)))
    }

    pub fn dsl_items(&mut self) -> ParseResult<Vec<Node<Item>>> {
        let mut items = vec![];
        while !self.out_of_tokens() {
            items.push(self.dsl_item()?);
        }
        Ok(items)
    }
}
