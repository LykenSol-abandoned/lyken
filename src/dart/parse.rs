use std::slice;
use dart::dsl::*;
use dart::lex::{Token, stringify};

pub struct Parser<'a> {
    tokens: slice::Iter<'a, Token>,
    cur: Option<&'a Token>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        let mut tokens = tokens.iter();
        Parser {
            cur: tokens.next(),
            tokens: tokens,
        }
    }

    fn out_of_tokens(&self) -> bool {
        self.cur.is_none()
    }

    fn is_punctuation(&self, c: char) -> bool {
        if let Some(token) = self.cur {
            *token == Token::Punctuation(c)
        } else {
            false
        }
    }

    fn is_keyword(&self, s: &str) -> bool {
        if let Some(token) = self.cur {
            token.as_ident() == Some(s)
        } else {
            false
        }
    }

    fn bump(&mut self) {
        loop {
            self.cur = self.tokens.next();
            match self.cur {
                Some(&Token::WhiteSpace(_)) => {}
                _ => break,
            }
        }
    }

    fn eat_punctuation(&mut self, c: char) -> bool {
        if self.is_punctuation(c) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn eat_keyword(&mut self, s: &str) -> bool {
        if self.is_keyword(s) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn parse_ident(&mut self) -> String {
        let ident = self.cur.unwrap().as_ident().unwrap().to_string();
        self.bump();
        ident
    }

    fn parse_dart(&mut self) -> String {
        let mut depth = 0;
        let mut code = vec![];
        while let Some(token) = self.cur {
            if let Token::Punctuation(c) = *token {
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
            code.push(token.clone());
            self.cur = self.tokens.next();
        }
        stringify(&code)
    }

    fn parse_expr(&mut self) -> Expr {
        Expr::VerbatimDart(self.parse_dart())
    }

    fn parse_field(&mut self) -> Field {
        let name = self.parse_ident();
        assert!(self.eat_punctuation(':'));
        Field {
            name: name,
            value: self.parse_expr(),
        }
    }

    fn parse_fields(&mut self) -> Vec<Field> {
        let mut fields = vec![];
        while !self.out_of_tokens() {
            if self.is_punctuation('}') {
                break;
            }
            fields.push(self.parse_field());
            if !self.eat_punctuation(',') {
                break;
            }
        }
        fields
    }

    fn parse_component_part(&mut self) -> ComponentPart {
        let name = self.parse_ident();
        assert!(self.eat_punctuation('{'));
        let fields = self.parse_fields();
        assert!(self.eat_punctuation('}'));
        ComponentPart::Instance(Instance { name, fields })
    }

    fn parse_component_parts(&mut self) -> Vec<ComponentPart> {
        let mut parts = vec![];
        while !self.out_of_tokens() {
            if self.is_punctuation('}') {
                break;
            }
            parts.push(self.parse_component_part());
        }
        parts
    }

    fn parse_item(&mut self) -> Item {
        if self.eat_keyword("def") {
            let name = self.parse_ident();
            assert!(self.eat_punctuation('{'));
            let parts = self.parse_component_parts();
            assert!(self.eat_punctuation('}'));
            Item::ComponentDef(name, parts)
        } else if self.eat_keyword("dart") {
            assert!(self.eat_punctuation('{'));
            let dart = self.parse_dart();
            assert!(self.eat_punctuation('}'));
            Item::VerbatimDart(dart)
        } else {
            panic!("unknown item");
        }
    }

    pub fn parse_items(&mut self) -> Vec<Item> {
        let mut items = vec![];
        while !self.out_of_tokens() {
            items.push(self.parse_item());
        }
        items
    }
}
