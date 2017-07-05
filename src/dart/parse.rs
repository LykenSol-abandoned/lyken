use std::cell::Cell;
use dart::dsl::*;
use dart::lex::{Token, stringify};

pub fn parse_dsl(tokens: &[Token]) -> Vec<Item> {
    let allow_whitespace = Cell::new(false);
    let mut tokens = tokens.iter().filter(|token| {
        match **token {
            Token::WhiteSpace(_) => allow_whitespace.get(),
            _ => true
        }
    });
    let mut items = vec![];
    while let Some(token) = tokens.next() {
        if token.as_ident() == Some("def") {
            let name = tokens.next().unwrap().as_ident().unwrap().to_string();
            assert_eq!(*tokens.next().unwrap(), Token::Punctuation('{'));
            let mut parts = vec![];
            while let Some(token) = tokens.next() {
                if *token == Token::Punctuation('}') {
                    break;
                }
                let name = token.as_ident().unwrap().to_string();
                assert_eq!(*tokens.next().unwrap(), Token::Punctuation('{'));
                let mut fields = vec![];
                while let Some(token) = tokens.next() {
                    if *token == Token::Punctuation('}') {
                        break;
                    }
                    let name = token.as_ident().unwrap().to_string();
                    assert_eq!(*tokens.next().unwrap(), Token::Punctuation(':'));
                    let mut expr = vec![];
                    let mut depth = 0;
                    let mut ends_in_brace = false;
                    allow_whitespace.set(true);
                    while let Some(token) = tokens.next() {
                        if let Token::Punctuation(c) = *token {
                            if depth == 0 {
                                if c == ',' {
                                    break;
                                }
                                if c == '}' {
                                    ends_in_brace = true;
                                    break;
                                }
                            }
                            if "{[(".contains(c) {
                                depth += 1;
                            }
                            if "}])".contains(c) {
                                depth -= 1;
                            }
                        }
                        expr.push(token.clone());
                    }
                    allow_whitespace.set(false);
                    let expr = Expr { dart: stringify(&expr) };
                    let field = Field {
                        name: name,
                        value: expr,
                    };
                    fields.push(field);
                    if ends_in_brace {
                        break;
                    }
                }
                let comp_part = ComponentPart::Instance(name, fields);
                parts.push(comp_part);
            }
            items.push(Item::ComponentDef(name, parts));
        }
        if token.as_ident() == Some("dart") {
            assert_eq!(*tokens.next().unwrap(), Token::Punctuation('{'));
            let mut code = vec![];
            let mut depth = 0;
            allow_whitespace.set(true);
            while let Some(token) = tokens.next() {
                if let Token::Punctuation(c) = *token {
                    if depth == 0 {
                        if c == '}' {
                            break;
                        }
                    }
                    if "{[(".contains(c) {
                        depth += 1;
                    }
                    if "}])".contains(c) {
                        depth -= 1;
                    }
                }
                code.push(token.clone());
            }
            allow_whitespace.set(false);
            items.push(Item::VerbatimDart(stringify(&code)));
        }
    }
    items
}
