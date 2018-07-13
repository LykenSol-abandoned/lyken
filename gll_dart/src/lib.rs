#![feature(decl_macro)]

pub mod parse {
    include!(concat!(env!("OUT_DIR"), "/parse.rs"));
}

#[test]
fn lex() {
    parse::Parser::with_str("   foo123.n=x+y/*dart /**/ parser*/", |mut parser, range| {
        let mut tokens = parse::Tokens::parse(&mut parser, range).unwrap();
        let mut v = vec![];
        while let parse::Tokens {
            first: Some(first),
            rest: Some(rest),
        } = tokens.one().unwrap()
        {
            v.push(first);
            tokens = rest;
        }
        assert_eq!(
            v.iter()
                .map(|x| format!("{:?}", x))
                .collect::<Vec<String>>(),
            [
                "0..3 => Token { ws: 0..3 }",
                "3..9 => Token { ident: 3..9 }",
                "9..10 => Token { punct: 9..10 }",
                "10..11 => Token { ident: 10..11 }",
                "11..12 => Token { punct: 11..12 }",
                "12..13 => Token { ident: 12..13 }",
                "13..14 => Token { punct: 13..14 }",
                "14..15 => Token { ident: 14..15 }",
                "15..35 => Token { comment: 15..35 }"
            ]
        );
    });
}
