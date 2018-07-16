#![feature(decl_macro)]

#[allow(non_camel_case_types)]
pub mod parse {
    include!(concat!(env!("OUT_DIR"), "/parse.rs"));
}

#[test]
fn lex() {
    parse::Parser::with_str(
        // HACK(eddyb) because of how Whitespace is handled in build.rs *between* tokens
        std::fs::read_to_string("input.c").unwrap().trim(),
        |mut parser, range| {
            let mut tokens = parse::translation_unit::parse(&mut parser, range).unwrap();
            let parser = tokens.parser;

            /*parser
                .gss
                .print(&mut std::fs::File::create(concat!("target/c-gss.dot")).unwrap())
                .unwrap();
            parser
                .sppf
                .print(&mut std::fs::File::create(concat!("target/c-sppf.dot")).unwrap())
                .unwrap();*/

            assert_eq!(tokens.span, range);
            let mut v = vec![];
            while let parse::translation_unit {
                rest: Some(rest),
                last: Some(last),
            } = tokens.one().unwrap()
            {
                v.push(last);
                tokens = rest;
            }
            // FIXME(eddyb) just create the Vec in the right order or something.
            v.reverse();
            for token in v {
                println!("{:?}: {:#?}", parser.input(token.span), token);
            }
            unimplemented!() //panic!("{}", v.len());
        },
    );
}
