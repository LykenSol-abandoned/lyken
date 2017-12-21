use std::fs::File;

pub mod gamma0 {
    include!(concat!(env!("OUT_DIR"), "/gamma0.rs"));
}

#[test]
fn gamma_0() {
    let mut parser = gamma0::Parser::default();
    gamma0::A::parse(&mut parser, "aac");
    parser
        .gss
        .print(&mut File::create("target/gamma0-gss.dot").unwrap())
        .unwrap();
    parser
        .sppf
        .print(&mut File::create("target/gamma0-sppf.dot").unwrap())
        .unwrap();
}
