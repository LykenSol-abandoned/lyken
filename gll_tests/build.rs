#![feature(decl_macro)]

extern crate lyken_gll;

use lyken_gll::grammar::grammar;
use std::env;
use std::fs::File;
use std::path::PathBuf;

fn main() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    let mut gll10_g0 = grammar!{
        S = {s0: S0}
          | {s1: S1}
          | {s2: S2};
        S0 = { {a: A} {s: S} {d: 'd'} };
        S1 = { {b: B} {s: S} };
        S2 = {};

        A = {a1: A1}
          | {a2: A2};
        A1 = { 'a' };
        A2 = { 'c' };

        B = {b1: B1}
          | {b2: B2};
        B1 = { 'a' };
        B2 = { 'b' };
    };
    gll10_g0.generate(&mut File::create(&out_dir.join("gll10_g0.rs")).unwrap());

    let mut gll13_g1 = grammar!{
        S = {x: X}
          | {y: Y}
          | {z: Z};
        X = { 'a' {s: S} 'b' };
        Y = { 'd' };
        Z = { 'a' 'd' 'b' };
    };
    gll13_g1.generate(&mut File::create(&out_dir.join("gll13_g1.rs")).unwrap());

    let mut gll15_g0 = grammar!{
        A = {x: X}
          | {y: Y}
          | {z: Z};
        X = { 'a' {a: A} 'b' };
        Y = { 'a' {a: A} 'c' };
        Z = { 'a' };
    };
    gll15_g0.generate(&mut File::create(&out_dir.join("gll15_g0.rs")).unwrap());
}
