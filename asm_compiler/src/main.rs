mod ast;
mod compile;
mod link;
mod token;

use std::{
    env,
    fs::{self, File},
    io::{Read, Write},
    path::Path,
};

use ast::parse;
use compile::compile;
use itertools::Itertools;
use shared::{time, write_json};
use token::tokenize;

fn main() -> anyhow::Result<()> {
    let mut args = env::args();
    let Some(_current_path) = args.next() else { panic!("Cannot get current dir") };
    let Some(in_path) = args.next() else { panic!("No input file given") };
    let Some(out_path) = args.next() else { panic!("No output path given") };
    let Some(res_path) = args.next() else { panic!("No res path given") };

    let in_path = Path::new(&in_path);
    assert!(
        in_path.extension().and_then(|x| x.to_str()) == Some("ascm"),
        "input file must be a .ascm file"
    );

    let mut in_file = File::open(in_path).expect("input file should open");
    let input = {
        let mut buf = String::new();
        in_file.read_to_string(&mut buf).expect("should be able to read input file");
        buf
    };

    let tokens = time("Tokenizing...", || tokenize(input))?;
    let ast = time("Parsing...", || parse(&tokens))?;
    let linked = time("Linking...", || link::link(&ast));
    let ez = time("Transpiling to EZ...", || compile(&linked))?;
    let ir = time("Transpiling to IR...", || ez.compile());
    let js_val = time("Compiling to JSON...", || ir.compile());
    let json = time("Serializing...", || format!("{:#}", js_val));

    time("Exporting...", || write_json(&json, in_path, &out_path, &res_path));

    Ok(())
}
