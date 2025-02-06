use std::{env, fs::File, io::Read, path::Path};

use anyhow::anyhow;
use ast::parse;
use compile::compile;
use itertools::Itertools;
use shared::{time, write_json};
use token::tokenize;

mod ast;
mod compile;
mod link;
mod token;

fn main() -> anyhow::Result<()> {
    let mut args = env::args();
    let Some(_current_path) = args.next() else { panic!("Cannot get current dir") };
    let Some(in_path) = args.next() else { panic!("No input file given") };
    let Some(out_path) = args.next() else { panic!("No output path given") };
    let Some(res_path) = args.next() else { panic!("No res path given") };

    let in_path = Path::new(&in_path);
    assert!(
        in_path.extension().and_then(|x| x.to_str()) == Some("hscm"),
        "input file must be a .hscm file"
    );

    let mut in_file = File::open(in_path).expect("input file should open");
    let input = {
        let mut buf = String::new();
        in_file.read_to_string(&mut buf).expect("should be able to read input file");
        buf
    };

    let tokens = time("Tokenizing...", || tokenize(&input))?;
    // dbg!(&tokens);
    let ast = time("Parsing...", || parse(tokens))?;
    // dbg!(&ast);
    let linked = time("Linking...", || link::link(ast))?;
    dbg!(&linked);
    let asm = time("Compiling high-level to asm...", || compile(linked))?;
    // dbg!(&compiled);
    let ez = time("Transpiling to EZ...", || {
        asm_compiler::compile::compile(&asm.iter().map(AsRef::as_ref).collect_vec())
    })?;
    let ir = time("Transpiling to IR...", || ez.compile());
    let js_val = time("Compiling to JSON...", || ir.compile());
    let json = time("Serializing...", || format!("{:#}", js_val));

    time("Exporting...", || write_json(&json, in_path, &out_path, &res_path));

    Ok(())
}
