use std::{env, fs::File, io::Read, path::Path};

use ast::parse;
use shared::time;
use token::tokenize;

mod ast;
mod compile;
mod token;

fn main() -> Result<(), ()> {
    let mut args = env::args();
    let Some(_current_path) = args.next() else { panic!("Cannot get current dir") };
    let Some(in_path) = args.next() else { panic!("No input file given") };
    let Some(out_path) = args.next() else { panic!("No output path given") };

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
    dbg!(&tokens);
    let ast = time("Parsing...", || parse(tokens));
    dbg!(&ast);

    Ok(())
}
