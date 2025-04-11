use std::{
    env,
    fs::{self, File},
    io::Read,
    path::Path,
};

use ast::parse;
use compile::compile;
use link::link;
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
    let linked = time("Linking...", || link(ast))?;
    // dbg!(&linked);
    let mem_opt_ast = time("Compiling high-level to designation IR...", || compile(linked))?;

    time("Writing intermediate opt file...", || {
        let asm_export = mem_opt::export::export(mem_opt_ast.iter().map(AsRef::as_ref));
        let name = in_path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .expect("input file should have stem");

        let path = Path::new(&out_path).join(format!("{}.opt", name));
        fs::write(path, asm_export).expect("opt export should succeed");
    });

    // let mem_opt_designated =
    //     time("Designating registers...", || mem_opt::designate::designate_registers(&mem_opt_ast));
    // let asm_ast = time("Compiling to asm...", || mem_opt::compile::compile(mem_opt_designated))?;

    // time("Writing intermediate asm file...", || {
    //     let asm_export = asm_compiler::export::export(&asm_ast);
    //     let name = in_path
    //         .file_stem()
    //         .and_then(|stem| stem.to_str())
    //         .expect("input file should have stem");

    //     let path = Path::new(&out_path).join(format!("{}.ascm", name));
    //     fs::write(path, asm_export).expect("asm export should succeed");
    // });

    // let asm_linked = time("Linking asm...", || asm_compiler::link::link(&asm_ast));
    // // dbg!(&compiled);
    // let ez = time("Transpiling to EZ...", || asm_compiler::compile::compile(&asm_linked))?;
    // let ir = time("Transpiling to IR...", || ez.compile());
    // let js_val = time("Compiling to JSON...", || ir.compile());
    // let json = time("Serializing...", || format!("{:#}", js_val));

    // time("Exporting...", || write_json(&json, in_path, &out_path, &res_path));

    Ok(())
}
