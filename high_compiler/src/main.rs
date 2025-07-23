use std::{
    fs::{self, File},
    io::Read,
    path::Path,
};

use clap::Parser;
use compile::compile;
use grammar_ast::parse;
use link::link;
use shared::{time, time_total, write_json};
use token::tokenize;

use crate::ast_error::report_ast_errors;

mod add_builtins;
mod ast_error;
mod ast_parse;
mod compile;
pub mod grammar_ast;
mod grammar_to_logic;
mod link;
mod logic_ast;
mod src_pos;
mod token;
mod token_feed;
mod typecheck;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Enables stack monitoring
    #[arg(long)]
    dev: bool,
    /// The Steplo source code file (.lo) to compile
    src_path: String,
    /// The folder to save the compiled program (and other build artifacts) within
    out_path: String,
    /// The folder containing Scratch resources
    res_path: String,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    println!("STEPLO COMPILER");

    time_total("Total compile time:", || compile_all(args))?;

    Ok(())
}

fn compile_all(args: Args) -> anyhow::Result<()> {
    let src_path = Path::new(&args.src_path);
    assert!(
        src_path.extension().and_then(|x| x.to_str()) == Some("lo"),
        "input file must be a steplo source code (.lo) file"
    );

    let mut in_file = File::open(src_path).expect("input file should open");
    let input = {
        let mut buf = String::new();
        in_file.read_to_string(&mut buf).expect("should be able to read input file");
        buf
    };

    let tokens = time("Tokenizing...", || tokenize(&input))?;
    // dbg!(&tokens);
    let ast = match time("Parsing grammar...", || parse(tokens.into())) {
        Ok(ast) => ast,
        Err(err) => {
            report_ast_errors(&input, src_path, err);
            return Ok(());
        },
    };

    let ast = time("Converting grammar...", || logic_ast::Program::try_from(&ast))?;

    let ast = time("Adding built-in functions...", || add_builtins::add_builtins(ast));

    time("Typechecking...", || typecheck::typecheck(&ast))?;

    // dbg!(&ast);
    let linked = time("Linking...", || link(&ast))?;
    // dbg!(&linked);
    let mem_opt_ast = time("Compiling high-level to designation IR...", || compile(linked))?;

    time("Writing intermediate opt 0 file...", || {
        let asm_export = mem_opt::export::export(mem_opt_ast.iter().map(AsRef::as_ref));
        let name = src_path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .expect("input file should have stem");

        let path = Path::new(&args.out_path).join(format!("{}.opt0", name));
        fs::write(path, asm_export).expect("opt export should succeed");
    });

    let mem_opt1_ast =
        time("Optimizing code...", || mem_opt::opt::optimize(mem_opt_ast.iter().cloned()));

    time("Writing intermediate opt 1 file...", || {
        let asm_export = mem_opt::export::export(mem_opt1_ast.iter().map(AsRef::as_ref));
        let name = src_path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .expect("input file should have stem");

        let path = Path::new(&args.out_path).join(format!("{}.opt1", name));
        fs::write(path, asm_export).expect("opt export should succeed");
    });

    let mem_opt_designated =
        time("Designating registers...", || mem_opt::designate::designate_registers(&mem_opt1_ast));

    let ez = time("Compiling to EZ...", || {
        mem_opt::compile::compile(
            mem_opt_designated.iter().map(AsRef::as_ref),
            mem_opt::compile::CompileOptions { stack_monitoring: args.dev },
        )
    });

    let ir = time("Transpiling to IR...", || ez.compile());
    let js_val = time("Compiling to JSON...", || ir.compile());
    let json = time("Serializing...", || format!("{:#}", js_val));

    time("Exporting...", || write_json(&json, src_path, &args.out_path, &args.res_path));

    Ok(())
}
