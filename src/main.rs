mod ez;
mod high_compiler;
mod inline;
mod ir;
mod mem_opt;
mod utils;

use std::{
    fs::{self, File},
    io::Read,
    path::Path,
    sync::Arc,
};

use high_compiler::{compile_error, grammar_ast, grammar_to_logic, link, srced, token};

use clap::Parser;
use grammar_ast::parse;
use include_dir::{Dir, include_dir};
use link::link;
use token::tokenize;
use utils::{time, time_total, write_json};

use crate::{
    compile_error::{CompileErrorSet, report_compile_errors},
    grammar_to_logic::grammar_to_ast,
    high_compiler::{add_builtins, logic_ast, type_resolved_ast, typecheck},
    srced::Srced,
};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The Steplo source code file (.lo) to compile
    src_path: String,
    /// The folder to save the compiled program (and other build artifacts) within
    out_path: String,

    /// Enable stack monitoring
    #[arg(long)]
    dev: bool,
    /// Disable optimizations
    #[arg(long)]
    no_opt: bool,
    /// Output intermediate optimization artifacts
    #[arg(long)]
    out_opt: bool,
    /// The platform to optimize the compiled project for
    #[arg(long)]
    target: SerdeTarget,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, clap::ValueEnum)]
pub enum SerdeTarget {
    Scratch,
    Turbowarp,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Target {
    Scratch,
    TurboWarp,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    println!("STEPLO COMPILER");

    time_total("Total compile time:", || compile_all(args))?;

    Ok(())
}

fn compile_all(args: Args) -> anyhow::Result<()> {
    static RES_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/res");

    let src_path = Path::new(&args.src_path);
    assert!(
        src_path.extension().and_then(|x| x.to_str()) == Some("lo"),
        "input file must be a Steplo source code (.lo) file"
    );

    let mut in_file = File::open(src_path).expect("input file should open");
    let input = {
        let mut buf = String::new();
        in_file.read_to_string(&mut buf).expect("should be able to read input file");
        buf
    };

    let tokens = time("Tokenizing...", || tokenize(&input))?;

    let ast = match compile_set_fallables(tokens) {
        Ok(ast) => ast,
        Err(err) => {
            report_compile_errors(&input, src_path, err);
            return Ok(());
        },
    };

    let linked = time("Linking...", || link(&ast));
    let inline_opt_ast = time("Compiling high-level to inlining IR...", || {
        high_compiler::compile_to_inline::compile(&linked)
    })?;

    if args.out_opt {
        time("Writing intermediate inline 0 file...", || {
            let asm_export = inline::export::export(inline_opt_ast.iter().map(AsRef::as_ref));
            let name = src_path
                .file_stem()
                .and_then(|stem| stem.to_str())
                .expect("input file should have stem");

            let path = Path::new(&args.out_path).join(format!("{name}.inline0"));
            fs::write(path, asm_export).expect("inline export should succeed");
        });
    }

    let inline_opt_ast = if args.no_opt {
        inline_opt_ast
    } else {
        let inline_opt_ast =
            time("Optimizing inline IR...", || inline::opt::optimize(&inline_opt_ast));

        if args.out_opt {
            time("Writing intermediate inline 1 file...", || {
                let asm_export = inline::export::export(inline_opt_ast.iter().map(AsRef::as_ref));
                let name = src_path
                    .file_stem()
                    .and_then(|stem| stem.to_str())
                    .expect("input file should have stem");

                let path = Path::new(&args.out_path).join(format!("{name}.inline1"));
                fs::write(path, asm_export).expect("inline export should succeed");
            });
        }

        inline_opt_ast
    };

    let mem_opt_ast = time("Compiling high-level to designation IR...", || {
        inline::compile_to_mem_opt::compile(&inline_opt_ast)
    })?;

    if args.out_opt {
        time("Writing intermediate opt 0 file...", || {
            let asm_export = mem_opt::export::export(mem_opt_ast.iter().map(AsRef::as_ref));
            let name = src_path
                .file_stem()
                .and_then(|stem| stem.to_str())
                .expect("input file should have stem");

            let path = Path::new(&args.out_path).join(format!("{name}.opt0"));
            fs::write(path, asm_export).expect("opt export should succeed");
        });
    }

    let mem_opt_ast = if args.no_opt {
        mem_opt_ast
    } else {
        let mem_opt_ast = time("Optimizing designation IR...", || {
            mem_opt::opt::optimize(mem_opt_ast.iter().cloned())
        });

        if args.out_opt {
            time("Writing intermediate opt 1 file...", || {
                let asm_export = mem_opt::export::export(mem_opt_ast.iter().map(AsRef::as_ref));
                let name = src_path
                    .file_stem()
                    .and_then(|stem| stem.to_str())
                    .expect("input file should have stem");

                let path = Path::new(&args.out_path).join(format!("{name}.opt1"));
                fs::write(path, asm_export).expect("opt export should succeed");
            });
        }

        mem_opt_ast
    };

    let mem_opt_designated =
        time("Designating registers...", || mem_opt::designate::designate_registers(&mem_opt_ast));

    let target = match args.target {
        SerdeTarget::Scratch => Target::Scratch,
        SerdeTarget::Turbowarp => Target::TurboWarp,
    };

    let ez = time("Compiling to EZ...", || {
        mem_opt::compile::compile(
            mem_opt_designated.iter().map(AsRef::as_ref),
            mem_opt::compile::CompileOptions { stack_monitoring: args.dev, target },
        )
    });

    let ir = time("Transpiling to IR...", || ez.compile());
    let js_val = time("Compiling to JSON...", || ir.compile());
    let json = time("Serializing...", || format!("{js_val:#}"));

    time("Exporting...", || {
        write_json(&json, src_path, Path::new(&args.out_path), &RES_DIR, target)
    });

    Ok(())
}

fn compile_set_fallables(
    tokens: Vec<Srced<token::Token>>,
) -> Result<logic_ast::Ref<type_resolved_ast::Program>, CompileErrorSet> {
    let ast = time("Parsing grammar...", || parse(tokens.into()))?;
    let ast = time("Converting grammar...", || grammar_to_ast(&Arc::new(ast)))?;
    let ast = time("Adding built-in functions...", || add_builtins::add_builtins(&ast));
    let ast = time("Typechecking...", || typecheck::typecheck(&ast))?;
    Ok(ast)
}
