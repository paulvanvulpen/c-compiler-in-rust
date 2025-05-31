use crate::Args;
use anyhow::{Context, Result};
use log::info;
use std::path::Path;
use visualize::Visualizer;

mod assembly_generator;
mod code_emission;
mod generator;
mod lexer;
mod parser;
mod semantic_analysis;
mod tacky;
mod visualize;

pub fn run_compiler(args: &Args, input_file_path: &Path) -> Result<()> {
    let lexer_tokens = lexer::run_lexer(input_file_path)?;

    if args.lex {
        return Ok(());
    }

    let ast = parser::run_parser(&lexer_tokens)?;
    info!("\nAST:\n{}", ast.visualize(0).as_str());

    if args.parse {
        return Ok(());
    }

    let ast = semantic_analysis::run_semantic_analysis(ast)?;

    if args.validate {
        return Ok(());
    }

    let tacky_ast = tacky::run_tacky_generator(ast)?;
    info!("TACKY:\n{}", tacky_ast.visualize(0).as_str());
    if args.tacky {
        return Ok(());
    }

    let mut assembly_ast = assembly_generator::run_assembly_generator(tacky_ast)
        .context("Running the assembly generator.")?;
    info!("ASSEMBLY AST\n{}", assembly_ast.visualize(0).as_str());

    let allocation_size = assembly_generator::replace_pseudo_registers(&mut assembly_ast);
    info!(
        "ASSEMBLY AST after replace pseudo registers\nallocated: {} bytes\n{}",
        allocation_size,
        assembly_ast.visualize(0).as_str()
    );

    assembly_generator::fix_up_invalid_instructions(&mut assembly_ast, allocation_size);
    info!(
        "ASSEMBLY AST after fixing up invalid instructions\n{}",
        assembly_ast.visualize(0).as_str()
    );

    if args.codegen {
        return Ok(());
    }

    code_emission::run_code_emission(assembly_ast, input_file_path)
        .context("Code emission failed")?;

    Ok(())
}
