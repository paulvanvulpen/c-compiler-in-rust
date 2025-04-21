use crate::Args;
use visualize::Visualizer;

mod assembly_generator;
mod code_emission;
pub mod lexer;
mod parser;
mod tacky;
mod visualize;

pub(in crate::compiler_driver) fn run_compiler(
    args: &Args,
    input_file_path: &std::path::Path,
) -> std::io::Result<()> {
    let mut lexer_tokens: Vec<lexer::Token> = vec![];
    lexer::run_lexer(input_file_path, &mut lexer_tokens)?;

    if args.lex {
        return Ok(());
    }

    let ast = parser::run_parser(&lexer_tokens)?;
    println!("AST:\n{}", ast.visualize(0).as_str());

    if args.parse {
        return Ok(());
    }

    let tacky_ast = tacky::run_tacky_generator(ast)?;
    println!("TACKY:\n{}", tacky_ast.visualize(0).as_str());
    if args.tacky {
        return Ok(());
    }

    let mut assembly_ast =
        assembly_generator::run_assembly_generator(tacky_ast).expect("Assembly generator failed");
    println!("ASSEMBLY AST\n{}", assembly_ast.visualize(0).as_str());

    let allocation_size = assembly_generator::replace_pseudo_registers(&mut assembly_ast);
    println!(
        "ASSEMBLY AST after replace pseudo registers\nallocated: {} bytes\n{}",
        allocation_size,
        assembly_ast.visualize(0).as_str()
    );

    assembly_generator::fix_up_invalid_instructions(&mut assembly_ast, allocation_size);
    println!(
        "ASSEMBLY AST after fixing up invalid instructions\n{}",
        assembly_ast.visualize(0).as_str()
    );

    if args.codegen {
        return Ok(());
    }

    code_emission::run_code_emission(assembly_ast, input_file_path).expect("Code emission failed");

    Ok(())
}
