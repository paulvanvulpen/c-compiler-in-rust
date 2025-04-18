use crate::Args;
use node::Visualizer;

pub mod lexer;
mod parser;
mod assembly_generator;
mod code_emission;
mod node;
mod tacky;

pub(in crate::compiler_driver) fn run_compiler(args : &Args, input_file_path: &std::path::Path) -> std::io::Result<()>
{
	let mut lexer_tokens: Vec<lexer::Token> = vec![];
	lexer::run_lexer(input_file_path, &mut lexer_tokens)?;

	if args.lex
	{
		return Ok(());
	}

	let ast = parser::run_parser(&lexer_tokens)?;
	println!("{}", ast.visualize(0).as_str());

	if args.parse
	{
		return Ok(());
	}

	let tacky_ast = tacky::run_tacky_generator(ast)?;
	if args.tacky
	{
		return Ok(());
	}

/*	let assembly_ast = assembly_generator::run_assembly_generator(ast).expect("Assembly generator failed");
	println!("{}", assembly_ast.visualize(0).as_str());

	if args.codegen
	{
		return Ok(());
	}

	code_emission::run_code_emission(assembly_ast, input_file_path).expect("Code emission failed");
*/
	Ok(())
}
