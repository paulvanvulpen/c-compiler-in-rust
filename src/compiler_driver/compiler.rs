use crate::Args;

pub mod lexer;
mod parser;
mod assembly_generator;
mod code_emission;

pub(in crate::compiler_driver) fn run_compiler(args : &Args, input_file_path: &std::path::Path) -> std::io::Result<()>
{
	let mut lexer_tokens: Vec<lexer::Token> = vec![];
	lexer::run_lexer(input_file_path, &mut lexer_tokens)?;

	if args.lex
	{
		return Ok(());
	}

	let ast = parser::run_parser(&lexer_tokens[..])?;

	if args.parse
	{
		return Ok(());
	}

	assembly_generator::run_assembly_generator().expect("Assembly generator failed");

	if args.codegen
	{
		return Ok(());
	}

	code_emission::run_code_emission().expect("Code emission failed");
	Ok(())
}
