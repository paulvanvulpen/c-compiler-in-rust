use crate::Args;

mod lexer;
mod parser;
mod assembly_generator;
mod code_emission;

pub(in crate::compiler_driver) fn run_compiler(args : &Args, input_file_path: &std::path::Path) -> std::io::Result<()>
{
	lexer::run_lexer(input_file_path)?;

	if args.lex
	{
		return Ok(());
	}

	parser::run_parser()?;

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
