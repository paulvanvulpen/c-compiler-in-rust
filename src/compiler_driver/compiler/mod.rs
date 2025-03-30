use crate::Args;

mod lexer;
mod parser;
mod assembly_generator;
mod code_emission;

pub(in crate::compiler_driver) fn run_compiler(args : &Args, input_file_path: &std::path::PathBuf) -> std::io::Result<()>
{
	lexer::run_lexer(input_file_path).expect("Lexer failed");

	if args.lex
	{
		return Ok(());
	}
	parser::run_parser().expect("Parser failed");

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
