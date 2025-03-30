mod compiler;

use std::io;
use std::process::Command;
use crate::Args;

pub fn compile(args : &Args) -> io::Result<()>
{
	let project_root = env!("CARGO_MANIFEST_DIR");
	if args.code_emission.is_some()
	{
		let input_file_path = std::path::Path::new(project_root).join(&args.code_emission.as_ref().unwrap().as_str());
		return emit_assembly(&args, &input_file_path);
	}

	let input_file_path = std::path::Path::new(project_root).join(&args.source_file.as_ref().unwrap().as_str());
	emit_binary(&args, &input_file_path)
}

pub fn emit_binary(args : &Args, input_file_path: &std::path::PathBuf) -> io::Result<()>
{
	run_preprocessor(&input_file_path).expect("Failed to run the preprocessor");
	run_compiler(&args, &input_file_path).expect("Failed to compile the source file");
	run_assemble_and_link(&input_file_path).expect("Failed to run the assembler and linker");
	Ok(())
}

pub fn emit_assembly(args : &Args, input_file_path: &std::path::PathBuf) -> io::Result<()>
{
	run_preprocessor(&input_file_path).expect("Failed to run the preprocessor");
	run_compiler(&args, &input_file_path).expect("Failed to compile the source file");
	Ok(())
}

fn run_preprocessor(input_file_path: &std::path::PathBuf) -> io::Result<()>
{
	assert_eq!("c", input_file_path.extension().unwrap());

	let pre_processor_output_file_name = input_file_path.with_extension("i");
	let status = Command::new("gcc")
		.args(["-E", "-P", input_file_path.to_str().unwrap(), "-o", pre_processor_output_file_name.to_str().unwrap()])
		.status().expect("failed to preprocess the source file");

	// Check if command succeeded
	if status.success() {
		println!("Preprocessing successful: {}", pre_processor_output_file_name.to_str().unwrap());
		std::fs::remove_file(pre_processor_output_file_name).expect("Failed to remove the preprocessing file despite that gcc reports it created it successfully");
	} else {
		eprintln!("Error: Preprocessing failed");
	}

	Ok(())
}

fn run_compiler(args : &Args, input_file_path: &std::path::PathBuf) -> io::Result<()>
{
	compiler::lexer(input_file_path).expect("Lexer failed");

	if args.lex
	{
		return Ok(());
	}
	compiler::parser().expect("Parser failed");

	if args.parse
	{
		return Ok(());
	}

	compiler::assembly_generator().expect("Assembly generator failed");

	if args.codegen
	{
		return Ok(());
	}

	compiler::code_emission().expect("Code emission failed");
	Ok(())
}

fn run_assemble_and_link(input_file_path: &std::path::PathBuf) -> io::Result<()>
{
	let assembly_file_name = input_file_path.with_extension("s");
	let status = Command::new("gcc")
		.args([assembly_file_name.to_str().unwrap(), "-o", input_file_path.with_extension("").to_str().unwrap()])
		.status()?;

	if status.success()
	{
		println!("Assemble and link successful: {}", "return_2");
		std::fs::remove_file(assembly_file_name).expect("Failed to remove the assembly file despite that gcc reports it created it successfully");
	} else {
		eprintln!("Error: Preprocessing failed");
	}

	Ok(())
}
