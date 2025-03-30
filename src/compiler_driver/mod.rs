mod compiler;

use std::io;
use std::process::Command;

pub fn run_preprocessor(input_file_path: &std::path::PathBuf) -> io::Result<()>
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

pub fn run_compiler(input_file_path: &std::path::PathBuf) -> io::Result<()>
{
	compiler::lexer(input_file_path).expect("Lexer failed");
	compiler::parser().expect("Parser failed");
	compiler::assembly_generator().expect("Assembly generator failed");
	compiler::code_emission().expect("Code emission failed");
	Ok(())
}

pub fn run_assemble_and_link(input_file_path: &std::path::PathBuf) -> io::Result<()>
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
