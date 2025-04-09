mod compiler;

use std::io;
use std::process::Command;
use crate::Args;

pub fn compile(args : &Args) -> io::Result<()>
{
	match (&args.code_emission, &args.source_file)
	{
		(Some(code_path), _) => {
			let input_file_path = std::path::Path::new(code_path);
			emit_assembly(args, input_file_path)
		}
		(None, Some(source_path)) => {
			let input_file_path = std::path::Path::new(source_path);
			emit_binary(args, input_file_path)
		}
		(None, None) => {
			Err(io::Error::new(io::ErrorKind::InvalidInput, "No input file provided"))
		}
	}
}

fn emit_binary(args : &Args, input_file_path: &std::path::Path) -> io::Result<()>
{
	run_preprocessor(&input_file_path).expect("Failed to run the preprocessor");
	compiler::run_compiler(&args, &input_file_path.with_extension("i"))?;
	run_assemble_and_link(&input_file_path)
}

fn emit_assembly(args : &Args, input_file_path: &std::path::Path) -> io::Result<()>
{
	run_preprocessor(&input_file_path)?;
	compiler::run_compiler(&args, &input_file_path.with_extension("i"))
}

fn run_preprocessor(input_file_path: &std::path::Path) -> io::Result<()>
{
	assert_eq!("c", input_file_path.extension().unwrap());

	let pre_processor_output_file_name = input_file_path.with_extension("i");
	let status = Command::new("gcc")
		.args(["-E", "-P", input_file_path.to_str().unwrap(), "-o", pre_processor_output_file_name.to_str().unwrap()])
		.status().expect("failed to preprocess the source file");

	// Check if command succeeded
	if status.success() {
		println!("Preprocessing successful: {}", pre_processor_output_file_name.to_str().unwrap());
		//std::fs::remove_file(pre_processor_output_file_name).expect("Failed to remove the preprocessing file despite that gcc reports it created it successfully");
	} else {
		eprintln!("Error: Preprocessing failed");
	}

	Ok(())
}

fn run_assemble_and_link(input_file_path: &std::path::Path) -> io::Result<()>
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
