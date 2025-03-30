mod compiler_driver;

use std::io;
use clap::Parser;

/// A rust-based C compiler.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
	/// The main source file to compile
	source_file: String,

	/// Run the lexer, but stop before parsing
	#[arg(long)]
	lex: bool,

	/// Run the lexer and parser, but stop before assembly generation
	#[arg(long)]
	parse: bool,

	/// Directs it to perform lexing, parsing and assembly generation, but stop before code emission
	#[arg(long)]
	codegen: bool,
}

fn main() -> io::Result<()>
{
	let args = Args::parse();
	let project_root = env!("CARGO_MANIFEST_DIR");
	let input_file_path = std::path::Path::new(project_root).join(&args.source_file);

	compiler_driver::run_preprocessor(&input_file_path).expect("Failed to run the preprocessor");
	compiler_driver::run_compiler(&input_file_path).expect("Failed to compile the source file");
	compiler_driver::run_assemble_and_link(&input_file_path).expect("Failed to run the assembler and linker");
	Ok(())
}
