mod compiler_driver;

use std::io;
use clap::Parser;

/// A rust-based C compiler.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
	/// The main source file to compile and emit an executable
	#[arg(required_unless_present = "code_emission")]
	source_file: Option<String>,

	/// an alternative mode to compile and emit an assembly file
	#[arg(short = 'S', conflicts_with = "source_file")]
	code_emission: Option<String>,

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
	compiler_driver::compile(&args)
}
