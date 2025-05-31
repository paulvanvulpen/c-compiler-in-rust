use anyhow::Result;
use clap::Parser;
use env_logger::Env;

mod compiler_driver;

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

    /// Run the lexer and parser, but stop before TACKY
    #[arg(long)]
    validate: bool,

    /// Run the lexer, parser, validator and TACKY, but stop before assembly generation
    #[arg(long)]
    tacky: bool,

    /// Directs it to perform lexing, parsing and assembly generation, but stop before code emission
    #[arg(long)]
    codegen: bool,
}

/// A rust-based C compiler.
fn main() -> Result<()> {
    env_logger::Builder::from_env(Env::default().default_filter_or("info")).init();

    let args = Args::parse();
    compiler_driver::compile(&args)?;
    Ok(())
}
