use anyhow::Result;
use clap::Parser;
use env_logger::Env;

mod compiler_driver;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg()]
    source_files: Vec<String>,

    /// Compile only; do not assemble or link
    #[arg(short = 'S')]
    compile_only: bool,

    /// Compile and assemble, but do not link.
    #[arg(short = 'c')]
    compile_and_assemble: bool,

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
    compiler_driver::run(&args)?;
    Ok(())
}
