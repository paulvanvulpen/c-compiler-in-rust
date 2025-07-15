use crate::Args;
use anyhow::{Context, Result, bail};
use log::info;

use std::path::Path;
use std::process::Command;

mod compiler;

fn run_preprocessor(input: &Path) -> Result<()> {
    if input.extension().and_then(|e| e.to_str()) != Some("c") {
        bail!("Expected a '.c' file, but got `{}`.", input.display())
    }

    let out = input.with_extension("i");
    let status = Command::new("gcc")
        .args([
            "-E",
            "-P",
            input.to_str().unwrap(),
            "-o",
            out.to_str().unwrap(),
        ])
        .status()
        .context("calling gcc for preprocessing step")?;

    // Check if command succeeded
    if status.success() {
        info!("Preprocessing successful: {}", out.to_str().unwrap());
        // std::fs::remove_file(out).context("Removing the preprocessed .i file")?;
    } else {
        bail!("Preprocessing failed for `{}`", input.display())
    }

    Ok(())
}

fn run_assembler(input: &Path) -> Result<()> {
    let asm = input.with_extension("s");
    let out = input.with_extension("o");
    let status = Command::new("gcc")
        .args(["-c", asm.to_str().unwrap(), "-o", out.to_str().unwrap()])
        .status()
        .context("calling gcc for assemble step")?;

    if status.success() {
        info!("Assembling successful: {}", out.to_str().unwrap());
    } else {
        bail!("Assembling failed for `{}`", input.display())
    }
    Ok(())
}

fn run_linker(args: &Args) -> Result<()> {
    let input_files = args
        .source_files
        .iter()
        .map(|s| {
            let s = Path::new(s);
            s.with_extension("o").to_string_lossy().into_owned()
        })
        .collect::<Vec<String>>()
        .join(" ");

    let exe = Path::new(&args.source_files[0]).with_extension("");
    let status = Command::new("gcc")
        .args([input_files.as_str(), "-o", exe.to_str().unwrap()])
        .status()
        .context("assembling step")?;
    if status.success() {
        info!("link successful: {}", &args.source_files[0]);
        for source_file in input_files.split_whitespace() {
            std::fs::remove_file(source_file)
                .with_context(|| format!("removing object file `{}`", source_file))?
        }
    } else {
        bail!("link failed for `{}`.", &args.source_files[0]);
    }

    Ok(())
}

pub fn run(args: &Args) -> Result<()> {
    for source_file in &args.source_files {
        let source_file = Path::new(source_file);
        run_preprocessor(source_file).context("preprocessing step")?;
        compiler::run_compiler(&args, &source_file.with_extension("i"))
            .context("compilation step")?;
        if args.lex || args.parse || args.validate || args.codegen || args.tacky || args.codegen {
            continue;
        }
        if args.compile_only {
            continue;
        }
    }

    if args.compile_only
        || args.lex
        || args.parse
        || args.validate
        || args.codegen
        || args.tacky
        || args.codegen
    {
        return Ok(());
    }

    for source_file in &args.source_files {
        let source_file = Path::new(source_file);
        run_assembler(&source_file).context("assembling step")?;
    }

    if args.compile_and_assemble {
        return Ok(());
    }

    run_linker(args).context("linking step")?;
    Ok(())
}
