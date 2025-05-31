use crate::Args;
use anyhow::{Context, Result, bail};
use log::info;

use std::path::Path;
use std::process::Command;

mod compiler;

pub fn compile(args: &Args) -> Result<()> {
    // decide between assembly vs binary
    if let Some(code_path) = &args.code_emission {
        let input = Path::new(code_path);
        emit_assembly(args, input)
            .with_context(|| format!("Assembly emission failed for `{}`.", code_path))?
    } else if let Some(src) = &args.source_file {
        let input = Path::new(src);
        emit_binary(args, input)
            .with_context(|| format!("Binary emission failed for `{}`.", src))?
    } else {
        bail!("No input file provided.")
    }
    Ok(())
}

fn emit_binary(args: &Args, input_file_path: &Path) -> Result<()> {
    run_preprocessor(&input_file_path).context("preprocessing step")?;
    compiler::run_compiler(&args, &input_file_path.with_extension("i"))
        .context("compilation step")?;
    if args.lex || args.parse || args.validate || args.codegen || args.tacky || args.codegen {
        return Ok(());
    }
    run_assemble_and_link(&input_file_path).context("assembling & linking")?;
    Ok(())
}

fn emit_assembly(args: &Args, input_file_path: &Path) -> Result<()> {
    run_preprocessor(&input_file_path).context("preprocessing step")?;
    compiler::run_compiler(&args, &input_file_path.with_extension("i"))
        .context("compilation step")?;
    Ok(())
}

fn run_preprocessor(input: &Path) -> Result<()> {
    if input.extension().and_then(|e| e.to_str()) != Some("c") {
        bail!("Expected a '.c' file, but got `{}`.", input.display());
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
        bail!("preprocessing failed for `{}`", input.display())
    }

    Ok(())
}

fn run_assemble_and_link(input: &Path) -> Result<()> {
    let asm = input.with_extension("s");
    let exe = input.with_extension("");
    let status = Command::new("gcc")
        .args([asm.to_str().unwrap(), "-o", exe.to_str().unwrap()])
        .status()
        .context("calling gcc for assemble and link step")?;

    if status.success() {
        info!(
            "Assemble and link successful: {}",
            input.with_extension("c").display()
        );
        // std::fs::remove_file(&asm)
        //     .with_context(|| format!("removing assembly file `{}`", asm.display()))?
    } else {
        bail!("Assembly & link failed for `{}`.", asm.display());
    }

    Ok(())
}
