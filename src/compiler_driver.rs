use crate::Args;
mod compiler;
use anyhow::Context;
use std::process::Command;

pub fn compile(args: &Args) -> anyhow::Result<()> {
    // decide between assembly vs binary
    if let Some(code_path) = &args.code_emission {
        let input = std::path::Path::new(code_path);
        emit_assembly(args, input)
            .with_context(|| format!("Assembly emission failed for `{}`", code_path))?
    } else if let Some(src) = &args.source_file {
        let input = std::path::Path::new(src);
        emit_binary(args, input).with_context(|| format!("binary emission failed for `{}`", src))?
    } else {
        anyhow::bail!("no input file provided")
    }
    Ok(())
}

fn emit_binary(args: &Args, input_file_path: &std::path::Path) -> anyhow::Result<()> {
    run_preprocessor(&input_file_path).context("preprocessing step")?;
    compiler::run_compiler(&args, &input_file_path.with_extension("i"))
        .context("compilation step")?;
    run_assemble_and_link(&input_file_path).context("assembling & linking")?;
    Ok(())
}

fn emit_assembly(args: &Args, input_file_path: &std::path::Path) -> anyhow::Result<()> {
    run_preprocessor(&input_file_path).context("preprocessing step")?;
    compiler::run_compiler(&args, &input_file_path.with_extension("i"))
        .context("compilation step")?;
    Ok(())
}

fn run_preprocessor(input: &std::path::Path) -> anyhow::Result<()> {
    if input.extension().and_then(|e| e.to_str()) != Some("c") {
        anyhow::bail!("expected a '.c' file, but got `{}`", input.display());
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
        println!("Preprocessing successful: {}", out.to_str().unwrap());
        // std::fs::remove_file(out).context("Removing the preprocessed .i file")?;
    } else {
        anyhow::bail!("preprocessing failed for `{}`", input.display())
    }

    Ok(())
}

fn run_assemble_and_link(input: &std::path::Path) -> anyhow::Result<()> {
    let asm = input.with_extension("s");
    let exe = input.with_extension("");
    let status = Command::new("gcc")
        .args([asm.to_str().unwrap(), "-o", exe.to_str().unwrap()])
        .status()
        .context("calling gcc for assemble and link step")?;

    if status.success() {
        println!("Assemble and link successful: {}", "return_2");
        std::fs::remove_file(&asm)
            .with_context(|| format!("removing temp file `{}`", asm.display()))?
    } else {
        anyhow::bail!("Assembly & link failed for `{}`", asm.display());
    }

    Ok(())
}
