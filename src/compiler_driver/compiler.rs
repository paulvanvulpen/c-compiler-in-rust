use std::io;

use std::io::BufRead;
use unicode_segmentation::UnicodeSegmentation;

pub(in crate::compiler_driver) fn lexer(input_file_path: &std::path::PathBuf) -> io::Result<()>
{
	let file = std::fs::File::open(input_file_path).expect("Failed to open input file");
	let reader = io::BufReader::new(file);

	for line in reader.lines()
	{
		let line = line?;
		let tokens = line.split_word_bounds().collect::<Vec<&str>>();
		for token in tokens.iter()
		{
			println!("{}", token);
		}
	}

	Ok(())
}

pub(in crate::compiler_driver) fn parser() -> io::Result<()>
{
	Ok(())
}

pub(in crate::compiler_driver) fn assembly_generator() -> io::Result<()>
{
	Ok(())
}

pub(in crate::compiler_driver) fn code_emission() -> io::Result<()>
{
	Ok(())
}
