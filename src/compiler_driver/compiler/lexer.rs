use std::io::{self, BufRead, Error};
use regex::Regex;
use unicode_segmentation::UnicodeSegmentation;
use crate::compiler_driver::compiler::lexer::Token::{TokenCloseBrace, TokenCloseParenthesis, TokenConstant, TokenIdentifier, TokenInt, TokenOpenBrace, TokenOpenParenthesis, TokenReturn, TokenSemicolon, TokenVoid};

enum Token
{
	TokenIdentifier(String),
	TokenConstant(String),
	TokenInt,
	TokenVoid,
	TokenReturn,
	TokenOpenParenthesis,
	TokenCloseParenthesis,
	TokenOpenBrace,
	TokenCloseBrace,
	TokenSemicolon
}

pub fn run_lexer(input_file_path: &std::path::Path) -> io::Result<()>
{
	let file = std::fs::File::open(input_file_path).expect("Failed to open input file");
	let reader = io::BufReader::new(file);
	let identifier = Regex::new(r"[a-zA-Z_]\w*").expect("this doesn't define a regex");
	let constant = Regex::new(r"\d+").expect("this doesn't define a regex");
	let whitespace = Regex::new(r"\s+").expect("this doesn't define a regex");

	let mut tokens: Vec<Token> = vec![];
	let mut parenthesis_count: i32 = 0;
	let mut brace_count: i32 = 0;

	for line in reader.lines()
	{
		let line = line.expect("Failed to read line");
		for token in line.split_word_bounds()
		{
			if whitespace.is_match(&token)
			{
				continue;
			}

			let foundToken : Option<Token> = match token
			{
				"int" => Some(TokenInt),
				"void" => Some(TokenVoid),
				"return"=> Some(TokenReturn),
				"("=> { parenthesis_count += 1; Some(TokenOpenParenthesis) },
				")"=> { parenthesis_count -= 1; Some(TokenCloseParenthesis) },
				"{" => { brace_count += 1; Some(TokenOpenBrace) },
				"}" => { brace_count -= 1; Some(TokenCloseBrace) },
				";" => Some(TokenSemicolon),
				_ if identifier.is_match(&token) => { Some(TokenIdentifier(token.to_string())) }
				_ if constant.is_match(&token) => { Some(TokenConstant(token.to_string())) }
				_ => None
			};

			tokens.push(
			foundToken.ok_or_else(|| {
					io::Error::new(
						io::ErrorKind::InvalidData,
						format!("Unrecognized token: {}", token)
					)
				})?
			);
		}
	}

	if parenthesis_count > 0 { return Err(Error::new(io::ErrorKind::InvalidData,format!("Missing close parenthesis ')'")))}
	if parenthesis_count < 0 { return Err(Error::new(io::ErrorKind::InvalidData,format!("Missing open parenthesis '('")))}
	if brace_count > 0 { return Err(Error::new(io::ErrorKind::InvalidData,format!("Missing close brace '}}'")))}
	if brace_count < 0 { return Err(Error::new(io::ErrorKind::InvalidData,format!("Missing open brace '{{'")))}

	Ok(())
}
