use std::io::{self, BufRead};
use regex::Regex;
use unicode_segmentation::UnicodeSegmentation;

pub enum Token
{
	TokenIdentifier(String),
	TokenConstant(usize),
	TokenInt,
	TokenVoid,
	TokenReturn,
	TokenOpenParenthesis,
	TokenCloseParenthesis,
	TokenOpenBrace,
	TokenCloseBrace,
	TokenSemicolon
}

pub fn run_lexer(input_file_path: &std::path::Path, out_lexer_tokens: &mut Vec<Token>) -> io::Result<()>
{
	let file = std::fs::File::open(input_file_path).expect("Failed to open input file");
	let reader = io::BufReader::new(file);
	let identifier = Regex::new(r"^[a-zA-Z_]\w*").expect("this doesn't define a regex");
	let constant = Regex::new(r"\d+\b").expect("this doesn't define a regex");
	let whitespace = Regex::new(r"\s+").expect("this doesn't define a regex");

	for line in reader.lines()
	{
		let line = line.expect("Failed to read line");
		for token in line.split_word_bounds()
		{
			if whitespace.is_match(&token)
			{
				continue;
			}

			let found_token: Option<Token> = match token
			{
				"int" => Some(Token::TokenInt),
				"void" => Some(Token::TokenVoid),
				"return"=> Some(Token::TokenReturn),
				"("=> Some(Token::TokenOpenParenthesis),
				")"=> Some(Token::TokenCloseParenthesis),
				"{" => Some(Token::TokenOpenBrace),
				"}" => Some(Token::TokenCloseBrace),
				";" => Some(Token::TokenSemicolon),
				_ if identifier.is_match(&token) => { Some(Token::TokenIdentifier(token.to_string())) }
				_ if constant.is_match(&token) => { Some(Token::TokenConstant(token.parse::<usize>().unwrap())) }
				_ => None
			};

			out_lexer_tokens.push(
			found_token.ok_or_else(|| {
					io::Error::new(
						io::ErrorKind::InvalidData,
						format!("Unrecognized token: {}", token)
					)
				})?
			);
		}
	}

	Ok(())
}
