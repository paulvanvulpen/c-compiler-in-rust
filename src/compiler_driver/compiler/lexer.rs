use regex::Regex;
use std::io::{self, BufRead};
use unicode_segmentation::UnicodeSegmentation;

#[derive(Clone)]
pub enum Token {
    TokenIdentifier(String),
    TokenConstant(usize),
    TokenInt,
    TokenVoid,
    TokenReturn,
    TokenOpenParenthesis,
    TokenCloseParenthesis,
    TokenOpenBrace,
    TokenCloseBrace,
    TokenSemicolon,
    TokenTilde,
    TokenHyphen,
    TokenDoubleHyphen,
}

pub fn run_lexer(
    input_file_path: &std::path::Path,
    out_lexer_tokens: &mut Vec<Token>,
) -> io::Result<()> {
    let file = std::fs::File::open(input_file_path).expect("Failed to open input file");
    let reader = io::BufReader::new(file);
    let identifier = Regex::new(r"^[a-zA-Z_]\w*").expect("this doesn't define a regex");
    let constant = Regex::new(r"\d+\b").expect("this doesn't define a regex");
    let whitespace = Regex::new(r"\s+").expect("this doesn't define a regex");
    let mut last_found_token: Option<Token> = None;

    for line in reader.lines() {
        let line = line.expect("Failed to read line");
        for token in line.split_word_bounds() {
            if let Some(prev_token) = &last_found_token {
                match (prev_token, token) {
                    (Token::TokenHyphen, "-") => {
                        last_found_token = Some(Token::TokenHyphen);
                        if let Some(last_found_lexer_token) = out_lexer_tokens.last_mut() {
                            *last_found_lexer_token = Token::TokenDoubleHyphen;
                        }
                        continue;
                    }
                    (Token::TokenDoubleHyphen, "-") => {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!("Unrecognized token: {}", token),
                        ));
                    }
                    _ => {}
                }
            }

            if whitespace.is_match(&token) {
                last_found_token = None;
                continue;
            }

            let found_token: Option<Token> = match token {
                "int" => Some(Token::TokenInt),
                "void" => Some(Token::TokenVoid),
                "return" => Some(Token::TokenReturn),
                "(" => Some(Token::TokenOpenParenthesis),
                ")" => Some(Token::TokenCloseParenthesis),
                "{" => Some(Token::TokenOpenBrace),
                "}" => Some(Token::TokenCloseBrace),
                ";" => Some(Token::TokenSemicolon),
                "~" => Some(Token::TokenTilde),
                "-" => Some(Token::TokenHyphen),
                _ if identifier.is_match(&token) => Some(Token::TokenIdentifier(token.to_string())),
                _ if constant.is_match(&token) => {
                    Some(Token::TokenConstant(token.parse::<usize>().unwrap()))
                }
                _ => None,
            };

            last_found_token = found_token.clone();
            out_lexer_tokens.push(found_token.ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("Unrecognized token: {}", token),
                )
            })?);
        }
    }

    Ok(())
}
