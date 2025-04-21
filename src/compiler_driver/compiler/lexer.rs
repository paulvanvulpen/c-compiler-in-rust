use anyhow::Context;
use regex::Regex;
use std::io::{self, BufRead};
use std::path::Path;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Clone)]
pub enum Token {
    Identifier(String),
    Constant(usize),
    Int,
    Void,
    Return,
    OpenParenthesis,
    CloseParenthesis,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Tilde,
    Hyphen,
    DoubleHyphen,
}

pub fn run_lexer(input_file_path: &Path) -> anyhow::Result<Vec<Token>> {
    let file = std::fs::File::open(input_file_path)
        .with_context(|| format!("opening `{}`", input_file_path.display()))?;
    let reader = io::BufReader::new(file);
    let identifier = Regex::new(r"^[a-zA-Z_]\w*").expect("this doesn't define a regex");
    let constant = Regex::new(r"\d+\b").expect("this doesn't define a regex");
    let whitespace = Regex::new(r"\s+").expect("this doesn't define a regex");
    let mut last_found_token: Option<Token> = None;
    let mut out_lexer_tokens = vec![];

    for line in reader.lines() {
        let line = line.expect("Failed to read line");
        for token in line.split_word_bounds() {
            if let Some(prev_token) = &last_found_token {
                match (prev_token, token) {
                    (Token::Hyphen, "-") => {
                        last_found_token = Some(Token::Hyphen);
                        if let Some(last_found_lexer_token) = out_lexer_tokens.last_mut() {
                            *last_found_lexer_token = Token::DoubleHyphen;
                        }
                        continue;
                    }
                    (Token::DoubleHyphen, "-") => {
                        anyhow::bail!("Unrecognized token: {}", token)
                    }
                    _ => {}
                }
            }

            if whitespace.is_match(&token) {
                last_found_token = None;
                continue;
            }

            let found_token: Option<Token> = match token {
                "int" => Some(Token::Int),
                "void" => Some(Token::Void),
                "return" => Some(Token::Return),
                "(" => Some(Token::OpenParenthesis),
                ")" => Some(Token::CloseParenthesis),
                "{" => Some(Token::OpenBrace),
                "}" => Some(Token::CloseBrace),
                ";" => Some(Token::Semicolon),
                "~" => Some(Token::Tilde),
                "-" => Some(Token::Hyphen),
                _ if identifier.is_match(&token) => Some(Token::Identifier(token.to_string())),
                _ if constant.is_match(&token) => Some(Token::Constant(token.parse::<usize>()?)),
                _ => None,
            };

            last_found_token = found_token.clone();
            out_lexer_tokens.push(
                found_token.ok_or_else(|| anyhow::anyhow!("Unrecognized token `{}`", token))?,
            );
        }
    }

    Ok(out_lexer_tokens)
}
