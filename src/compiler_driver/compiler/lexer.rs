use anyhow::Context;
use regex::Regex;
use std::io::{self, BufRead};
use std::path::Path;

use lazy_static::lazy_static;

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
    Plus,
    Asterisk,
    ForwardSlash,
    PercentSign,
    Pipe,
    DoublePipe,
    Ampersand,
    DoubleAmpersand,
    Caret,
    OpenAngleBracket,
    DoubleOpenAngleBracket,
    CloseAngleBracket,
    DoubleCloseAngleBracket,
    Exclamation,
    Equal,
    DoubleEqual,
    ExclamationEqual,
    OpenAngleBracketEqual,
    CloseAngleBracketEqual,
}

lazy_static! {
    static ref IDENTIFIER_REGEX: Regex = Regex::new(r"^[A-Za-z_]\w*\b").unwrap();
    static ref CONSTANT_REGEX: Regex = Regex::new(r"^\d+\b").unwrap();
    static ref DECREMENT_OPERATOR_REGEX: Regex = Regex::new(r"^--").unwrap();
    static ref LOGICAL_OR_REGEX: Regex = Regex::new(r"^\|\|").unwrap();
    static ref LOGICAL_AND_REGEX: Regex = Regex::new(r"^&&").unwrap();
    static ref LOGICAL_SHIFT_LEFT_REGEX: Regex = Regex::new(r"^<<").unwrap();
    static ref LOGICAL_SHIFT_RIGHT_REGEX: Regex = Regex::new(r"^>>").unwrap();
    static ref LOGICAL_EQUAL_REGEX: Regex = Regex::new(r"^==").unwrap();
    static ref LOGICAL_LESS_THAN_EQUAL_REGEX: Regex = Regex::new(r"^<=").unwrap();
    static ref LOGICAL_MORE_THAN_EQUAL_REGEX: Regex = Regex::new(r"^>=").unwrap();
    static ref LOGICAL_NOT_EQUAL_REGEX: Regex = Regex::new(r"^!=").unwrap();
}

fn lex(partial_line: &str) -> (Option<Token>, &str) {
    let partial_line = partial_line.trim();

    if let Some(token) = IDENTIFIER_REGEX.find(&partial_line) {
        let (token_str, remainder) = partial_line.split_at(token.end());
        return match token_str {
            "int" => (Some(Token::Int), remainder),
            "void" => (Some(Token::Void), remainder),
            "return" => (Some(Token::Return), remainder),
            _ => (Some(Token::Identifier(String::from(token_str))), remainder),
        };
    }

    if let Some(token) = CONSTANT_REGEX.find(&partial_line) {
        let (token_str, remainder) = partial_line.split_at(token.end());
        return (
            Some(Token::Constant(token_str.parse::<usize>().unwrap())),
            remainder,
        );
    }

    if let Some(token) = DECREMENT_OPERATOR_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::DoubleHyphen), remainder);
    }

    if let Some(token) = LOGICAL_OR_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::DoublePipe), remainder);
    }

    if let Some(token) = LOGICAL_AND_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::DoubleAmpersand), remainder);
    }

    if let Some(token) = LOGICAL_SHIFT_LEFT_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::DoubleOpenAngleBracket), remainder);
    }

    if let Some(token) = LOGICAL_SHIFT_RIGHT_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::DoubleCloseAngleBracket), remainder);
    }

    if let Some(token) = LOGICAL_EQUAL_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::DoubleEqual), remainder);
    }
    if let Some(token) = LOGICAL_LESS_THAN_EQUAL_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::OpenAngleBracketEqual), remainder);
    }
    if let Some(token) = LOGICAL_MORE_THAN_EQUAL_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::CloseAngleBracketEqual), remainder);
    }
    if let Some(token) = LOGICAL_NOT_EQUAL_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::ExclamationEqual), remainder);
    }
    if !partial_line.is_empty() {
        let (token_str, remainder) = partial_line.split_at(1);
        return match token_str {
            "(" => (Some(Token::OpenParenthesis), remainder),
            ")" => (Some(Token::CloseParenthesis), remainder),
            "{" => (Some(Token::OpenBrace), remainder),
            "}" => (Some(Token::CloseBrace), remainder),
            ";" => (Some(Token::Semicolon), remainder),
            "~" => (Some(Token::Tilde), remainder),
            "-" => (Some(Token::Hyphen), remainder),
            "+" => (Some(Token::Plus), remainder),
            "*" => (Some(Token::Asterisk), remainder),
            "/" => (Some(Token::ForwardSlash), remainder),
            "%" => (Some(Token::PercentSign), remainder),
            "|" => (Some(Token::Pipe), remainder),
            "&" => (Some(Token::Ampersand), remainder),
            "^" => (Some(Token::Caret), remainder),
            "<" => (Some(Token::OpenAngleBracket), remainder),
            ">" => (Some(Token::CloseAngleBracket), remainder),
            "!" => (Some(Token::Exclamation), remainder),
            "=" => (Some(Token::Equal), remainder),
            _ => (None, remainder),
        };
    }

    (None, partial_line)
}

pub fn run_lexer(input_file_path: &Path) -> anyhow::Result<Vec<Token>> {
    let file = std::fs::File::open(input_file_path)
        .with_context(|| format!("opening `{}`", input_file_path.display()))?;
    let reader = io::BufReader::new(file);
    let mut out_lexer_tokens = vec![];

    for line in reader.lines() {
        let line = line.context("Reading a line in the input file.")?;
        let mut remainder = line.as_str();

        while !remainder.is_empty() {
            let (token, new_remainder) = lex(&remainder);
            out_lexer_tokens
                .push(token.ok_or_else(|| anyhow::anyhow!("Unrecognized token `{}`", remainder))?);
            remainder = new_remainder;
        }
    }

    Ok(out_lexer_tokens)
}
