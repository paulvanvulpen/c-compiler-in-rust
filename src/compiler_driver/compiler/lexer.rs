use anyhow::Context;
use regex::Regex;
use std::io::{self, BufRead};
use std::path::Path;

use lazy_static::lazy_static;

#[derive(Clone, Debug)]
pub enum Token {
    Identifier(String),
    Constant(usize),
    Int,
    Void,
    If,
    Else,
    Return,
    Do,
    While,
    For,
    Break,
    Continue,
    Goto,
    OpenParenthesis,
    CloseParenthesis,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Tilde,
    Hyphen,
    DoubleHyphen,
    DoublePlus,
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
    PlusEqual,
    HyphenEqual,
    AsteriskEqual,
    ForwardSlashEqual,
    PercentSignEqual,
    AmpersandEqual,
    PipeEqual,
    CaretEqual,
    DoubleOpenAngleBracketEqual,
    DoubleCloseAngleBracketEqual,
    QuestionMark,
    Colon,
    Switch,
    Case,
    Default,
    Comma,
}

lazy_static! {
    static ref IDENTIFIER_REGEX: Regex = Regex::new(r"^[A-Za-z_]\w*\b").unwrap();
    static ref CONSTANT_REGEX: Regex = Regex::new(r"^\d+\b").unwrap();
    static ref DECREMENT_OPERATOR_REGEX: Regex = Regex::new(r"^--").unwrap();
    static ref INCREMENT_OPERATOR_REGEX: Regex = Regex::new(r"^\+\+").unwrap();
    static ref LOGICAL_OR_REGEX: Regex = Regex::new(r"^\|\|").unwrap();
    static ref LOGICAL_AND_REGEX: Regex = Regex::new(r"^&&").unwrap();
    static ref SHIFT_LEFT_ASSIGNMENT_REGEX: Regex = Regex::new(r"^<<=").unwrap();
    static ref SHIFT_RIGHT_ASSIGNMENT_REGEX: Regex = Regex::new(r"^>>=").unwrap();
    static ref SHIFT_LEFT_REGEX: Regex = Regex::new(r"^<<").unwrap();
    static ref SHIFT_RIGHT_REGEX: Regex = Regex::new(r"^>>").unwrap();
    static ref LOGICAL_EQUAL_REGEX: Regex = Regex::new(r"^==").unwrap();
    static ref LOGICAL_LESS_THAN_EQUAL_REGEX: Regex = Regex::new(r"^<=").unwrap();
    static ref LOGICAL_MORE_THAN_EQUAL_REGEX: Regex = Regex::new(r"^>=").unwrap();
    static ref LOGICAL_NOT_EQUAL_REGEX: Regex = Regex::new(r"^!=").unwrap();
    static ref ADDITION_ASSIGNMENT_REGEX: Regex = Regex::new(r"^\+=").unwrap();
    static ref SUBTRACTION_ASSIGNMENT_REGEX: Regex = Regex::new(r"^-=").unwrap();
    static ref MULTIPLICATION_ASSIGNMENT_REGEX: Regex = Regex::new(r"^\*=").unwrap();
    static ref DIVISION_ASSIGNMENT_REGEX: Regex = Regex::new(r"^/=").unwrap();
    static ref MODULO_ASSIGNMENT_REGEX: Regex = Regex::new(r"^%=").unwrap();
    static ref BITWISE_AND_ASSIGNMENT_REGEX: Regex = Regex::new(r"^&=").unwrap();
    static ref BITWISE_OR_ASSIGNMENT_REGEX: Regex = Regex::new(r"^\|=").unwrap();
    static ref BITWISE_XOR_ASSIGNMENT_REGEX: Regex = Regex::new(r"^\^=").unwrap();
}

fn lex(partial_line: &str) -> (Option<Token>, &str) {
    let partial_line = partial_line.trim();

    if let Some(token) = IDENTIFIER_REGEX.find(&partial_line) {
        let (token_str, remainder) = partial_line.split_at(token.end());
        return match token_str {
            "int" => (Some(Token::Int), remainder),
            "void" => (Some(Token::Void), remainder),
            "return" => (Some(Token::Return), remainder),
            "if" => (Some(Token::If), remainder),
            "else" => (Some(Token::Else), remainder),
            "do" => (Some(Token::Do), remainder),
            "while" => (Some(Token::While), remainder),
            "for" => (Some(Token::For), remainder),
            "break" => (Some(Token::Break), remainder),
            "continue" => (Some(Token::Continue), remainder),
            "goto" => (Some(Token::Goto), remainder),
            "switch" => (Some(Token::Switch), remainder),
            "case" => (Some(Token::Case), remainder),
            "default" => (Some(Token::Default), remainder),
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
    if let Some(token) = INCREMENT_OPERATOR_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::DoublePlus), remainder);
    }
    if let Some(token) = LOGICAL_OR_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::DoublePipe), remainder);
    }
    if let Some(token) = LOGICAL_AND_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::DoubleAmpersand), remainder);
    }
    if let Some(token) = SHIFT_LEFT_ASSIGNMENT_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::DoubleOpenAngleBracketEqual), remainder);
    }
    if let Some(token) = SHIFT_RIGHT_ASSIGNMENT_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::DoubleCloseAngleBracketEqual), remainder);
    }
    if let Some(token) = SHIFT_LEFT_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::DoubleOpenAngleBracket), remainder);
    }
    if let Some(token) = SHIFT_RIGHT_REGEX.find(&partial_line) {
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
    if let Some(token) = ADDITION_ASSIGNMENT_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::PlusEqual), remainder);
    }
    if let Some(token) = SUBTRACTION_ASSIGNMENT_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::HyphenEqual), remainder);
    }
    if let Some(token) = MULTIPLICATION_ASSIGNMENT_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::AsteriskEqual), remainder);
    }
    if let Some(token) = DIVISION_ASSIGNMENT_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::ForwardSlashEqual), remainder);
    }
    if let Some(token) = MODULO_ASSIGNMENT_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::PercentSignEqual), remainder);
    }
    if let Some(token) = BITWISE_AND_ASSIGNMENT_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::AmpersandEqual), remainder);
    }
    if let Some(token) = BITWISE_OR_ASSIGNMENT_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::PipeEqual), remainder);
    }
    if let Some(token) = BITWISE_XOR_ASSIGNMENT_REGEX.find(&partial_line) {
        let (_token_str, remainder) = partial_line.split_at(token.end());
        return (Some(Token::CaretEqual), remainder);
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
            "?" => (Some(Token::QuestionMark), remainder),
            ":" => (Some(Token::Colon), remainder),
            "," => (Some(Token::Comma), remainder),
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
