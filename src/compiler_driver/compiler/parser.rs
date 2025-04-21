use super::lexer::Token;
use anyhow::anyhow;
use std::mem::discriminant;

mod visualize;

// Implementation AST Nodes in Zephyr Abstract Syntax Description Language (ASDL)
// program = Program(function_definition)
// function_definition = Function(identifier name, statement body)
// statement = Return(exp)
// exp = Constant(int) | Unary(unary_operator, exp)
// unary_operator = Complement | Negate
pub enum AbstractSyntaxTree {
    Program(Program),
}

// Comments on the enums in Backus-Naur form (EBNF)

// <program>
pub enum Program {
    Program(FunctionDefinition),
}

// <function>
pub enum FunctionDefinition {
    Function { identifier: String, body: Statement },
}

// <statement>
pub enum Statement {
    Return(Expression),
}

// <exp>
pub enum Expression {
    Constant(usize),
    Unary(UnaryOperator, Box<Expression>),
}

// <unop>
pub enum UnaryOperator {
    Complement,
    Negate,
}

// <program> ::= <function>
fn parse_program(lexer_tokens: &[Token]) -> anyhow::Result<Program> {
    match parse_function(lexer_tokens) {
        Ok(function_definition) => Ok(Program::Program(function_definition)),
        Err(e) => Err(e),
    }
}

// <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
fn parse_function(lexer_tokens: &[Token]) -> anyhow::Result<FunctionDefinition> {
    let lexer_tokens = expect(Token::Int, lexer_tokens)?;
    let (identifier, lexer_tokens) = parse_identifier(lexer_tokens)?;
    let lexer_tokens = expect(Token::OpenParenthesis, lexer_tokens)?;
    let lexer_tokens = expect(Token::Void, lexer_tokens)?;
    let lexer_tokens = expect(Token::CloseParenthesis, lexer_tokens)?;
    let lexer_tokens = expect(Token::OpenBrace, lexer_tokens)?;
    let (body, lexer_tokens) = parse_statement(lexer_tokens)?;
    let lexer_tokens = expect(Token::CloseBrace, lexer_tokens)?;

    match lexer_tokens {
        t if t.is_empty() => Ok(FunctionDefinition::Function { identifier, body }),
        _ => Err(fail()),
    }
}

// <identifier> ::= ? An identifier token ?
fn parse_identifier(lexer_tokens: &[Token]) -> anyhow::Result<(String, &[Token])> {
    match &lexer_tokens[0] {
        Token::Identifier(identifier) => Ok((identifier.clone(), &lexer_tokens[1..])),
        _ => Err(fail()),
    }
}

// <statement> ::= "return" <exp> ";"
fn parse_statement(lexer_tokens: &[Token]) -> anyhow::Result<(Statement, &[Token])> {
    let lexer_tokens = expect(Token::Return, lexer_tokens)?;
    let (exp, lexer_tokens) = parse_exp(lexer_tokens)?;
    let lexer_tokens = expect(Token::Semicolon, lexer_tokens)?;
    Ok((Statement::Return(exp), lexer_tokens))
}

// <exp> ::= <int> | <unop> <exp> | "(" <exp> ")"
// <int> ::= ? A constant token ?
fn parse_exp(lexer_tokens: &[Token]) -> anyhow::Result<(Expression, &[Token])> {
    match &lexer_tokens[0] {
        Token::Tilde | Token::Hyphen => {
            let (unary_op, lexer_tokens) = parse_unary_operator(&lexer_tokens)?;
            let (expr, lexer_tokens) = parse_exp(lexer_tokens)?;
            Ok((Expression::Unary(unary_op, Box::new(expr)), lexer_tokens))
        }
        Token::Constant(constant) => Ok((Expression::Constant(*constant), &lexer_tokens[1..])),
        Token::OpenParenthesis => {
            let lexer_tokens = expect(Token::OpenParenthesis, lexer_tokens)?;
            let (exp, lexer_tokens) = parse_exp(lexer_tokens)?;
            let lexer_tokens = expect(Token::CloseParenthesis, lexer_tokens)?;
            Ok((exp, lexer_tokens))
        }
        _ => Err(fail()),
    }
}

// <unop> ::= "-" | "~"
fn parse_unary_operator(lexer_tokens: &[Token]) -> anyhow::Result<(UnaryOperator, &[Token])> {
    match &lexer_tokens[0] {
        Token::Hyphen => Ok((UnaryOperator::Negate, &lexer_tokens[1..])),
        Token::Tilde => Ok((UnaryOperator::Complement, &lexer_tokens[1..])),
        _ => Err(fail()),
    }
}

fn expect(expected: Token, lexer_tokens: &[Token]) -> anyhow::Result<&[Token]> {
    match lexer_tokens {
        t if t.is_empty() || (discriminant(&expected) != discriminant(&lexer_tokens[0])) => {
            Err(fail())
        }
        _ => Ok(&lexer_tokens[1..]),
    }
}

fn fail() -> anyhow::Error {
    anyhow!("Syntax error")
}

pub fn run_parser(lexer_tokens: &[Token]) -> anyhow::Result<AbstractSyntaxTree> {
    let program = parse_program(lexer_tokens)?;
    Ok(AbstractSyntaxTree::Program(program))
}
