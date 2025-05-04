use super::lexer::Token;
use anyhow::{Error, Result, anyhow};
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
    BinaryOperation {
        binary_operator: BinaryOperator,
        left_operand: Box<Expression>,
        right_operand: Box<Expression>,
    },
}

// <unop>
pub enum UnaryOperator {
    Complement,
    Negate,
}

// <binop>
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    LeftShift,
    RightShift,
    BitwiseAnd,
    BitwiseXOr,
    BitwiseOr,
}

impl BinaryOperator {
    fn precedence(&self) -> u8 {
        match self {
            BinaryOperator::Multiply => 50,
            BinaryOperator::Divide => 50,
            BinaryOperator::Remainder => 50,
            BinaryOperator::Add => 45,
            BinaryOperator::Subtract => 45,
            BinaryOperator::LeftShift => 40,
            BinaryOperator::RightShift => 40,
            BinaryOperator::BitwiseAnd => 35,
            BinaryOperator::BitwiseXOr => 34,
            BinaryOperator::BitwiseOr => 33,
        }
    }
}

// <program> ::= <function>
fn parse_program(lexer_tokens: &[Token]) -> Result<Program> {
    match parse_function(lexer_tokens) {
        Ok(function_definition) => Ok(Program::Program(function_definition)),
        Err(e) => Err(e),
    }
}

// <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
fn parse_function(lexer_tokens: &[Token]) -> Result<FunctionDefinition> {
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
fn parse_identifier(lexer_tokens: &[Token]) -> Result<(String, &[Token])> {
    match &lexer_tokens[0] {
        Token::Identifier(identifier) => Ok((identifier.clone(), &lexer_tokens[1..])),
        _ => Err(fail()),
    }
}

// <statement> ::= "return" <exp> ";"
fn parse_statement(lexer_tokens: &[Token]) -> Result<(Statement, &[Token])> {
    let lexer_tokens = expect(Token::Return, lexer_tokens)?;
    let (expression, lexer_tokens) = parse_expression(lexer_tokens, 0)?;
    let lexer_tokens = expect(Token::Semicolon, lexer_tokens)?;
    Ok((Statement::Return(expression), lexer_tokens))
}

fn get_binary_operator_precedence(token: &Token) -> Option<u8> {
    match token {
        Token::Hyphen => Some(BinaryOperator::Subtract.precedence()),
        Token::Plus => Some(BinaryOperator::Add.precedence()),
        Token::Asterisk => Some(BinaryOperator::Multiply.precedence()),
        Token::ForwardSlash => Some(BinaryOperator::Divide.precedence()),
        Token::PercentSign => Some(BinaryOperator::Remainder.precedence()),
        Token::DoubleOpenAngleBracket => Some(BinaryOperator::LeftShift.precedence()),
        Token::DoubleCloseAngleBracket => Some(BinaryOperator::RightShift.precedence()),
        Token::Ampersand => Some(BinaryOperator::BitwiseAnd.precedence()),
        Token::Caret => Some(BinaryOperator::BitwiseXOr.precedence()),
        Token::Pipe => Some(BinaryOperator::BitwiseOr.precedence()),
        _ => None,
    }
}

// <factor> ::= <int> | <unop> <factor> | "(" <exp> ")"
// <int> ::= ? A constant token ?
fn parse_factor(lexer_tokens: &[Token]) -> Result<(Expression, &[Token])> {
    match &lexer_tokens[0] {
        Token::Tilde | Token::Hyphen => {
            let (unary_op, lexer_tokens) = parse_unary_operator(&lexer_tokens)?;
            let (factor, lexer_tokens) = parse_factor(lexer_tokens)?;
            Ok((Expression::Unary(unary_op, Box::new(factor)), lexer_tokens))
        }
        Token::Constant(constant) => Ok((Expression::Constant(*constant), &lexer_tokens[1..])),
        Token::OpenParenthesis => {
            let lexer_tokens = expect(Token::OpenParenthesis, lexer_tokens)?;
            let (expression, lexer_tokens) = parse_expression(lexer_tokens, 0)?;
            let lexer_tokens = expect(Token::CloseParenthesis, lexer_tokens)?;
            Ok((expression, lexer_tokens))
        }
        _ => Err(fail()),
    }
}

// <exp> ::= <factor> | <exp> <binop> <exp>
fn parse_expression(lexer_tokens: &[Token], min_precedence: u8) -> Result<(Expression, &[Token])> {
    let (mut left, mut lexer_tokens) = parse_factor(lexer_tokens)?;
    let mut right;
    let mut binary_operator;
    while get_binary_operator_precedence(&lexer_tokens[0]) >= Some(min_precedence) {
        (binary_operator, lexer_tokens) = parse_binary_operator(&lexer_tokens)?;
        (right, lexer_tokens) = parse_expression(lexer_tokens, binary_operator.precedence() + 1)?;
        left = Expression::BinaryOperation {
            binary_operator,
            left_operand: Box::new(left),
            right_operand: Box::new(right),
        };
    }

    Ok((left, lexer_tokens))
}

fn parse_binary_operator(lexer_tokens: &[Token]) -> Result<(BinaryOperator, &[Token])> {
    match &lexer_tokens[0] {
        Token::Plus => Ok((BinaryOperator::Add, &lexer_tokens[1..])),
        Token::Hyphen => Ok((BinaryOperator::Subtract, &lexer_tokens[1..])),
        Token::Asterisk => Ok((BinaryOperator::Multiply, &lexer_tokens[1..])),
        Token::ForwardSlash => Ok((BinaryOperator::Divide, &lexer_tokens[1..])),
        Token::PercentSign => Ok((BinaryOperator::Remainder, &lexer_tokens[1..])),
        Token::DoubleOpenAngleBracket => Ok((BinaryOperator::LeftShift, &lexer_tokens[1..])),
        Token::DoubleCloseAngleBracket => Ok((BinaryOperator::RightShift, &lexer_tokens[1..])),
        Token::Ampersand => Ok((BinaryOperator::BitwiseAnd, &lexer_tokens[1..])),
        Token::Caret => Ok((BinaryOperator::BitwiseXOr, &lexer_tokens[1..])),
        Token::Pipe => Ok((BinaryOperator::BitwiseOr, &lexer_tokens[1..])),
        _ => Err(fail()),
    }
}

// <unop> ::= "-" | "~"
fn parse_unary_operator(lexer_tokens: &[Token]) -> Result<(UnaryOperator, &[Token])> {
    match &lexer_tokens[0] {
        Token::Hyphen => Ok((UnaryOperator::Negate, &lexer_tokens[1..])),
        Token::Tilde => Ok((UnaryOperator::Complement, &lexer_tokens[1..])),
        _ => Err(fail()),
    }
}

fn expect(expected: Token, lexer_tokens: &[Token]) -> Result<&[Token]> {
    match lexer_tokens {
        t if t.is_empty() || (discriminant(&expected) != discriminant(&lexer_tokens[0])) => {
            Err(fail())
        }
        _ => Ok(&lexer_tokens[1..]),
    }
}

fn fail() -> Error {
    anyhow!("Syntax error")
}

pub fn run_parser(lexer_tokens: &[Token]) -> Result<AbstractSyntaxTree> {
    let program = parse_program(lexer_tokens)?;
    Ok(AbstractSyntaxTree::Program(program))
}
