use super::lexer::Token;
use anyhow::{Error, Result, anyhow};
use std::mem::discriminant;

mod visualize;

// Implementation AST Nodes in Zephyr Abstract Syntax Description Language (ASDL)
// program = Program(function_definition)
// function_definition = Function(identifier name, block_item* body)
// statement = Return(exp) | Expression(exp) | Null
// declaration = Declaration(identifier name, exp? init)
// block_item = S(statement) | D(declaration)
// exp = Constant(int) | Var(identifier) | Unary(unary_operator, exp) | Binary(binary_operator, exp, exp) | Assignment(exp, exp)
// unary_operator = Complement | Negate | Not
// binary_operator = Add | Subtract | Multiply | Divide | Remainder | And | Or | Equal | NotEqual | LessThan | LessOrEqual | GreaterThan | GreaterOrEqual

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
    Function {
        identifier: String,
        body: Vec<BlockItem>,
    },
}

// <block-item>
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

// <declaration>
pub enum Declaration {
    Declaration {
        identifier: String,
        init: Option<Expression>,
    },
}

// <statement>
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    Null,
}

// <exp>
pub enum Expression {
    Constant(usize),
    Var {
        identifier: String,
    },
    Unary(UnaryOperator, Box<Expression>),
    BinaryOperation {
        binary_operator: BinaryOperator,
        left_operand: Box<Expression>,
        right_operand: Box<Expression>,
    },
    Assignment(Box<Expression>, Box<Expression>),
}

// <unop>
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
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
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterOrEqual,
    GreaterThan,
    Assign,
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
            BinaryOperator::LessThan => 37,
            BinaryOperator::LessOrEqual => 37,
            BinaryOperator::GreaterThan => 37,
            BinaryOperator::GreaterOrEqual => 37,
            BinaryOperator::Equal => 36,
            BinaryOperator::NotEqual => 36,
            BinaryOperator::BitwiseAnd => 35,
            BinaryOperator::BitwiseXOr => 34,
            BinaryOperator::BitwiseOr => 33,
            BinaryOperator::And => 10,
            BinaryOperator::Or => 5,
            BinaryOperator::Assign => 1,
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

// <function> ::= "int" <identifier> "(" "void" ")" "{" { <block-item> } "}"
fn parse_function(lexer_tokens: &[Token]) -> Result<FunctionDefinition> {
    let lexer_tokens = expect(Token::Int, lexer_tokens)?;
    let (identifier, lexer_tokens) = parse_identifier(lexer_tokens)?;
    let lexer_tokens = expect(Token::OpenParenthesis, lexer_tokens)?;
    let lexer_tokens = expect(Token::Void, lexer_tokens)?;
    let lexer_tokens = expect(Token::CloseParenthesis, lexer_tokens)?;
    let mut lexer_tokens = expect(Token::OpenBrace, lexer_tokens)?;

    let mut body = vec![];
    while !matches!(&lexer_tokens[0], Token::CloseBrace) {
        let (block_item, updated_lexer_tokens) = parse_block_item(lexer_tokens)?;
        lexer_tokens = updated_lexer_tokens;
        body.push(block_item);
    }
    let lexer_tokens = &lexer_tokens[1..];
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

// <block-item> ::= <statement> | <declaration>
fn parse_block_item(lexer_tokens: &[Token]) -> Result<(BlockItem, &[Token])> {
    match &lexer_tokens[0] {
        Token::Int => {
            let (declaration, lexer_tokens) = parse_declaration(&lexer_tokens)?;
            Ok((BlockItem::Declaration(declaration), &lexer_tokens))
        }
        _ => {
            let (statement, lexer_tokens) = parse_statement(&lexer_tokens)?;
            Ok((BlockItem::Statement(statement), &lexer_tokens))
        }
    }
}

fn parse_declaration(lexer_tokens: &[Token]) -> Result<(Declaration, &[Token])> {
    let lexer_tokens = expect(Token::Int, lexer_tokens)?;
    let (identifier, lexer_tokens) = parse_identifier(lexer_tokens)?;
    if !matches!(lexer_tokens[0], Token::Semicolon) {
        let lexer_tokens = expect(Token::Equal, lexer_tokens)?;
        let (expression, lexer_tokens) = parse_expression(lexer_tokens, 0)?;
        let lexer_tokens = expect(Token::Semicolon, lexer_tokens)?;
        Ok((
            Declaration::Declaration {
                identifier,
                init: Some(expression),
            },
            lexer_tokens,
        ))
    } else {
        let lexer_tokens = expect(Token::Semicolon, lexer_tokens)?;
        Ok((
            Declaration::Declaration {
                identifier,
                init: None,
            },
            lexer_tokens,
        ))
    }
}

// <statement> ::= "return" <exp> ";" | <exp> ";" | ";"
fn parse_statement(lexer_tokens: &[Token]) -> Result<(Statement, &[Token])> {
    match lexer_tokens[0] {
        Token::Return => {
            let lexer_tokens = &lexer_tokens[1..];
            let (expression, lexer_tokens) = parse_expression(lexer_tokens, 0)?;
            let lexer_tokens = expect(Token::Semicolon, lexer_tokens)?;
            Ok((Statement::Return(expression), lexer_tokens))
        }
        Token::Semicolon => Ok((Statement::Null, &lexer_tokens[1..])),
        _ => {
            let (expression, lexer_tokens) = parse_expression(lexer_tokens, 0)?;
            let lexer_tokens = expect(Token::Semicolon, lexer_tokens)?;
            Ok((Statement::Expression(expression), &lexer_tokens))
        }
    }
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
        Token::DoubleAmpersand => Some(BinaryOperator::And.precedence()),
        Token::DoublePipe => Some(BinaryOperator::Or.precedence()),
        Token::DoubleEqual => Some(BinaryOperator::Equal.precedence()),
        Token::ExclamationEqual => Some(BinaryOperator::NotEqual.precedence()),
        Token::OpenAngleBracket => Some(BinaryOperator::LessThan.precedence()),
        Token::OpenAngleBracketEqual => Some(BinaryOperator::LessOrEqual.precedence()),
        Token::CloseAngleBracket => Some(BinaryOperator::GreaterThan.precedence()),
        Token::CloseAngleBracketEqual => Some(BinaryOperator::GreaterOrEqual.precedence()),
        Token::Equal => Some(BinaryOperator::Assign.precedence()),
        Token::Identifier(_)
        | Token::Constant(_)
        | Token::Int
        | Token::Void
        | Token::Return
        | Token::OpenParenthesis
        | Token::CloseParenthesis
        | Token::OpenBrace
        | Token::CloseBrace
        | Token::Semicolon
        | Token::Tilde
        | Token::DoubleHyphen
        | Token::Exclamation => None,
    }
}

// <factor> ::= <int> | <unop> <factor> | "(" <exp> ")"
// <int> ::= ? A constant token ?
fn parse_factor(lexer_tokens: &[Token]) -> Result<(Expression, &[Token])> {
    match &lexer_tokens[0] {
        Token::Tilde | Token::Hyphen | Token::Exclamation => {
            let (unary_op, lexer_tokens) = parse_unary_operator(&lexer_tokens)?;
            let (factor, lexer_tokens) = parse_factor(lexer_tokens)?;
            Ok((Expression::Unary(unary_op, Box::new(factor)), lexer_tokens))
        }
        Token::Constant(constant) => Ok((Expression::Constant(*constant), &lexer_tokens[1..])),
        Token::Identifier(identifier) => Ok((
            Expression::Var {
                identifier: identifier.clone(),
            },
            &lexer_tokens[1..],
        )),
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
        match &lexer_tokens[0] {
            Token::Equal => {
                lexer_tokens = &lexer_tokens[1..];
                (right, lexer_tokens) =
                    parse_expression(lexer_tokens, BinaryOperator::Assign.precedence())?;
                left = Expression::Assignment(Box::new(left), Box::new(right));
            }
            _ => {
                (binary_operator, lexer_tokens) = parse_binary_operator(&lexer_tokens)?;
                (right, lexer_tokens) =
                    parse_expression(lexer_tokens, binary_operator.precedence() + 1)?;
                left = Expression::BinaryOperation {
                    binary_operator,
                    left_operand: Box::new(left),
                    right_operand: Box::new(right),
                };
            }
        }
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
        Token::DoubleAmpersand => Ok((BinaryOperator::And, &lexer_tokens[1..])),
        Token::DoublePipe => Ok((BinaryOperator::Or, &lexer_tokens[1..])),
        Token::DoubleEqual => Ok((BinaryOperator::Equal, &lexer_tokens[1..])),
        Token::ExclamationEqual => Ok((BinaryOperator::NotEqual, &lexer_tokens[1..])),
        Token::OpenAngleBracket => Ok((BinaryOperator::LessThan, &lexer_tokens[1..])),
        Token::OpenAngleBracketEqual => Ok((BinaryOperator::LessOrEqual, &lexer_tokens[1..])),
        Token::CloseAngleBracket => Ok((BinaryOperator::GreaterThan, &lexer_tokens[1..])),
        Token::CloseAngleBracketEqual => Ok((BinaryOperator::GreaterOrEqual, &lexer_tokens[1..])),
        _ => Err(fail()),
    }
}

// <unop> ::= "-" | "~" | "!"
fn parse_unary_operator(lexer_tokens: &[Token]) -> Result<(UnaryOperator, &[Token])> {
    match &lexer_tokens[0] {
        Token::Hyphen => Ok((UnaryOperator::Negate, &lexer_tokens[1..])),
        Token::Tilde => Ok((UnaryOperator::Complement, &lexer_tokens[1..])),
        Token::Exclamation => Ok((UnaryOperator::Not, &lexer_tokens[1..])),
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
