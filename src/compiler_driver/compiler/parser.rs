use std::io;
use std::mem::discriminant;
use crate::compiler_driver::compiler::lexer::Token;

// Implementation AST Nodes in Zyphyr Abstract Syntax Description Language (ADSL)
// program = Program(function_definition)
// function_definition = Function(identifier name, statement body)
// statement = Return(exp)
// exp = Constant(int)
pub enum AbstractSyntaxTree
{
	Program(Program),
}

// Comments on the enums in Backus-Naur form (EBNF)

// <program>
pub enum Program
{
	Program(FunctionDefinition)
}

// <function>
pub enum FunctionDefinition
{
	Function{ identifier : String, body : Statement }
}

// <statement>
pub enum Statement
{
	Return(Expression),
}

// <exp>
pub enum Expression
{
	Constant(usize)
}

// <program> ::= <function>
fn parse_program(lexer_tokens : &[Token]) -> io::Result<Program>
{
	match parse_function(lexer_tokens)
	{
		Ok(function_definition) => Ok(Program::Program(function_definition)),
		Err(e) => Err(e)
	}
}

// <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
fn parse_function(lexer_tokens : &[Token]) -> io::Result<FunctionDefinition>
{
	let lexer_tokens = expect(Token::TokenInt, lexer_tokens)?;
	let (identifier, lexer_tokens) = parse_identifier(lexer_tokens)?;
	let lexer_tokens = expect(Token::TokenOpenParenthesis, lexer_tokens)?;
	let lexer_tokens = expect(Token::TokenVoid, lexer_tokens)?;
	let lexer_tokens = expect(Token::TokenCloseParenthesis, lexer_tokens)?;
	let lexer_tokens = expect(Token::TokenOpenBrace, lexer_tokens)?;
	let (body, lexer_tokens) = parse_statement(lexer_tokens)?;

	let lexer_tokens = expect(Token::TokenCloseBrace, lexer_tokens)?;

	match lexer_tokens
	{
		t if t.is_empty() => Ok(FunctionDefinition::Function { identifier, body }),
		_ => Err(fail()),
	}
}

// <identifier> ::= ? An identifier token ?
fn parse_identifier(lexer_tokens : &[Token]) -> io::Result<(String, &[Token])>
{
	match &lexer_tokens[0]
	{
		Token::TokenIdentifier(identifier) => Ok((identifier.clone(), &lexer_tokens[1..])),
		_ => Err(fail())
	}
}

// <statement> ::= "return" <exp> ";"
fn parse_statement(lexer_tokens : &[Token]) -> io::Result<(Statement, &[Token])>
{
	let lexer_tokens = expect(Token::TokenReturn, lexer_tokens)?;
	let (exp, lexer_tokens) = parse_exp(lexer_tokens)?;

	let lexer_tokens = expect(Token::TokenSemicolon, lexer_tokens)?;
	Ok((Statement::Return(exp), lexer_tokens))
}

// <exp> ::= <int>
// <int> ::= ? A constant token ?
fn parse_exp(lexer_tokens : &[Token]) -> io::Result<(Expression, &[Token])>
{
	match &lexer_tokens[0]
	{
		Token::TokenConstant(constant) => Ok((Expression::Constant(*constant), &lexer_tokens[1..])),
		_ => Err(fail())
	}
}

fn expect(expected: Token, lexer_tokens: &[Token]) -> io::Result<&[Token]>
{
	match lexer_tokens
	{
		t if t.is_empty() || (discriminant(&expected) != discriminant(&lexer_tokens[0])) => Err(fail()),
		_ =>  Ok(&lexer_tokens[1..]),
	}
}

fn fail() -> io::Error
{
	io::Error::new(io::ErrorKind::InvalidData,"Syntax error")
}

pub fn run_parser(lexer_tokens : &[Token]) -> io::Result<AbstractSyntaxTree>
{
	let program = parse_program(lexer_tokens)?;
	Ok(AbstractSyntaxTree::Program(program))

	// Ok(
	// 	AST::Program(
	// 		Program::Program(
	// 			FunctionDefinition::Function
	// 			{
	// 				identifier : String::from("main"),
	// 				body: Statement::Return(
	// 					Expression::Constant(
	// 						2
	// 					)
	// 				)
	// 			}
	// 		)
	// 	)
	// )
}
