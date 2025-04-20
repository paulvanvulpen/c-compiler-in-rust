use std::io;
use std::mem::discriminant;
use super::lexer::Token;
use super::visualize;

// Implementation AST Nodes in Zephyr Abstract Syntax Description Language (ASDL)
// program = Program(function_definition)
// function_definition = Function(identifier name, statement body)
// statement = Return(exp)
// exp = Constant(int) | Unary(unary_operator, exp)
// unary_operator = Complement | Negate
pub enum AbstractSyntaxTree
{
	Program(Program),
}

impl visualize::Visualizer for AbstractSyntaxTree
{
	fn visualize(&self, _depth : u8) -> String
	{
		let AbstractSyntaxTree::Program(program) = self;
		program.visualize(0)
	}
}

// Comments on the enums in Backus-Naur form (EBNF)

// <program>
pub enum Program
{
	Program(FunctionDefinition)
}

impl visualize::Visualizer for Program
{
	fn visualize(&self, depth : u8) -> String
	{
		let Program::Program(function_definition) = self;
		String::from(format!(
		"Program(\n\
		{}\n\
		)", function_definition.visualize(depth + 1)))
	}
}

// <function>
pub enum FunctionDefinition
{
	Function{ identifier : String, body : Statement }
}

impl visualize::Visualizer for FunctionDefinition
{
	fn visualize(&self, depth : u8) -> String
	{
		let FunctionDefinition::Function { identifier, body } = self;
		let prefix = "    ".repeat(depth as usize);
		format!(
			"{prefix}Function(\n\
			{prefix}    name={identifier}\n\
			{prefix}    body={}\n\
			{prefix})", body.visualize(depth + 1)
		)
	}
}

// <statement>
pub enum Statement
{
	Return(Expression),
}

impl visualize::Visualizer for Statement
{
	fn visualize(&self, depth : u8) -> String
	{
		let Statement::Return(expression) = self;
		{
			let prefix = "    ".repeat(depth as usize);
			format!(
				"Return(\n\
				{prefix}    {}\n\
				{prefix})", expression.visualize(depth + 1)
			)
		}
	}
}

// <exp>
pub enum Expression
{
	Constant(usize),
	Unary(UnaryOperator, Box<Expression>),
}

impl visualize::Visualizer for Expression {
	fn visualize(&self, depth : u8) -> String {
		match self {
			Expression::Constant(value) => {
				format!("Constant({value})")
			},
			Expression::Unary(unary_operator, boxed_expression) => {
				format!("{}{}", unary_operator.visualize(depth + 1), boxed_expression.visualize(depth + 1))
			},
		}
	}
}

// <unop>
pub enum UnaryOperator
{
	Complement,
	Negate
}

impl visualize::Visualizer for UnaryOperator {
	fn visualize(&self, _depth: u8) -> String
	{
		match self {
			UnaryOperator::Complement => String::from("~"),
			UnaryOperator::Negate => String::from("-"),
		}
	}
}

// <program> ::= <function>
fn parse_program(lexer_tokens : &[Token]) -> io::Result<Program> {
	match parse_function(lexer_tokens)
	{
		Ok(function_definition) => Ok(Program::Program(function_definition)),
		Err(e) => Err(e)
	}
}

// <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
fn parse_function(lexer_tokens : &[Token]) -> io::Result<FunctionDefinition> {
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
fn parse_identifier(lexer_tokens : &[Token]) -> io::Result<(String, &[Token])> {
	match &lexer_tokens[0]
	{
		Token::TokenIdentifier(identifier) => Ok((identifier.clone(), &lexer_tokens[1..])),
		_ => Err(fail())
	}
}

// <statement> ::= "return" <exp> ";"
fn parse_statement(lexer_tokens : &[Token]) -> io::Result<(Statement, &[Token])> {
	let lexer_tokens = expect(Token::TokenReturn, lexer_tokens)?;
	let (exp, lexer_tokens) = parse_exp(lexer_tokens)?;
	let lexer_tokens = expect(Token::TokenSemicolon, lexer_tokens)?;
	Ok((Statement::Return(exp), lexer_tokens))
}

// <exp> ::= <int> | <unop> <exp> | "(" <exp> ")"
// <int> ::= ? A constant token ?
fn parse_exp(lexer_tokens : &[Token]) -> io::Result<(Expression, &[Token])> {
	match &lexer_tokens[0] {
		Token::TokenTilde | Token::TokenHyphen => {
			let (unary_op, lexer_tokens) = parse_unary_operator(&lexer_tokens)?;
			let (expr, lexer_tokens) = parse_exp(lexer_tokens)?;
			Ok((Expression::Unary(unary_op, Box::new(expr)), lexer_tokens))
		}
		Token::TokenConstant(constant) => Ok((Expression::Constant(*constant), &lexer_tokens[1..])),
		Token::TokenOpenParenthesis => {
			let lexer_tokens = expect(Token::TokenOpenParenthesis, lexer_tokens)?;
			let (exp, lexer_tokens) = parse_exp(lexer_tokens)?;
			let lexer_tokens = expect(Token::TokenCloseParenthesis, lexer_tokens)?;
			Ok((exp, lexer_tokens))
		}
		_ => Err(fail())
	}
}

// <unop> ::= "-" | "~"
fn parse_unary_operator(lexer_tokens : &[Token]) -> io::Result<(UnaryOperator, &[Token])> {
	match &lexer_tokens[0] {
		Token::TokenHyphen => Ok((UnaryOperator::Negate, &lexer_tokens[1..])),
		Token::TokenTilde => Ok((UnaryOperator::Complement, &lexer_tokens[1..])),
		_ => Err(fail())
	}
}

fn expect(expected: Token, lexer_tokens: &[Token]) -> io::Result<&[Token]> {
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

pub fn run_parser(lexer_tokens : &[Token]) -> io::Result<AbstractSyntaxTree> {
	let program = parse_program(lexer_tokens)?;
	Ok(AbstractSyntaxTree::Program(program))
}
