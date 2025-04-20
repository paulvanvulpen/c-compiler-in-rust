use super::{node, parser};

// Implementation AST Nodes in Zephyr Abstract Syntax Description Language (ASDL)
// program = Program (function_definition)
// function_definition = Function(identifier, instruction* body)
// instruction = Return(val) | Unary(unary_operator, val src, val dst)
// val = Constant(int) | Var(identifier)
// unary_operator = Complement | Negate
pub enum TackyAbstractSyntaxTree {
	Program(Program),
}

impl node::Visualizer for TackyAbstractSyntaxTree {
	fn visualize(&self, _depth: u8) -> String {
		let TackyAbstractSyntaxTree::Program(program) = self;
		program.visualize(0)
	}
}

pub enum Program {
	Program(FunctionDefinition),
}

impl node::Visualizer for Program
{
	fn visualize(&self, depth : u8) -> String
	{
		let Program::Program(function_definition) = self;
		format!(
		"Program(\n\
		{}\n\
		)", function_definition.visualize(depth + 1))
	}
}

pub enum FunctionDefinition {
	Function{ identifier : String, instructions : Vec<Instruction>},
}

impl node::Visualizer for FunctionDefinition
{
	fn visualize(&self, depth : u8) -> String
	{
		let FunctionDefinition::Function{identifier, instructions} = self;
			let prefix = "    ".repeat(depth as usize);
			let instructions_str = instructions.iter()
				.map(|instruction| instruction.visualize(depth + 1))
				.collect::<Vec<String>>()
				.join(format!("\n{prefix}        ").as_str());

			format!(
			"{prefix}Function(\n\
			{prefix}    name={identifier}\n\
			{prefix}    instructions=\n\
			{prefix}        {}\n\
			{prefix})", instructions_str
			)
	}
}

pub enum Instruction {
	Return(Val),
	Unary{unary_operator : UnaryOperator, src : Val, dst : Val},
}

impl node::Visualizer for Instruction
{
	fn visualize(&self, depth : u8) -> String
	{
		match self
		{
			Instruction::Return(val) => format!("Return({})", val.visualize(depth + 1)),
			Instruction::Unary{ unary_operator, src, dst } => {
				String::from(
					format!("Unary({}, {}, {})",
						unary_operator.visualize(depth + 1),
						src.visualize(depth + 1),
						dst.visualize(depth + 1),
					)
				)
			}
		}

	}
}

pub enum UnaryOperator {
	Complement,
	Negate,
}

impl node::Visualizer for UnaryOperator
{
	fn visualize(&self, _depth : u8) -> String
	{
		match self
		{
			UnaryOperator::Complement => String::from("Complement"),
			UnaryOperator::Negate => String::from("Negate"),
		}
	}
}

#[derive(Clone)]
pub enum Val {
	Constant(usize),
	Var(String)
}

impl node::Visualizer for Val
{
	fn visualize(&self, _depth : u8) -> String
	{
		match self
		{
			Val::Constant(value) => format!("Constant({value})"),
			Val::Var(value) => String::from(format!("Var(\"{value}\")")),
		}
	}
}

fn convert_unary_operator(unary_operator : parser::UnaryOperator) -> UnaryOperator {
	match unary_operator {
		parser::UnaryOperator::Complement => UnaryOperator::Complement,
		parser::UnaryOperator::Negate => UnaryOperator::Negate,
	}
}

static COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
fn make_temporary() -> String {
	let id = COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
	format!("tmp.{id}")
}

fn get_destination(instruction_ref : &Instruction) -> Val
{
	match instruction_ref {
		Instruction::Return(val) => {
			(*val).clone()
		}
		Instruction::Unary { dst, ..} => {
			(*dst).clone()
		},
	}
}
fn convert_boxed_expression(boxed_expression : Box<parser::Expression>) -> Vec<Instruction> {
	match *boxed_expression {
		parser::Expression::Constant(value) => {
			let mut instructions : Vec<Instruction> = vec![];
			instructions.push(Instruction::Return(Val::Constant(value)));
			instructions
		},
		parser::Expression::Unary(unary_operator, boxed_expression) => {
			let mut instructions = convert_boxed_expression(boxed_expression);
			let src : Val = match instructions.last() {
				Some(instruction_ref) => get_destination(instruction_ref),
				None => panic!("instruction is missing a valid destination"),
			};
			let dest_name = make_temporary();
			let dst = Val::Var(dest_name);
			let unary_operator = convert_unary_operator(unary_operator);
			instructions.push(Instruction::Unary { unary_operator, src, dst });
			instructions
		},
	}
}
fn convert_statement(statement : parser::Statement) -> Vec<Instruction> {
	match statement {
		parser::Statement::Return(expression) => {
			let mut instructions = convert_boxed_expression(Box::new(expression));
			let final_destination : Val = match instructions.last() {
				Some(instruction_ref) => get_destination(instruction_ref),
				None => panic!("instruction is missing a valid destination"),
			};
			instructions.remove(0);
			instructions.push(Instruction::Return(final_destination));
			instructions
		}
	}
}

fn convert_function_definition(function_definition : parser::FunctionDefinition) -> FunctionDefinition {
	match function_definition {
		parser::FunctionDefinition::Function{ identifier, body } => {
			FunctionDefinition::Function { identifier, instructions : convert_statement(body)}
		}
	}
}

fn convert_program(program : parser::Program) -> Program
{
	match program {
		parser::Program::Program(function_definition) => {
			Program::Program(convert_function_definition(function_definition))
		}
	}
}
fn convert_ast(ast : parser::AbstractSyntaxTree) -> TackyAbstractSyntaxTree {
	match ast {
		parser::AbstractSyntaxTree::Program(program) => {
			TackyAbstractSyntaxTree::Program(convert_program(program))
		}
	}
}

pub fn run_tacky_generator(ast: parser::AbstractSyntaxTree) -> std::io::Result<TackyAbstractSyntaxTree> {
	let tacky_ast = convert_ast(ast);
	Ok(tacky_ast)
}
