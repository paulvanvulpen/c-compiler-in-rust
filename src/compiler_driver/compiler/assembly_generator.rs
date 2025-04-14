use super::parser;
use super::node;

pub enum AssemblyAbstractSyntaxTree
{
	Program(Program),
}

impl node::Visualizer for AssemblyAbstractSyntaxTree
{
	fn visualize(&self, depth : u8) -> String
	{
		if let AssemblyAbstractSyntaxTree::Program(program) = self
		{
			return program.visualize(0);
		}

		String::new()
	}
}

// <program>
pub enum Program
{
	Program(FunctionDefinition)
}

impl node::Visualizer for Program
{
	fn visualize(&self, depth : u8) -> String
	{
		if let Program::Program(function_definition) = self
		{
			return String::from(format!(
			"Program(\n\
			{}\n\
			)", function_definition.visualize(depth + 1)))
		}

		String::new()
	}
}

// <function>
pub enum FunctionDefinition
{
	Function{ identifier : String, instructions : Vec<Instruction> }
}

impl node::Visualizer for FunctionDefinition
{
	fn visualize(&self, depth : u8) -> String
	{
		if let FunctionDefinition::Function{identifier, instructions} = self
		{
			let prefix = "    ".repeat(depth as usize);
			let instructions_str = instructions.iter()
			.map(|instruction| instruction.visualize(depth + 1))
			.collect::<Vec<String>>()
			.join(format!("\n{prefix}        ").as_str());

			return format!(
				"{prefix}Function(\n\
				{prefix}    name={identifier}\n\
				{prefix}    instructions=\n\
				{prefix}        {}\n\
				{prefix})", instructions_str
			);
		}

		String::new()
	}
}

pub enum Instruction
{
	Mov(Operand, Operand),
	Ret
}

impl node::Visualizer for Instruction
{
	fn visualize(&self, depth : u8) -> String
	{
		match self
		{
			Instruction::Mov(op1, op2) => format!("Mov({}, {})", op1.visualize(depth + 1), op2.visualize(depth + 1)),
			Instruction::Ret => String::from("Ret")
		}

	}
}

pub enum Operand
{
	Immediate(usize),
	Register(Register)
}

impl node::Visualizer for Operand
{
	fn visualize(&self, depth : u8) -> String
	{
		match self
		{
			Operand::Immediate(value) => format!("Imm({value})"),
			Operand::Register(register) => register.visualize(depth + 1)
		}
	}
}

pub enum Register
{
	EAX,
}

impl node::Visualizer for Register
{
	fn visualize(&self, depth : u8) -> String
	{
		String::from("EAX")
	}
}

fn convert_expression(expression : parser::Expression) -> Operand {
    match expression {
        parser::Expression::Constant(constant) => Operand::Immediate(constant),
    }
}

fn convert_statement(statement : parser::Statement) -> Vec<Instruction> {
    match statement {
        parser::Statement::Return(expression) => {
            vec![
                Instruction::Mov(convert_expression(expression), Operand::Register(Register::EAX)),
                Instruction::Ret
            ]
        }
    }
}

fn convert_function_definition(function_definition : parser::FunctionDefinition) -> FunctionDefinition {
    match function_definition {
        parser::FunctionDefinition::Function{ identifier, body } => {
            FunctionDefinition::Function { identifier, instructions: convert_statement(body) }
        }
    }
}

fn convert_program(program : parser::Program) -> Program {
    match program {
        parser::Program::Program(function_definition) => {
            Program::Program(convert_function_definition(function_definition))
        }
    }
}

fn convert_ast(ast : parser::AbstractSyntaxTree) -> AssemblyAbstractSyntaxTree {
	match ast {
		parser::AbstractSyntaxTree::Program(program) => {
			AssemblyAbstractSyntaxTree::Program(convert_program(program))
		}
	}
}
pub fn run_assembly_generator(ast: parser::AbstractSyntaxTree) -> std::io::Result<AssemblyAbstractSyntaxTree>
{
	let assembly_ast = convert_ast(ast);
	Ok(assembly_ast)
}
