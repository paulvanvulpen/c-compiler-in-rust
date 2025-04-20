use std::collections::HashMap;
use super::{node, tacky};

// Implementation AST Nodes in Zephyr Abstract Syntax Description Language (ASDL)
// program = Program(function_definition)
// function_definition = Function(identifier name, instruction* instructions)
// instruction = Mov(operand src, operand dst)
//				| Unary(unary_operator, operand)
//				| AllocateStack(int)
//				| Ret
// unary_operator = Neg | Not
// operand = Imm(int) | Reg(reg) | Pseudo(identifier) | Stack(int)
// reg = AX | R10
pub enum AssemblyAbstractSyntaxTree
{
	Program(Program),
}

impl node::Visualizer for AssemblyAbstractSyntaxTree  {
	fn visualize(&self, _depth : u8) -> String
	{
		let AssemblyAbstractSyntaxTree::Program(program) = self;
		program.visualize(0)
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
	Function{ identifier : String, instructions : Vec<Instruction> }
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
	Mov(Operand, Operand),
	Unary(UnaryOperator, Operand),
	Ret
}

impl node::Visualizer for Instruction {
	fn visualize(&self, depth : u8) -> String {
		match self {
			Instruction::Mov(op1, op2) => format!("Mov({}, {})", op1.visualize(depth + 1), op2.visualize(depth + 1)),
			Instruction::Unary(unary_operator, dst) => format!("Unary({}, {})", unary_operator.visualize(depth + 1), dst.visualize(depth + 1)),
			Instruction::Ret => String::from("Ret")
		}

	}
}


pub enum UnaryOperator {
	Neg,
	Not,
}

impl node::Visualizer for UnaryOperator {
	fn visualize(&self, _depth : u8) -> String {
		match self {
			UnaryOperator::Neg => String::from("Neg"),
			UnaryOperator::Not => String::from("Not"),
		}
	}
}

pub enum Operand {
	Immediate(usize),
	Register(Register),
	Pseudo{ identifier : String},
	Stack{ offset: isize },
}

impl node::Visualizer for Operand {
	fn visualize(&self, depth : u8) -> String
	{
		match self
		{
			Operand::Immediate(value) => format!("Imm({value})"),
			Operand::Register(register) => register.visualize(depth + 1),
			Operand::Pseudo { identifier } => format!("Pseudo({identifier}"),
			Operand::Stack { offset } => format!("Stack({offset})"),
		}
	}
}

pub enum Register {
	AX,
	R10
}

impl node::Visualizer for Register {
	fn visualize(&self, _depth : u8) -> String {
		match self {
			Register::AX => String::from("Reg(AX)"),
			Register::R10 => String::from("Reg(R10)"),
		}
	}
}

fn convert_val(val : tacky::Val) -> Operand {
	match val {
		tacky::Val::Constant(value) => Operand::Immediate(value),
		tacky::Val::Var(identifier) => Operand::Pseudo { identifier },
	}
}

fn convert_unary_operator(unary_operator : tacky::UnaryOperator) -> UnaryOperator {
	match unary_operator {
		tacky::UnaryOperator::Complement => UnaryOperator::Not,
		tacky::UnaryOperator::Negate => UnaryOperator::Neg,
	}

}

fn convert_instruction(instruction : tacky::Instruction) -> Vec<Instruction> {
	match instruction {
		tacky::Instruction::Return(val) => {
			vec![
				Instruction::Mov(convert_val(val), Operand::Register(Register::AX)),
				Instruction::Ret
			]
		},
		tacky::Instruction::Unary { unary_operator, src, dst} => {
			vec![
				Instruction::Mov(convert_val(src), convert_val(dst.clone())),
				Instruction::Unary(convert_unary_operator(unary_operator), convert_val(dst))
			]
		}
	}
}

fn convert_function_definition(function_definition : tacky::FunctionDefinition) -> FunctionDefinition {
	match function_definition {
		tacky::FunctionDefinition::Function{ identifier, instructions } => {
			FunctionDefinition::Function { identifier, instructions: {
				instructions.into_iter()
					.flat_map(convert_instruction)
					.collect()
				}
			}
		}
	}
}

fn convert_program(program : tacky::Program) -> Program {
	match program {
		tacky::Program::Program(function_definition) => {
			Program::Program(convert_function_definition(function_definition))
		}
	}
}

fn convert_ast(ast : tacky::TackyAbstractSyntaxTree) -> AssemblyAbstractSyntaxTree {
	match ast {
		tacky::TackyAbstractSyntaxTree::Program(program) => {
			AssemblyAbstractSyntaxTree::Program(convert_program(program))
		}
	}
}
pub fn run_assembly_generator(tacky_ast: tacky::TackyAbstractSyntaxTree) -> std::io::Result<AssemblyAbstractSyntaxTree> {
	let assembly_ast = convert_ast(tacky_ast);
	Ok(assembly_ast)
}
pub fn replace_pseudo_registers(assembly_ast: &mut AssemblyAbstractSyntaxTree) -> isize {
	let mut temporary_to_offset : HashMap<String, isize> = HashMap::new();
	let mut alloc_size : isize = 0;

	let AssemblyAbstractSyntaxTree::Program(Program::Program(FunctionDefinition::Function { instructions, ..})) = assembly_ast;

	let mut replace_pseudo_with_stack = |operand: &mut Operand| {
		if let Operand::Pseudo { identifier } = operand {
			let id = std::mem::take(identifier);
			let offset = temporary_to_offset.entry(id).or_insert_with(|| {
				alloc_size += 4;
				-alloc_size
			});
			*operand = Operand::Stack { offset: *offset };
		}
	};

	for instruction in instructions.iter_mut() {
		match instruction {
			Instruction::Unary(.., operand) => {
				replace_pseudo_with_stack(operand);
			},
			Instruction::Mov(op1, op2) => {
				replace_pseudo_with_stack(op1);
				replace_pseudo_with_stack(op2);
			},
			Instruction::Ret => {}
		}
	}
	alloc_size
}
