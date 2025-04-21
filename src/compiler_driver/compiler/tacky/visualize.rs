use crate::compiler_driver::compiler::{tacky, visualize};

impl visualize::Visualizer for tacky::TackyAbstractSyntaxTree {
	fn visualize(&self, _depth: u8) -> String {
		let tacky::TackyAbstractSyntaxTree::Program(program) = self;
		program.visualize(0)
	}
}

impl visualize::Visualizer for tacky::Program
{
	fn visualize(&self, depth : u8) -> String
	{
		let tacky::Program::Program(function_definition) = self;
		format!(
		"Program(\n\
		{}\n\
		)", function_definition.visualize(depth + 1))
	}
}

impl visualize::Visualizer for tacky::FunctionDefinition
{
	fn visualize(&self, depth : u8) -> String
	{
		let tacky::FunctionDefinition::Function{identifier, instructions} = self;
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

impl visualize::Visualizer for tacky::Instruction
{
	fn visualize(&self, depth : u8) -> String
	{
		match self
		{
			tacky::Instruction::Return(val) => format!("Return({})", val.visualize(depth + 1)),
			tacky::Instruction::Unary{ unary_operator, src, dst } => {
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

impl visualize::Visualizer for tacky::UnaryOperator
{
	fn visualize(&self, _depth : u8) -> String
	{
		match self
		{
			tacky::UnaryOperator::Complement => String::from("Complement"),
			tacky::UnaryOperator::Negate => String::from("Negate"),
		}
	}
}

impl visualize::Visualizer for tacky::Val
{
	fn visualize(&self, _depth : u8) -> String
	{
		match self
		{
			tacky::Val::Constant(value) => format!("Constant({value})"),
			tacky::Val::Var(value) => String::from(format!("Var(\"{value}\")")),
		}
	}
}
