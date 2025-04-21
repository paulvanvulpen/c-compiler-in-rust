use crate::compiler_driver::compiler::{parser, visualize};

impl visualize::Visualizer for parser::Program
{
	fn visualize(&self, depth : u8) -> String
	{
		let parser::Program::Program(function_definition) = self;
		String::from(format!(
		"Program(\n\
		{}\n\
		)", function_definition.visualize(depth + 1)))
	}
}

impl visualize::Visualizer for parser::AbstractSyntaxTree
{
	fn visualize(&self, _depth : u8) -> String
	{
		let parser::AbstractSyntaxTree::Program(program) = self;
		program.visualize(0)
	}
}

impl visualize::Visualizer for parser::FunctionDefinition
{
	fn visualize(&self, depth : u8) -> String
	{
		let parser::FunctionDefinition::Function { identifier, body } = self;
		let prefix = "    ".repeat(depth as usize);
		format!(
			"{prefix}Function(\n\
			{prefix}    name={identifier}\n\
			{prefix}    body={}\n\
			{prefix})", body.visualize(depth + 1)
		)
	}
}

impl visualize::Visualizer for parser::Statement
{
	fn visualize(&self, depth : u8) -> String
	{
		let parser::Statement::Return(expression) = self;
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

impl visualize::Visualizer for parser::Expression {
	fn visualize(&self, depth : u8) -> String {
		match self {
			parser::Expression::Constant(value) => {
				format!("Constant({value})")
			},
			parser::Expression::Unary(unary_operator, boxed_expression) => {
				format!("{}{}", unary_operator.visualize(depth + 1), boxed_expression.visualize(depth + 1))
			},
		}
	}
}

impl visualize::Visualizer for parser::UnaryOperator {
	fn visualize(&self, _depth: u8) -> String
	{
		match self {
			parser::UnaryOperator::Complement => String::from("~"),
			parser::UnaryOperator::Negate => String::from("-"),
		}
	}
}