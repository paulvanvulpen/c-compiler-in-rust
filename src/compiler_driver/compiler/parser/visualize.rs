use crate::compiler_driver::compiler::{parser, visualize};

impl visualize::Visualizer for parser::Program {
    fn visualize(&self, depth: u8) -> String {
        let parser::Program::Program(function_definition) = self;
        format!(
            "Program(\n\
        {}\n\
        )",
            function_definition.visualize(depth + 1)
        )
    }
}

impl visualize::Visualizer for parser::AbstractSyntaxTree {
    fn visualize(&self, _depth: u8) -> String {
        let parser::AbstractSyntaxTree::Program(program) = self;
        program.visualize(0)
    }
}

impl visualize::Visualizer for parser::FunctionDefinition {
    fn visualize(&self, depth: u8) -> String {
        let parser::FunctionDefinition::Function { identifier, body } = self;
        let indent = "    ";
        let prefix = indent.repeat(depth as usize);
        format!(
            "{prefix}Function(name={identifier}, body=\n\
            {}\n\
            {prefix})",
            body.visualize(depth + 1)
        )
    }
}

impl visualize::Visualizer for parser::Statement {
    fn visualize(&self, depth: u8) -> String {
        let parser::Statement::Return(expression) = self;
        {
            let indent = "    ";
            let prefix = indent.repeat(depth as usize);
            format!(
                "{prefix}Return(\n\
                {}\n\
                {prefix})",
                expression.visualize(depth + 1)
            )
        }
    }
}

impl visualize::Visualizer for parser::Expression {
    fn visualize(&self, depth: u8) -> String {
        let prefix = "    ".repeat(depth as usize);
        match self {
            parser::Expression::Constant(value) => {
                format!("{prefix}Constant({value})")
            }
            parser::Expression::Unary(unary_operator, boxed_expression) => {
                format!(
                    "{prefix}{}{}",
                    unary_operator.visualize(depth + 1),
                    boxed_expression.visualize(depth + 1)
                )
            }
            parser::Expression::BinaryOperation {
                binary_operator,
                left_operand,
                right_operand,
            } => {
                let prefix = "    ".repeat(depth as usize);
                format!(
                    "{prefix}BinaryOperation({}:\n\
                    {},\n\
                    {}\n\
                    {prefix})",
                    binary_operator.visualize(depth + 1),
                    left_operand.visualize(depth + 1),
                    right_operand.visualize(depth + 1),
                )
            }
        }
    }
}

impl visualize::Visualizer for parser::BinaryOperator {
    fn visualize(&self, _depth: u8) -> String {
        match self {
            parser::BinaryOperator::Add => String::from("Add"),
            parser::BinaryOperator::Subtract => String::from("Subtract"),
            parser::BinaryOperator::Multiply => String::from("Multiply"),
            parser::BinaryOperator::Divide => String::from("Divide"),
            parser::BinaryOperator::Remainder => String::from("Remainder"),
            parser::BinaryOperator::LeftShift => String::from("LeftShift"),
            parser::BinaryOperator::RightShift => String::from("RightShift"),
            parser::BinaryOperator::BitwiseAnd => String::from("BitwiseAnd"),
            parser::BinaryOperator::BitwiseXOr => String::from("BitwiseXOr"),
            parser::BinaryOperator::BitwiseOr => String::from("BitwiseOr"),
        }
    }
}

impl visualize::Visualizer for parser::UnaryOperator {
    fn visualize(&self, _depth: u8) -> String {
        match self {
            parser::UnaryOperator::Complement => String::from("~"),
            parser::UnaryOperator::Negate => String::from("-"),
        }
    }
}
